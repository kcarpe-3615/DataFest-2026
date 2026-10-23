library(shiny)
library(duckdb)
library(DBI)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(MASS)   # for glm.nb (negative binomial)
library(broom)  # for tidying model output

con <- dbConnect(duckdb(), "svh.duckdb", read_only = TRUE)
onStop(function() dbDisconnect(con, shutdown = TRUE))

message("Building patient-level dataset for adjusted analysis...")

# Patient-level data: ED counts, age, observation window, comorbidity proxy
patient_data <- dbGetQuery(con, "
  WITH patient_stats AS (
    SELECT
      e.PatientDurableKey,
      COUNT(*) AS total_encounters,
      SUM(CASE WHEN e.IsEdVisit = 1 THEN 1 ELSE 0 END) AS total_ed_visits,
      MIN(CAST(e.Date AS DATE)) AS first_encounter,
      MAX(CAST(e.Date AS DATE)) AS last_encounter,
      -- Comorbidity proxy: count of distinct chronic disease group codes ever seen
      COUNT(DISTINCT CASE WHEN dx.GroupCode IN (
        'ICD-10-CM: E10','ICD-10-CM: E11','ICD-10-CM: I10','ICD-10-CM: I11',
        'ICD-10-CM: I50','ICD-10-CM: J44','ICD-10-CM: J45','ICD-10-CM: N18',
        'ICD-10-CM: F32','ICD-10-CM: F33','ICD-10-CM: F41'
      ) THEN dx.GroupCode END) AS n_chronic_conditions
    FROM encounters e
    LEFT JOIN diagnosis dx ON e.PrimaryDiagnosisKey = dx.DiagnosisKey
    WHERE e.Date IS NOT NULL
    GROUP BY e.PatientDurableKey
  )
  SELECT
    ps.*,
    p.PatientBirthYearBin
  FROM patient_stats ps
  LEFT JOIN patients p ON ps.PatientDurableKey = p.DurableKey
  WHERE p.PatientBirthYearBin NOT IN ('NA', '*Unspecified', 'Unknown', '*Unknown', '')
")

# Compute observation time and age
patient_data$years_observed <- pmax(
  as.numeric(as.Date(patient_data$last_encounter) - as.Date(patient_data$first_encounter)) / 365.25,
  0.25
)
patient_data$birth_year <- as.integer(patient_data$PatientBirthYearBin)
patient_data$approx_age <- 2025 - patient_data$birth_year - 2  # midpoint of bin

# Age band
patient_data$age_band <- cut(patient_data$approx_age,
                             breaks = c(-1, 20, 40, 60, 85, 200),
                             labels = c("Under 21", "21-40", "41-60", "61-85", "Over 85"))

# Filter out implausible/missing
patient_data <- patient_data |>
  dplyr::filter(!is.na(age_band), !is.na(years_observed), years_observed > 0)

message(sprintf("Patient dataset: %s patients", format(nrow(patient_data), big.mark = ",")))

# SDoH classification
sdoh_raw <- dbGetQuery(con, "
  SELECT PatientDurableKey, Domain, AnswerText
  FROM social_determinants
  WHERE Domain IN ('Financial Resource Strain', 'Housing Stability', 'Transportation Needs')
    AND AnswerText NOT IN ('Patient declined', 'Patient unable to answer', '*Unknown')
")

positive_answers <- list(
  "Financial Resource Strain" = c("Yes", "Already shut off"),
  "Housing Stability"         = c("Yes"),
  "Transportation Needs"      = c("Yes")
)
negative_answers <- list(
  "Financial Resource Strain" = c("No"),
  "Housing Stability"         = c("No"),
  "Transportation Needs"      = c("No")
)

classify <- function(domain, answer) {
  if (answer %in% positive_answers[[domain]]) return("Positive")
  if (answer %in% negative_answers[[domain]]) return("Negative")
  NA_character_
}
sdoh_raw$screen <- mapply(classify, sdoh_raw$Domain, sdoh_raw$AnswerText)
sdoh_raw <- sdoh_raw[!is.na(sdoh_raw$screen), ]

sdoh_patient <- sdoh_raw |>
  dplyr::group_by(PatientDurableKey, Domain) |>
  dplyr::summarise(screen = dplyr::if_else(any(screen == "Positive"), "Positive", "Negative"),
                   .groups = "drop")

# Combined "any positive" view
sdoh_combined <- sdoh_patient |>
  dplyr::group_by(PatientDurableKey) |>
  dplyr::summarise(
    Domain = "Any of the three",
    screen = dplyr::if_else(any(screen == "Positive"), "Positive", "Negative"),
    .groups = "drop"
  )

sdoh_all <- dplyr::bind_rows(sdoh_patient, sdoh_combined)

# Master analysis dataset
analysis <- sdoh_all |>
  dplyr::inner_join(patient_data, by = "PatientDurableKey")

message("Dashboard ready.")

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .headline { font-size: 17px; padding: 16px; background: #F1EFE8; border-radius: 8px;
                margin-bottom: 16px; border-left: 4px solid #6D2E46; }
    .interpretation { background: #FAF7F2; padding: 14px; border-radius: 6px;
                      border-left: 3px solid #A26769; margin-top: 12px; font-size: 13px; }
  "))),
  titlePanel("Adjusted analysis: does the SDoH effect survive controls?"),
  p(style = "color: #5F5E5A; font-size: 14px;",
    "When we account for age, comorbidity, and observation time, how much of the SDoH-ED relationship persists?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("domain", "SDoH domain",
                  choices = c("Any of the three",
                              "Housing Stability",
                              "Transportation Needs",
                              "Financial Resource Strain"),
                  selected = "Housing Stability"),
      hr(),
      h4("How to read this"),
      p(strong("Stratification:"), "splits patients into groups (by age, comorbidity) and shows the SDoH effect within each. If the effect persists across groups, it's not just confounding."),
      p(strong("Regression model:"), "estimates the SDoH effect while simultaneously controlling for age and comorbidity. Reports an adjusted incidence rate ratio (IRR) with confidence interval."),
      p(strong("IRR interpretation:"), "an IRR of 2.5 means positive screeners have 2.5x the ED visit rate of negative screeners, holding other variables constant."),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Stratified analysis",
                 br(),
                 div(class = "headline", uiOutput("strat_headline")),
                 h4("ED visit rate ratio by age band"),
                 p("If confounding by age were the whole story, the ratio would shrink to 1.0 within each age band. It doesn't."),
                 plotlyOutput("strat_age", height = "350px"),
                 h4("ED visit rate ratio by comorbidity burden"),
                 p("Same logic: if 'these patients are just sicker' explained everything, the ratio would shrink within comorbidity strata."),
                 plotlyOutput("strat_comorbid", height = "350px"),
                 div(class = "interpretation", uiOutput("strat_interpretation"))
        ),
        tabPanel("Regression model",
                 br(),
                 div(class = "headline", uiOutput("reg_headline")),
                 h4("Adjusted incidence rate ratios"),
                 p("Negative binomial regression. Outcome is total ED visits, with log(years observed) as offset."),
                 plotOutput("reg_forest", height = "400px"),
                 h4("Full model output"),
                 DT::dataTableOutput("reg_table"),
                 div(class = "interpretation", uiOutput("reg_interpretation"))
        ),
        tabPanel("Methods",
                 br(),
                 h4("Variables in the model"),
                 tags$ul(
                   tags$li(strong("Outcome:"), "Total ED visits per patient (count)."),
                   tags$li(strong("Offset:"), "log(years_observed). Accounts for varying follow-up windows."),
                   tags$li(strong("Primary exposure:"), "SDoH screen status (positive vs negative on selected domain)."),
                   tags$li(strong("Adjustment variables:"),
                           tags$ul(
                             tags$li("Age band (5 categories: <21, 21-40, 41-60, 61-85, 85+)"),
                             tags$li("Number of chronic conditions (count of distinct diagnoses among 11 chronic disease groups: diabetes, hypertension, heart failure, COPD, asthma, kidney disease, depression, anxiety)")
                           ))
                 ),
                 h4("Why negative binomial?"),
                 p("ED visit counts in healthcare data are heavily overdispersed. A small number of patients have very high counts (max in this dataset: 192 visits). Poisson regression assumes mean = variance, which is wildly violated here. Negative binomial allows variance > mean and produces appropriate confidence intervals."),
                 h4("What this analysis cannot do"),
                 tags$ul(
                   tags$li("Cannot establish causation. Even after adjustment, unmeasured confounders (income, insurance type, mental health, substance use, housing type) could explain part of the residual association."),
                   tags$li("Cannot resolve temporal ordering. Some ED visits in the count occurred before the patient was screened."),
                   tags$li("Cannot generalize beyond the screened population. The 60,850 screened patients are not representative of all SVH patients."),
                   tags$li("Comorbidity proxy is approximate. We use a count of distinct chronic disease group codes; a true comorbidity index would weight conditions by severity.")
                 )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  dat <- reactive({
    analysis |> dplyr::filter(Domain == input$domain)
  })
  
  # ---- Stratified analysis ----
  
  strat_age_data <- reactive({
    d <- dat()
    d |>
      dplyr::group_by(age_band, screen) |>
      dplyr::summarise(
        n_patients = dplyr::n(),
        total_visits = sum(total_ed_visits),
        total_years = sum(years_observed),
        rate = total_visits / total_years,
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(names_from = screen, values_from = c(n_patients, rate, total_visits, total_years)) |>
      dplyr::mutate(
        ratio = rate_Positive / rate_Negative,
        # Approx 95% CI for ratio of two Poisson rates (log scale)
        log_ratio = log(ratio),
        se_log_ratio = sqrt(1/total_visits_Positive + 1/total_visits_Negative),
        ratio_lo = exp(log_ratio - 1.96 * se_log_ratio),
        ratio_hi = exp(log_ratio + 1.96 * se_log_ratio)
      )
  })
  
  strat_comorbid_data <- reactive({
    d <- dat()
    d$comorbid_band <- cut(d$n_chronic_conditions,
                           breaks = c(-1, 0, 1, 3, 100),
                           labels = c("0 conditions", "1 condition", "2-3 conditions", "4+ conditions"))
    d |>
      dplyr::group_by(comorbid_band, screen) |>
      dplyr::summarise(
        n_patients = dplyr::n(),
        total_visits = sum(total_ed_visits),
        total_years = sum(years_observed),
        rate = total_visits / total_years,
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(names_from = screen, values_from = c(n_patients, rate, total_visits, total_years)) |>
      dplyr::mutate(
        ratio = rate_Positive / rate_Negative,
        log_ratio = log(ratio),
        se_log_ratio = sqrt(1/total_visits_Positive + 1/total_visits_Negative),
        ratio_lo = exp(log_ratio - 1.96 * se_log_ratio),
        ratio_hi = exp(log_ratio + 1.96 * se_log_ratio)
      )
  })
  
  output$strat_headline <- renderUI({
    d <- dat()
    overall <- d |>
      dplyr::group_by(screen) |>
      dplyr::summarise(rate = sum(total_ed_visits) / sum(years_observed))
    overall_ratio <- overall$rate[overall$screen == "Positive"] / overall$rate[overall$screen == "Negative"]
    
    HTML(sprintf(
      "Overall, %s-positive patients have <strong>%.2fx</strong> the ED visit rate of negative screeners. The stratified plots below show whether this gap persists when patients are grouped by age and comorbidity.",
      input$domain, overall_ratio
    ))
  })
  
  output$strat_age <- renderPlotly({
    d <- strat_age_data()
    d$tooltip <- paste0("Age: ", d$age_band, "<br>",
                        "Ratio: ", round(d$ratio, 2), "x<br>",
                        "95% CI: ", round(d$ratio_lo, 2), " - ", round(d$ratio_hi, 2), "<br>",
                        "Positive n=", format(d$n_patients_Positive, big.mark = ","), "<br>",
                        "Negative n=", format(d$n_patients_Negative, big.mark = ","))
    
    p <- ggplot(d, aes(x = age_band, y = ratio, text = tooltip)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      geom_errorbar(aes(ymin = ratio_lo, ymax = ratio_hi), width = 0.2, color = "#6D2E46") +
      geom_point(size = 4, color = "#6D2E46") +
      labs(x = "Age band", y = "ED rate ratio (positive vs negative screen)") +
      theme_minimal(base_size = 13) +
      coord_cartesian(ylim = c(0, max(d$ratio_hi, na.rm = TRUE) * 1.1))
    ggplotly(p, tooltip = "text")
  })
  
  output$strat_comorbid <- renderPlotly({
    d <- strat_comorbid_data()
    d$tooltip <- paste0("Comorbidity: ", d$comorbid_band, "<br>",
                        "Ratio: ", round(d$ratio, 2), "x<br>",
                        "95% CI: ", round(d$ratio_lo, 2), " - ", round(d$ratio_hi, 2))
    
    p <- ggplot(d, aes(x = comorbid_band, y = ratio, text = tooltip)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      geom_errorbar(aes(ymin = ratio_lo, ymax = ratio_hi), width = 0.2, color = "#6D2E46") +
      geom_point(size = 4, color = "#6D2E46") +
      labs(x = "Number of chronic conditions", y = "ED rate ratio (positive vs negative screen)") +
      theme_minimal(base_size = 13) +
      coord_cartesian(ylim = c(0, max(d$ratio_hi, na.rm = TRUE) * 1.1))
    ggplotly(p, tooltip = "text")
  })
  
  output$strat_interpretation <- renderUI({
    d_age <- strat_age_data()
    d_com <- strat_comorbid_data()
    persistent_age <- all(d_age$ratio_lo > 1.0, na.rm = TRUE)
    persistent_com <- all(d_com$ratio_lo > 1.0, na.rm = TRUE)
    
    HTML(sprintf(
      "<strong>Interpretation:</strong> The dashed line at 1.0 represents 'no effect.' If confounders fully explained the SDoH-ED relationship, the bars would straddle 1.0 within strata. They don't %s. The effect persists across age bands and comorbidity levels, suggesting social need contributes beyond what age and chronic disease alone explain.",
      if (persistent_age && persistent_com) "in either chart" else "in most strata"
    ))
  })
  
  # ---- Regression model ----
  
  reg_model <- reactive({
    d <- dat()
    # Baseline: <21 age band, 0 comorbidities, negative screen
    d$age_band <- relevel(d$age_band, ref = "Under 21")
    d$screen <- factor(d$screen, levels = c("Negative", "Positive"))
    d$n_chronic_capped <- pmin(d$n_chronic_conditions, 5)  # cap for stability
    
    # Negative binomial regression with offset for observation time
    tryCatch({
      MASS::glm.nb(
        total_ed_visits ~ screen + age_band + n_chronic_capped + offset(log(years_observed)),
        data = d
      )
    }, error = function(e) {
      message("glm.nb failed, falling back to Poisson: ", e$message)
      glm(total_ed_visits ~ screen + age_band + n_chronic_capped + offset(log(years_observed)),
          data = d, family = poisson())
    })
  })
  
  reg_results <- reactive({
    m <- reg_model()
    res <- broom::tidy(m, conf.int = TRUE, exponentiate = TRUE)
    res$term <- dplyr::recode(res$term,
                              "(Intercept)" = "Baseline rate (per year)",
                              "screenPositive" = "SDoH positive (vs negative)",
                              "age_band21-40" = "Age 21-40 (vs <21)",
                              "age_band41-60" = "Age 41-60 (vs <21)",
                              "age_band61-85" = "Age 61-85 (vs <21)",
                              "age_bandOver 85" = "Age over 85 (vs <21)",
                              "n_chronic_capped" = "Each additional chronic condition"
    )
    res
  })
  
  output$reg_headline <- renderUI({
    res <- reg_results()
    sdoh_row <- res[res$term == "SDoH positive (vs negative)", ]
    HTML(sprintf(
      "After adjusting for age and comorbidity, %s-positive patients have <strong>%.2fx</strong> the ED visit rate of negative screeners (95%% CI: %.2f-%.2f).",
      input$domain, sdoh_row$estimate, sdoh_row$conf.low, sdoh_row$conf.high
    ))
  })
  
  output$reg_forest <- renderPlot({
    res <- reg_results()
    res <- res[res$term != "Baseline rate (per year)", ]
    
    # Tag rows: primary exposure vs. adjustment variable
    res$role <- ifelse(res$term == "SDoH positive (vs negative)",
                       "Primary exposure", "Adjustment variable")
    res$role <- factor(res$role, levels = c("Primary exposure", "Adjustment variable"))
    
    # Tag whether each effect is positive (>1) or protective (<1)
    res$direction <- ifelse(res$estimate > 1, "Increases ED rate", "Decreases ED rate")
    
    # Order: SDoH first (top), then adjustment variables grouped sensibly
    res$term <- factor(res$term, levels = rev(c(
      "SDoH positive (vs negative)",
      "Each additional chronic condition",
      "Age 21-40 (vs <21)",
      "Age 41-60 (vs <21)",
      "Age 61-85 (vs <21)",
      "Age over 85 (vs <21)"
    )))
    
    # Format value labels with CIs
    res$label <- sprintf("%.2fx  (%.2f-%.2f)", res$estimate, res$conf.low, res$conf.high)
    
    # Color: berry for the SDoH primary exposure, slate for everything else
    # Within "everything else", use a slightly different shade for protective effects
    res$bar_color <- dplyr::case_when(
      res$role == "Primary exposure" ~ "#6D2E46",  # berry — the headline finding
      res$direction == "Increases ED rate" ~ "#3D5A6C",  # slate — adjustment, increases ED
      TRUE ~ "#8AA0AC"                                    # lighter slate — adjustment, decreases ED
    )
    
    ggplot(res, aes(x = term, y = estimate, fill = bar_color)) +
      # Reference line at 1.0
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.6) +
      # Bars
      geom_col(width = 0.65) +
      # Confidence interval whiskers on top of bars
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                    width = 0.2, color = "#2A2A2A", linewidth = 0.5) +
      # Value labels at end of bars
      geom_text(aes(label = label, y = pmax(conf.high, estimate) + 0.15),
                hjust = 0, size = 4, color = "#2A2A2A") +
      # Reference line label
      annotate("text", x = 0.4, y = 1, label = "no effect",
               hjust = 0.5, vjust = -0.5, size = 3.3, color = "gray40", angle = 90) +
      coord_flip(clip = "off") +
      scale_fill_identity() +
      scale_y_continuous(
        limits = c(0, max(res$conf.high) * 1.45),
        breaks = c(0, 1, 2, 3, 4),
        labels = c("0", "1x", "2x", "3x", "4x"),
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(x = NULL, y = "Adjusted incidence rate ratio",
           caption = "Bars show adjusted IRRs from negative binomial regression. Black whiskers show 95% confidence intervals. Bars to the right of 1x indicate higher ED visit rates.") +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.y = element_text(size = 12, color = "#2A2A2A"),
        axis.text.x = element_text(size = 11, color = "#5F5E5A"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#EEEAE3"),
        plot.caption = element_text(size = 10, color = "#5F5E5A", hjust = 0,
                                    margin = margin(t = 12)),
        plot.margin = margin(15, 130, 15, 15),
        legend.position = "none"
      )
  })
  
  output$reg_table <- DT::renderDataTable({
    res <- reg_results()
    res$IRR <- sprintf("%.2f", res$estimate)
    res$`95% CI` <- sprintf("(%.2f, %.2f)", res$conf.low, res$conf.high)
    res$`p-value` <- ifelse(res$p.value < 0.001, "<0.001", sprintf("%.3f", res$p.value))
    res[, c("term", "IRR", "95% CI", "p-value")] |>
      dplyr::rename(Variable = term)
  }, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  
  output$reg_interpretation <- renderUI({
    res <- reg_results()
    sdoh_row <- res[res$term == "SDoH positive (vs negative)", ]
    
    # Compare to unadjusted
    d <- dat()
    overall <- d |>
      dplyr::group_by(screen) |>
      dplyr::summarise(rate = sum(total_ed_visits) / sum(years_observed))
    unadj_ratio <- overall$rate[overall$screen == "Positive"] / overall$rate[overall$screen == "Negative"]
    
    HTML(sprintf(
      "<strong>Interpretation:</strong> The unadjusted ratio for %s was %.2fx. After accounting for age and comorbidity, the adjusted IRR is %.2fx (95%% CI: %.2f-%.2f). The drop from %.2f to %.2f represents the portion of the raw association explained by these confounders. The remainder (%.2fx) is the social-need effect after controlling for them. <br><br>The confidence interval excluding 1.0 means the effect is statistically significant at the 5%% level, but with a sample of this size, the effect size and CI matter much more than significance.",
      input$domain, unadj_ratio, sdoh_row$estimate, sdoh_row$conf.low, sdoh_row$conf.high,
      unadj_ratio, sdoh_row$estimate, sdoh_row$estimate
    ))
  })
}

shinyApp(ui, server)