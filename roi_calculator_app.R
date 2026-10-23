library(shiny)
library(duckdb)
library(DBI)
library(ggplot2)
library(plotly)
library(dplyr)

con <- dbConnect(duckdb(), "svh.duckdb", read_only = TRUE)
onStop(function() dbDisconnect(con, shutdown = TRUE))

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

message("Building dataset...")

ed_data <- dbGetQuery(con, "
  SELECT PatientDurableKey,
         SUM(CASE WHEN IsEdVisit = 1 THEN 1 ELSE 0 END) AS n_ed_visits,
         MIN(CAST(Date AS DATE)) AS first_encounter,
         MAX(CAST(Date AS DATE)) AS last_encounter
  FROM encounters
  WHERE Date IS NOT NULL
  GROUP BY PatientDurableKey
")
ed_data$years_observed <- pmax(
  as.numeric(as.Date(ed_data$last_encounter) - as.Date(ed_data$first_encounter)) / 365.25, 0.25)
ed_data$ed_per_year <- ed_data$n_ed_visits / ed_data$years_observed

sdoh_raw <- dbGetQuery(con, "
  SELECT PatientDurableKey, Domain, AnswerText
  FROM social_determinants
  WHERE Domain IN ('Financial Resource Strain', 'Housing Stability', 'Transportation Needs')
    AND AnswerText NOT IN ('Patient declined', 'Patient unable to answer', '*Unknown')
")

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

# Per-domain stats
domain_stats <- sdoh_patient |>
  dplyr::filter(screen == "Positive") |>
  dplyr::inner_join(ed_data, by = "PatientDurableKey") |>
  dplyr::group_by(Domain) |>
  dplyr::summarise(
    n_patients = dplyr::n(),
    mean_ed_per_year = mean(ed_per_year, na.rm = TRUE),
    median_ed_per_year = median(ed_per_year, na.rm = TRUE),
    total_ed_per_year = sum(ed_per_year, na.rm = TRUE),
    .groups = "drop"
  )

# "Any domain" option (deduped)
any_domain_patients <- sdoh_patient |>
  dplyr::filter(screen == "Positive") |>
  dplyr::distinct(PatientDurableKey) |>
  dplyr::inner_join(ed_data, by = "PatientDurableKey")

any_domain_row <- data.frame(
  Domain = "Any of the 3 domains",
  n_patients = nrow(any_domain_patients),
  mean_ed_per_year = mean(any_domain_patients$ed_per_year, na.rm = TRUE),
  median_ed_per_year = median(any_domain_patients$ed_per_year, na.rm = TRUE),
  total_ed_per_year = sum(any_domain_patients$ed_per_year, na.rm = TRUE)
)

domain_stats <- rbind(domain_stats, any_domain_row)

# Helper to get patient pool for a given domain
get_pool <- function(domain) {
  if (domain == "Any of the 3 domains") {
    return(any_domain_patients)
  }
  sdoh_patient |>
    dplyr::filter(Domain == domain, screen == "Positive") |>
    dplyr::inner_join(ed_data, by = "PatientDurableKey")
}

ui <- fluidPage(
  titlePanel("SDoH intervention ROI calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("domain", "Focus on which SDoH domain?",
                  choices = c("Financial Resource Strain",
                              "Housing Stability",
                              "Transportation Needs",
                              "Any of the 3 domains"),
                  selected = "Housing Stability"),
      checkboxInput("target_high", "Target only high-ED users (top 25%)", value = FALSE),
      helpText("Realistic programs focus on patients most likely to benefit."),
      hr(),
      h4("Intervention assumptions"),
      sliderInput("reduction_pct", "ED visit reduction (%)",
                  min = 10, max = 50, value = 35, step = 5, post = "%"),
      helpText("RCT median: 35%"),
      sliderInput("cost_per_visit", "Hospital cost per ED visit ($)",
                  min = 500, max = 3000, value = 1200, step = 100, pre = "$"),
      numericInput("chw_cost", "Annual cost per CHW ($)",
                   value = 75000, min = 40000, max = 120000, step = 5000),
      numericInput("caseload", "Patients per CHW",
                   value = 120, min = 50, max = 250),
      sliderInput("reach_pct", "% of target patients reached",
                  min = 25, max = 100, value = 60, step = 5, post = "%"),
      width = 3
    ),
    mainPanel(
      h3("Per-domain comparison"),
      p("Before modeling one intervention, compare how each domain looks:"),
      tableOutput("domain_comparison"),
      h3(textOutput("selected_header")),
      uiOutput("headline"),
      h3("Financial projection"),
      plotlyOutput("roi_chart", height = "400px"),
      h3("Detailed breakdown"),
      tableOutput("breakdown"),
      h3("Sensitivity analysis"),
      p("Net savings across different reduction and reach scenarios, using your current cost assumptions."),
      plotlyOutput("sensitivity", height = "450px"),
      h3("Evidence base and citations"),
      uiOutput("citations")
    )
  )
)

server <- function(input, output, session) {
  
  # Current targeted pool based on UI selections
  target_pool <- reactive({
    pool <- get_pool(input$domain)
    if (input$target_high) {
      threshold <- quantile(pool$ed_per_year, 0.75, na.rm = TRUE)
      pool <- pool[pool$ed_per_year >= threshold, ]
    }
    pool
  })
  
  projection <- reactive({
    pool <- target_pool()
    n_target <- nrow(pool)
    total_ed <- sum(pool$ed_per_year, na.rm = TRUE)
    
    n_reached <- round(n_target * input$reach_pct / 100)
    baseline_visits <- total_ed * input$reach_pct / 100
    visits_avoided <- baseline_visits * input$reduction_pct / 100
    savings <- visits_avoided * input$cost_per_visit
    
    chws_needed <- ceiling(n_reached / input$caseload)
    program_cost <- chws_needed * input$chw_cost
    
    list(
      n_target = n_target, n_reached = n_reached,
      baseline_visits = baseline_visits, visits_avoided = visits_avoided,
      savings = savings, chws_needed = chws_needed,
      program_cost = program_cost, net = savings - program_cost,
      roi = if (program_cost > 0) savings / program_cost else NA
    )
  })
  
  output$domain_comparison <- renderTable({
    domain_stats |>
      dplyr::mutate(
        n_patients = format(n_patients, big.mark = ","),
        mean_ed_per_year = sprintf("%.2f", mean_ed_per_year),
        median_ed_per_year = sprintf("%.2f", median_ed_per_year),
        total_ed_per_year = format(round(total_ed_per_year), big.mark = ",")
      ) |>
      dplyr::rename(
        "Patients" = n_patients,
        "Mean ED/yr" = mean_ed_per_year,
        "Median ED/yr" = median_ed_per_year,
        "Total ED/yr" = total_ed_per_year
      )
  })
  
  output$selected_header <- renderText({
    paste0("Projection for: ", input$domain,
           if (input$target_high) " (top 25% ED users only)" else "")
  })
  
  output$headline <- renderUI({
    p <- projection()
    tagList(
      tags$p(style = "font-size: 17px;",
             sprintf("Reaching %s%% of %s target patients with a %s%% ED reduction:",
                     input$reach_pct, format(p$n_target, big.mark = ","),
                     input$reduction_pct)),
      tags$ul(
        tags$li(sprintf("ED visits avoided: %s/year", format(round(p$visits_avoided), big.mark = ","))),
        tags$li(sprintf("Gross savings: $%s", format(round(p$savings), big.mark = ","))),
        tags$li(sprintf("Program cost (%d CHWs): $%s", p$chws_needed, format(p$program_cost, big.mark = ","))),
        tags$li(tags$strong(
          sprintf("Net: $%s (ROI: %.2fx)",
                  format(round(p$net), big.mark = ","), p$roi),
          style = if (p$net > 0) "color: #1D9E75;" else "color: #D85A30;"))
      )
    )
  })
  
  output$roi_chart <- renderPlotly({
    p <- projection()
    df <- data.frame(
      category = factor(c("Savings from avoided ED visits", "CHW program cost"),
                        levels = c("Savings from avoided ED visits", "CHW program cost")),
      value = c(p$savings, -p$program_cost),
      display = c(p$savings, p$program_cost)
    )
    df$label <- paste0("$", format(round(df$display), big.mark = ","))
    
    plot <- ggplot(df, aes(x = category, y = value, fill = category,
                           text = paste0(category, "<br>$", format(round(display), big.mark = ",")))) +
      geom_col() +
      geom_text(aes(label = label, y = ifelse(value > 0, value * 1.05, value * 1.15)), size = 4) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      scale_fill_manual(values = c("Savings from avoided ED visits" = "#1D9E75",
                                   "CHW program cost" = "#D85A30"), guide = "none") +
      scale_y_continuous(labels = function(x) paste0("$", format(x / 1e6, nsmall = 1), "M")) +
      labs(x = NULL, y = "Annual dollars") +
      theme_minimal(base_size = 13)
    ggplotly(plot, tooltip = "text")
  })
  
  output$breakdown <- renderTable({
    p <- projection()
    data.frame(
      Line = c("Target patients identified", "Reached by program",
               "Baseline ED visits (this group)", "ED visits avoided",
               "Gross savings", "CHWs needed", "Program cost",
               "Net savings", "ROI"),
      Value = c(format(p$n_target, big.mark = ","),
                format(p$n_reached, big.mark = ","),
                format(round(p$baseline_visits), big.mark = ","),
                format(round(p$visits_avoided), big.mark = ","),
                paste0("$", format(round(p$savings), big.mark = ",")),
                p$chws_needed,
                paste0("$", format(p$program_cost, big.mark = ",")),
                paste0("$", format(round(p$net), big.mark = ",")),
                sprintf("%.2fx", p$roi))
    )
  }, colnames = FALSE, striped = TRUE)
  
  output$sensitivity <- renderPlotly({
    pool <- target_pool()
    total_ed <- sum(pool$ed_per_year, na.rm = TRUE)
    n_target <- nrow(pool)
    
    grid <- expand.grid(reduction = seq(10, 50, by = 5), reach = seq(30, 100, by = 10))
    grid$net_savings <- mapply(function(red, rea) {
      n_reached <- n_target * rea / 100
      savings <- total_ed * rea / 100 * red / 100 * input$cost_per_visit
      cost <- ceiling(n_reached / input$caseload) * input$chw_cost
      savings - cost
    }, grid$reduction, grid$reach)
    
    p <- ggplot(grid, aes(x = reduction, y = reach, fill = net_savings / 1e6,
                          text = paste0("Reduction: ", reduction, "%<br>",
                                        "Reach: ", reach, "%<br>",
                                        "Net: $", format(round(net_savings), big.mark = ",")))) +
      geom_tile() +
      scale_fill_gradient2(low = "#E24B4A", mid = "#F1EFE8", high = "#1D9E75",
                           midpoint = 0, name = "Net ($M)") +
      labs(x = "ED reduction (%)", y = "% reached") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })
  
  output$citations <- renderUI({
    tagList(
      tags$h4("ED visit reduction estimates"),
      tags$ul(
        tags$li(tags$strong("Jack et al. (2017),"), " Journal of General Internal Medicine. Systematic review of 34 US CHW intervention studies for chronic disease. ",
                "Found significant ED reductions of 23-51% (median ~35%) in 14 studies. ",
                tags$a(href="https://pmc.ncbi.nlm.nih.gov/articles/PMC5331010/", "Link", target="_blank")),
        tags$li(tags$strong("Kangovi et al. (2020),"), " Health Affairs. IMPaCT RCT at Penn Medicine for socially-at-risk Medicaid patients. ",
                "Returned $2.47 per $1 invested within fiscal year. ",
                tags$a(href="https://www.healthaffairs.org/doi/10.1377/hlthaff.2019.00981", "Link", target="_blank")),
        tags$li(tags$strong("Commonwealth Fund ROI Evidence Review (2022)."), " Housing interventions: 28-70% ED reduction (median 36%). Medically tailored meals: median 44% inpatient reduction. ",
                tags$a(href="https://www.commonwealthfund.org/sites/default/files/2022-09/ROI_calculator_evidence_review_2022_update_Sept_2022.pdf", "Link", target="_blank"))
      ),
      tags$h4("ED visit cost estimates"),
      tags$ul(
        tags$li(tags$strong("AHRQ HCUP Statistical Brief #268 (2017)."), " Hospital cost-to-charge ratios applied to Nationwide ED Sample yielded median ED treatment costs around $530 for treat-and-release visits. ",
                tags$a(href="https://hcup-us.ahrq.gov/reports/statbriefs/sb268-ED-Costs-2017.jsp", "Link", target="_blank")),
        tags$li(tags$strong("UnitedHealthcare 2023 median allowed amounts:"), " ~$1,700 per in-network ED visit. Recent private claims analyses show averages around $2,700 for uninsured visits."),
        tags$li(tags$strong("Default $1,200 used here"), " represents a midpoint between AHRQ direct facility cost and typical negotiated rates.")
      ),
      tags$h4("CHW caseload and compensation"),
      tags$ul(
        tags$li(tags$strong("Penn IMPaCT program:"), " CHWs manage roughly 30-40 patients per 6-month cycle, supporting 100-150 active patients annually with onboarding/graduation churn."),
        tags$li(tags$strong("CHW compensation (BLS, 2024):"), " Median community health worker wage ~$48,000/year. With benefits and overhead, all-in cost typically $65,000-$80,000.")
      ),
      tags$h4("SDoH → ED utilization (foundation of this analysis)"),
      tags$ul(
        tags$li(tags$strong("Davis et al. (2020),"), " Am J Emerg Med. VHA study of 293,872 patients: adverse SDoH showed strong positive association with ED use after controlling for demographics and comorbidities. ",
                tags$a(href="https://pubmed.ncbi.nlm.nih.gov/32739860/", "Link", target="_blank")),
        tags$li(tags$strong("Moscrop et al. (2021),"), " DC Medicaid study of 8,943 beneficiaries: income, homelessness, and health literacy independently predicted frequent ED use. ",
                tags$a(href="https://pmc.ncbi.nlm.nih.gov/articles/PMC9228973/", "Link", target="_blank")),
        tags$li(tags$strong("Berkowitz et al. (2023),"), " national NHIS analysis of 82,364 adults: six social risk categories all predicted ED use independently. ",
                tags$a(href="https://pmc.ncbi.nlm.nih.gov/articles/PMC11931704/", "Link", target="_blank"))
      )
    )
  })
}

shinyApp(ui, server)