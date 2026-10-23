library(shiny)
library(duckdb)
library(DBI)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

con <- dbConnect(duckdb(), "../../svh.duckdb", read_only = TRUE)  # database lives in the repo root (see build_database.R)
onStop(function() dbDisconnect(con, shutdown = TRUE))

# ACSC code list — AHRQ-inspired Prevention Quality Indicators
acsc_codes <- c(
  "ICD-10-CM: E10", "ICD-10-CM: E11", "ICD-10-CM: E13", "ICD-10-CM: E14",
  "ICD-10-CM: J45", "ICD-10-CM: J44", "ICD-10-CM: J18", "ICD-10-CM: J13",
  "ICD-10-CM: I11", "ICD-10-CM: I50", "ICD-10-CM: I10",
  "ICD-10-CM: N39", "ICD-10-CM: L03",
  "ICD-10-CM: E86", "ICD-10-CM: K52", "ICD-10-CM: R56"
)
acsc_codes_sql <- paste0("('", paste(acsc_codes, collapse = "','"), "')")

message("Loading patient ED data...")

patient_ed_data <- dbGetQuery(con, sprintf("
  SELECT
    e.PatientDurableKey,
    SUM(CASE WHEN e.IsEdVisit = 1 THEN 1 ELSE 0 END) AS total_ed_visits,
    SUM(CASE WHEN e.IsEdVisit = 1 AND dx.GroupCode IN %s THEN 1 ELSE 0 END) AS acsc_ed_visits,
    MIN(CAST(e.Date AS DATE)) AS first_encounter,
    MAX(CAST(e.Date AS DATE)) AS last_encounter
  FROM encounters e
  LEFT JOIN diagnosis dx ON e.PrimaryDiagnosisKey = dx.DiagnosisKey
  WHERE e.Date IS NOT NULL
  GROUP BY e.PatientDurableKey
", acsc_codes_sql))

patient_ed_data$years_observed <- pmax(
  as.numeric(as.Date(patient_ed_data$last_encounter) - as.Date(patient_ed_data$first_encounter)) / 365.25,
  0.25
)
patient_ed_data$total_ed_per_year <- patient_ed_data$total_ed_visits / patient_ed_data$years_observed
patient_ed_data$acsc_ed_per_year <- patient_ed_data$acsc_ed_visits / patient_ed_data$years_observed

message("Loading SDoH data...")

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

# Master analysis dataset
analysis <- sdoh_patient |>
  dplyr::inner_join(patient_ed_data, by = "PatientDurableKey")

# ACSC condition-level breakdown — one query, reused across domains
message("Loading ACSC condition breakdown...")

acsc_by_condition <- dbGetQuery(con, sprintf("
  SELECT
    e.PatientDurableKey,
    dx.GroupCode,
    COUNT(*) AS n_visits
  FROM encounters e
  JOIN diagnosis dx ON e.PrimaryDiagnosisKey = dx.DiagnosisKey
  WHERE e.IsEdVisit = 1
    AND dx.GroupCode IN %s
  GROUP BY e.PatientDurableKey, dx.GroupCode
", acsc_codes_sql))

# Human-readable condition labels
condition_labels <- data.frame(
  GroupCode = acsc_codes,
  condition = c(
    "Type 1 diabetes", "Type 2 diabetes", "Other diabetes", "Unspecified diabetes",
    "Asthma", "COPD", "Bacterial pneumonia", "Pneumococcal pneumonia",
    "Hypertensive heart disease", "Heart failure", "Essential hypertension",
    "Urinary tract infection", "Cellulitis",
    "Dehydration", "Gastroenteritis", "Seizures"
  )
)
acsc_by_condition <- acsc_by_condition |>
  dplyr::left_join(condition_labels, by = "GroupCode")

message("Dashboard ready.")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .headline { font-size: 20px; padding: 20px; background: #F1EFE8; border-radius: 8px;
                margin-bottom: 20px; border-left: 4px solid #D4537E; }
    .metric-box { background: white; border: 1px solid #D3D1C7; border-radius: 8px;
                  padding: 16px; margin-bottom: 12px; text-align: center; }
    .metric-value { font-size: 32px; font-weight: 500; color: #D4537E; }
    .metric-label { font-size: 13px; color: #5F5E5A; }
  "))),
  titlePanel("When social needs drive preventable ED visits"),
  p(style = "color: #5F5E5A; font-size: 15px;",
    "An analysis of Stormont Vail Health patients, 2022-2025. Positive screeners = patients who reported having the social need. ACSC = ambulatory care sensitive condition (AHRQ-defined preventable ED visits)."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("domain", "Social determinant of health",
                  choices = c("Housing Stability",
                              "Transportation Needs",
                              "Financial Resource Strain"),
                  selected = "Housing Stability"),
      hr(),
      h4("About this analysis"),
      p("Compares ED visit patterns between patients who screened positive vs. negative on a specific SDoH domain."),
      p(strong("Total ED visits"), "= all emergency department visits."),
      p(strong("ACSC visits"), "= ED visits for conditions that well-managed primary care should prevent (diabetes complications, asthma, COPD, pneumonia, UTI, etc.)."),
      p(strong("Key insight:"), "If positive screeners had simply 'more of the same' kinds of ED visits, the ACSC share would be similar to negative screeners. A wider gap in ACSC use suggests their ED visits are disproportionately preventable."),
      width = 3
    ),
    mainPanel(
      div(class = "headline", uiOutput("headline")),
      
      fluidRow(
        column(3, div(class = "metric-box",
                      div(class = "metric-value", textOutput("m_n_pos", inline = TRUE)),
                      div(class = "metric-label", "patients screened positive"))),
        column(3, div(class = "metric-box",
                      div(class = "metric-value", textOutput("m_total_mult", inline = TRUE)),
                      div(class = "metric-label", "× more total ED use"))),
        column(3, div(class = "metric-box",
                      div(class = "metric-value", textOutput("m_acsc_mult", inline = TRUE)),
                      div(class = "metric-label", "× more preventable ED use"))),
        column(3, div(class = "metric-box",
                      div(class = "metric-value", textOutput("m_share_diff", inline = TRUE)),
                      div(class = "metric-label", "pp higher ACSC share")))
      ),
      
      h3("ED utilization: total vs. preventable"),
      p("Positive screeners use the ED more across the board, but the gap widens for preventable conditions."),
      plotlyOutput("rate_chart", height = "380px"),
      
      h3("Share of ED visits that were preventable"),
      p("The key insight: not just more ED visits, but a larger fraction of those visits are for conditions primary care should have managed."),
      plotlyOutput("share_chart", height = "280px"),
      
      h3("Which conditions drive the gap?"),
      p("ACSC visits per 1,000 patients, broken down by specific condition."),
      plotlyOutput("condition_chart", height = "480px"),
      
      h3("How concentrated is the problem?"),
      p("Distribution of ACSC ED visits across patients. If concentrated in a few heavy users, interventions should be targeted; if spread broadly, systemic."),
      plotlyOutput("concentration_chart", height = "300px"),
      
      h3("Methodology and caveats"),
      tags$ul(
        tags$li("ACSC classification follows AHRQ's Prevention Quality Indicators framework, simplified for this analysis. Not every coded visit in an ACSC group is truly preventable — some are genuine emergencies. We use aggregate patterns to reduce noise from individual miscoding."),
        tags$li("This is an observational analysis showing association, not causation. Positive screeners differ from negative screeners in many ways beyond the specific social need; those differences may contribute to the observed gap."),
        tags$li("The SDoH survey was rolled out starting with hospital admissions and is still expanding, so screened patients are not a random sample of the full SVH population. Findings apply to the screened subset."),
        tags$li("ED visits are normalized to a per-year rate using each patient's observed time window (first to last encounter). Patients with less than 3 months of observation have their denominator floored at 0.25 years to avoid inflation.")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Subset analysis data to selected domain
  domain_data <- reactive({
    analysis |> dplyr::filter(Domain == input$domain)
  })
  
  # Summary statistics
  stats <- reactive({
    d <- domain_data()
    d |>
      dplyr::group_by(screen) |>
      dplyr::summarise(
        n_patients = dplyr::n(),
        mean_total_ed_per_year = mean(total_ed_per_year, na.rm = TRUE),
        mean_acsc_ed_per_year = mean(acsc_ed_per_year, na.rm = TRUE),
        total_ed_sum = sum(total_ed_visits),
        acsc_ed_sum = sum(acsc_ed_visits),
        pct_ed_that_is_acsc = sum(acsc_ed_visits) / sum(total_ed_visits) * 100,
        .groups = "drop"
      )
  })
  
  # --- Headline text and metric boxes ---
  
  output$headline <- renderUI({
    s <- stats()
    pos <- s[s$screen == "Positive", ]
    neg <- s[s$screen == "Negative", ]
    total_mult <- pos$mean_total_ed_per_year / neg$mean_total_ed_per_year
    acsc_mult <- pos$mean_acsc_ed_per_year / neg$mean_acsc_ed_per_year
    
    HTML(sprintf(
      "At SVH, patients who screen positive for <strong>%s</strong> use the emergency department <strong>%.1f× more often</strong> overall — but <strong>%.1f× more often</strong> for preventable conditions. Their ED visits aren't just more frequent; they're disproportionately for conditions good primary care should have prevented.",
      input$domain, total_mult, acsc_mult
    ))
  })
  
  output$m_n_pos <- renderText({
    format(stats()[stats()$screen == "Positive", ]$n_patients, big.mark = ",")
  })
  
  output$m_total_mult <- renderText({
    s <- stats()
    sprintf("%.1f", s[s$screen == "Positive", ]$mean_total_ed_per_year /
              s[s$screen == "Negative", ]$mean_total_ed_per_year)
  })
  
  output$m_acsc_mult <- renderText({
    s <- stats()
    sprintf("%.1f", s[s$screen == "Positive", ]$mean_acsc_ed_per_year /
              s[s$screen == "Negative", ]$mean_acsc_ed_per_year)
  })
  
  output$m_share_diff <- renderText({
    s <- stats()
    diff <- s[s$screen == "Positive", ]$pct_ed_that_is_acsc -
      s[s$screen == "Negative", ]$pct_ed_that_is_acsc
    sprintf("+%.1f", diff)
  })
  
  # --- Total vs ACSC rate chart ---
  
  output$rate_chart <- renderPlotly({
    s <- stats()
    df <- data.frame(
      screen = rep(s$screen, 2),
      metric = rep(c("Total ED visits", "Preventable (ACSC) ED visits"), each = 2),
      rate = c(s$mean_total_ed_per_year, s$mean_acsc_ed_per_year)
    )
    df$metric <- factor(df$metric, levels = c("Total ED visits", "Preventable (ACSC) ED visits"))
    df$tooltip <- paste0(df$screen, " screen<br>", df$metric, "<br>",
                         round(df$rate, 3), " per patient per year")
    
    p <- ggplot(df, aes(x = metric, y = rate, fill = screen, text = tooltip)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("Negative" = "#378ADD", "Positive" = "#D4537E"),
                        name = NULL) +
      labs(x = NULL, y = "ED visits per patient per year") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- Share-that-is-ACSC chart ---
  
  output$share_chart <- renderPlotly({
    s <- stats()
    s$tooltip <- paste0(s$screen, " screeners<br>",
                        round(s$pct_ed_that_is_acsc, 1), "% of ED visits were preventable<br>",
                        format(s$acsc_ed_sum, big.mark = ","), " / ",
                        format(s$total_ed_sum, big.mark = ","), " total ED visits")
    
    p <- ggplot(s, aes(x = screen, y = pct_ed_that_is_acsc, fill = screen, text = tooltip)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = paste0(round(pct_ed_that_is_acsc, 1), "%")),
                vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("Negative" = "#378ADD", "Positive" = "#D4537E"),
                        guide = "none") +
      labs(x = NULL, y = "% of ED visits that were preventable (ACSC)") +
      theme_minimal(base_size = 13) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- Condition-level breakdown ---
  
  condition_stats <- reactive({
    d <- domain_data() |> dplyr::select(PatientDurableKey, screen)
    
    # Patient counts by screen status (denominator)
    patient_counts <- d |>
      dplyr::group_by(screen) |>
      dplyr::summarise(n_patients = dplyr::n(), .groups = "drop")
    
    # ACSC visits by patient joined with screen status, grouped by condition
    cond <- acsc_by_condition |>
      dplyr::inner_join(d, by = "PatientDurableKey") |>
      dplyr::group_by(condition, screen) |>
      dplyr::summarise(total_visits = sum(n_visits), .groups = "drop") |>
      dplyr::left_join(patient_counts, by = "screen") |>
      dplyr::mutate(visits_per_1000 = total_visits / n_patients * 1000)
    
    cond
  })
  
  output$condition_chart <- renderPlotly({
    c <- condition_stats()
    req(nrow(c) > 0)
    
    # Order conditions by total visits in the positive group
    ordering <- c |>
      dplyr::filter(screen == "Positive") |>
      dplyr::arrange(desc(visits_per_1000)) |>
      dplyr::pull(condition)
    c$condition <- factor(c$condition, levels = rev(ordering))
    c$tooltip <- paste0(c$condition, "<br>",
                        c$screen, " screeners: ",
                        round(c$visits_per_1000, 1), " visits/1000 patients<br>",
                        "(", format(c$total_visits, big.mark = ","), " visits across ",
                        format(c$n_patients, big.mark = ","), " patients)")
    
    p <- ggplot(c, aes(x = condition, y = visits_per_1000, fill = screen, text = tooltip)) +
      geom_col(position = "dodge", width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c("Negative" = "#378ADD", "Positive" = "#D4537E"),
                        name = NULL) +
      labs(x = NULL, y = "ED visits per 1,000 patients per period") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- Concentration chart ---
  
  output$concentration_chart <- renderPlotly({
    d <- domain_data()
    d_pos <- d |> dplyr::filter(screen == "Positive")
    
    bands <- data.frame(
      band = factor(c("0 ACSC visits", "1 ACSC visit", "2-3 ACSC visits",
                      "4-9 ACSC visits", "10+ ACSC visits"),
                    levels = c("0 ACSC visits", "1 ACSC visit", "2-3 ACSC visits",
                               "4-9 ACSC visits", "10+ ACSC visits")),
      lo = c(0, 1, 2, 4, 10),
      hi = c(0, 1, 3, 9, Inf)
    )
    
    counts <- dplyr::bind_rows(lapply(1:nrow(bands), function(i) {
      pos_n <- sum(d_pos$acsc_ed_visits >= bands$lo[i] & d_pos$acsc_ed_visits <= bands$hi[i])
      neg_n <- sum(d$acsc_ed_visits >= bands$lo[i] & d$acsc_ed_visits <= bands$hi[i] &
                     d$screen == "Negative")
      data.frame(band = bands$band[i],
                 Positive = pos_n / nrow(d_pos) * 100,
                 Negative = neg_n / sum(d$screen == "Negative") * 100)
    }))
    
    long <- tidyr::pivot_longer(counts, cols = c("Positive", "Negative"),
                                names_to = "screen", values_to = "pct")
    long$tooltip <- paste0(long$screen, " screeners<br>",
                           long$band, "<br>",
                           round(long$pct, 1), "% of patients in this group")
    
    p <- ggplot(long, aes(x = band, y = pct, fill = screen, text = tooltip)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("Negative" = "#378ADD", "Positive" = "#D4537E"),
                        name = NULL) +
      labs(x = NULL, y = "% of patients in this group") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
