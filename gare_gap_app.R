library(shiny)
library(duckdb)
library(DBI)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

con <- dbConnect(duckdb(), "svh.duckdb", read_only = TRUE)
onStop(function() dbDisconnect(con, shutdown = TRUE))

# Primary care specialty definition
PRIMARY_CARE_SPECIALTIES <- c("Family Medicine", "Primary Care",
                              "Internal Medicine", "Pediatrics")

# ACSC codes (reused from previous analysis)
acsc_codes <- c(
  "ICD-10-CM: E10", "ICD-10-CM: E11", "ICD-10-CM: E13", "ICD-10-CM: E14",
  "ICD-10-CM: J45", "ICD-10-CM: J44", "ICD-10-CM: J18", "ICD-10-CM: J13",
  "ICD-10-CM: I11", "ICD-10-CM: I50", "ICD-10-CM: I10",
  "ICD-10-CM: N39", "ICD-10-CM: L03",
  "ICD-10-CM: E86", "ICD-10-CM: K52", "ICD-10-CM: R56"
)
acsc_codes_sql <- paste0("('", paste(acsc_codes, collapse = "','"), "')")
pc_specialties_sql <- paste0("('", paste(PRIMARY_CARE_SPECIALTIES, collapse = "','"), "')")

message("Computing ED visits with prior-care lookbacks (this takes ~60 seconds)...")

# The heart of this analysis: for every ED visit, compute days since last primary care visit.
# We use DuckDB's window function: for each ED visit, find the max date of any prior PC visit.
ed_with_lookback <- dbGetQuery(con, sprintf("
  WITH pc_visits AS (
    SELECT e.PatientDurableKey, CAST(e.Date AS DATE) AS pc_date
    FROM encounters e
    JOIN departments d ON e.DepartmentKey = d.DepartmentKey
    WHERE d.DepartmentSpecialty IN %s
      AND e.Date IS NOT NULL
      AND e.IsEdVisit = 0
  ),
  ed_visits AS (
    SELECT
      e.EncounterKey,
      e.PatientDurableKey,
      CAST(e.Date AS DATE) AS ed_date,
      e.PrimaryDiagnosisKey,
      dx.GroupCode,
      CASE WHEN dx.GroupCode IN %s THEN 1 ELSE 0 END AS is_acsc
    FROM encounters e
    LEFT JOIN diagnosis dx ON e.PrimaryDiagnosisKey = dx.DiagnosisKey
    WHERE e.IsEdVisit = 1 AND e.Date IS NOT NULL
  )
  SELECT
    ed.EncounterKey,
    ed.PatientDurableKey,
    ed.ed_date,
    ed.is_acsc,
    -- Days since most recent PC visit prior to this ED visit
    DATE_DIFF('day', MAX(pc.pc_date), ed.ed_date) AS days_since_pc
  FROM ed_visits ed
  LEFT JOIN pc_visits pc
    ON ed.PatientDurableKey = pc.PatientDurableKey
    AND pc.pc_date < ed.ed_date
  GROUP BY ed.EncounterKey, ed.PatientDurableKey, ed.ed_date, ed.is_acsc
", pc_specialties_sql, acsc_codes_sql))

# Classify care engagement based on days since last PC
ed_with_lookback$engagement <- dplyr::case_when(
  is.na(ed_with_lookback$days_since_pc) ~ "Never saw PC",
  ed_with_lookback$days_since_pc <= 30  ~ "Engaged (≤30 days)",
  ed_with_lookback$days_since_pc <= 90  ~ "Irregular (31-90 days)",
  ed_with_lookback$days_since_pc <= 180 ~ "Lapsed (91-180 days)",
  TRUE                                   ~ "Disengaged (>180 days)"
)
ed_with_lookback$engagement <- factor(ed_with_lookback$engagement,
                                      levels = c("Engaged (≤30 days)", "Irregular (31-90 days)",
                                                 "Lapsed (91-180 days)", "Disengaged (>180 days)", "Never saw PC"))

message(sprintf("Analyzed %s ED visits.", format(nrow(ed_with_lookback), big.mark = ",")))

# Load SDoH data
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

# Join to get ED visits tagged with screen status per domain
# Collapse all three domains into one "any positive" screen status per patient
sdoh_combined <- sdoh_patient |>
  dplyr::group_by(PatientDurableKey) |>
  dplyr::summarise(
    Domain = "combined",
    screen = dplyr::if_else(any(screen == "Positive"), "Positive", "Negative"),
    .groups = "drop"
  )

# Bind the combined view to the per-domain view
sdoh_patient_all <- dplyr::bind_rows(sdoh_patient, sdoh_combined)

# Join to get ED visits tagged with screen status per domain
analysis_master <- ed_with_lookback |>
  dplyr::inner_join(sdoh_patient_all, by = "PatientDurableKey", relationship = "many-to-many")

message("Dashboard ready.")
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .headline { font-size: 19px; padding: 18px; background: #F1EFE8; border-radius: 8px;
                margin-bottom: 20px; border-left: 4px solid #D4537E; }
    .metric-box { background: white; border: 1px solid #D3D1C7; border-radius: 8px;
                  padding: 14px; margin-bottom: 12px; text-align: center; }
    .metric-value { font-size: 28px; font-weight: 500; color: #D4537E; }
    .metric-label { font-size: 12px; color: #5F5E5A; }
  "))),
  titlePanel("Care gaps before the ED"),
  p(style = "color: #5F5E5A; font-size: 14px;",
    "For every ED visit at SVH, how long had it been since that patient's last primary care visit? Primary care = Family Medicine, Primary Care, Internal Medicine, or Pediatrics office visits."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("domain", "Social determinant view",
                  choices = c("Any of the three (combined)" = "combined",
                              "Housing Stability" = "Housing Stability",
                              "Transportation Needs" = "Transportation Needs",
                              "Financial Resource Strain" = "Financial Resource Strain"),
                  selected = "combined"),
      checkboxInput("acsc_only", "Limit to preventable (ACSC) ED visits only", FALSE),
      hr(),
      h4("About this analysis"),
      p("For every ED visit, we look backwards through the patient's history to find their most recent primary care visit. The days between tells us whether the patient had an active primary care relationship."),
      p(strong("Engaged:"), "seen in PC within 30 days (normal follow-up rhythm)"),
      p(strong("Irregular:"), "31-90 days (stretching limits of chronic care)"),
      p(strong("Lapsed:"), "91-180 days (real gap in care)"),
      p(strong("Disengaged:"), "180+ days (lost to primary care)"),
      p(strong("Never saw PC:"), "no primary care visit ever recorded at SVH"),
      width = 3
    ),
    mainPanel(
      div(class = "headline", uiOutput("headline")),
      
      fluidRow(
        column(4, div(class = "metric-box",
                      div(class = "metric-value", textOutput("m_pos_disengaged")),
                      div(class = "metric-label", "% of positive-screener ED visits came after 90+ day gap"))),
        column(4, div(class = "metric-box",
                      div(class = "metric-value", textOutput("m_neg_disengaged")),
                      div(class = "metric-label", "same metric for negative screeners"))),
        column(4, div(class = "metric-box",
                      div(class = "metric-value", textOutput("m_never_pc_pos")),
                      div(class = "metric-label", "% of positive-screener ED visits from patients who never saw PC at SVH")))
      ),
      
      h3("Engagement pattern before ED visit"),
      p("What was each patient's relationship with primary care in the days before their ED visit?"),
      plotlyOutput("engagement_chart", height = "380px"),
      
      h3("How long since the last primary care visit?"),
      p("Distribution of gap lengths. If positive screeners' curve is shifted right, they're consistently showing up to the ED after longer periods away from primary care."),
      plotlyOutput("gap_density", height = "380px"),
      
      h3("Does the gap predict preventable visits?"),
      p("Among ED visits that followed a long care gap, what fraction were for preventable (ACSC) conditions? This is the tightest causal argument: care disengagement → preventable crisis."),
      plotlyOutput("gap_vs_acsc", height = "380px"),
      
      h3("Methodology"),
      tags$ul(
        tags$li("'Primary care' is defined as non-ED encounters in departments flagged as Family Medicine, Primary Care, Internal Medicine, or Pediatrics. Urgent Care is excluded because it represents acute rather than preventive care."),
        tags$li("'Days since last PC' is calculated per ED visit by finding the most recent primary care encounter in the patient's history before that ED visit."),
        tags$li("Patients with no primary care history at SVH are shown as 'Never saw PC'. They may have received primary care elsewhere; we can only see what's in SVH's data."),
        tags$li("This is an observational analysis. Long gaps are associated with ED visits; it doesn't prove the gap caused the visit. However, combined with the ACSC finding (preventable conditions driving ED use), the pattern suggests care disengagement plays a meaningful role.")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Subset to selected domain (and optionally ACSC-only)
  dat <- reactive({
    d <- analysis_master |> dplyr::filter(Domain == input$domain)
    if (input$acsc_only) d <- d |> dplyr::filter(is_acsc == 1)
    d
  })
  
  # --- Summary metrics ---
  
  metrics <- reactive({
    d <- dat()
    d |>
      dplyr::group_by(screen) |>
      dplyr::summarise(
        n_visits = dplyr::n(),
        pct_90plus = mean(days_since_pc > 90 | is.na(days_since_pc), na.rm = FALSE) * 100,
        median_days = median(days_since_pc, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$headline <- renderUI({
    m <- metrics()
    pos <- m[m$screen == "Positive", ]
    neg <- m[m$screen == "Negative", ]
    visit_type <- if (input$acsc_only) "preventable ED visit" else "ED visit"
    domain_label <- if (input$domain == "combined") "socially at-risk" else paste0(input$domain, "-positive")
    neg_label <- if (input$domain == "combined") "others" else paste0(input$domain, "-negative")
    
    HTML(sprintf(
      "When a <strong>%s</strong> patient shows up for a %s, <strong>%.0f%%</strong> of the time they haven't seen a primary care doctor in over 90 days — compared to <strong>%.0f%%</strong> for %s patients.",
      domain_label, visit_type, pos$pct_90plus, neg$pct_90plus, neg_label
    ))
  })
  
  output$m_pos_disengaged <- renderText({
    sprintf("%.0f%%", metrics()[metrics()$screen == "Positive", ]$pct_90plus)
  })
  output$m_neg_disengaged <- renderText({
    sprintf("%.0f%%", metrics()[metrics()$screen == "Negative", ]$pct_90plus)
  })
  output$m_never_pc_pos <- renderText({
    d <- dat()
    d_pos <- d[d$screen == "Positive", ]
    pct <- mean(is.na(d_pos$days_since_pc)) * 100
    sprintf("%.0f%%", pct)
  })
  
  # --- Engagement stacked bar chart ---
  
  output$engagement_chart <- renderPlotly({
    d <- dat()
    
    eng <- d |>
      dplyr::count(screen, engagement) |>
      dplyr::group_by(screen) |>
      dplyr::mutate(pct = n / sum(n) * 100) |>
      dplyr::ungroup()
    
    eng$tooltip <- paste0(eng$screen, " screeners<br>",
                          eng$engagement, "<br>",
                          round(eng$pct, 1), "% (", format(eng$n, big.mark = ","), " visits)")
    
    # Color gradient: green engaged → red disengaged
    pal <- c("Engaged (≤30 days)" = "#1D9E75",
             "Irregular (31-90 days)" = "#97C459",
             "Lapsed (91-180 days)" = "#EF9F27",
             "Disengaged (>180 days)" = "#D85A30",
             "Never saw PC" = "#791F1F")
    
    p <- ggplot(eng, aes(x = screen, y = pct, fill = engagement, text = tooltip)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = pal, name = "Care engagement before ED visit") +
      labs(x = NULL, y = "% of ED visits") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "right",
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.5, "cm"),
            plot.margin = margin(10, 10, 10, 10)) +
      guides(fill = guide_legend(reverse = TRUE, ncol = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- Gap distribution density ---
  
  output$gap_density <- renderPlotly({
    d <- dat() |>
      dplyr::filter(!is.na(days_since_pc), days_since_pc <= 730)  # cap at 2 years for readability
    
    p <- ggplot(d, aes(x = days_since_pc, fill = screen, color = screen)) +
      geom_density(alpha = 0.4) +
      geom_vline(xintercept = c(30, 90, 180), linetype = "dotted", color = "grey50") +
      annotate("text", x = 30, y = Inf, label = "30d", vjust = 1.5, hjust = -0.2, size = 3, color = "grey50") +
      annotate("text", x = 90, y = Inf, label = "90d", vjust = 1.5, hjust = -0.2, size = 3, color = "grey50") +
      annotate("text", x = 180, y = Inf, label = "180d", vjust = 1.5, hjust = -0.2, size = 3, color = "grey50") +
      scale_fill_manual(values = c("Negative" = "#378ADD", "Positive" = "#D4537E"), name = NULL) +
      scale_color_manual(values = c("Negative" = "#185FA5", "Positive" = "#993556"), name = NULL) +
      labs(x = "Days since last primary care visit (capped at 730)",
           y = "Density of ED visits") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # --- Gap vs. ACSC: does a long gap predict preventable visits? ---
  
  output$gap_vs_acsc <- renderPlotly({
    # Use the full domain data (not filtered to ACSC-only, since we're computing ACSC rate)
    d <- analysis_master |> dplyr::filter(Domain == input$domain)
    
    # Bucket by engagement, compute % that are ACSC
    buckets <- d |>
      dplyr::group_by(screen, engagement) |>
      dplyr::summarise(
        n_visits = dplyr::n(),
        n_acsc = sum(is_acsc, na.rm = TRUE),
        pct_acsc = n_acsc / n_visits * 100,
        .groups = "drop"
      )
    
    buckets$tooltip <- paste0(buckets$screen, " screeners<br>",
                              buckets$engagement, "<br>",
                              round(buckets$pct_acsc, 1), "% of ED visits were ACSC<br>",
                              "(", format(buckets$n_acsc, big.mark = ","), " / ",
                              format(buckets$n_visits, big.mark = ","), " visits)")
    
    p <- ggplot(buckets, aes(x = engagement, y = pct_acsc, fill = screen, text = tooltip)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("Negative" = "#378ADD", "Positive" = "#D4537E"), name = NULL) +
      labs(x = NULL, y = "% of ED visits that were for preventable (ACSC) conditions") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)