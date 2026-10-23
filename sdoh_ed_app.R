library(shiny)
library(duckdb)
library(DBI)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

con <- dbConnect(duckdb(), "svh.duckdb", read_only = TRUE)
onStop(function() dbDisconnect(con, shutdown = TRUE))

# Classification: for each domain, what answers count as positive vs negative screen
positive_answers <- list(
  "Financial Resource Strain" = c("Yes", "Already shut off"),
  "Food insecurity"           = c("Sometimes true", "Often true"),
  "Housing Stability"         = c("Yes"),
  "Transportation Needs"      = c("Yes"),
  "intimate partner violance" = c("Yes"),
  "Utilities"                 = c("Somewhat hard", "Hard", "Very hard")
)

negative_answers <- list(
  "Financial Resource Strain" = c("No"),
  "Food insecurity"           = c("Never true"),
  "Housing Stability"         = c("No"),
  "Transportation Needs"      = c("No"),
  "intimate partner violance" = c("No"),
  "Utilities"                 = c("Not hard at all", "Not very hard")
)

# Build the analysis dataset once at startup.
# Steps:
#   1. For each patient × domain, flag whether they screened positive or negative
#   2. For each patient, count ED visits and observation time
#   3. Join so we have: patient, domain, screen (pos/neg), ED visits per year
build_analysis_data <- function() {
  # ED visits and observation window per patient
  ed_data <- dbGetQuery(con, "
  SELECT
    PatientDurableKey,
    SUM(CASE WHEN IsEdVisit = 1 THEN 1 ELSE 0 END) AS n_ed_visits,
    MIN(CAST(Date AS DATE)) AS first_encounter,
    MAX(CAST(Date AS DATE)) AS last_encounter
  FROM encounters
  WHERE Date IS NOT NULL
  GROUP BY PatientDurableKey
")
  
  ed_data$years_observed <- pmax(
    as.numeric(as.Date(ed_data$last_encounter) - as.Date(ed_data$first_encounter)) / 365.25,
    0.25  # minimum 3 months to avoid dividing by tiny numbers
  )
  ed_data$ed_per_year <- ed_data$n_ed_visits / ed_data$years_observed
  
  # All SDoH responses, cleaned
  sdoh_raw <- dbGetQuery(con, "
    SELECT PatientDurableKey, Domain, AnswerText
    FROM social_determinants
    WHERE Domain IS NOT NULL
      AND Domain NOT IN ('NA', '*Unspecified')
      AND AnswerText NOT IN ('Patient declined', 'Patient unable to answer', '*Unknown')
  ")
  
  # Classify each response
  classify_response <- function(domain, answer) {
    pos <- positive_answers[[domain]]
    neg <- negative_answers[[domain]]
    if (!is.null(pos) && answer %in% pos) return("Positive")
    if (!is.null(neg) && answer %in% neg) return("Negative")
    NA_character_
  }
  
  sdoh_raw$screen <- mapply(classify_response, sdoh_raw$Domain, sdoh_raw$AnswerText)
  sdoh_raw <- sdoh_raw[!is.na(sdoh_raw$screen), ]
  
  # If a patient has BOTH positive and negative answers for the same domain (at different
  # visits), treat them as "ever positive" — once a social risk is flagged, the patient is at risk.
  sdoh_patient <- sdoh_raw |>
    dplyr::group_by(PatientDurableKey, Domain) |>
    dplyr::summarise(
      screen = dplyr::if_else(any(screen == "Positive"), "Positive", "Negative"),
      .groups = "drop"
    )
  
  # Join with ED data
  analysis <- sdoh_patient |>
    dplyr::inner_join(ed_data, by = "PatientDurableKey") |>
    dplyr::filter(!is.na(ed_per_year))
  
  analysis
}

# Load once at startup
message("Building analysis dataset... (this takes ~30 seconds)")
analysis_data <- build_analysis_data()
message(sprintf("Loaded %d patient-domain observations.", nrow(analysis_data)))

ui <- fluidPage(
  titlePanel("SDoH and ED utilization at SVH"),
  sidebarLayout(
    sidebarPanel(
      h4("What this analysis does"),
      p("For each social determinant of health domain, compares ED visits per year between patients who screened positive (reported the problem) and those who screened negative."),
      hr(),
      radioButtons("metric", "Compare using",
                   choices = c("ED visits per year" = "rate",
                               "Any ED use (%)" = "any")),
      hr(),
      p(strong("Interpreting the results:")),
      p("Risk ratio > 1: positive screeners use the ED more."),
      p("p-value < 0.05: the difference is unlikely to be chance."),
      width = 3
    ),
    mainPanel(
      h3("Summary across all domains"),
      plotlyOutput("summary_chart", height = "400px"),
      h3("Detailed results"),
      DTOutput("results_table"),
      h3("Distribution of ED visits by domain"),
      p("Each pair of boxes shows the spread of ED visits for positive vs. negative screeners."),
      plotlyOutput("distribution_chart", height = "500px"),
      h3("Sample sizes"),
      tableOutput("sample_sizes")
    )
  )
)

server <- function(input, output, session) {
  
  # Core analysis: for each domain, compute rates, risk ratios, and significance
  results <- reactive({
    analysis_data |>
      dplyr::group_by(Domain, screen) |>
      dplyr::summarise(
        n_patients = dplyr::n(),
        mean_ed_per_year = mean(ed_per_year),
        median_ed_per_year = median(ed_per_year),
        pct_any_ed = mean(n_ed_visits > 0) * 100,
        .groups = "drop"
      )
  })
  
  # Per-domain statistical tests (t-test on ED rate)
  stats <- reactive({
    domains <- unique(analysis_data$Domain)
    
    do.call(rbind, lapply(domains, function(d) {
      subset <- analysis_data[analysis_data$Domain == d, ]
      pos <- subset$ed_per_year[subset$screen == "Positive"]
      neg <- subset$ed_per_year[subset$screen == "Negative"]
      
      if (length(pos) < 10 || length(neg) < 10) {
        return(data.frame(Domain = d, n_pos = length(pos), n_neg = length(neg),
                          mean_pos = NA, mean_neg = NA,
                          risk_ratio = NA, p_value = NA))
      }
      
      test <- t.test(pos, neg)
      rr <- mean(pos) / mean(neg)
      
      data.frame(
        Domain = d,
        n_pos = length(pos),
        n_neg = length(neg),
        mean_pos = round(mean(pos), 2),
        mean_neg = round(mean(neg), 2),
        risk_ratio = round(rr, 2),
        p_value = signif(test$p.value, 3)
      )
    }))
  })
  
  output$summary_chart <- renderPlotly({
    s <- stats()
    req(nrow(s) > 0)
    s <- s[!is.na(s$risk_ratio), ]
    
    s$significant <- s$p_value < 0.05
    s$label <- paste0(s$Domain, "\nRR = ", s$risk_ratio,
                      ", p = ", s$p_value,
                      "\n(", s$n_pos, " pos / ", s$n_neg, " neg)")
    
    p <- ggplot(s, aes(x = reorder(Domain, risk_ratio), y = risk_ratio,
                       fill = significant, text = label)) +
      geom_col() +
      geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#D4537E", "FALSE" = "#B4B2A9"),
                        labels = c("TRUE" = "Statistically significant",
                                   "FALSE" = "Not significant"),
                        name = NULL) +
      labs(x = NULL, y = "Risk ratio (positive vs. negative screeners)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$results_table <- renderDT({
    s <- stats()
    s$interpretation <- ifelse(is.na(s$risk_ratio), "Insufficient data",
                               ifelse(s$p_value < 0.05,
                                      paste0("Positive screeners use ED ",
                                             round((s$risk_ratio - 1) * 100), "% more"),
                                      "No significant difference"))
    datatable(s, options = list(pageLength = 10))
  })
  
  output$distribution_chart <- renderPlotly({
    # Cap outliers for readability (99th percentile)
    cap <- quantile(analysis_data$ed_per_year, 0.99, na.rm = TRUE)
    d <- analysis_data
    d$ed_per_year_capped <- pmin(d$ed_per_year, cap)
    
    p <- ggplot(d, aes(x = Domain, y = ed_per_year_capped, fill = screen)) +
      geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
      scale_fill_manual(values = c("Negative" = "#378ADD", "Positive" = "#D4537E")) +
      labs(x = NULL, y = "ED visits per year (capped at 99th %ile)", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "bottom")
    
    ggplotly(p)
  })
  
  output$sample_sizes <- renderTable({
    analysis_data |>
      dplyr::group_by(Domain, screen) |>
      dplyr::summarise(n_patients = dplyr::n(), .groups = "drop") |>
      tidyr::pivot_wider(names_from = screen, values_from = n_patients, values_fill = 0)
  })
}

shinyApp(ui, server)
