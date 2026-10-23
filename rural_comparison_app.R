library(shiny)
library(duckdb)
library(DBI)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

con <- dbConnect(duckdb(), "svh.duckdb", read_only = TRUE)
onStop(function() dbDisconnect(con, shutdown = TRUE))

# Hospital reference point (Stormont Vail main hospital, Topeka)
HOSPITAL_LAT <- 39.0473
HOSPITAL_LON <- -95.6752

# Fetch cohorts of rural and urban patients, joined to all their encounters.
fetch_cohort_encounters <- function(distance_filter, cohort_size) {
  query <- sprintf("
    WITH patient_distances AS (
      SELECT
        p.DurableKey,
        69.0 * SQRT(
          POWER(TRY_CAST(t.CENTLAT AS DOUBLE) - %f, 2) +
          POWER((TRY_CAST(t.CENTLON AS DOUBLE) - (%f)) * COS(RADIANS(%f)), 2)
        ) AS miles_from_hospital
      FROM patients p
      JOIN tigercensuscodes t ON p.CensusBlockGroupFipsCode = t.GEOID
      WHERE p.CensusBlockGroupFipsCode != '*Unspecified'
        AND p.CensusBlockGroupFipsCode IS NOT NULL
        AND t.CENTLAT != 'NA'
        AND t.CENTLON != 'NA'
    ),
    cohort AS (
      SELECT pd.DurableKey, pd.miles_from_hospital
      FROM patient_distances pd
      LEFT JOIN encounters e ON pd.DurableKey = e.PatientDurableKey
      WHERE %s
      GROUP BY pd.DurableKey, pd.miles_from_hospital
      HAVING COUNT(e.EncounterKey) BETWEEN 20 AND 200
      ORDER BY RANDOM()
      LIMIT %d
    )
    SELECT
      c.DurableKey AS patient_key,
      c.miles_from_hospital,
      e.EncounterKey, e.Date, e.Type,
      e.IsEdVisit, e.IsHospitalAdmission,
      dx.DiagnosisName, dx.GroupName,
      d.DepartmentName, d.DepartmentSpecialty
    FROM cohort c
    LEFT JOIN encounters e ON c.DurableKey = e.PatientDurableKey
    LEFT JOIN diagnosis dx ON e.PrimaryDiagnosisKey = dx.DiagnosisKey
    LEFT JOIN departments d ON e.DepartmentKey = d.DepartmentKey
  ", HOSPITAL_LAT, HOSPITAL_LON, HOSPITAL_LAT, distance_filter, cohort_size)
  
  dbGetQuery(con, query)
}

ui <- fluidPage(
  titlePanel("SVH rural vs urban: comparing patient journeys"),
  sidebarLayout(
    sidebarPanel(
      h4("Cohort setup"),
      numericInput("cohort_size", "Patients per group", value = 50, min = 10, max = 200),
      numericInput("rural_miles", "Rural: miles from hospital (≥)", value = 60, min = 30),
      numericInput("urban_miles", "Urban: miles from hospital (≤)", value = 5, min = 0, max = 30),
      actionButton("refresh", "Resample cohorts", class = "btn-primary"),
      hr(),
      helpText("Each click pulls a new random sample of patients from each cohort. Cohort size of 50+ stabilizes the comparison."),
      width = 3
    ),
    mainPanel(
      h3("Cohort summary"),
      tableOutput("summary"),
      h3("Encounter type mix"),
      p("What kind of encounters does each cohort have? Different mix = different style of care."),
      plotlyOutput("type_mix", height = "400px"),
      h3("Top diagnosis groups"),
      p("Top 10 diagnosis groups by encounter volume in each cohort."),
      plotlyOutput("diagnosis_mix", height = "500px"),
      h3("Top department specialties"),
      p("Where are they being seen? Primary care, specialists, ED, imaging?"),
      plotlyOutput("dept_mix", height = "500px"),
      h3("Days between encounters"),
      p("Distribution of gap-days between consecutive encounters. Rural patients may have longer gaps (travel burden)."),
      plotlyOutput("gaps", height = "350px"),
      h3("Same-day clustering"),
      p("Percent of encounters that happened on a day when the patient had 2+ encounters. Rural patients may cluster more (single-trip efficiency)."),
      tableOutput("clustering")
    )
  )
)

server <- function(input, output, session) {
  
  cohorts <- eventReactive(input$refresh, {
    rural_filter <- sprintf("pd.miles_from_hospital >= %f", input$rural_miles)
    urban_filter <- sprintf("pd.miles_from_hospital <= %f", input$urban_miles)
    
    withProgress(message = "Sampling cohorts...", {
      rural <- fetch_cohort_encounters(rural_filter, input$cohort_size) |>
        dplyr::mutate(cohort = "Rural")
      urban <- fetch_cohort_encounters(urban_filter, input$cohort_size) |>
        dplyr::mutate(cohort = "Urban")
    })
    
    dplyr::bind_rows(rural, urban) |>
      dplyr::mutate(cohort = factor(cohort, levels = c("Urban", "Rural")))
  }, ignoreNULL = FALSE)
  

  
  output$summary <- renderTable({
    d <- cohorts()
    d |>
      dplyr::group_by(cohort) |>
      dplyr::summarise(
        n_patients = dplyr::n_distinct(patient_key),
        avg_miles_from_hospital = round(mean(miles_from_hospital, na.rm = TRUE), 1),
        total_encounters = dplyr::n(),
        avg_encounters_per_patient = round(dplyr::n() / dplyr::n_distinct(patient_key), 1),
        .groups = "drop"
      )
  })
  
  output$type_mix <- renderPlotly({
    d <- cohorts()
    req(nrow(d) > 0)
    
    mix <- d |>
      dplyr::filter(!is.na(Type)) |>
      dplyr::count(cohort, Type) |>
      dplyr::group_by(cohort) |>
      dplyr::mutate(pct = n / sum(n) * 100) |>
      dplyr::ungroup()
    
    # Keep only types with ≥2% in at least one cohort, to declutter
    keep <- mix |>
      dplyr::group_by(Type) |>
      dplyr::filter(max(pct) >= 2) |>
      dplyr::pull(Type) |> unique()
    mix <- mix |> dplyr::filter(Type %in% keep)
    
    p <- ggplot(mix, aes(x = reorder(Type, pct), y = pct, fill = cohort,
                         text = paste0(cohort, "<br>", Type, "<br>",
                                       round(pct, 1), "% (", n, " encounters)"))) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = c("Urban" = "#378ADD", "Rural" = "#D85A30")) +
      labs(x = NULL, y = "% of encounters in cohort", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$diagnosis_mix <- renderPlotly({
    d <- cohorts()
    req(nrow(d) > 0)
    
    # Top groups from the combined pool, then compare share in each cohort
    top_groups <- d |>
      dplyr::filter(!is.na(GroupName)) |>
      dplyr::count(GroupName, sort = TRUE) |>
      head(15) |>
      dplyr::pull(GroupName)
    
    mix <- d |>
      dplyr::filter(GroupName %in% top_groups) |>
      dplyr::count(cohort, GroupName) |>
      dplyr::group_by(cohort) |>
      dplyr::mutate(pct = n / sum(n) * 100) |>
      dplyr::ungroup()
    
    p <- ggplot(mix, aes(x = reorder(GroupName, pct), y = pct, fill = cohort,
                         text = paste0(cohort, "<br>", GroupName, "<br>",
                                       round(pct, 1), "% (", n, " encounters)"))) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = c("Urban" = "#378ADD", "Rural" = "#D85A30")) +
      labs(x = NULL, y = "% of cohort encounters", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$dept_mix <- renderPlotly({
    d <- cohorts()
    req(nrow(d) > 0)
    
    top_specs <- d |>
      dplyr::filter(!is.na(DepartmentSpecialty)) |>
      dplyr::count(DepartmentSpecialty, sort = TRUE) |>
      head(15) |>
      dplyr::pull(DepartmentSpecialty)
    
    mix <- d |>
      dplyr::filter(DepartmentSpecialty %in% top_specs) |>
      dplyr::count(cohort, DepartmentSpecialty) |>
      dplyr::group_by(cohort) |>
      dplyr::mutate(pct = n / sum(n) * 100) |>
      dplyr::ungroup()
    
    p <- ggplot(mix, aes(x = reorder(DepartmentSpecialty, pct), y = pct, fill = cohort,
                         text = paste0(cohort, "<br>", DepartmentSpecialty, "<br>",
                                       round(pct, 1), "% (", n, " encounters)"))) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = c("Urban" = "#378ADD", "Rural" = "#D85A30")) +
      labs(x = NULL, y = "% of cohort encounters", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$gaps <- renderPlotly({
    d <- cohorts()
    req(nrow(d) > 0)
    
    gaps <- d |>
      dplyr::filter(!is.na(Date)) |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::arrange(patient_key, Date) |>
      dplyr::group_by(patient_key, cohort) |>
      dplyr::mutate(gap = as.numeric(Date - dplyr::lag(Date))) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(gap), gap > 0, gap < 365)  # same-day gaps and year+ outliers trimmed
    
    p <- ggplot(gaps, aes(x = gap, fill = cohort, color = cohort)) +
      geom_density(alpha = 0.4) +
      scale_fill_manual(values = c("Urban" = "#378ADD", "Rural" = "#D85A30")) +
      scale_color_manual(values = c("Urban" = "#185FA5", "Rural" = "#993C1D")) +
      labs(x = "Days between encounters", y = "Density", fill = NULL, color = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  output$clustering <- renderTable({
    d <- cohorts()
    req(nrow(d) > 0)
    
    d |>
      dplyr::filter(!is.na(Date)) |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::group_by(cohort, patient_key, Date) |>
      dplyr::mutate(same_day_count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::group_by(cohort) |>
      dplyr::summarise(
        total_encounters = dplyr::n(),
        pct_in_cluster = round(sum(same_day_count > 1) / dplyr::n() * 100, 1),
        avg_encounters_per_visit_day = round(mean(same_day_count), 2),
        .groups = "drop"
      )
  })
}

shinyApp(ui, server)