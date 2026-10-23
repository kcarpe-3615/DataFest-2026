library(shiny)
library(duckdb)
library(DBI)
library(DT)
library(ggplot2)
library(plotly)


#test with key 2490034, 490575, 5361846

con <- dbConnect(duckdb(), "svh.duckdb", read_only = TRUE)
onStop(function() dbDisconnect(con, shutdown = TRUE))

get_patient_journey <- function(con, patient_key) {
  profile <- dbGetQuery(con, "
    SELECT * FROM patients WHERE DurableKey = ?", params = list(patient_key))
  
  encounters <- dbGetQuery(con, "
    SELECT
      e.EncounterKey, e.Date, e.Type, e.VisitType,
      e.IsEdVisit, e.IsHospitalAdmission, e.IsInpatientAdmission,
      e.AdmissionInstant, e.DischargeInstant, e.AdmissionSource,
      dx.DiagnosisName, dx.DiagnosisValue, dx.GroupName,
      d.DepartmentName, d.DepartmentSpecialty, d.DepartmentType,
      p.PrimarySpecialty AS AttendingSpecialty, p.Type AS AttendingType
    FROM encounters e
    LEFT JOIN diagnosis dx ON e.PrimaryDiagnosisKey = dx.DiagnosisKey
    LEFT JOIN departments d ON e.DepartmentKey = d.DepartmentKey
    LEFT JOIN providers p ON e.AttendingProviderDurableKey = p.DurableKey
    WHERE e.PatientDurableKey = ?
    ORDER BY e.Date", params = list(patient_key))
  
  sdoh <- dbGetQuery(con, "
    SELECT EncounterKey, Domain, DisplayName, AnswerText
    FROM social_determinants
    WHERE PatientDurableKey = ?", params = list(patient_key))
  
  list(profile = profile, encounters = encounters, sdoh = sdoh)
}

ui <- fluidPage(
  titlePanel("SVH patient viewer"),
  sidebarLayout(
    sidebarPanel(
      numericInput("patient_key", "Patient DurableKey", value = NA),
      actionButton("load", "Load patient"),
      selectInput("timeline_group", "Filter timeline to diagnosis group", choices = "All"),
      width = 3
    ),
    mainPanel(
      h3("Profile"),
      tableOutput("profile"),
      h3("At a glance"),
      tableOutput("summary_stats"),
      h3("Departments visited"),
      plotOutput("departments", height = "280px"),
      h3("Encounter volume over time"),
      plotOutput("volume", height = "220px"),
      h3("Top diagnosis groups"),
      plotOutput("top_groups", height = "300px"),
      h3("Timeline"),
      plotlyOutput("timeline", height = "800px", width = "1000px"),
      radioButtons("timeline_view", "Timeline layout",
                   choices = c("By encounter type" = "by_type",
                               "By diagnosis group (swim lanes)" = "by_group"),
                   selected = "by_type"),
      h3("Encounters"),
      DTOutput("encounters"),
      h3("Journeys (grouped by DiagnosisValue)"),
      DTOutput("journeys"),
      h3("Social determinants"),
      DTOutput("sdoh")
    )
  )
)

server <- function(input, output, session) {
  patient_data <- eventReactive(input$load, {
    req(input$patient_key)
    get_patient_journey(con, input$patient_key)
  })
  
  output$profile <- renderTable({ patient_data()$profile })
  
  output$summary_stats <- renderTable({
    enc <- patient_data()$encounters
    req(nrow(enc) > 0)
    enc$Date <- as.Date(enc$Date)
    
    data.frame(
      Metric = c("Total encounters", "Date range", "Days active",
                 "ED visits", "Hospital admissions",
                 "Unique diagnoses", "Unique departments", "Unique providers"),
      Value = c(
        nrow(enc),
        paste(min(enc$Date, na.rm = TRUE), "→", max(enc$Date, na.rm = TRUE)),
        as.integer(max(enc$Date, na.rm = TRUE) - min(enc$Date, na.rm = TRUE)),
        sum(enc$IsEdVisit == "Yes", na.rm = TRUE),
        sum(enc$IsHospitalAdmission == "Yes", na.rm = TRUE),
        length(unique(enc$DiagnosisName[!is.na(enc$DiagnosisName)])),
        length(unique(enc$DepartmentName[!is.na(enc$DepartmentName)])),
        length(unique(enc$AttendingSpecialty[!is.na(enc$AttendingSpecialty)]))
      )
    )
  })
  
  output$departments <- renderPlot({
    enc <- patient_data()$encounters
    req(nrow(enc) > 0)
    
    top <- enc |>
      dplyr::filter(!is.na(DepartmentSpecialty)) |>
      dplyr::count(DepartmentSpecialty, sort = TRUE) |>
      head(12)
    
    ggplot(top, aes(x = n, y = reorder(DepartmentSpecialty, n))) +
      geom_col(fill = "#1D9E75") +
      geom_text(aes(label = n), hjust = -0.3, size = 3.5) +
      labs(x = "Encounters", y = NULL) +
      theme_minimal(base_size = 12) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  output$volume <- renderPlot({
    enc <- patient_data()$encounters
    req(nrow(enc) > 0)
    enc$Date <- as.Date(enc$Date)
    enc$month <- as.Date(format(enc$Date, "%Y-%m-01"))
    
    monthly <- enc |>
      dplyr::count(month, name = "encounters")
    
    ggplot(monthly, aes(x = month, y = encounters)) +
      geom_col(fill = "#378ADD") +
      labs(x = NULL, y = "Encounters per month") +
      theme_minimal(base_size = 12)
  })
  
  output$timeline <- renderPlotly({
    enc <- patient_data()$encounters
    req(nrow(enc) > 0)
    enc$Date <- as.Date(enc$Date)
    
    if (!is.null(input$timeline_group) && input$timeline_group != "All") {
      enc <- enc[enc$GroupName == input$timeline_group, ]
      req(nrow(enc) > 0)
    }
    
    if (input$timeline_view == "by_group") {
      # Swim lane view
      top_journeys <- enc |>
        dplyr::filter(!is.na(GroupName)) |>
        dplyr::count(GroupName, sort = TRUE) |>
        head(10) |>
        dplyr::pull(GroupName)
      
      enc_top <- enc |> dplyr::filter(GroupName %in% top_journeys)
      req(nrow(enc_top) > 0)
      enc_top$GroupName <- factor(enc_top$GroupName, levels = rev(top_journeys))
      
      enc_top$tooltip <- paste0(
        "<b>", enc_top$Date, "</b><br>",
        "Type: ", enc_top$Type, "<br>",
        "Diagnosis: ", enc_top$DiagnosisName, "<br>",
        "Department: ", enc_top$DepartmentName
      )
      
      p <- ggplot(enc_top, aes(x = Date, y = GroupName, color = GroupName, text = tooltip)) +
        geom_point(size = 2.5, alpha = 0.8) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none",
              panel.grid.major.y = element_line(color = "grey90"),
              panel.grid.minor = element_blank())
      
    } else {
      # Original by-type view with journey lines and severity
      enc$severity <- dplyr::case_when(
        enc$IsHospitalAdmission == "Yes" ~ "Hospital admission",
        enc$IsEdVisit == "Yes" ~ "ED visit",
        enc$Type == "Surgery" ~ "Surgery",
        TRUE ~ "Routine"
      )
      enc$severity <- factor(enc$severity,
                             levels = c("Routine", "ED visit", "Hospital admission", "Surgery"))
      
      journey_lines <- enc |>
        dplyr::filter(!is.na(DiagnosisValue)) |>
        dplyr::group_by(DiagnosisValue) |>
        dplyr::filter(dplyr::n() >= 2) |>
        dplyr::ungroup()
      
      enc$tooltip <- paste0(
        "<b>", enc$Date, "</b><br>",
        "Type: ", enc$Type, "<br>",
        "Severity: ", enc$severity, "<br>",
        "Diagnosis: ", enc$DiagnosisName, "<br>",
        "Group: ", enc$GroupName, "<br>",
        "Department: ", enc$DepartmentName
      )
      
      p <- ggplot(enc, aes(x = Date, y = Type)) +
        geom_line(data = journey_lines,
                  aes(group = DiagnosisValue, color = GroupName),
                  alpha = 0.3, linewidth = 0.5) +
        geom_point(aes(color = GroupName, shape = severity, size = severity, text = tooltip),
                   alpha = 0.8,
                   position = position_jitter(height = 0.2, width = 0, seed = 1)) +
        scale_shape_manual(values = c("Routine" = 16, "ED visit" = 17,
                                      "Hospital admission" = 15, "Surgery" = 18)) +
        scale_size_manual(values = c("Routine" = 2, "ED visit" = 3.5,
                                     "Hospital admission" = 4, "Surgery" = 4)) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none", panel.grid.minor = element_blank())
    }
    
    ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$journeys <- renderDT({
    patient_data()$encounters |>
      dplyr::filter(!is.na(GroupName)) |>
      dplyr::group_by(GroupName) |>
      dplyr::summarise(
        encounters = dplyr::n(),
        unique_diagnoses = dplyr::n_distinct(DiagnosisName),
        first_date = min(as.Date(Date), na.rm = TRUE),
        last_date = max(as.Date(Date), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(desc(encounters)) |>
      datatable(options = list(pageLength = 15))
  })
  
  output$sdoh <- renderDT({
    sd <- patient_data()$sdoh
    req(nrow(sd) > 0)
    
    summary <- sd |>
      dplyr::filter(Domain != "NA", !is.na(Domain)) |>
      dplyr::group_by(Domain) |>
      dplyr::summarise(
        n_questions = dplyr::n(),
        answers = paste(unique(AnswerText), collapse = "; "),
        .groups = "drop"
      )
    
    if (nrow(summary) == 0) {
      summary <- data.frame(Note = "No domain-tagged SDoH data for this patient")
    }
    datatable(summary, options = list(pageLength = 15))
  })
  
  observe({
    enc <- patient_data()$encounters
    req(nrow(enc) > 0)
    groups <- sort(unique(enc$GroupName[!is.na(enc$GroupName)]))
    updateSelectInput(session, "timeline_group",
                      choices = c("All", groups),
                      selected = "All")
  })
  
  output$top_groups <- renderPlot({
    enc <- patient_data()$encounters
    req(nrow(enc) > 0)
    
    top <- enc |>
      dplyr::filter(!is.na(GroupName)) |>
      dplyr::count(GroupName, sort = TRUE) |>
      head(15)
    
    ggplot(top, aes(x = n, y = reorder(GroupName, n))) +
      geom_col(fill = "#378ADD") +
      geom_text(aes(label = n), hjust = -0.3, size = 3.5) +
      labs(x = "Encounters", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank()) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
}

shinyApp(ui, server)