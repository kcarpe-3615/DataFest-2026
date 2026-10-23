library(shiny)
library(duckdb)
library(DBI)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

con <- dbConnect(duckdb(), "svh.duckdb", read_only = TRUE)
onStop(function() dbDisconnect(con, shutdown = TRUE))

# Classification of screening types by intended target sex.
# VERIFY this against your actual DiagnosisName values from Step 1.
screening_classification <- data.frame(
  keyword = c("breast", "mammogram", "cervix", "cervical", "vagina", "ovary", "uterus",
              "prostate", "testis", "testicular",
              "colon", "rectum", "intestinal tract", "small intestine",
              "lung", "respiratory", "skin", "bladder", "stomach",
              "nervous system", "site unspecified", "other sites"),
  target = c("female", "female", "female", "female", "female", "female", "female",
             "male", "male", "male",
             "both", "both", "both", "both",
             "both", "both", "both", "both", "both",
             "both", "both", "both")
)

# Pull all screening encounters joined to patient sex + age bin once at startup.
# This is a one-time cost; after this the app is fast.
screening_data <- dbGetQuery(con, "
  SELECT
    p.DurableKey AS patient_key,
    p.SexAssignedAtBirth AS sex,
    p.PatientBirthYearBin AS birth_year,
    dx.DiagnosisName AS screening_name,
    e.Date
  FROM encounters e
  JOIN diagnosis dx ON e.PrimaryDiagnosisKey = dx.DiagnosisKey
  JOIN patients p ON e.PatientDurableKey = p.DurableKey
  WHERE dx.GroupName = 'Encounter for screening for malignant neoplasms'
")

# Patient denominators by sex and age bin, for computing rates.
patient_denominators <- dbGetQuery(con, "
  SELECT SexAssignedAtBirth AS sex,
         PatientBirthYearBin AS birth_year,
         COUNT(*) AS n_patients
  FROM patients
  WHERE SexAssignedAtBirth IN ('Male', 'Female')
    AND PatientBirthYearBin NOT IN ('NA', '*Unspecified', 'Unknown', '*Unknown', '')
  GROUP BY sex, birth_year
")

# Classify screenings by target sex using keyword matching
classify_screening <- function(name) {
  name_lower <- tolower(name)
  for (i in seq_len(nrow(screening_classification))) {
    if (grepl(screening_classification$keyword[i], name_lower, fixed = TRUE)) {
      return(screening_classification$target[i])
    }
  }
  "unclassified"
}

screening_data$target_sex <- sapply(screening_data$screening_name, classify_screening)

ui <- fluidPage(
  titlePanel("SVH cancer screening: rates by sex"),
  sidebarLayout(
    sidebarPanel(
      selectInput("scope", "Which screenings to show",
                  choices = c("Both-sex screenings only (apples-to-apples)" = "both",
                              "All screenings" = "all",
                              "Sex-specific screenings only" = "specific"),
                  selected = "both"),
      checkboxGroupInput("exclude_screenings", "Exclude categories (to see detail)",
                         choices = NULL),
      selectInput("age_filter", "Age group",
                  choices = c("All ages" = "all",
                              "Under 45" = "under45",
                              "45 to 65" = "mid",
                              "Over 65" = "over65"),
                  selected = "all"),
      checkboxInput("per_1000", "Show rate per 1,000 patients", value = TRUE),
      width = 3
    ),
    mainPanel(
      h3("Screening rates by sex"),
      plotlyOutput("rates_chart", height = "500px"),
      h3("Raw numbers"),
      DTOutput("rates_table"),
      h3("Screening types classified"),
      p("Review this to make sure the classification is correct."),
      DTOutput("classification_table")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    exclusion_choices <- unique(screening_data$screening_name)
    # Shorten display names for the checkbox labels
    names(exclusion_choices) <- gsub("\\(\\s*ICD-10-CM:[^)]+\\)", "", exclusion_choices) |>
      gsub("Encounter for screening.*?of ", "", x = _) |>
      gsub("Encounter for screening.*?,\\s*", "", x = _) |>
      trimws() |>
      tools::toTitleCase()
    updateCheckboxGroupInput(session, "exclude_screenings",
                             choices = exclusion_choices)
  })
  
  # Apply age filter to both numerator and denominator
  filtered_data <- reactive({
    sd <- screening_data
    pd <- patient_denominators
    
    if (input$age_filter != "all") {
      # Convert birth year to numeric for filtering
      sd$birth_year_num <- suppressWarnings(as.integer(sd$birth_year))
      pd$birth_year_num <- suppressWarnings(as.integer(pd$birth_year))
      
      cutoffs <- switch(input$age_filter,
                        under45 = c(1981, 9999),   # born 1981+ is under 45 today
                        mid     = c(1961, 1981),   # born 1961-1980 is 45-64
                        over65  = c(0, 1961)       # born 1960 or earlier is over 65
      )
      sd <- sd[!is.na(sd$birth_year_num) & sd$birth_year_num >= cutoffs[1] & sd$birth_year_num < cutoffs[2], ]
      pd <- pd[!is.na(pd$birth_year_num) & pd$birth_year_num >= cutoffs[1] & pd$birth_year_num < cutoffs[2], ]
    }
    
    # Apply exclusions
    if (!is.null(input$exclude_screenings) && length(input$exclude_screenings) > 0) {
      sd <- sd[!sd$screening_name %in% input$exclude_screenings, ]
    }
    
    # Apply scope filter
    if (input$scope == "both") {
      sd <- sd[sd$target_sex == "both", ]
    } else if (input$scope == "specific") {
      sd <- sd[sd$target_sex %in% c("male", "female"), ]
    }
    
    list(screenings = sd, denominators = pd)
  })
  
  # Compute rates per screening type per sex
  rates <- reactive({
    d <- filtered_data()
    sd <- d$screenings
    pd <- d$denominators
    
    # Restrict BOTH numerator and denominator to the same filtered set:
    # Male/Female only, with valid birth years
    sd <- sd[sd$sex %in% c("Male", "Female") &
               !sd$birth_year %in% c("NA", "*Unspecified", "Unknown", "*Unknown", "") &
               !is.na(sd$birth_year), ]
    req(nrow(sd) > 0)
    
    patients_by_sex <- pd |>
      dplyr::filter(sex %in% c("Male", "Female")) |>
      dplyr::group_by(sex) |>
      dplyr::summarise(n_patients = sum(n_patients), .groups = "drop")
    
    counts <- sd |>
      dplyr::count(screening_name, sex, target_sex, name = "n_screenings") |>
      dplyr::left_join(patients_by_sex, by = "sex") |>
      dplyr::mutate(
        rate_per_1000 = round(n_screenings / n_patients * 1000, 2)
      )
    
    counts
  })
  
  output$rates_chart <- renderPlotly({
    r <- rates()
    req(nrow(r) > 0)
    
    # Shorten long diagnosis names for readability
    r$short_name <- r$screening_name
    r$short_name <- gsub("Encounter for screening mammogram for malignant neoplasm of ", "", r$short_name)
    r$short_name <- gsub("Encounter for other screening for malignant neoplasm of ", "other ", r$short_name)
    r$short_name <- gsub("Encounter for screening for malignant neoplasm of ", "", r$short_name)
    r$short_name <- gsub("Encounter for screening for malignant neoplasms, ", "", r$short_name)
    r$short_name <- gsub("Encounter for screening for malignant neoplasm, ", "", r$short_name)
    r$short_name <- gsub("\\(\\s*ICD-10-CM:[^)]+\\)", "", r$short_name)
    r$short_name <- trimws(r$short_name)
    r$short_name <- tools::toTitleCase(r$short_name)
    
    y_value <- if (input$per_1000) r$rate_per_1000 else r$n_screenings
    y_label <- if (input$per_1000) "Screenings per 1,000 patients" else "Total screenings"
    
    p <- ggplot(r, aes(x = reorder(short_name, y_value), y = y_value, fill = sex,
                       text = paste0(
                         "<b>", short_name, "</b><br>",
                         "Sex: ", sex, "<br>",
                         "Screenings: ", n_screenings, "<br>",
                         "Patients of this sex: ", n_patients, "<br>",
                         "Rate per 1000: ", rate_per_1000
                       ))) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = c("Female" = "#D4537E", "Male" = "#378ADD")) +
      labs(x = NULL, y = y_label, fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$rates_table <- renderDT({
    r <- rates()
    r |>
      dplyr::select(screening_name, target_sex, sex, n_screenings, n_patients, rate_per_1000) |>
      dplyr::arrange(screening_name, sex) |>
      datatable(options = list(pageLength = 15))
  })
  
  output$classification_table <- renderDT({
    screening_data |>
      dplyr::count(screening_name, target_sex, sort = TRUE) |>
      datatable(options = list(pageLength = 20))
  })
}

shinyApp(ui, server)