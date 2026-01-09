library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(readxl)
library(digest)

# Data file configuration
DATA_FILE <- "ehr_data.rds"
ENCRYPTION_KEY_FILE <- ".ehr_key"

# Function to get or create encryption key
get_encryption_key <- function() {
  if (file.exists(ENCRYPTION_KEY_FILE)) {
    readLines(ENCRYPTION_KEY_FILE, warn = FALSE)[1]
  } else {
    # Generate a random key on first run
    key <- digest(paste(Sys.time(), runif(1)), algo = "sha256")
    writeLines(key, ENCRYPTION_KEY_FILE)
    message("Created new encryption key file: ", ENCRYPTION_KEY_FILE)
    message("IMPORTANT: Keep this file secure and backed up!")
    key
  }
}

# Function to encrypt data
encrypt_data <- function(data, key) {
  serialized <- serialize(data, NULL)
  key_bytes <- charToRaw(key)
  encrypted <- raw(length(serialized))
  for (i in seq_along(serialized)) {
    encrypted[i] <- as.raw(bitwXor(as.integer(serialized[i]), 
                                    as.integer(key_bytes[(i %% length(key_bytes)) + 1])))
  }
  encrypted
}

# Function to decrypt data
decrypt_data <- function(encrypted, key) {
  key_bytes <- charToRaw(key)
  decrypted <- raw(length(encrypted))
  for (i in seq_along(encrypted)) {
    decrypted[i] <- as.raw(bitwXor(as.integer(encrypted[i]), 
                                    as.integer(key_bytes[(i %% length(key_bytes)) + 1])))
  }
  unserialize(decrypted)
}

# Function to save all data
save_ehr_data <- function(patients, problems, medications, allergies, visits, vitals) {
  key <- get_encryption_key()
  data_list <- list(
    patients = patients,
    problems = problems,
    medications = medications,
    allergies = allergies,
    visits = visits,
    vitals = vitals,
    saved_at = Sys.time()
  )
  encrypted <- encrypt_data(data_list, key)
  writeBin(encrypted, DATA_FILE)
  message("Data saved and encrypted to ", DATA_FILE)
}

# Function to load all data
load_ehr_data <- function() {
  if (!file.exists(DATA_FILE)) {
    return(NULL)
  }
  
  tryCatch({
    key <- get_encryption_key()
    encrypted <- readBin(DATA_FILE, "raw", n = file.info(DATA_FILE)$size)
    data_list <- decrypt_data(encrypted, key)
    message("Data loaded from ", DATA_FILE)
    data_list
  }, error = function(e) {
    warning("Failed to load data: ", e$message)
    NULL
  })
}

# Load ICD-10 codes from Excel file
# Update the file path to match your file location
icd10_file <- "ICD Codes 2025.xlsx"

if (file.exists(icd10_file)) {
  icd10_codes <- read_excel(icd10_file)
  
  # Rename columns to standard names (adjust if your column names differ)
  names(icd10_codes) <- c("code", "description")
  
  # Clean the data
  icd10_codes <- icd10_codes[!is.na(icd10_codes$code), ]
  icd10_codes$code <- as.character(icd10_codes$code)
  icd10_codes$description <- as.character(icd10_codes$description)
  
  message(paste("Loaded", nrow(icd10_codes), "ICD-10 codes"))
} else {
  # Fallback to sample codes if file not found
  warning("ICD-10 file not found. Using sample codes.")
  icd10_codes <- data.frame(
    code = c("E11.9", "I10", "E78.5", "J44.9", "M79.3", "Z00.00"),
    description = c(
      "Type 2 diabetes mellitus without complications",
      "Essential (primary) hypertension",
      "Hyperlipidemia, unspecified",
      "Chronic obstructive pulmonary disease, unspecified",
      "Panniculitis, unspecified",
      "Encounter for general adult medical examination without abnormal findings"
    ),
    stringsAsFactors = FALSE
  )
}

# Load CPT codes from Excel file
cpt_file <- "CPT Codes 2026.xlsx"

if (file.exists(cpt_file)) {
  cpt_codes <- read_excel(cpt_file)
  
  # Rename columns to standard names (adjust if your column names differ)
  names(cpt_codes) <- c("code", "description")
  
  # Clean the data
  cpt_codes <- cpt_codes[!is.na(cpt_codes$code), ]
  cpt_codes$code <- as.character(cpt_codes$code)
  cpt_codes$description <- as.character(cpt_codes$description)
  
  message(paste("Loaded", nrow(cpt_codes), "CPT codes"))
} else {
  # Fallback to sample codes if file not found
  warning("CPT file not found. Using sample codes.")
  cpt_codes <- data.frame(
    code = c("99213", "99214", "99215", "99203", "99204", "99397"),
    description = c(
      "Office outpatient visit 15 minutes",
      "Office outpatient visit 25 minutes",
      "Office outpatient visit 40 minutes",
      "Office outpatient new patient visit 30 minutes",
      "Office outpatient new patient visit 45 minutes",
      "Preventive visit, established patient, 65+ years"
    ),
    stringsAsFactors = FALSE
  )
}

ui <- dashboardPage(
  dashboardHeader(title = "Madrigal EHR"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Patients", tabName = "patients", icon = icon("users")),
      menuItem("Chart", tabName = "chart", icon = icon("file-medical"))
    ),
    hr(),
    h4("Current Patient", style = "padding-left: 15px;"),
    textOutput("current_patient_sidebar"),
    hr(),
    h5("Problem List", style = "padding-left: 15px;"),
    actionButton("add_problem", "Add Problem", icon = icon("plus"), 
                 style = "margin-left: 15px; margin-bottom: 10px;"),
    uiOutput("problem_list_ui"),
    hr(),
    h5("Medications", style = "padding-left: 15px;"),
    actionButton("add_medication", "Add Medication", icon = icon("plus"),
                 style = "margin-left: 15px; margin-bottom: 10px;"),
    uiOutput("medication_list_ui"),
    hr(),
    h5("Allergies", style = "padding-left: 15px;"),
    actionButton("add_allergy", "Add Allergy", icon = icon("plus"),
                 style = "margin-left: 15px; margin-bottom: 10px;"),
    uiOutput("allergy_list_ui")
  ),
  
  dashboardBody(
    tabItems(
      # Patients tab
      tabItem(tabName = "patients",
        fluidRow(
          box(width = 12, title = "Patient List",
            actionButton("new_patient", "New Patient", icon = icon("user-plus"),
                        class = "btn-primary"),
            downloadButton("export_chart", "Export Chart", class = "btn-success"),
            hr(),
            DTOutput("patient_table")
          )
        )
      ),
      
      # Chart tab
      tabItem(tabName = "chart",
        fluidRow(
          box(width = 12, title = "Patient Information",
            fluidRow(
              column(4, textInput("patient_name", "Name", "")),
              column(4, dateInput("patient_dob", "Date of Birth", value = NULL)),
              column(4, textInput("patient_mrn", "MRN", "", placeholder = "Auto-generated"))
            ),
            fluidRow(
              column(6, textInput("patient_phone", "Phone Number", "")),
              column(6, textInput("patient_email", "Email Address", ""))
            )
          )
        ),
        
        tabsetPanel(
          id = "chart_tabs",
          
          # Visits Tab
          tabPanel("Visits",
            br(),
            actionButton("new_visit", "New Visit", icon = icon("plus"), class = "btn-primary"),
            hr(),
            uiOutput("visits_ui")
          ),
          
          # Vitals Tab
          tabPanel("Vitals",
            br(),
            fluidRow(
              column(12,
                h4("Add Vital Signs"),
                dateInput("vital_date", "Date:", value = Sys.Date())
              )
            ),
            fluidRow(
              column(3, numericInput("vital_weight", "Weight (lbs)", value = NULL, min = 0)),
              column(3, numericInput("vital_height", "Height (inches)", value = NULL, min = 0)),
              column(3, numericInput("vital_waist", "Waist (inches)", value = NULL, min = 0)),
              column(3, numericInput("vital_hr", "Heart Rate (bpm)", value = NULL, min = 0))
            ),
            fluidRow(
              column(3, numericInput("vital_bp_sys", "BP Systolic", value = NULL, min = 0)),
              column(3, numericInput("vital_bp_dia", "BP Diastolic", value = NULL, min = 0)),
              column(3, numericInput("vital_resp", "Resp Rate (/min)", value = NULL, min = 0)),
              column(3, numericInput("vital_ox", "Pulse Ox (%)", value = NULL, min = 0, max = 100))
            ),
            fluidRow(
              column(12, br(), actionButton("add_vital", "Add Vitals", icon = icon("plus"),
                                          class = "btn-primary"))
            ),
            hr(),
            DTOutput("vitals_table")
          ),
          
          # Future Features Tab
          tabPanel("Future Features",
            br(),
            h4("Planned Integrations"),
            tags$ul(
              tags$li("Digital consent forms and intake questionnaires"),
              tags$li("Calendly integration for appointment booking"),
              tags$li("Enhanced chart export with PDF formatting")
            ),
            p("These features are prepared for future development.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Load existing data or initialize empty dataframes
  existing_data <- load_ehr_data()
  
  # Reactive values to store data
  patients <- reactiveVal(
    if (!is.null(existing_data)) existing_data$patients 
    else data.frame(
      id = integer(),
      name = character(),
      dob = character(),
      mrn = character(),
      phone = character(),
      email = character(),
      stringsAsFactors = FALSE
    )
  )
  
  problems <- reactiveVal(
    if (!is.null(existing_data)) existing_data$problems
    else data.frame(
      patient_id = integer(),
      id = integer(),
      text = character(),
      stringsAsFactors = FALSE
    )
  )
  
  medications <- reactiveVal(
    if (!is.null(existing_data)) existing_data$medications
    else data.frame(
      patient_id = integer(),
      id = integer(),
      text = character(),
      stringsAsFactors = FALSE
    )
  )
  
  allergies <- reactiveVal(
    if (!is.null(existing_data)) existing_data$allergies
    else data.frame(
      patient_id = integer(),
      id = integer(),
      text = character(),
      stringsAsFactors = FALSE
    )
  )
  
  visits <- reactiveVal(
    if (!is.null(existing_data)) existing_data$visits
    else data.frame(
      patient_id = integer(),
      id = integer(),
      date = character(),
      visit_type = character(),
      chief_complaint = character(),
      subjective = character(),
      objective = character(),
      assessment_plan = character(),
      icd10_codes = character(),
      cpt_codes = character(),
      stringsAsFactors = FALSE
    )
  )
  
  vitals <- reactiveVal(
    if (!is.null(existing_data)) existing_data$vitals
    else data.frame(
      patient_id = integer(),
      id = integer(),
      date = character(),
      weight = numeric(),
      height = numeric(),
      waist = numeric(),
      bmi = numeric(),
      hr = numeric(),
      bp_systolic = numeric(),
      bp_diastolic = numeric(),
      resp_rate = numeric(),
      pulse_ox = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  # Auto-save function that triggers on data changes
  auto_save <- reactive({
    list(patients(), problems(), medications(), allergies(), visits(), vitals())
  })
  
  observeEvent(auto_save(), {
    save_ehr_data(patients(), problems(), medications(), allergies(), visits(), vitals())
  }, ignoreInit = TRUE)
  
  current_patient_id <- reactiveVal(NULL)
  
  # Patient table output
  output$patient_table <- renderDT({
    datatable(patients(), selection = 'single', options = list(pageLength = 10))
  })
  
  # Current patient in sidebar
  output$current_patient_sidebar <- renderText({
    if (is.null(current_patient_id())) {
      "No patient selected"
    } else {
      pt <- patients()[patients()$id == current_patient_id(), ]
      if (nrow(pt) > 0) {
        paste(pt$name, "-", pt$mrn)
      } else {
        "No patient selected"
      }
    }
  })
  
  # Select patient from table
  observeEvent(input$patient_table_rows_selected, {
    if (length(input$patient_table_rows_selected) > 0) {
      selected_row <- input$patient_table_rows_selected
      pt_data <- patients()
      current_patient_id(pt_data[selected_row, "id"])
      
      # Update patient info fields
      updateTextInput(session, "patient_name", value = pt_data[selected_row, "name"])
      updateDateInput(session, "patient_dob", value = as.Date(pt_data[selected_row, "dob"]))
      updateTextInput(session, "patient_mrn", value = pt_data[selected_row, "mrn"])
      updateTextInput(session, "patient_phone", value = pt_data[selected_row, "phone"])
      updateTextInput(session, "patient_email", value = pt_data[selected_row, "email"])
      
      # Switch to chart tab
      updateTabItems(session, "sidebarMenu", "chart")
    }
  })
  
  # New patient
  observeEvent(input$new_patient, {
    new_id <- as.integer(Sys.time())
    new_mrn <- paste0("MRN", new_id)
    
    new_patient <- data.frame(
      id = new_id,
      name = "New Patient",
      dob = as.character(Sys.Date()),
      mrn = new_mrn,
      phone = "",
      email = "",
      stringsAsFactors = FALSE
    )
    
    patients(rbind(patients(), new_patient))
    current_patient_id(new_id)
    
    updateTextInput(session, "patient_name", value = "New Patient")
    updateDateInput(session, "patient_dob", value = Sys.Date())
    updateTextInput(session, "patient_mrn", value = new_mrn)
    updateTextInput(session, "patient_phone", value = "")
    updateTextInput(session, "patient_email", value = "")
    
    updateTabItems(session, "sidebarMenu", "chart")
  })
  
  # Update patient info when fields change
  observe({
    if (!is.null(current_patient_id())) {
      pt_data <- patients()
      idx <- which(pt_data$id == current_patient_id())
      if (length(idx) > 0) {
        pt_data[idx, "name"] <- input$patient_name
        pt_data[idx, "dob"] <- as.character(input$patient_dob)
        pt_data[idx, "phone"] <- input$patient_phone
        pt_data[idx, "email"] <- input$patient_email
        patients(pt_data)
      }
    }
  })
  
  # Problem list UI
  output$problem_list_ui <- renderUI({
    if (is.null(current_patient_id())) return(NULL)
    
    pt_problems <- problems()[problems()$patient_id == current_patient_id(), ]
    if (nrow(pt_problems) == 0) return(p("No problems listed", style = "padding-left: 15px;"))
    
    lapply(1:nrow(pt_problems), function(i) {
      prob <- pt_problems[i, ]
      div(style = "padding-left: 15px; padding-right: 15px; margin-bottom: 5px;",
        fluidRow(
          column(10, p(prob$text, style = "margin: 0;")),
          column(2, actionButton(paste0("remove_prob_", prob$id), "×", 
                                class = "btn-danger btn-xs"))
        )
      )
    })
  })
  
  # Medication list UI
  output$medication_list_ui <- renderUI({
    if (is.null(current_patient_id())) return(NULL)
    
    pt_meds <- medications()[medications()$patient_id == current_patient_id(), ]
    if (nrow(pt_meds) == 0) return(p("No medications listed", style = "padding-left: 15px;"))
    
    lapply(1:nrow(pt_meds), function(i) {
      med <- pt_meds[i, ]
      div(style = "padding-left: 15px; padding-right: 15px; margin-bottom: 5px;",
        fluidRow(
          column(10, p(med$text, style = "margin: 0;")),
          column(2, actionButton(paste0("remove_med_", med$id), "×",
                                class = "btn-danger btn-xs"))
        )
      )
    })
  })
  
  # Allergy list UI
  output$allergy_list_ui <- renderUI({
    if (is.null(current_patient_id())) return(NULL)
    
    pt_allergies <- allergies()[allergies()$patient_id == current_patient_id(), ]
    if (nrow(pt_allergies) == 0) return(p("No allergies listed", style = "padding-left: 15px;"))
    
    lapply(1:nrow(pt_allergies), function(i) {
      allergy <- pt_allergies[i, ]
      div(style = "padding-left: 15px; padding-right: 15px; margin-bottom: 5px;",
        fluidRow(
          column(10, p(allergy$text, style = "margin: 0; color: red; font-weight: bold;")),
          column(2, actionButton(paste0("remove_allergy_", allergy$id), "×",
                                class = "btn-danger btn-xs"))
        )
      )
    })
  })
  
  # Add problem
  observeEvent(input$add_problem, {
    if (is.null(current_patient_id())) {
      showNotification("Please select a patient first", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Add Problem",
      textInput("new_problem_text", "Problem:", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_problem", "Add", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_add_problem, {
    if (nchar(input$new_problem_text) > 0) {
      new_prob <- data.frame(
        patient_id = current_patient_id(),
        id = as.integer(Sys.time()),
        text = input$new_problem_text,
        stringsAsFactors = FALSE
      )
      problems(rbind(problems(), new_prob))
      removeModal()
    }
  })
  
  # Add medication
  observeEvent(input$add_medication, {
    if (is.null(current_patient_id())) {
      showNotification("Please select a patient first", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Add Medication",
      textInput("new_medication_text", "Medication:", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_medication", "Add", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_add_medication, {
    if (nchar(input$new_medication_text) > 0) {
      new_med <- data.frame(
        patient_id = current_patient_id(),
        id = as.integer(Sys.time()),
        text = input$new_medication_text,
        stringsAsFactors = FALSE
      )
      medications(rbind(medications(), new_med))
      removeModal()
    }
  })
  
  # Add allergy
  observeEvent(input$add_allergy, {
    if (is.null(current_patient_id())) {
      showNotification("Please select a patient first", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Add Allergy",
      textInput("new_allergy_text", "Allergy:", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_allergy", "Add", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_add_allergy, {
    if (nchar(input$new_allergy_text) > 0) {
      new_allergy <- data.frame(
        patient_id = current_patient_id(),
        id = as.integer(Sys.time()),
        text = input$new_allergy_text,
        stringsAsFactors = FALSE
      )
      allergies(rbind(allergies(), new_allergy))
      removeModal()
    }
  })
  
  # Dynamic observers for remove buttons
  observe({
    pt_problems <- problems()[problems()$patient_id == current_patient_id(), ]
    lapply(pt_problems$id, function(prob_id) {
      observeEvent(input[[paste0("remove_prob_", prob_id)]], {
        problems(problems()[problems()$id != prob_id, ])
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    pt_meds <- medications()[medications()$patient_id == current_patient_id(), ]
    lapply(pt_meds$id, function(med_id) {
      observeEvent(input[[paste0("remove_med_", med_id)]], {
        medications(medications()[medications()$id != med_id, ])
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    pt_allergies <- allergies()[allergies()$patient_id == current_patient_id(), ]
    lapply(pt_allergies$id, function(allergy_id) {
      observeEvent(input[[paste0("remove_allergy_", allergy_id)]], {
        allergies(allergies()[allergies()$id != allergy_id, ])
      }, ignoreInit = TRUE)
    })
  })
  
  # New visit
  observeEvent(input$new_visit, {
    if (is.null(current_patient_id())) {
      showNotification("Please select a patient first", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "New Visit",
      size = "l",
      dateInput("visit_date", "Date:", value = Sys.Date()),
      radioButtons("visit_type", "Visit Type:", 
                   choices = c("Video" = "video", "Phone" = "phone", "In Person" = "in_person", "Asynchronous" = "asynchronous"),
                   selected = "video", inline = TRUE),
      textInput("visit_cc", "Chief Complaint:", ""),
      
      h5("Subjective"),
      textAreaInput("visit_subjective", NULL, "", rows = 4, 
                   placeholder = "Patient's description of symptoms, history..."),
      
      h5("Objective"),
      textAreaInput("visit_objective", NULL, "", rows = 4,
                   placeholder = "Physical exam findings, vital signs, test results..."),
      
      h5("Assessment/Plan"),
      textAreaInput("visit_assessment_plan", NULL, "", rows = 4,
                   placeholder = "Diagnosis, treatment plan, follow-up..."),
      
      # ICD-10 search and selection
      h5("ICD-10 Codes"),
      textInput("icd10_search", "Search ICD-10:", placeholder = "Type code or description..."),
      selectInput("visit_icd10", NULL, 
                 choices = character(0),
                 multiple = TRUE, size = 5, selectize = FALSE),
      
      # CPT search and selection
      h5("CPT Codes"),
      textInput("cpt_search", "Search CPT:", placeholder = "Type code or description..."),
      selectInput("visit_cpt", NULL,
                 choices = character(0),
                 multiple = TRUE, size = 5, selectize = FALSE),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_new_visit", "Save Visit", class = "btn-primary")
      )
    ))
  })
  
  # Dynamic ICD-10 search
  observe({
    search_term <- input$icd10_search
    
    if (!is.null(search_term) && nchar(search_term) > 0) {
      # Search in both code and description
      matches <- icd10_codes[
        grepl(search_term, icd10_codes$code, ignore.case = TRUE) |
        grepl(search_term, icd10_codes$description, ignore.case = TRUE),
      ]
      
      # Limit to top 100 matches for performance
      if (nrow(matches) > 100) {
        matches <- matches[1:100, ]
      }
      
      choices <- paste(matches$code, "-", matches$description)
    } else {
      # Show common codes if no search term
      common_codes <- icd10_codes[1:min(20, nrow(icd10_codes)), ]
      choices <- paste(common_codes$code, "-", common_codes$description)
    }
    
    updateSelectInput(session, "visit_icd10", choices = choices)
  })
  
  # Dynamic CPT search
  observe({
    search_term <- input$cpt_search
    
    if (!is.null(search_term) && nchar(search_term) > 0) {
      # Search in both code and description
      matches <- cpt_codes[
        grepl(search_term, cpt_codes$code, ignore.case = TRUE) |
        grepl(search_term, cpt_codes$description, ignore.case = TRUE),
      ]
      
      # Limit to top 100 matches for performance
      if (nrow(matches) > 100) {
        matches <- matches[1:100, ]
      }
      
      choices <- paste(matches$code, "-", matches$description)
    } else {
      # Show common codes if no search term
      common_codes <- cpt_codes[1:min(20, nrow(cpt_codes)), ]
      choices <- paste(common_codes$code, "-", common_codes$description)
    }
    
    updateSelectInput(session, "visit_cpt", choices = choices)
  })
  
  observeEvent(input$confirm_new_visit, {
    new_visit <- data.frame(
      patient_id = current_patient_id(),
      id = as.integer(Sys.time()),
      date = as.character(input$visit_date),
      visit_type = input$visit_type,
      chief_complaint = input$visit_cc,
      subjective = input$visit_subjective,
      objective = input$visit_objective,
      assessment_plan = input$visit_assessment_plan,
      icd10_codes = paste(input$visit_icd10, collapse = "; "),
      cpt_codes = paste(input$visit_cpt, collapse = "; "),
      stringsAsFactors = FALSE
    )
    visits(rbind(visits(), new_visit))
    removeModal()
  })
  
  # Visits UI
  output$visits_ui <- renderUI({
    if (is.null(current_patient_id())) return(p("No patient selected"))
    
    pt_visits <- visits()[visits()$patient_id == current_patient_id(), ]
    if (nrow(pt_visits) == 0) return(p("No visits recorded"))
    
    lapply(1:nrow(pt_visits), function(i) {
      visit <- pt_visits[i, ]
      visit_type_label <- switch(visit$visit_type,
                                 "video" = "Video Visit",
                                 "phone" = "Phone Visit",
                                 "in_person" = "In Person Visit",
                                 "asynchronous" = "Asynchronous Visit",
                                 "Unknown")
      box(width = 12, title = paste(visit_type_label, "-", visit$date),
        p(strong("Chief Complaint:"), visit$chief_complaint),
        hr(),
        p(strong("Subjective:")),
        p(visit$subjective, style = "margin-left: 20px; white-space: pre-wrap;"),
        p(strong("Objective:")),
        p(visit$objective, style = "margin-left: 20px; white-space: pre-wrap;"),
        p(strong("Assessment/Plan:")),
        p(visit$assessment_plan, style = "margin-left: 20px; white-space: pre-wrap;"),
        hr(),
        p(strong("ICD-10:"), visit$icd10_codes),
        p(strong("CPT:"), visit$cpt_codes)
      )
    })
  })
  
  # Add vitals
  observeEvent(input$add_vital, {
    if (is.null(current_patient_id())) {
      showNotification("Please select a patient first", type = "error")
      return()
    }
    
    # Calculate BMI if weight and height are provided (using lbs and inches)
    bmi <- NA
    if (!is.na(input$vital_weight) && !is.na(input$vital_height)) {
      # BMI = (weight in lbs / (height in inches)^2) * 703
      bmi <- round((input$vital_weight / (input$vital_height^2)) * 703, 1)
    }
    
    new_vital <- data.frame(
      patient_id = current_patient_id(),
      id = as.integer(Sys.time()),
      date = as.character(input$vital_date),
      weight = ifelse(is.na(input$vital_weight), NA, input$vital_weight),
      height = ifelse(is.na(input$vital_height), NA, input$vital_height),
      waist = ifelse(is.na(input$vital_waist), NA, input$vital_waist),
      bmi = bmi,
      hr = ifelse(is.na(input$vital_hr), NA, input$vital_hr),
      bp_systolic = ifelse(is.na(input$vital_bp_sys), NA, input$vital_bp_sys),
      bp_diastolic = ifelse(is.na(input$vital_bp_dia), NA, input$vital_bp_dia),
      resp_rate = ifelse(is.na(input$vital_resp), NA, input$vital_resp),
      pulse_ox = ifelse(is.na(input$vital_ox), NA, input$vital_ox),
      stringsAsFactors = FALSE
    )
    
    vitals(rbind(vitals(), new_vital))
    
    # Clear inputs
    updateDateInput(session, "vital_date", value = Sys.Date())
    updateNumericInput(session, "vital_weight", value = NA)
    updateNumericInput(session, "vital_height", value = NA)
    updateNumericInput(session, "vital_waist", value = NA)
    updateNumericInput(session, "vital_hr", value = NA)
    updateNumericInput(session, "vital_bp_sys", value = NA)
    updateNumericInput(session, "vital_bp_dia", value = NA)
    updateNumericInput(session, "vital_resp", value = NA)
    updateNumericInput(session, "vital_ox", value = NA)
  })
  
  # Vitals table
  output$vitals_table <- renderDT({
    if (is.null(current_patient_id())) return(NULL)
    
    pt_vitals <- vitals()[vitals()$patient_id == current_patient_id(), ]
    
    if (nrow(pt_vitals) > 0) {
      # Create BP column combining systolic/diastolic
      pt_vitals$BP <- ifelse(!is.na(pt_vitals$bp_systolic) & !is.na(pt_vitals$bp_diastolic),
                             paste0(pt_vitals$bp_systolic, "/", pt_vitals$bp_diastolic),
                             NA)
      
      # Select and rename columns for display
      display_vitals <- pt_vitals[, c("date", "weight", "height", "bmi", "waist", 
                                      "hr", "BP", "resp_rate", "pulse_ox")]
      names(display_vitals) <- c("Date", "Weight (lbs)", "Height (in)", "BMI", 
                                 "Waist (in)", "HR (bpm)", "BP (mmHg)", 
                                 "RR (/min)", "SpO2 (%)")
    } else {
      display_vitals <- data.frame()
    }
    
    datatable(display_vitals, options = list(pageLength = 10))
  })
  
  # Export chart
  output$export_chart <- downloadHandler(
    filename = function() {
      if (is.null(current_patient_id())) {
        "chart_export.json"
      } else {
        pt <- patients()[patients()$id == current_patient_id(), ]
        paste0(pt$mrn, "_chart.json")
      }
    },
    content = function(file) {
      if (is.null(current_patient_id())) {
        showNotification("Please select a patient first", type = "error")
        return()
      }
      
      pt <- patients()[patients()$id == current_patient_id(), ]
      pt_problems <- problems()[problems()$patient_id == current_patient_id(), ]
      pt_meds <- medications()[medications()$patient_id == current_patient_id(), ]
      pt_allergies <- allergies()[allergies()$patient_id == current_patient_id(), ]
      pt_visits <- visits()[visits()$patient_id == current_patient_id(), ]
      pt_vitals <- vitals()[vitals()$patient_id == current_patient_id(), ]
      
      chart_data <- list(
        patient = pt,
        problems = pt_problems,
        medications = pt_meds,
        allergies = pt_allergies,
        visits = pt_visits,
        vitals = pt_vitals
      )
      
      writeLines(toJSON(chart_data, pretty = TRUE), file)
    }
  )
}

shinyApp(ui, server)