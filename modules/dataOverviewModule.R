# UI function for data overview
dataOverviewUI <- function(id) {
  ns <- NS(id)
  tagList(
                 fluidRow(
                   hr(),
                   div(
                     class = "upload-container",
                     fileInput(ns("file1"), 
                               label = tags$span("Upload Premium Data as an Excel or CSV File", class = "upload-label"),
                               accept = c(".xlsx", ".xls", ".csv")),
                     textInput(ns("valDate"), tags$span("Valuation Date", class = "upload-label"), value = "31/12/2023"),
                     numericInput(ns("cutoffYear"), tags$span("Set Policy Start Year Threshold", class = "upload-label"), value = 2013, min = 1990, max = 3000)),
                    hr(), br(), 
                    div(
                      class = "upload-container",
                     tags$p(class = "instruction-header", "How to Prepare Data before Upload:"),
                     tags$ul(
                       class = "list-item",
                       tags$li(class = "custom-list-item", "Ensure the data format is CSV or Excel file."),
                       tags$li(class = "custom-list-item", "The Required Columns are:-"),
                       tags$ul(
                         class = "sub-list-item" ,
                         tags$li(class = "custom-list-item", icon("calendar"), tags$b("BegDate: - "), " Is the policy start date Column."),
                         tags$li(class = "custom-list-item", icon("calendar-alt"), tags$b("EndDate: - "), " Is the policy end date Column."),
                         tags$li(class = "custom-list-item", icon("clipboard-check"), tags$b("AuthDate: - "), " Is the policy underwriting date Column."),
                         tags$li(class = "custom-list-item", icon("briefcase"), tags$b("IRA CLASS: - "), " Is the class of Business Column."),
                         tags$li(class = "custom-list-item", icon("dollar-sign"), tags$b("Premium: - "), " Is the Premium Column.")
                       )
                     )
                   ),
                   hr(),
                   br(),
                   bs4Card(
                     title = "Data Overview",
                     status = "white",
                     solidHeader = TRUE,
                     width = 12,
                     DTOutput(ns("viewData"))
                   )
                 )
  )
}



# Server function for data overview
dataOverviewServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive value for storing the uploaded data with validation
    data <- reactive({
      req(input$file1)
      inFile <- input$file1

    withProgress(message = 'Reading and validating data...', {
      setProgress(0.2)

      # Attempt to read data
      file_extension <- tools::file_ext(inFile$name)
      tryCatch({
      if (file_extension %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(inFile$datapath) %>%
        mutate(
          Premium = as.numeric(Premium),
          Commission = as.numeric(Commission)
        )
      } else if (file_extension == "csv") {
        df <- read_csv(inFile$datapath, 
                       col_types = cols(
                         Premium = col_number(), 
                         AuthDate = col_character(),
                         BegDate = col_character(),
                         EndDate = col_character(), 
                         Commission = col_number()))
      } else {
        stop("Unsupported file type. Please upload a CSV or Excel file.")
      }
        
        # Validate necessary columns
        requiredColumns <- c("Premium", "AuthDate", "BegDate", "EndDate", "Commission", "IRA CLASS")
        if (!all(requiredColumns %in% names(df))) {
          stop("Data must contain the following columns: ", paste(requiredColumns, collapse=", "))
        }

        # Parse dates using lubridate to allow multiple formats
        # orders = c("dmy", "ymd", "mdy") means it will try day-month-year, then year-month-day, then month-day-year
        df <- df %>%
            mutate(
            AuthDate = lubridate::parse_date_time(AuthDate, orders = c("dmy", "ymd", "mdy")),
            BegDate  = lubridate::parse_date_time(BegDate,  orders = c("dmy", "ymd", "mdy")),
            EndDate  = lubridate::parse_date_time(EndDate,  orders = c("dmy", "ymd", "mdy"))
            )

        # Check if any of the date columns failed to parse
        if (any(is.na(df$AuthDate)) || any(is.na(df$BegDate)) || any(is.na(df$EndDate))) {
            warning("Some date values could not be parsed. Please ensure dates are in a recognized format.")
        }

        setProgress(1)
        return(df) 

      }, error = function(e) {
        # Handle errors in data format
        showModal(modalDialog(
          title = "Error in data format",
          paste("Please check your CSV file for the correct columns and data formats. Details: ", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
    })
  })

  reactiveCutoffYear <- reactive({ input$cutoffYear })

  # Process data when go button is clicked
  processedData <- reactive({
    req(data())
    Val_Date <- as.Date(dmy(input$valDate))
    withProgress(message = 'Processing data...', value = 0, {
      setProgress(0.5)  # Indicate progress at halfway
      processed <- data() %>%
        mutate(
          Auth_year = year(AuthDate),
          Duration = as.numeric(difftime(EndDate, BegDate, units = "days")) + 1,
          Unearned_Duration = ifelse(BegDate <= Val_Date & EndDate >= Val_Date, as.numeric(difftime(EndDate, Val_Date, units = "days")), ifelse(BegDate > Val_Date, as.numeric(Duration), ifelse(EndDate <= Val_Date, 0, NA))),
          Earned_Duration = Duration - Unearned_Duration,
          Gross_UPR= ifelse(Auth_year < input$cutoffYear, 0, (as.numeric(Unearned_Duration)/as.numeric(Duration)) * Premium),
          DAC= ifelse(Auth_year < input$cutoffYear, 0, (as.numeric(Unearned_Duration)/as.numeric(Duration)) * -Commission),
          GEP = (as.numeric(Earned_Duration)/as.numeric(Duration))*Premium
        )
      setProgress(1)  # Complete the progress bar when done
      return(processed)
    })
  })
    
    # Render the data table
  
  output$viewData <- renderDT({
    req(processedData())
    datatable(processedData(),
              options = list(scrollX = TRUE, 
                             pageLength = 30,
                             autoWidth = TRUE,
                             paging = TRUE,
                             searching = FALSE,
                             info = FALSE,
                             initComplete = JS(
                               "function(settings, json) {",
                               "  $(this.api().table().header()).css({",
                               "    'background-color': '#FFFFFF',", 
                               "    'color': '#000000'",  
                               "  });",
                               "}"
                             )))
  })
  
  return(list(
    data = reactive({ processedData() }),
    cutoffYear = reactive({ input$cutoffYear })
  ))

  })
}

