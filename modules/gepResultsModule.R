# GEP Results UI function
actionButton("goButton", "Calculate GEP Summary", class = "btn btn-primary btn-primary-custom")
gepResultsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Gross Earned Premiums Results by IRA Class",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          hr(),
          numericInput(ns("startYear"), "Select Start Year of Analysis", value = 2022, min = 2000, max = 3000),
          hr(),
          numericInput(ns("endYear"), "Select End Year of Analysis", value = 2023, min = 2000, max = 3000),
          hr(),
          ),
          br(),
         fluidRow(
          hr(),
          selectInput(ns("timePeriod"), "Select Time Period for Analysis", choices = c("Monthly", "Quarterly"), selected = "Monthly"),
          hr(),
          selectInput(ns("endPeriod"), "Select End Month/Quarter for Analysis", choices = c("All" = "All"), selected = "All"),
          hr()
        ),
        fluidRow(hr(), hr(),
          downloadButton(ns("downloadData"), "Download GEP Summary Table", class = "btn btn-primary btn-primary-custom"),
        ),
        br(),
        fluidRow(
            hr(),
            actionButton(ns("goButton"), "Calculate GEP Summary", class = "btn btn-primary btn-primary-custom"),
            hr()),
        DTOutput(ns("summaryData"))
    )
    )
  )
}



# GEP Results server function
gepResultsServer <- function(id, processedData, cutoffYear) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(input$timePeriod) 
      if (input$timePeriod == "Monthly") {
        updateSelectInput(session, "endPeriod", choices = c("All" = "All", month.name), selected = "All")
      } else {  # Quarterly
        updateSelectInput(session, "endPeriod", choices = c("All" = "All", "Q1" = "Q1", "Q2" = "Q2", "Q3" = "Q3", "Q4" = "Q4"), selected = "All")
      }
    })

    # Reactive function for summarizing data by IRA Class
    summaryData <- eventReactive(input$goButton, {
      req(processedData())
      req(input$startYear)

      withProgress(message = 'Calculating summaries...', {
        setProgress(0)  # Initialize progress
        data <- processedData()
        timePeriod <- input$timePeriod
        cutoffYearVal <- cutoffYear()
        
        results <- calculatePremiums(data, input$startYear, input$endYear, input$endPeriod, timePeriod, cutoffYearVal)
        
        summarized <- results %>%
          group_by(`IRA CLASS`) %>%
          summarize(across(contains("_EP"), sum, na.rm = TRUE))
        
        # Apply formatting with commas to all summarized columns
        summarized <- summarized %>%
          mutate(across(contains("_EP"), scales::comma))
        return(summarized)
      })
    })
    
    output$summaryData <- renderDT({
      req(summaryData())
      datatable(summaryData(), options = list(
        paging = TRUE,
        scrollX = TRUE, 
        pageLength = 20,
        autoWidth = TRUE,
        searching = FALSE,
        info = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({",
          "    'background-color': '#FFFFFF',", 
          "    'color': '#000000'",  
          "  });",
          "}"
        )
      ))
    })
    
    # Add a download handler for the summary data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Earned Premiums Summary-Data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(summaryData())  # Ensure the summary data is ready before attempting to download
        write.csv(summaryData(), file, row.names = FALSE)
      }
    )
  })
}
