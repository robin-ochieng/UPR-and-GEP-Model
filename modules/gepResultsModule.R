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
          hr()
        ),
        fluidRow(
          hr(),
          selectInput(ns("endYearMonth"), "Select Month for End Year of Analysis", 
                      choices = c("All" = "All", 
                                  "January" = "January", 
                                  "February" = "February", 
                                  "March" = "March", 
                                  "April" = "April", 
                                  "May" = "May", 
                                  "June" = "June", 
                                  "July" = "July", 
                                  "August" = "August", 
                                  "September" = "September", 
                                  "October" = "October", 
                                  "November" = "November", 
                                  "December" = "December"), 
                      selected = "All"),
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


    # Reactive function for summarizing data by IRA Class
    summaryData <- eventReactive(input$goButton, {
      req(processedData())
      withProgress(message = 'Calculating summaries...', {
        setProgress(0)  # Initialize progress
        data <- processedData()
        
        total_operations <- length(input$startYear:input$endYear) * 12  # Assuming up to 12 months per year
        operations_done <- 0
        
        for (yr in input$startYear:input$endYear) {
          year_months <- define_months(yr)
          if (yr == input$endYear && input$endYearMonth != "All") {
            end_month_index <- match(input$endYearMonth, month.name)
            months_to_iterate <- month.name[1:end_month_index]
          } else {
            months_to_iterate <- month.name
          }
          
          for (month in months_to_iterate) {
            month_EP_col <- paste0(month, "_", yr, "_EP")
            data[[month_EP_col]] <- calculate_EP_Months(data, year_months, month, yr, cutoffYear())
            operations_done <- operations_done + 1
            setProgress(operations_done / total_operations)
          }
        }
        
        
        summarized <- data %>%
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
