# NEP Results UI function
nepResultsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Net Earned Premiums Results by IRA Class",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          hr(),
          numericInput(ns("netStartYear"), "Select Start Year of Analysis", value = 2022, min = 2000, max = 3000),
          hr(),
          numericInput(ns("netEndYear"), "Select End Year of Analysis", value = 2023, min = 2000, max = 3000),
          hr(),
        ),
        br(),
        fluidRow(
          hr(),
          selectInput(ns("netTimePeriod"), "Select Time Period for Analysis", choices = c("Monthly", "Quarterly"), selected = "Monthly"),
          hr(),
          selectInput(ns("netEndPeriod"), "Select End Month/Quarter for Analysis", choices = c("All" = "All"), selected = "All"),
          hr()
        ),
        fluidRow(hr(), hr(),
          downloadButton(ns("downloadNetData"), "Download NEP Summary Table", class = "btn btn-primary btn-primary-custom"),
        ),
        br(),
        fluidRow(
            hr(),
            actionButton(ns("netGoButton"), "Calculate NEP Summary", class = "btn btn-primary btn-primary-custom"),
            hr()),
        DTOutput(ns("netSummaryData"))
      )
    )
  )
}


# NEP Results server function
nepResultsServer <- function(id, processedData, cutoffYear) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamic update for netEndPeriod based on netTimePeriod selection
    observe({
      req(input$netTimePeriod) 
      if (input$netTimePeriod == "Monthly") {
        updateSelectInput(session, "netEndPeriod", choices = c("All" = "All", month.name), selected = "All")
      } else {  # Quarterly
        updateSelectInput(session, "netEndPeriod", choices = c("All" = "All", "Q1" = "Q1", "Q2" = "Q2", "Q3" = "Q3", "Q4" = "Q4"), selected = "All")
      }
    })

    # Reactive values for NEP manual refresh
    nepParams <- reactiveValues(
      trigger = 0  # Used to force recalculation when button is clicked
    )
    
    # Observe button click to trigger NEP recalculation
    observeEvent(input$netGoButton, {
      nepParams$trigger <- nepParams$trigger + 1
    })

    # Reactive function for summarizing Net EP data by IRA Class
    netSummaryData <- reactive({
      req(processedData())
      req(input$netStartYear)
      req(input$netEndYear)
      req(input$netTimePeriod)
      
      # Also react to button clicks for manual refresh
      nepParams$trigger

      withProgress(message = 'Calculating Net EP summaries...', {
        setProgress(0)  # Initialize progress
        data <- processedData()
        timePeriod <- input$netTimePeriod
        cutoffYearVal <- cutoffYear()
        
        results <- calculateNetPremiums(data, input$netStartYear, input$netEndYear, input$netEndPeriod, timePeriod, cutoffYearVal)
        
        summarized <- results %>%
          group_by(`IRA CLASS`) %>%
          summarize(across(contains("_NEP"), sum, na.rm = TRUE))
        
        # Add totals row
        totals_row <- summarized %>%
          summarize(across(contains("_NEP"), sum, na.rm = TRUE)) %>%
          mutate(`IRA CLASS` = "TOTAL") %>%
          select(`IRA CLASS`, everything())
        
        summarized <- bind_rows(summarized, totals_row)
        
        # Apply formatting with commas to all summarized columns
        summarized <- summarized %>%
          mutate(across(contains("_NEP"), scales::comma))
        return(summarized)
      })
    })
    
    output$netSummaryData <- renderDT({
      req(netSummaryData())
      datatable(netSummaryData(), options = list(
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
        ),
        rowCallback = JS(
          "function(row, data, index) {",
          "  if (data[0] === 'TOTAL') {",
          "    $(row).css('font-weight', 'bold');",
          "    $(row).css('background-color', '#E8F4FD');",
          "  }",
          "}"
        )
      ))
    })
    
    # Add a download handler for the Net EP summary data
    output$downloadNetData <- downloadHandler(
      filename = function() {
        paste("Net Earned Premiums Summary-Data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(netSummaryData())  # Ensure the summary data is ready before attempting to download
        write.csv(netSummaryData(), file, row.names = FALSE)
      }
    )
  })
}
