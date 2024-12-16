# UPR Summaries UI function
uprSummariesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("UPRSumBox"), width = 6),
      valueBoxOutput(ns("DACSumBox"), width = 6)
    ),
    fluidRow(
      bs4Card(
        title = "Class-wise Gross UPR Summarization",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        fluidRow(hr(), hr(),
          downloadButton(ns("downloadUPR"), "Download Gross UPR Summary Table", class = "btn btn-primary btn-primary-custom"),
        ),
        br(),
        fluidRow(
            hr(),
            actionButton(ns("calcClassWiseUPR"), "Calculate  Class-wise UPR", class = "btn btn-primary btn-primary-custom"),
            hr()),
        DTOutput(ns("classWiseUPR"))
      )
    ),
    fluidRow(
      bs4Card(
        title = "Class-wise Gross UPR Plot",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        plotOutput(ns("classWiseUPRPlot"))
      )
    )
  )
}


# UPR Summaries server function
uprSummariesServer <- function(id, processedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate and display Gross UPR Sum
    output$UPRSumBox <- renderValueBox({
      req(input$calcClassWiseUPR, processedData())
      upr_sum <- sum(processedData()$Gross_UPR, na.rm = TRUE)
      formatted_upr_sum <- comma(upr_sum)
      valueBox(
        value = formatted_upr_sum,
        subtitle = "Gross UPR Sum",
        icon = icon("dollar-sign", class = "fa-2x", style = "color: white;"),
        color = "white"
      )
    })

    # Calculate and display DAC Sum
    output$DACSumBox <- renderValueBox({
      req(input$calcClassWiseUPR, processedData())
      dac_sum <- sum(processedData()$DAC, na.rm = TRUE)
      formatted_dac_sum <- comma(dac_sum)
      valueBox(
        value = formatted_dac_sum,
        subtitle = "DAC Sum",
        icon = icon("coins", class = "fa-2x", style = "color: white;"),
        color = "white"
      )
    })

    # Reactive function for class-wise UPR summarization
    classWiseUPR <- eventReactive(input$calcClassWiseUPR, {
      req(processedData())
      processedData() %>%
        group_by(`IRA CLASS`) %>%
        summarise(
          `Class wise Gross UPR Sum` = sum(Gross_UPR, na.rm = TRUE), 
          `Class wise DAC Sum` = sum(DAC, na.rm = TRUE)
        ) %>%
        mutate(
          `Class wise Gross UPR Sum` = scales::comma(`Class wise Gross UPR Sum`), 
          `Class wise DAC Sum` = scales::comma(`Class wise DAC Sum`)
        )
    })

    # Display class-wise UPR summarization with enhanced styling
    output$classWiseUPR <- renderDT({
      req(classWiseUPR())
      datatable(classWiseUPR(), options = list(
        pageLength = 30,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        class = 'cell-border stripe'
      ))
    })

    # Define download handler for the UPR table
    output$downloadUPR <- downloadHandler(
      filename = function() {
        paste("Class-wise-Gross-UPR-Summary", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(classWiseUPR())
        write.csv(classWiseUPR(), file, row.names = FALSE)
      }
    )

    # Render the bar graph for class-wise UPR
    output$classWiseUPRPlot <- renderPlot({
      req(classWiseUPR())
      data <- classWiseUPR() %>%
        mutate(`Class wise Gross UPR Sum` = as.numeric(gsub(",", "", `Class wise Gross UPR Sum`)))
        # Order data by `Class wise Gross UPR Sum` in descending order
      data <- data %>%
        mutate(`IRA CLASS` = reorder(`IRA CLASS`, -`Class wise Gross UPR Sum`))

      ggplot(data, aes(y = `IRA CLASS`, x = `Class wise Gross UPR Sum`, fill = `IRA CLASS`)) +
        geom_bar(stat = "identity", color = "black", fill = "#2575fc") +
        geom_text(aes(label = paste0(round(`Class wise Gross UPR Sum` / 1e6, 0), "M")),
                  vjust = -0.5, color = "black", size = 3.7, hjust = -0.1) +
        labs(title = "Class-wise Gross UPR Summary",
             x = "IRA Class",
             y = "Gross UPR Sum") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text.x = element_text(angle = 0, hjust = 1),
              legend.position = "none",
              panel.grid = element_blank())
    })
  })
}


