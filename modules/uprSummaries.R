# UPR Summaries UI function
uprSummariesUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Direct Business Section Header
    fluidRow(
      column(12, 
        tags$h4("Direct Business Summary", class = "section-header", 
                style = "margin-top: 20px; margin-bottom: 15px; font-weight: bold; color: #102A56;")
      )
    ),
    fluidRow(
      valueBoxOutput(ns("UPRSumBox"), width = 6),
      valueBoxOutput(ns("DACSumBox"), width = 6)
    ),
    # Reinsurance Section Header
    fluidRow(
      column(12, 
        tags$h4("Reinsurance Summary", class = "section-header", 
                style = "margin-top: 30px; margin-bottom: 15px; font-weight: bold; color: #0137A6;")
      )
    ),
    fluidRow(
      valueBoxOutput(ns("RIUPRSumBox"), width = 6),
      valueBoxOutput(ns("RIDACSumBox"), width = 6)
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
        withSpinner(DTOutput(ns("classWiseUPR")), type = 6, color = "#0137A6")
      )
    ),  
    fluidRow(
      bs4Card(
        title = "Class-wise Gross UPR Plot",
        status = "white",
        solidHeader = TRUE,
        width = 6,
        withSpinner(plotOutput(ns("classWiseUPRPlot")), type = 6, color = "#0137A6")
      ),
      bs4Card(
        title = "Class-wise RI Gross UPR Plot",
        status = "white",
        solidHeader = TRUE,
        width = 6,
        withSpinner(plotOutput(ns("classWiseRIUPRPlot")), type = 6, color = "#0137A6")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Class-wise DAC Plot",
        status = "white",
        solidHeader = TRUE,
        width = 6,
        withSpinner(plotOutput(ns("classWiseDACPlot")), type = 6, color = "#0137A6")
      ),
      bs4Card(
        title = "Class-wise RI DAC Plot",
        status = "white",
        solidHeader = TRUE,
        width = 6,
        withSpinner(plotOutput(ns("classWiseRIDACPlot")), type = 6, color = "#0137A6")
      )
    )
  )
}


# UPR Summaries server function
uprSummariesServer <- function(id, processedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate and display Gross UPR Sum - Auto-displays when data is available
    output$UPRSumBox <- renderValueBox({
      req(processedData())
      upr_sum <- sum(processedData()$Gross_UPR, na.rm = TRUE)
      formatted_upr_sum <- comma(upr_sum)
      valueBox(
        value = formatted_upr_sum,
        subtitle = "Gross UPR Sum",
        icon = icon("dollar-sign", class = "fa-2x", style = "color: white;"),
        color = "white"
      )
    })

    # Calculate and display DAC Sum - Auto-displays when data is available
    output$DACSumBox <- renderValueBox({
      req(processedData())
      dac_sum <- sum(processedData()$DAC, na.rm = TRUE)
      formatted_dac_sum <- comma(dac_sum)
      valueBox(
        value = formatted_dac_sum,
        subtitle = "DAC Sum",
        icon = icon("coins", class = "fa-2x", style = "color: white;"),
        color = "white"
      )
    })

    # Calculate and display RI Gross UPR Sum - Auto-displays when data is available
    output$RIUPRSumBox <- renderValueBox({
      req(processedData())
      ri_upr_sum <- sum(processedData()$RI_Gross_UPR, na.rm = TRUE)
      formatted_ri_upr_sum <- comma(ri_upr_sum)
      valueBox(
        value = formatted_ri_upr_sum,
        subtitle = "RI Gross UPR Sum",
        icon = icon("shield-alt", class = "fa-2x", style = "color: white;"),
        color = "white"
      )
    })

    # Calculate and display RI DAC Sum - Auto-displays when data is available
    output$RIDACSumBox <- renderValueBox({
      req(processedData())
      ri_dac_sum <- sum(processedData()$RI_DAC, na.rm = TRUE)
      formatted_ri_dac_sum <- comma(ri_dac_sum)
      valueBox(
        value = formatted_ri_dac_sum,
        subtitle = "RI DAC Sum",
        icon = icon("handshake", class = "fa-2x", style = "color: white;"),
        color = "white"
      )
    })

    # Reactive function for class-wise UPR summarization - Auto-calculates when data is available
    classWiseUPR <- reactive({
      req(processedData())
      data <- processedData() %>%
        group_by(`IRA CLASS`) %>%
        summarise(
          `Class wise Gross UPR Sum` = sum(Gross_UPR, na.rm = TRUE), 
          `Class wise DAC Sum` = sum(DAC, na.rm = TRUE),
          `Class wise RI Gross UPR Sum` = sum(RI_Gross_UPR, na.rm = TRUE),
          `Class wise RI DAC Sum` = sum(RI_DAC, na.rm = TRUE)
        )
      
      # Add totals row
      totals_row <- data.frame(
        `IRA CLASS` = "TOTAL",
        `Class wise Gross UPR Sum` = sum(data$`Class wise Gross UPR Sum`, na.rm = TRUE),
        `Class wise DAC Sum` = sum(data$`Class wise DAC Sum`, na.rm = TRUE),
        `Class wise RI Gross UPR Sum` = sum(data$`Class wise RI Gross UPR Sum`, na.rm = TRUE),
        `Class wise RI DAC Sum` = sum(data$`Class wise RI DAC Sum`, na.rm = TRUE),
        check.names = FALSE
      )
      data <- bind_rows(data, totals_row)
      
      data %>%
        mutate(
          `Class wise Gross UPR Sum` = scales::comma(`Class wise Gross UPR Sum`), 
          `Class wise DAC Sum` = scales::comma(`Class wise DAC Sum`),
          `Class wise RI Gross UPR Sum` = scales::comma(`Class wise RI Gross UPR Sum`),
          `Class wise RI DAC Sum` = scales::comma(`Class wise RI DAC Sum`)
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
        class = 'cell-border stripe',
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

    # Render the bar graph for class-wise RI UPR
    output$classWiseRIUPRPlot <- renderPlot({
      req(classWiseUPR())
      data <- classWiseUPR() %>%
        mutate(`Class wise RI Gross UPR Sum` = as.numeric(gsub(",", "", `Class wise RI Gross UPR Sum`)))
      data <- data %>%
        mutate(`IRA CLASS` = reorder(`IRA CLASS`, -`Class wise RI Gross UPR Sum`))

      ggplot(data, aes(y = `IRA CLASS`, x = `Class wise RI Gross UPR Sum`, fill = `IRA CLASS`)) +
        geom_bar(stat = "identity", color = "black", fill = "#28a745") +
        geom_text(aes(label = paste0(round(`Class wise RI Gross UPR Sum` / 1e6, 0), "M")),
                  vjust = -0.5, color = "black", size = 3.7, hjust = -0.1) +
        labs(title = "Class-wise RI Gross UPR Summary",
             x = "IRA Class",
             y = "RI Gross UPR Sum") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text.x = element_text(angle = 0, hjust = 1),
              legend.position = "none",
              panel.grid = element_blank())
    })


        # Render the bar graph for class-wise UPR
    output$classWiseDACPlot <- renderPlot({
      req(classWiseUPR())
      data <- classWiseUPR() %>%
        mutate(`Class wise DAC Sum` = as.numeric(gsub(",", "", `Class wise DAC Sum`)))
        # Order data by `Class wise DAC Sum` in descending order
      data <- data %>%
        mutate(`IRA CLASS` = reorder(`IRA CLASS`, -`Class wise DAC Sum`))

      ggplot(data, aes(y = `IRA CLASS`, x = `Class wise DAC Sum`, fill = `IRA CLASS`)) +
        geom_bar(stat = "identity", color = "black", fill = "#2575fc") +
        geom_text(aes(label = paste0(round(`Class wise DAC Sum` / 1e6, 0), "M")),
                  vjust = -0.5, color = "black", size = 3.7, hjust = -0.1) +
        labs(title = "Class wise DAC Summary",
             x = "IRA Class",
             y = "DAC Sum") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text.x = element_text(angle = 0, hjust = 1),
              legend.position = "none",
              panel.grid = element_blank())
    })

    # Render the bar graph for class-wise RI DAC
    output$classWiseRIDACPlot <- renderPlot({
      req(classWiseUPR())
      data <- classWiseUPR() %>%
        mutate(`Class wise RI DAC Sum` = as.numeric(gsub(",", "", `Class wise RI DAC Sum`)))
      data <- data %>%
        mutate(`IRA CLASS` = reorder(`IRA CLASS`, -`Class wise RI DAC Sum`))

      ggplot(data, aes(y = `IRA CLASS`, x = `Class wise RI DAC Sum`, fill = `IRA CLASS`)) +
        geom_bar(stat = "identity", color = "black", fill = "#28a745") +
        geom_text(aes(label = paste0(round(`Class wise RI DAC Sum` / 1e6, 0), "M")),
                  vjust = -0.5, color = "black", size = 3.7, hjust = -0.1) +
        labs(title = "Class wise RI DAC Summary",
             x = "IRA Class",
             y = "RI DAC Sum") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text.x = element_text(angle = 0, hjust = 1),
              legend.position = "none",
              panel.grid = element_blank())
    })
  })
}


