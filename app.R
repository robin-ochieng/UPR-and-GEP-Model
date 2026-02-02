library(shiny)
library(bs4Dash)
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(zoo)
library(ggplot2)
library(scales)
library(bslib)
library(DT)
library(shinycssloaders)
library(shinyjs)

source("modules/landingPageModule.R", local = TRUE)[1]
source("modules/dataOverviewModule.R", local = TRUE)[1]
source("modules/uprSummaries.R", local = TRUE)[1]
source("modules/gepResultsModule.R", local = TRUE)[1]
source("modules/nepResultsModule.R", local = TRUE)[1]
source("modules/helperFunctions.R")


options(shiny.maxRequestSize = 1000 * 1024^2)  # 100 MB

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#F3F8FF",
  fg = "#102A56",
  primary = "#0137A6",
  secondary = "#6EA8FE",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#F3F8FF",
  navbar_fg = "#102A56"
)

# Define the User Interface for the Application
ui <- bs4DashPage(
  dark = NULL,
  title = "LRC Model",
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  header = bs4DashNavbar(
    status = "white",
    skin = "dark",
    sidebarIcon = NULL,
    controlbarIcon = NULL,
    tags$li(
      class = "text-center header-title-container",  # Added a new class for more specific styling
      tags$h4("LRC Model", class = "header-title")
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    tags$div(
      class = "menu-container",
    bs4SidebarMenu(
      id = "sidebar",
      bs4SidebarMenuItem("Home", tabName = "landing", icon = icon("home")),
      bs4SidebarMenuItem("Data Overview", tabName = "dataOverview", icon = icon("table")),
      bs4SidebarMenuItem("UPR Summaries", tabName = "uprSummaries", icon = icon("chart-bar")),
      bs4SidebarMenuItem("LRC", tabName = "lrc", icon = icon("calculator")),
      bs4SidebarMenuItem("ARC", tabName = "arc", icon = icon("coins")),
      bs4SidebarMenuItem("Net LRC", tabName = "netLrc", icon = icon("balance-scale")),
      bs4SidebarMenuItem("GEP Results", tabName = "gepResults", icon = icon("chart-line")),
      bs4SidebarMenuItem("NEP Results", tabName = "nepResults", icon = icon("chart-area"))
    )),
    div(class = "sidebar-logo",
        img(src = "images/kenbright.png")
    )
  ),
  body = bs4DashBody(
    shinyjs::useShinyjs(),
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      includeCSS("www/css/landing_page.css"),
      tags$link(href = "https://fonts.googleapis.com/css?family=Mulish", rel = "stylesheet"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.1/moment.min.js"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon")
    ),
    bs4TabItems(
      bs4TabItem(tabName = "landing",
        landingPageUI("landing_page")
      ),
      bs4TabItem(tabName = "dataOverview",
        dataOverviewUI("data_overview")
      ),
      bs4TabItem(tabName = "uprSummaries",
        uprSummariesUI("upr_summaries")                 
      ),
      bs4TabItem(tabName = "lrc",
        fluidRow(
          column(12,
            tags$h4("Liability for Remaining Coverage (LRC)", class = "section-header", 
                    style = "margin-top: 20px; margin-bottom: 15px; font-weight: bold; color: #102A56;")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Class-wise LRC Summary",
            status = "white",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              hr(),
              downloadButton("downloadLRC", "Download LRC Summary Table", class = "btn btn-primary btn-primary-custom")
            ),
            br(),
            fluidRow(
              hr(),
              actionButton("calcLRC", "Calculate Class-wise LRC", class = "btn btn-primary btn-primary-custom"),
              hr()
            ),
            withSpinner(DTOutput("lrcTable"), type = 6, color = "#0137A6")
          )
        )
      ),
      bs4TabItem(tabName = "arc",
        fluidRow(
          column(12,
            tags$h4("Asset for Remaining Coverage (ARC)", class = "section-header", 
                    style = "margin-top: 20px; margin-bottom: 15px; font-weight: bold; color: #102A56;")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Class-wise ARC Summary",
            status = "white",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              hr(),
              downloadButton("downloadARC", "Download ARC Summary Table", class = "btn btn-primary btn-primary-custom")
            ),
            br(),
            fluidRow(
              hr(),
              actionButton("calcARC", "Calculate Class-wise ARC", class = "btn btn-primary btn-primary-custom"),
              hr()
            ),
            withSpinner(DTOutput("arcTable"), type = 6, color = "#0137A6")
          )
        )
      ),
      bs4TabItem(tabName = "netLrc",
        fluidRow(
          column(12,
            tags$h4("Net Liability for Remaining Coverage (Net LRC)", class = "section-header", 
                    style = "margin-top: 20px; margin-bottom: 15px; font-weight: bold; color: #102A56;")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Class-wise Net LRC Summary",
            status = "white",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              hr(),
              downloadButton("downloadNetLRC", "Download Net LRC Summary Table", class = "btn btn-primary btn-primary-custom")
            ),
            br(),
            fluidRow(
              hr(),
              actionButton("calcNetLRC", "Calculate Class-wise Net LRC", class = "btn btn-primary btn-primary-custom"),
              hr()
            ),
            withSpinner(DTOutput("netLrcTable"), type = 6, color = "#0137A6")
          )
        )
      ),
      bs4TabItem(tabName = "gepResults",
        gepResultsUI("gep_results")
      ),
      bs4TabItem(tabName = "nepResults",
        nepResultsUI("nep_results")
      )
    )
  ),
  footer = bs4DashFooter(
    left = NULL,
    right = NULL,
    tags$div(
      style = "width: 100%; text-align: center; color: #0137A6; font-weight: 600; font-size: 12px;",
      paste0("Developed by Kenbright AI © ", format(Sys.Date(), "%Y"))
    )
  )
)



# Define the server logic required to read the input and calculate outputs
server <- function(input, output, session) {
  
  # Store parent session for module navigation
  session$userData$parentSession <- session
  
  # Landing page server
  landingPageServer("landing_page")
  
  observeEvent(input$toggleControlbar, {
    updateBoxSidebar("controlbar")
  })

  overviewResults  <- dataOverviewServer("data_overview")
  
  processedData <- overviewResults$data
  
  cutoffYear <- overviewResults$cutoffYear

  uprSummariesServer("upr_summaries", processedData)

  gepResultsServer("gep_results", processedData, cutoffYear)

  nepResultsServer("nep_results", processedData, cutoffYear)

  # LRC Tab Logic - Using reactiveValues for editable table
  lrcValues <- reactiveValues(data = NULL)

  # Auto-calculate LRC when data is uploaded
  observe({
    req(processedData())
    df <- processedData() %>%
      group_by(`IRA CLASS`) %>%
      summarise(
        `Class wise Gross UPR Sum` = sum(Gross_UPR, na.rm = TRUE), 
        `Class wise DAC Sum` = sum(DAC, na.rm = TRUE)
      ) %>%
      mutate(
        `Premium Receivables` = 0,
        `Bad Debt` = 0,
        `LRC` = `Class wise Gross UPR Sum` - `Class wise DAC Sum` - `Premium Receivables` + `Bad Debt`
      )
    
    # Add totals row
    totals_row <- data.frame(
      `IRA CLASS` = "TOTAL",
      `Class wise Gross UPR Sum` = sum(df$`Class wise Gross UPR Sum`, na.rm = TRUE),
      `Class wise DAC Sum` = sum(df$`Class wise DAC Sum`, na.rm = TRUE),
      `Premium Receivables` = sum(df$`Premium Receivables`, na.rm = TRUE),
      `Bad Debt` = sum(df$`Bad Debt`, na.rm = TRUE),
      `LRC` = sum(df$`LRC`, na.rm = TRUE),
      check.names = FALSE
    )
    df <- bind_rows(df, totals_row)
    
    lrcValues$data <- df
  })

  # Also recalculate on button click (for manual refresh/reset after edits)
  observeEvent(input$calcLRC, {
    req(processedData())
    df <- processedData() %>%
      group_by(`IRA CLASS`) %>%
      summarise(
        `Class wise Gross UPR Sum` = sum(Gross_UPR, na.rm = TRUE), 
        `Class wise DAC Sum` = sum(DAC, na.rm = TRUE)
      ) %>%
      mutate(
        `Premium Receivables` = 0,
        `Bad Debt` = 0,
        `LRC` = `Class wise Gross UPR Sum` - `Class wise DAC Sum` - `Premium Receivables` + `Bad Debt`
      )
    
    # Add totals row
    totals_row <- data.frame(
      `IRA CLASS` = "TOTAL",
      `Class wise Gross UPR Sum` = sum(df$`Class wise Gross UPR Sum`, na.rm = TRUE),
      `Class wise DAC Sum` = sum(df$`Class wise DAC Sum`, na.rm = TRUE),
      `Premium Receivables` = sum(df$`Premium Receivables`, na.rm = TRUE),
      `Bad Debt` = sum(df$`Bad Debt`, na.rm = TRUE),
      `LRC` = sum(df$`LRC`, na.rm = TRUE),
      check.names = FALSE
    )
    df <- bind_rows(df, totals_row)
    
    lrcValues$data <- df
  })

  # Handle cell edits for LRC table
  observeEvent(input$lrcTable_cell_edit, {
    info <- input$lrcTable_cell_edit
    row <- info$row
    col <- info$col + 1  # DT uses 0-based index, R uses 1-based
    value <- as.numeric(gsub(",", "", info$value))  # Remove commas and convert to numeric
    
    if (is.na(value)) value <- 0
    
    # Update the value in the reactive data
    if (col == 4) {  # Premium Receivables column
      lrcValues$data[row, "Premium Receivables"] <- value
    } else if (col == 5) {  # Bad Debt column
      lrcValues$data[row, "Bad Debt"] <- value
    }
    
    # Recalculate LRC for the edited row
    lrcValues$data[row, "LRC"] <- lrcValues$data[row, "Class wise Gross UPR Sum"] - 
                                   lrcValues$data[row, "Class wise DAC Sum"] - 
                                   lrcValues$data[row, "Premium Receivables"] + 
                                   lrcValues$data[row, "Bad Debt"]
  })

  output$lrcTable <- renderDT({
    req(lrcValues$data)
    
    # Format data for display
    displayData <- lrcValues$data %>%
      mutate(
        `Class wise Gross UPR Sum` = scales::comma(`Class wise Gross UPR Sum`),
        `Class wise DAC Sum` = scales::comma(`Class wise DAC Sum`),
        `Premium Receivables` = scales::comma(`Premium Receivables`),
        `Bad Debt` = scales::comma(`Bad Debt`),
        `LRC` = scales::comma(`LRC`)
      )
    
    datatable(displayData, 
      options = list(
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
      ),
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 5)))  # Only columns 3,4 (Premium Receivables, Bad Debt) editable
    )
  })

  output$downloadLRC <- downloadHandler(
    filename = function() {
      paste("Class-wise-LRC-Summary-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(lrcValues$data)
      # Format for download
      downloadData <- lrcValues$data %>%
        mutate(
          `Class wise Gross UPR Sum` = scales::comma(`Class wise Gross UPR Sum`),
          `Class wise DAC Sum` = scales::comma(`Class wise DAC Sum`),
          `Premium Receivables` = scales::comma(`Premium Receivables`),
          `Bad Debt` = scales::comma(`Bad Debt`),
          `LRC` = scales::comma(`LRC`)
        )
      write.csv(downloadData, file, row.names = FALSE)
    }
  )

  # ARC Tab Logic - Using reactiveValues for editable table
  arcValues <- reactiveValues(data = NULL)

  # Auto-calculate ARC when data is uploaded
  observe({
    req(processedData())
    df <- processedData() %>%
      group_by(`IRA CLASS`) %>%
      summarise(
        `Class wise RI Gross UPR Sum` = sum(RI_Gross_UPR, na.rm = TRUE), 
        `Class wise RI DAC Sum` = sum(RI_DAC, na.rm = TRUE)
      ) %>%
      mutate(
        `Premium Receivables` = 0,
        `ARC` = `Class wise RI Gross UPR Sum` - `Class wise RI DAC Sum` - `Premium Receivables`
      )
    
    # Add totals row
    totals_row <- data.frame(
      `IRA CLASS` = "TOTAL",
      `Class wise RI Gross UPR Sum` = sum(df$`Class wise RI Gross UPR Sum`, na.rm = TRUE),
      `Class wise RI DAC Sum` = sum(df$`Class wise RI DAC Sum`, na.rm = TRUE),
      `Premium Receivables` = sum(df$`Premium Receivables`, na.rm = TRUE),
      `ARC` = sum(df$`ARC`, na.rm = TRUE),
      check.names = FALSE
    )
    df <- bind_rows(df, totals_row)
    
    arcValues$data <- df
  })

  # Also recalculate on button click (for manual refresh/reset after edits)
  observeEvent(input$calcARC, {
    req(processedData())
    df <- processedData() %>%
      group_by(`IRA CLASS`) %>%
      summarise(
        `Class wise RI Gross UPR Sum` = sum(RI_Gross_UPR, na.rm = TRUE), 
        `Class wise RI DAC Sum` = sum(RI_DAC, na.rm = TRUE)
      ) %>%
      mutate(
        `Premium Receivables` = 0,
        `ARC` = `Class wise RI Gross UPR Sum` - `Class wise RI DAC Sum` - `Premium Receivables`
      )
    
    # Add totals row
    totals_row <- data.frame(
      `IRA CLASS` = "TOTAL",
      `Class wise RI Gross UPR Sum` = sum(df$`Class wise RI Gross UPR Sum`, na.rm = TRUE),
      `Class wise RI DAC Sum` = sum(df$`Class wise RI DAC Sum`, na.rm = TRUE),
      `Premium Receivables` = sum(df$`Premium Receivables`, na.rm = TRUE),
      `ARC` = sum(df$`ARC`, na.rm = TRUE),
      check.names = FALSE
    )
    df <- bind_rows(df, totals_row)
    
    arcValues$data <- df
  })

  # Handle cell edits for ARC table
  observeEvent(input$arcTable_cell_edit, {
    info <- input$arcTable_cell_edit
    row <- info$row
    col <- info$col + 1  # DT uses 0-based index, R uses 1-based
    value <- as.numeric(gsub(",", "", info$value))  # Remove commas and convert to numeric
    
    if (is.na(value)) value <- 0
    
    # Update the value in the reactive data
    if (col == 4) {  # Premium Receivables column
      arcValues$data[row, "Premium Receivables"] <- value
    }
    
    # Recalculate ARC for the edited row
    arcValues$data[row, "ARC"] <- arcValues$data[row, "Class wise RI Gross UPR Sum"] - 
                                   arcValues$data[row, "Class wise RI DAC Sum"] - 
                                   arcValues$data[row, "Premium Receivables"]
  })

  output$arcTable <- renderDT({
    req(arcValues$data)
    
    # Format data for display
    displayData <- arcValues$data %>%
      mutate(
        `Class wise RI Gross UPR Sum` = scales::comma(`Class wise RI Gross UPR Sum`),
        `Class wise RI DAC Sum` = scales::comma(`Class wise RI DAC Sum`),
        `Premium Receivables` = scales::comma(`Premium Receivables`),
        `ARC` = scales::comma(`ARC`)
      )
    
    datatable(displayData, 
      options = list(
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
      ),
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 4)))  # Only column 3 (Premium Receivables) editable
    )
  })

  output$downloadARC <- downloadHandler(
    filename = function() {
      paste("Class-wise-ARC-Summary-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(arcValues$data)
      # Format for download
      downloadData <- arcValues$data %>%
        mutate(
          `Class wise RI Gross UPR Sum` = scales::comma(`Class wise RI Gross UPR Sum`),
          `Class wise RI DAC Sum` = scales::comma(`Class wise RI DAC Sum`),
          `Premium Receivables` = scales::comma(`Premium Receivables`),
          `ARC` = scales::comma(`ARC`)
        )
      write.csv(downloadData, file, row.names = FALSE)
    }
  )

  # Net LRC Tab Logic
  # Formula: Net LRC = LRC - ARC
  # Where LRC = Gross UPR - DAC - Premium Receivables + Bad Debt (from LRC tab)
  # And ARC = RI Gross UPR - RI DAC - Premium Receivables (from ARC tab)
  # Reads values from the edited LRC and ARC tables
  # Auto-calculates when LRC and ARC data are available
  netLrcData <- reactive({
    req(lrcValues$data, arcValues$data)
    
    # Get LRC values (already has LRC calculated with user edits)
    lrc_df <- lrcValues$data %>%
      select(`IRA CLASS`, `LRC`)
    
    # Get ARC values (already has ARC calculated with user edits)
    arc_df <- arcValues$data %>%
      select(`IRA CLASS`, `ARC`)
    
    # Join and calculate Net LRC
    result <- lrc_df %>%
      left_join(arc_df, by = "IRA CLASS") %>%
      mutate(
        `Net LRC` = `LRC` - `ARC`
      ) %>%
      select(`IRA CLASS`, `Net LRC`) %>%
      mutate(
        `Net LRC` = scales::comma(`Net LRC`)
      )
    
    result
  })

  output$netLrcTable <- renderDT({
    req(netLrcData())
    datatable(netLrcData(), 
      options = list(
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
      ),
      rownames = FALSE
    )
  })

  output$downloadNetLRC <- downloadHandler(
    filename = function() {
      paste("Class-wise-Net-LRC-Summary-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(netLrcData())
      write.csv(netLrcData(), file, row.names = FALSE)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)