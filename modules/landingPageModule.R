# Landing Page Module for LRC Model
# A premium, professional and modern landing page

landingPageUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Hero Section
    div(
      class = "landing-hero",
      div(
        class = "hero-content",
        div(
          class = "hero-badge",
          icon("shield-alt"),
          span("Actuarial Analytics Platform")
        ),
        h1(class = "hero-title", "LRC Model"),
        h2(class = "hero-subtitle", "Advanced Actuarial Analytics for Insurance Premium Management"),
        p(class = "hero-description",
          "Calculate Unearned Premium Reserves, Earned Premiums, and Liability for Remaining Coverage with precision and ease. Built for actuaries, by actuaries."
        ),
        div(
          class = "hero-cta-container",
          actionButton(ns("launchApp"), 
                       label = tagList(icon("rocket"), " Launch Application"),
                       class = "btn-hero-primary"),
          actionButton(ns("learnMore"), 
                       label = tagList(icon("arrow-down"), " Learn More"),
                       class = "btn-hero-secondary")
        ),
        div(
          class = "hero-stats",
          div(class = "stat-item",
              span(class = "stat-number", "6+"),
              span(class = "stat-label", "Key Metrics")),
          div(class = "stat-item",
              span(class = "stat-number", "100%"),
              span(class = "stat-label", "Accuracy")),
          div(class = "stat-item",
              span(class = "stat-number", "Real-time"),
              span(class = "stat-label", "Calculations"))
        )
      ),
      div(
        class = "hero-visual",
        div(
          class = "hero-card",
          div(class = "card-header-visual",
              icon("chart-line"),
              span("Live Dashboard Preview")),
          div(class = "card-metrics",
              div(class = "metric-row",
                  span(class = "metric-label", "Gross UPR"),
                  span(class = "metric-value positive", "125,450,320")),
              div(class = "metric-row",
                  span(class = "metric-label", "Net LRC"),
                  span(class = "metric-value", "98,234,100")),
              div(class = "metric-row",
                  span(class = "metric-label", "GEP Total"),
                  span(class = "metric-value positive", "245,678,900"))
          )
        )
      )
    ),
    
    # Features Section
    div(
      id = "features-section",
      class = "landing-section features-section",
      div(
        class = "section-header",
        span(class = "section-tag", "CAPABILITIES"),
        h2(class = "section-title", "Powerful Features for Premium Analytics"),
        p(class = "section-subtitle", "Everything you need to manage and analyze insurance premium reserves")
      ),
      div(
        class = "features-grid",
        # Feature 1: UPR Calculation
        div(
          class = "feature-card",
          div(class = "feature-icon-wrapper gradient-blue",
              icon("calculator")),
          h3(class = "feature-title", "UPR Calculation"),
          p(class = "feature-description", 
            "Calculate Unearned Premium Reserves and Deferred Acquisition Costs across all insurance classes with automatic class-wise breakdowns."),
          div(class = "feature-tags",
              span(class = "tag", "Gross UPR"),
              span(class = "tag", "DAC"),
              span(class = "tag", "RI UPR"))
        ),
        # Feature 2: Earned Premium Analysis
        div(
          class = "feature-card",
          div(class = "feature-icon-wrapper gradient-green",
              icon("chart-line")),
          h3(class = "feature-title", "Earned Premium Analysis"),
          p(class = "feature-description", 
            "Generate comprehensive Gross and Net Earned Premium summaries with flexible monthly or quarterly time periods."),
          div(class = "feature-tags",
              span(class = "tag", "GEP"),
              span(class = "tag", "NEP"),
              span(class = "tag", "Time Series"))
        ),
        # Feature 3: LRC/ARC Calculation
        div(
          class = "feature-card",
          div(class = "feature-icon-wrapper gradient-purple",
              icon("balance-scale")),
          h3(class = "feature-title", "LRC & ARC Calculation"),
          p(class = "feature-description", 
            "Compute Liability and Asset for Remaining Coverage with editable parameters for Premium Receivables and Bad Debt."),
          div(class = "feature-tags",
              span(class = "tag", "LRC"),
              span(class = "tag", "ARC"),
              span(class = "tag", "Net LRC"))
        ),
        # Feature 4: Export & Reporting
        div(
          class = "feature-card",
          div(class = "feature-icon-wrapper gradient-orange",
              icon("file-export")),
          h3(class = "feature-title", "Export & Reporting"),
          p(class = "feature-description", 
            "Download comprehensive CSV reports for all calculations. Export class-wise summaries and time-based analyses."),
          div(class = "feature-tags",
              span(class = "tag", "CSV Export"),
              span(class = "tag", "Reports"),
              span(class = "tag", "Analytics"))
        )
      )
    ),
    
    # How It Works Section
    div(
      class = "landing-section how-it-works-section",
      div(
        class = "section-header",
        span(class = "section-tag", "WORKFLOW"),
        h2(class = "section-title", "How It Works"),
        p(class = "section-subtitle", "Get started in four simple steps")
      ),
      div(
        class = "steps-container",
        div(
          class = "step-card",
          div(class = "step-number", "1"),
          div(class = "step-icon", icon("upload")),
          h4(class = "step-title", "Upload Your Data"),
          p(class = "step-description", "Import Excel or CSV files containing your policy premium data with required columns.")
        ),
        div(class = "step-connector"),
        div(
          class = "step-card",
          div(class = "step-number", "2"),
          div(class = "step-icon", icon("sliders-h")),
          h4(class = "step-title", "Configure Parameters"),
          p(class = "step-description", "Set valuation date, policy year thresholds, and analysis time periods.")
        ),
        div(class = "step-connector"),
        div(
          class = "step-card",
          div(class = "step-number", "3"),
          div(class = "step-icon", icon("cogs")),
          h4(class = "step-title", "Calculate Metrics"),
          p(class = "step-description", "Generate UPR, GEP, NEP, LRC, ARC, and Net LRC with one click.")
        ),
        div(class = "step-connector"),
        div(
          class = "step-card",
          div(class = "step-number", "4"),
          div(class = "step-icon", icon("download")),
          h4(class = "step-title", "Export Reports"),
          p(class = "step-description", "Download comprehensive reports and visualizations for your analysis.")
        )
      )
    ),
    
    # Outputs Showcase Section
    div(
      class = "landing-section outputs-section",
      div(
        class = "section-header",
        span(class = "section-tag", "OUTPUTS"),
        h2(class = "section-title", "Comprehensive Analytics Dashboard"),
        p(class = "section-subtitle", "Visual insights and detailed breakdowns for informed decision-making")
      ),
      div(
        class = "outputs-grid",
        div(
          class = "output-card large",
          div(class = "output-header",
              icon("table"),
              span("Class-wise UPR Summary")),
          div(class = "output-preview",
              tags$table(
                class = "preview-table",
                tags$thead(
                  tags$tr(
                    tags$th("IRA Class"),
                    tags$th("Gross UPR"),
                    tags$th("DAC"),
                    tags$th("RI UPR")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td("Motor Private"),
                    tags$td("45,234,500"),
                    tags$td("4,523,450"),
                    tags$td("12,340,200")
                  ),
                  tags$tr(
                    tags$td("Fire Industrial"),
                    tags$td("32,456,780"),
                    tags$td("3,245,678"),
                    tags$td("8,567,890")
                  ),
                  tags$tr(
                    tags$td("Marine"),
                    tags$td("18,765,430"),
                    tags$td("1,876,543"),
                    tags$td("5,432,100")
                  )
                )
              )
          )
        ),
        div(
          class = "output-card",
          div(class = "output-header",
              icon("chart-bar"),
              span("Visual Analytics")),
          div(class = "output-preview chart-preview",
              div(class = "bar-chart-mock",
                  div(class = "bar", style = "height: 80%;"),
                  div(class = "bar", style = "height: 60%;"),
                  div(class = "bar", style = "height: 90%;"),
                  div(class = "bar", style = "height: 45%;"),
                  div(class = "bar", style = "height: 70%;")
              )
          )
        ),
        div(
          class = "output-card",
          div(class = "output-header",
              icon("calculator"),
              span("LRC Calculation")),
          div(class = "output-preview",
              div(class = "formula-display",
                  p(class = "formula", "LRC = Gross UPR - DAC - Premium Receivables + Bad Debt"),
                  div(class = "result-row",
                      span("Net LRC:"),
                      span(class = "result-value", "98,234,100"))
              )
          )
        )
      )
    ),
    
    # Technical Specs Section
    div(
      class = "landing-section specs-section",
      div(
        class = "section-header",
        span(class = "section-tag", "SPECIFICATIONS"),
        h2(class = "section-title", "Data Requirements"),
        p(class = "section-subtitle", "Prepare your data with these required columns")
      ),
      div(
        class = "specs-container",
        div(
          class = "specs-card",
          h4(class = "specs-title", icon("database"), " Required Columns"),
          div(
            class = "specs-list",
            div(class = "spec-item",
                icon("calendar"), 
                div(tags$b("BegDate"), " - Policy start date")),
            div(class = "spec-item",
                icon("calendar-alt"), 
                div(tags$b("EndDate"), " - Policy end date")),
            div(class = "spec-item",
                icon("clipboard-check"), 
                div(tags$b("AuthDate"), " - Underwriting date")),
            div(class = "spec-item",
                icon("briefcase"), 
                div(tags$b("IRA CLASS"), " - Class of business")),
            div(class = "spec-item",
                icon("dollar-sign"), 
                div(tags$b("Gross Premium"), " - Gross premium amount")),
            div(class = "spec-item",
                icon("dollar-sign"), 
                div(tags$b("Net Premium"), " - Net premium amount")),
            div(class = "spec-item",
                icon("percent"), 
                div(tags$b("Commission"), " - Commission amount")),
            div(class = "spec-item",
                icon("shield-alt"), 
                div(tags$b("RI_Premium"), " - Reinsurance premium")),
            div(class = "spec-item",
                icon("handshake"), 
                div(tags$b("RI_Commission"), " - RI commission"))
          )
        ),
        div(
          class = "specs-card",
          h4(class = "specs-title", icon("file-alt"), " Supported Formats"),
          div(
            class = "format-icons",
            div(class = "format-item",
                div(class = "format-icon excel", "XLSX"),
                span("Excel Workbook")),
            div(class = "format-item",
                div(class = "format-icon excel", "XLS"),
                span("Excel 97-2003")),
            div(class = "format-item",
                div(class = "format-icon csv", "CSV"),
                span("Comma Separated"))
          ),
          div(class = "file-size-note",
              icon("info-circle"),
              span("Maximum file size: 1GB"))
        )
      )
    ),
    
    # CTA Section
    div(
      class = "landing-section cta-section",
      div(
        class = "cta-content",
        h2(class = "cta-title", "Ready to Get Started?"),
        p(class = "cta-description", 
          "Launch the LRC Model and streamline your actuarial calculations today."),
        actionButton(ns("launchAppBottom"), 
                     label = tagList(icon("rocket"), " Launch Application Now"),
                     class = "btn-cta-large")
      )
    ),
    
    # Footer
    div(
      class = "landing-footer",
      div(
        class = "footer-content",
        div(
          class = "footer-brand",
          img(src = "images/kenbright.png", class = "footer-logo", alt = "Kenbright"),
          p(class = "footer-tagline", "Innovative Insurance Solutions")
        ),
        div(
          class = "footer-links",
          div(class = "footer-column",
              h5("Product"),
              tags$a(href = "#", "Features"),
              tags$a(href = "#", "Documentation"),
              tags$a(href = "#", "Updates")),
          div(class = "footer-column",
              h5("Support"),
              tags$a(href = "#", "Help Center"),
              tags$a(href = "#", "Contact Us"),
              tags$a(href = "#", "Training"))
        ),
        div(
          class = "footer-copyright",
          p(paste0("© ", format(Sys.Date(), "%Y"), " Kenbright. All rights reserved.")),
          p("Developed by Kenbright AI")
        )
      )
    )
  )
}

# Landing Page Server
landingPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Navigate to Data Overview when Launch Application is clicked
    observeEvent(input$launchApp, {
      updatebs4TabItems(session = session$userData$parentSession, 
                        inputId = "sidebar", 
                        selected = "dataOverview")
    })
    
    observeEvent(input$launchAppBottom, {
      updatebs4TabItems(session = session$userData$parentSession, 
                        inputId = "sidebar", 
                        selected = "dataOverview")
    })
    
    # Smooth scroll to features section
    observeEvent(input$learnMore, {
      shinyjs::runjs("document.getElementById('features-section').scrollIntoView({behavior: 'smooth'});")
    })
    
  })
}
