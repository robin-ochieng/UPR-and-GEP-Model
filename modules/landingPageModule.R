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
          span("Actuarial Analytics Platform"),
          span(class = "badge-divider", "•"),
          span("IFRS 17")
        ),
        h1(class = "hero-title", "Automate LRC roll-forward with confidence"),
        h2(class = "hero-subtitle", "UPR, earned premium, and LRC for remaining coverage — fast, consistent, and configurable."),
        div(
          class = "hero-cta-container",
          actionButton(ns("launchApp"), 
                       label = tagList(icon("rocket"), " Launch LRC Model"),
                       class = "btn-hero-primary"),
          actionButton(ns("learnMore"), 
                       label = tagList(icon("arrow-down"), " Learn More"),
                       class = "btn-hero-secondary")
        ),
        div(
          class = "hero-export-note",
          span("Exports: Excel • CSV")
        ),
        div(
          class = "hero-stats",
          div(class = "stat-card",
              span(class = "stat-number", "6+"),
              span(class = "stat-label", "Key Metrics")),
          div(class = "stat-card",
              span(class = "stat-number", icon("check-circle")),
              span(class = "stat-label", "Audit-ready outputs")),
          div(class = "stat-card",
              span(class = "stat-number", "Real-time"),
              span(class = "stat-label", "Calculations"))
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
          p(class = "step-description", "Import Excel or CSV files containing your policy premium data with the required columns.")
        ),
        div(
          class = "step-card",
          div(class = "step-number", "2"),
          div(class = "step-icon", icon("sliders-h")),
          h4(class = "step-title", "Configure Parameters"),
          p(class = "step-description", "Set valuation date, policy year thresholds, and analysis time periods.")
        ),
        div(
          class = "step-card",
          div(class = "step-number", "3"),
          div(class = "step-icon", icon("cogs")),
          h4(class = "step-title", "Calculate Metrics"),
          p(class = "step-description", "Generate UPR, GEP, NEP, LRC, ARC, and Net LRC calculations with one click.")
        ),
        div(
          class = "step-card",
          div(class = "step-number", "4"),
          div(class = "step-icon", icon("download")),
          h4(class = "step-title", "Export Reports"),
          p(class = "step-description", "Download comprehensive reports and visualizations for your analysis.")
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
    
    # Simple Footer
    div(
      class = "landing-footer-simple",
      p(paste0("Developed by Kenbright AI © ", format(Sys.Date(), "%Y")))
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
    
    # Smooth scroll to features section
    observeEvent(input$learnMore, {
      shinyjs::runjs("document.getElementById('features-section').scrollIntoView({behavior: 'smooth'});")
    })
    
  })
}
