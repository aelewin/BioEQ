# Exports & Reports UI Module
# Standalone exports and reports interface

fluidPage(
  tags$head(
    tags$style(HTML("
      .export-section {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        border-left: 4px solid #007bff;
      }
      
      .report-generation-section {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 8px;
        border-left: 4px solid #28a745;
      }
      
      .export-section h4, .report-generation-section h4 {
        color: #495057;
        margin-bottom: 15px;
      }
      
      .export-section p, .report-generation-section p {
        color: #6c757d;
        margin-bottom: 20px;
      }
    "))
  ),
  
  fluidRow(
    column(12,
      div(
        style = "text-align: center; margin-bottom: 30px;",
        h2(
          icon("download", style = "margin-right: 10px; color: #6f42c1;"),
          "Exports & Reports",
          style = "color: #495057; font-weight: 600; margin-bottom: 10px;"
        ),
        p(
          "Download analysis results, generate comprehensive reports, and export data for further analysis.",
          style = "color: #6c757d; font-size: 16px; max-width: 600px; margin: 0 auto;"
        )
      )
    )
  ),
  
  # Results Required Message
  conditionalPanel(
    condition = "!output.results_available",
    fluidRow(
      column(12,
        div(
          class = "alert alert-warning text-center",
          style = "margin: 30px 0;",
          h4(icon("exclamation-triangle"), " Analysis Required"),
          p("Please complete the bioequivalence analysis first to access export and reporting features."),
          p("Go to: Data Upload → Analysis Setup → Run Analysis → Return here for exports")
        )
      )
    )
  ),
  
  # Main Export Content (shown when results are available)
  conditionalPanel(
    condition = "output.results_available",
    
    # Quick Export Section
    fluidRow(
      column(12,
        div(class = "export-section",
          h4("📥 Quick Export"),
          p("Download data files and basic reports for immediate use."),
          
          fluidRow(
            column(6,
              h5("📊 Data Files"),
              downloadButton("download_pk_data", 
                           "PK Results (CSV)", 
                           class = "btn-outline-primary btn-sm",
                           style = "margin: 3px; width: 100%;"),
              downloadButton("download_anova_data", 
                           "ANOVA Results (CSV)", 
                           class = "btn-outline-primary btn-sm",
                           style = "margin: 3px; width: 100%;"),
              downloadButton("download_subject_data", 
                           "Subject Data (CSV)", 
                           class = "btn-outline-primary btn-sm",
                           style = "margin: 3px; width: 100%;"),
              br(),
              actionButton("test_nca_data", 
                         "🔍 Test NCA Data", 
                         class = "btn-outline-warning btn-sm",
                         style = "margin: 3px; width: 100%;")
            ),
            column(6,
              h5("📋 Quick Reports"),
              downloadButton("download_summary_report", 
                           "Summary (HTML)", 
                           class = "btn-outline-success btn-sm",
                           style = "margin: 3px; width: 100%;"),
              downloadButton("download_basic_pdf", 
                           "Basic Report (PDF)", 
                           class = "btn-outline-success btn-sm",
                           style = "margin: 3px; width: 100%;")
            )
          )
        )
      )
    ),
    
    # Comprehensive Report Generation Section
    fluidRow(
      column(12,
        div(class = "report-generation-section",
          h4("📄 Comprehensive Report Generation"),
          p("Generate professional, regulatory-compliant reports with customizable sections and formats."),
          
          # Report generation UI (populated by server)
          uiOutput("report_options_ui"),
          
          # Report download section (shown after generation)
          conditionalPanel(
            condition = "output.show_report_downloads",
            uiOutput("report_download_ui")
          )
        )
      )
    ),
    
    # Analysis Information Footer
    fluidRow(
      column(12,
        hr(style = "border-color: #dee2e6; margin: 35px 0 25px 0;"),
        div(
          style = "text-align: center; background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          p(
            icon("info-circle", style = "margin-right: 5px;"),
            paste("Analysis completed on:", Sys.time()), 
            style = "color: #6c757d; font-size: 13px; margin: 0; font-weight: 500;"
          )
        )
      )
    )
  )
)
