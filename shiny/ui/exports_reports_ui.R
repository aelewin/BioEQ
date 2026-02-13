# Exports & Reports UI Module
# Clean card-based layout for CSV data exports and future reports

fluidPage(
  tags$head(
    tags$style(HTML("
      .export-card {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        border-left: 4px solid #007bff;
      }
      .export-card h4 {
        color: #495057;
        margin-bottom: 5px;
      }
      .export-card p.subtitle {
        color: #6c757d;
        margin-bottom: 15px;
      }
      .report-card {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        border-left: 4px solid #28a745;
      }
      .report-card h4 {
        color: #495057;
        margin-bottom: 5px;
      }
      .report-card p.subtitle {
        color: #6c757d;
        margin-bottom: 15px;
      }
      .export-btn {
        margin: 4px 0;
        width: 100%;
      }
      .export-btn-group {
        margin-bottom: 12px;
      }
      .export-btn-group h6 {
        color: #495057;
        font-weight: 600;
        margin-bottom: 8px;
        border-bottom: 1px solid #dee2e6;
        padding-bottom: 4px;
      }
    "))
  ),
  
  # Page header
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
          "Download analysis results as CSV files for further analysis or regulatory submissions.",
          style = "color: #6c757d; font-size: 16px; max-width: 700px; margin: 0 auto;"
        )
      )
    )
  ),
  
  # Analysis required message
  conditionalPanel(
    condition = "!output.export_data_available",
    fluidRow(
      column(12,
        div(
          class = "alert alert-warning text-center",
          style = "margin: 30px 0;",
          h4(icon("exclamation-triangle"), " Analysis Required"),
          p("Please complete the bioequivalence analysis first to access export features."),
          p("Go to: Data Upload \u2192 Analysis Setup \u2192 Run Analysis \u2192 Return here for exports")
        )
      )
    )
  ),
  
  # Main export content
  conditionalPanel(
    condition = "output.export_data_available",
    
    # ── Data Exports (CSV) ──
    fluidRow(
      column(12,
        div(class = "export-card",
          h4(icon("file-csv"), " Data Exports (CSV)"),
          p(class = "subtitle",
            "Download rectangular CSV files suitable for import into SAS, Phoenix WinNonlin, R, or Excel."),
          
          # Metadata toggle
          fluidRow(
            column(12,
              div(style = "margin-bottom: 15px; padding: 10px; background-color: #e9ecef; border-radius: 5px;",
                checkboxInput("export_include_metadata",
                  label = span(icon("info-circle"), " Include analysis metadata header rows in CSV exports"),
                  value = FALSE),
                conditionalPanel(
                  condition = "input.export_include_metadata",
                  p(style = "font-size: 0.85em; color: #856404; margin: 5px 0 0 25px;",
                    icon("exclamation-triangle"),
                    " Metadata rows will be prepended as comment lines (prefixed with #). ",
                    "Some software may require removing these before import.")
                )
              )
            )
          ),
          
          fluidRow(
            # Column 1: NCA Data
            column(4,
              div(class = "export-btn-group",
                h6(icon("flask"), " NCA Analysis"),
                downloadButton("download_nca_subject_data",
                  "NCA Subject-Level Data",
                  class = "btn-outline-primary btn-sm export-btn"),
                tags$small(class = "text-muted d-block", style = "margin: 2px 0 8px 0;",
                  "All PK parameters per subject \u00D7 treatment \u00D7 period"),
                downloadButton("download_nca_summary",
                  "NCA Summary Statistics",
                  class = "btn-outline-primary btn-sm export-btn"),
                tags$small(class = "text-muted d-block", style = "margin: 2px 0 0 0;",
                  "Test vs Reference means, CV%, ratio by parameter")
              )
            ),
            
            # Column 2: ANOVA & BE
            column(4,
              div(class = "export-btn-group",
                h6(icon("table"), " Statistical Analysis"),
                downloadButton("download_anova_results",
                  "ANOVA Results",
                  class = "btn-outline-primary btn-sm export-btn"),
                tags$small(class = "text-muted d-block", style = "margin: 2px 0 8px 0;",
                  "Type I/III SS tables, F-values, p-values (SAS-style)"),
                downloadButton("download_be_results",
                  "BE Assessment Results",
                  class = "btn-outline-primary btn-sm export-btn"),
                tags$small(class = "text-muted d-block", style = "margin: 2px 0 0 0;",
                  "CIs, GMR, limits, N, DF, pass/fail per parameter")
              )
            ),
            
            # Column 3: Raw Data
            column(4,
              div(class = "export-btn-group",
                h6(icon("database"), " Source Data"),
                downloadButton("download_raw_data",
                  "Uploaded Raw Data",
                  class = "btn-outline-primary btn-sm export-btn"),
                tags$small(class = "text-muted d-block", style = "margin: 2px 0 0 0;",
                  "Original uploaded dataset as-is")
              )
            )
          )
        )
      )
    ),
    
    # ── Reports (placeholder) ──
    fluidRow(
      column(12,
        div(class = "report-card",
          h4(icon("file-alt"), " Reports"),
          p(class = "subtitle", "Generate comprehensive analysis reports."),
          div(
            class = "alert alert-info",
            style = "margin-bottom: 0;",
            icon("tools"), " ",
            strong("Report generation is under active development. "),
            "Comprehensive PDF, HTML, and Word reports with ANOVA tables, plots, ",
            "and regulatory-ready formatting will be available in a future update."
          )
        )
      )
    ),
    
    # Footer
    fluidRow(
      column(12,
        hr(style = "border-color: #dee2e6; margin: 25px 0 20px 0;"),
        div(
          style = "text-align: center; background-color: #f8f9fa; padding: 12px; border-radius: 5px;",
          p(
            icon("info-circle", style = "margin-right: 5px;"),
            "All exports reflect the most recent completed analysis.",
            style = "color: #6c757d; font-size: 13px; margin: 0; font-weight: 500;"
          )
        )
      )
    )
  )
)
