# NCA Analysis UI Components
# User interface elements for the analysis results and execution

#' NCA Analysis Tab UI
nca_analysis_tab_ui <- function() {
  tabItem(
    tabName = "nca_analysis",
    
    # Header section
    fluidRow(
      column(12,
        div(style = "margin-bottom: 20px;",
          h2(icon("calculator"), " NCA Analysis & Results",
             style = "color: #2c3e50; margin-bottom: 10px;"),
          p("Execute bioequivalence analysis and view detailed results.",
            style = "color: #7f8c8d; font-size: 16px;")
        )
      )
    ),
    
    # Analysis control section
    fluidRow(
      column(12,
        box(
          title = "Analysis Execution", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          fluidRow(
            column(8,
              div(
                h4("Ready to Run Analysis", style = "color: #27ae60; margin-bottom: 15px;"),
                p("Your data and analysis parameters have been configured. Click the button below to execute the bioequivalence analysis.",
                  style = "margin-bottom: 20px;"),
                
                # Analysis status
                uiOutput("analysis_status"),
                
                br(),
                
                # Progress information
                conditionalPanel(
                  condition = "false", # Will be shown via JavaScript
                  id = "analysis_progress_section",
                  div(
                    h5("Analysis Progress"),
                    div(class = "progress",
                      div(id = "main_progress_bar", 
                          class = "progress-bar progress-bar-striped progress-bar-animated",
                          role = "progressbar",
                          style = "width: 0%",
                          "0%")
                    ),
                    div(id = "main_progress_message", "Ready to start...")
                  )
                )
              )
            ),
            
            column(4,
              div(style = "text-align: center;",
                # Large run analysis button
                div(class = "tooltip-wrapper",
                  actionButton(
                    "run_analysis",
                    HTML("<i class='fa fa-play'></i> Run Analysis"),
                    class = "btn btn-success btn-lg",
                    style = "font-size: 18px; padding: 15px 30px; margin-bottom: 15px; width: 100%;"
                  ),
                  span(class = "tooltip-text", 
                       "Execute NCA analysis and bioequivalence assessment with current data and parameters")
                ),
                
                br(),
                
                # Additional action buttons
                actionButton(
                  "clear_analysis",
                  HTML("<i class='fa fa-trash'></i> Clear Results"),
                  class = "btn btn-outline-secondary",
                  style = "margin-bottom: 10px; width: 100%;"
                ),
                
                actionButton(
                  "export_results",
                  HTML("<i class='fa fa-download'></i> Export Results"),
                  class = "btn btn-outline-primary",
                  style = "width: 100%;"
                ),
                
                br(), br(),
                
                # Quick info
                div(class = "alert alert-info", style = "text-align: left; font-size: 14px;",
                  icon("lightbulb"), strong(" Tip: "), 
                  "Analysis results are automatically cached. Re-running with the same data and parameters will use cached results."
                )
              )
            )
          )
        )
      )
    ),
    
    # Results tabs
    fluidRow(
      column(12,
        conditionalPanel(
          condition = "output.analysis_status",
          
          box(
            title = "Analysis Results", 
            status = "success", 
            solidHeader = TRUE,
            width = 12,
            
            tabsetPanel(
              id = "results_tabs",
              type = "pills",
              
              # NCA Results Tab
              tabPanel(
                title = HTML("<i class='fa fa-table'></i> NCA Results"),
                value = "nca_results",
                br(),
                
                div(
                  h4("Non-Compartmental Analysis Results"),
                  p("Individual pharmacokinetic parameters calculated for each subject and treatment.",
                    style = "color: #7f8c8d; margin-bottom: 20px;"),
                  
                  # Download button for NCA results
                  div(style = "margin-bottom: 15px;",
                    downloadButton(
                      "download_nca_results",
                      "Download NCA Results",
                      class = "btn btn-primary btn-sm",
                      icon = icon("download")
                    )
                  ),
                  
                  # NCA results table
                  DT::dataTableOutput("nca_results_table"),
                  
                  br(),
                  
                  # Data info
                  div(class = "alert alert-light",
                    h6("Data Information:"),
                    uiOutput("nca_data_info")
                  )
                )
              ),
              
              # Bioequivalence Results Tab
              tabPanel(
                title = HTML("<i class='fa fa-balance-scale'></i> Bioequivalence"),
                value = "be_results",
                br(),
                
                div(
                  h4("Bioequivalence Assessment"),
                  p("Statistical evaluation of bioequivalence based on confidence intervals and regulatory criteria.",
                    style = "color: #7f8c8d; margin-bottom: 20px;"),
                  
                  # BE summary
                  uiOutput("be_summary"),
                  
                  br(),
                  
                  # Detailed BE results table
                  conditionalPanel(
                    condition = "output.be_summary",
                    h5("Detailed Statistical Results"),
                    DT::dataTableOutput("be_detailed_table")
                  )
                )
              ),
              
              # Summary Statistics Tab
              tabPanel(
                title = HTML("<i class='fa fa-chart-bar'></i> Summary Statistics"),
                value = "summary_stats",
                br(),
                
                div(
                  h4("Descriptive Statistics"),
                  p("Summary statistics for all calculated pharmacokinetic parameters.",
                    style = "color: #7f8c8d; margin-bottom: 20px;"),
                  
                  # Summary statistics output
                  uiOutput("summary_statistics"),
                  
                  br(),
                  
                  # Statistical notes
                  div(class = "alert alert-info",
                    h6(icon("info-circle"), " Statistical Notes:"),
                    tags$ul(
                      tags$li("CV% = Coefficient of variation (SD/Mean × 100)"),
                      tags$li("All statistics calculated on untransformed data"),
                      tags$li("Missing values are excluded from calculations"),
                      tags$li("For bioequivalence analysis, log-transformed statistics are used")
                    )
                  )
                )
              ),
              
              # Analysis Log Tab
              tabPanel(
                title = HTML("<i class='fa fa-file-alt'></i> Analysis Log"),
                value = "analysis_log",
                br(),
                
                div(
                  h4("Analysis Configuration & Log"),
                  p("Detailed information about the analysis configuration and execution log.",
                    style = "color: #7f8c8d; margin-bottom: 20px;"),
                  
                  # Configuration summary
                  div(
                    h5("Analysis Configuration"),
                    verbatimTextOutput("analysis_config"),
                    
                    br(),
                    
                    h5("Execution Log"),
                    verbatimTextOutput("analysis_log_output"),
                    
                    br(),
                    
                    # Download configuration
                    downloadButton(
                      "download_config",
                      "Download Configuration",
                      class = "btn btn-outline-secondary",
                      icon = icon("cog")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Error handling section
    conditionalPanel(
      condition = "false", # Will be shown when there are errors
      id = "error_section",
      fluidRow(
        column(12,
          box(
            title = "Analysis Error", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            
            div(
              h4(icon("exclamation-triangle"), " Analysis Failed"),
              p("An error occurred during analysis execution. Please review the error details below and adjust your data or configuration."),
              
              div(id = "error_details",
                class = "alert alert-danger",
                # Error message will be inserted here via JavaScript
              ),
              
              h5("Troubleshooting Tips:"),
              div(class = "alert alert-info",
                tags$ul(
                  tags$li("Verify that all required columns are present in your data"),
                  tags$li("Check for missing values in critical columns (Subject, Treatment, Time, Concentration)"),
                  tags$li("Ensure numeric columns contain valid numbers"),
                  tags$li("Verify that your study design matches the data structure"),
                  tags$li("Check that treatment codes are consistent")
                )
              ),
              
              actionButton(
                "retry_analysis",
                HTML("<i class='fa fa-redo'></i> Retry Analysis"),
                class = "btn btn-warning"
              )
            )
          )
        )
      )
    )
  )
}

#' Helper function to create data info display
create_data_info_ui <- function(data_info) {
  if (is.null(data_info)) return(NULL)
  
  tagList(
    p(strong("Subjects: "), data_info$n_subjects),
    p(strong("Treatments: "), paste(data_info$treatments, collapse = ", ")),
    p(strong("Total Observations: "), data_info$n_observations)
  )
}
