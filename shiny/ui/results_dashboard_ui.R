# Results Dashboard UI Module
# Comprehensive results presentation with summary cards, tabbed interface, and export

results_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Custom CSS for enhanced styling with Navy Blue Theme
    tags$head(
      tags$style(HTML("
        /* Results Dashboard - Navy Blue Theme Styling */
        .summary-card {
          background: linear-gradient(135deg, var(--white) 0%, var(--neutral-100) 100%);
          border-radius: 16px;
          box-shadow: 0 8px 25px rgba(30, 58, 95, 0.1);
          padding: 25px;
          margin-bottom: 25px;
          border-left: 5px solid var(--blue-accent);
          transition: all 0.3s ease;
        }
        
        .summary-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 15px 35px rgba(30, 58, 95, 0.2);
        }
        
        .summary-card.success {
          border-left-color: var(--success);
          background: linear-gradient(135deg, #f0fff4 0%, #e6fffa 100%);
        }
        
        .summary-card.warning {
          border-left-color: var(--orange-accent);
          background: linear-gradient(135deg, #fffbf0 0%, #fef5e7 100%);
        }
        
        .summary-card.danger {
          border-left-color: var(--danger);
          background: linear-gradient(135deg, #fef2f2 0%, #fed7d7 100%);
        }
        
        .summary-card.navy {
          border-left-color: var(--navy-primary);
          background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%);
        }
        
        .metric-value {
          font-size: 28px;
          font-weight: 800;
          color: var(--navy-primary);
          margin: 8px 0;
          text-shadow: 0 2px 4px rgba(30, 58, 95, 0.1);
        }
        
        .metric-label {
          font-size: 13px;
          color: var(--neutral-600);
          text-transform: uppercase;
          letter-spacing: 1px;
          font-weight: 600;
        }
        
        .be-status {
          padding: 10px 20px;
          border-radius: 25px;
          font-weight: 700;
          display: inline-block;
          margin-top: 12px;
          font-size: 14px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          transition: all 0.3s ease;
        }
        
        .be-status:hover {
          transform: translateY(-1px);
          box-shadow: 0 6px 20px rgba(0,0,0,0.15);
        }
        
        .be-status.pass {
          background: linear-gradient(135deg, var(--success) 0%, #48bb78 100%);
          color: var(--white);
        }
        
        .be-status.fail {
          background: linear-gradient(135deg, var(--danger) 0%, #fc8181 100%);
          color: var(--white);
        }
        
        .be-status.inconclusive {
          background: linear-gradient(135deg, var(--orange-accent) 0%, var(--warning) 100%);
          color: var(--white);
        }
        
        .interpretation-box {
          background: linear-gradient(135deg, var(--neutral-100) 0%, var(--white) 100%);
          border: 2px solid var(--neutral-200);
          border-radius: 12px;
          padding: 20px;
          margin-top: 20px;
          box-shadow: 0 4px 15px rgba(30, 58, 95, 0.05);
        }
        
        .interpretation-box h5 {
          color: var(--navy-primary);
          font-weight: 700;
          margin-bottom: 15px;
          display: flex;
          align-items: center;
          gap: 8px;
        }
        
        .interpretation-box h6 {
          color: var(--navy-secondary);
          font-weight: 600;
          margin-bottom: 10px;
        }
        
        .export-section {
          background: linear-gradient(135deg, var(--white) 0%, var(--neutral-100) 100%);
          border: 2px solid var(--neutral-200);
          border-radius: 16px;
          padding: 25px;
          margin-top: 25px;
          box-shadow: 0 8px 25px rgba(30, 58, 95, 0.1);
        }
        
        .page-header h1 {
          color: var(--navy-primary);
          font-weight: 800;
          margin-bottom: 8px;
          display: flex;
          align-items: center;
          gap: 12px;
        }
        
        .page-header p {
          color: var(--neutral-600);
          font-size: 18px;
          font-weight: 500;
          margin-bottom: 35px;
        }
        
        /* Results-specific tab styling */
        .results-tabs .nav-tabs {
          border-bottom: 3px solid var(--neutral-200);
          background: linear-gradient(135deg, var(--neutral-100) 0%, var(--white) 100%);
          border-radius: 12px 12px 0 0;
          padding: 0 10px;
        }
        
        .results-tabs .nav-tabs > li > a {
          color: var(--neutral-600);
          font-weight: 600;
          font-size: 15px;
          padding: 15px 25px;
          margin: 5px;
          border-radius: 8px;
          transition: all 0.3s ease;
        }
        
        .results-tabs .nav-tabs > li.active > a {
          background: linear-gradient(135deg, var(--navy-primary) 0%, var(--navy-secondary) 100%);
          color: var(--white);
          border-bottom: 3px solid var(--orange-accent);
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(30, 58, 95, 0.3);
        }
        
        .results-tabs .nav-tabs > li > a:hover {
          background: var(--neutral-200);
          color: var(--navy-primary);
          transform: translateY(-1px);
        }
      "))
    ),
    
    # Alert section for important messages
    conditionalPanel(
      condition = "output.show_alert",
      ns = ns,
      fluidRow(
        column(12,
          uiOutput(ns("alert_message"))
        )
      )
    ),
    
    # Summary cards section with enhanced styling
    fluidRow(
      column(12,
        h3("", style = "color: var(--navy-secondary); margin-bottom: 25px; font-weight: 700; display: none;"),
        uiOutput(ns("summary_cards"))
      )
    ),
    
    # Main results tabbed interface with navy theme
    fluidRow(
      column(12,
        div(class = "results-tabs",
          tabsetPanel(
            id = ns("results_tabs"),
            type = "tabs",
          
          # Summary Tab - Simplified for ICH M13
          tabPanel(tags$span(icon("clipboard-list"), " Summary"),
            value = "overview",
            br(),
            fluidRow(
              column(12,
                div(class = "summary-card",
                  h4("🎯 Bioequivalence Results & Conclusion"),
                  uiOutput(ns("be_conclusions"))
                )
              )
            ),
            br(),
            fluidRow(
              column(12,
                div(class = "summary-card",
                  h4("📋 Carryover Assessment Summary"),
                  uiOutput(ns("carryover_summary"))
                )
              )
            )
          ),
          
          # BE Analysis Tab - Second tab
          tabPanel(tags$span(icon("balance-scale"), " BE Analysis"),
            value = "be_analysis",
            br(),
            fluidRow(
              column(12,
                h4("Complete Bioequivalence Analysis"),
                div(class = "summary-card",
                  h5("📊 Comprehensive BE Results"),
                  p("This section provides the complete bioequivalence analysis including all PK parameters, statistical tests, and regulatory conclusions."),
                  uiOutput(ns("complete_be_analysis"))
                )
              )
            ),
            br()
          ),

          # PK Comparison Tab - NEW Third tab (was Fourth)
          tabPanel(tags$span(icon("exchange-alt"), " PK Comparison"),
            value = "pk_comparison",
            br(),
            fluidRow(
              column(12,
                h4("PK Parameter Comparison - Test vs Reference"),
                p("Individual subject T/R comparisons with comprehensive summary statistics"),
                br(),
                
                # Parameter selection dropdown
                fluidRow(
                  column(4,
                    wellPanel(
                      h5("Select PK Parameter"),
                      uiOutput(ns("pk_comparison_parameter_select_ui")),
                      br(),
                      actionButton(ns("refresh_pk_comparison"), 
                                 icon = icon("refresh"), 
                                 "Refresh Display",
                                 class = "btn-sm btn-primary")
                    )
                  ),
                  column(8,
                    # Display area for the comparison results
                    uiOutput(ns("pk_comparison_display"))
                  )
                )
              )
            )
          ),

          # Subject Data Tab - Now Fourth tab (was Fifth)
          tabPanel(tags$span(icon("users"), " Subject Data"),
            value = "subject_data",
            br(),
            fluidRow(
              column(12,
                h4("Individual Subject Pharmacokinetic Results"),
                
                # Comprehensive column selection UI
                wellPanel(
                  h4("📊 Select Columns to Display"),
                  fluidRow(
                    column(2,
                      h5("Subject Information"),
                      checkboxGroupInput(
                        ns("subject_info_cols"),
                        label = NULL,
                        choices = c(
                          "Subject" = "Subject",
                          "Treatment" = "Treatment",
                          "Period" = "Period",
                          "Sequence" = "Sequence",
                          "Dose" = "dose"
                        ),
                        selected = character(0)
                      )
                    ),
                    column(3,
                      h5("Primary PK Parameters"),
                      uiOutput(ns("primary_pk_cols_ui"))
                    ),
                    column(3,
                      h5("Secondary PK Parameters"),
                      uiOutput(ns("secondary_pk_cols_ui"))
                    ),
                    column(2,
                      h5("Lambda-z Statistics"),
                      checkboxGroupInput(
                        ns("lambda_z_cols"),
                        label = NULL,
                        choices = c(
                          "λz Coefficient" = "lambda_z",
                          "λz R²" = "lambda_z_r_squared",
                          "λz P-value" = "lambda_z_p_value"
                        ),
                        selected = character(0)
                      )
                    ),
                    column(2,
                      h5("Log-Transformed Parameters"),
                      uiOutput(ns("log_pk_cols_ui"))
                    )
                  ),
                  fluidRow(
                    column(12,
                      br(),
                      actionButton(ns("select_all_cols"), "Select All", class = "btn-sm btn-primary"),
                      actionButton(ns("deselect_all_cols"), "Clear All", class = "btn-sm btn-warning"),
                      actionButton(ns("reset_default_cols"), "Default Selections", class = "btn-sm btn-info")
                    )
                  )
                ),
                
                DT::dataTableOutput(ns("individual_subject_table")),
                div(class = "interpretation-box",
                  h5("📖 Individual Subject Data Guide"),
                  uiOutput(ns("individual_subject_interpretation"))
                )
              )
            )
          ),
          
          # ANOVA Results Tab - Fifth tab
          tabPanel("� ANOVA Results",
            value = "anova_results",
            br(),
            fluidRow(
              column(12,
                h4("Analysis of Variance Results"),
                
                # Parameter Selection
                fluidRow(
                  column(6,
                    h5("Select PK Parameter"),
                    uiOutput(ns("anova_parameter_select_ui"))
                  ),
                  column(6,
                    div(style = "margin-top: 25px;",
                      actionButton(
                        ns("refresh_anova"),
                        "🔄 Refresh Display",
                        class = "btn btn-outline-primary"
                      )
                    )
                  )
                ),
                
                hr(),
                
                # ANOVA Output Display
                div(id = ns("anova_output_container"),
                  uiOutput(ns("anova_display"))
                )
              )
            )
          )
        ) # close tabsetPanel
      ) # close div
    ) # close column
    ), # close fluidRow
    
    # Footer with analysis timestamp - updated styling
    hr(style = "border-color: var(--neutral-300); margin: 35px 0 25px 0;"),
    fluidRow(
      column(12,
        p(paste("Analysis completed on:", Sys.time()), 
          style = "text-align: center; color: var(--neutral-500); font-size: 13px; margin-top: 20px; font-weight: 500;")
      )
    ),
    
    # Debug information panel (hidden by default)
    conditionalPanel(
      condition = "false",  # Change to "true" to enable debug mode
      fluidRow(
        column(12,
          hr(),
          h5("Debug Information"),
          verbatimTextOutput(ns("debug_info"))
        )
      )
    )
  )
}
