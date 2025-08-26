# Plots UI Module
# Interactive plotly visualizations for bioequivalence analysis

plots_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Custom CSS for plots interface
    tags$head(
      tags$style(HTML("
        /* Plots Dashboard - Navy Blue Theme Styling */
        .plots-container {
          background: var(--white);
          border-radius: 16px;
          box-shadow: 0 8px 25px rgba(30, 58, 95, 0.1);
          padding: 25px;
          margin-bottom: 25px;
          border-left: 5px solid var(--blue-accent);
          transition: all 0.3s ease;
        }
        
        .plots-container:hover {
          transform: translateY(-3px);
          box-shadow: 0 15px 35px rgba(30, 58, 95, 0.2);
        }
        
        .plot-display-area {
          background: var(--white);
          border-radius: 12px;
          box-shadow: 0 4px 20px rgba(30, 58, 95, 0.08);
          overflow: hidden;
        }
        
        .plot-card {
          background: var(--white);
          border-radius: 12px;
          box-shadow: 0 4px 15px rgba(30, 58, 95, 0.1);
          margin-bottom: 25px;
          border: 1px solid var(--neutral-200);
          overflow: hidden;
          transition: all 0.3s ease;
        }
        
        /* Results-specific tab styling for plots */
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
        
        .plot-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(30, 58, 95, 0.15);
        }
        
        .plot-card-header {
          background: linear-gradient(135deg, var(--navy-primary) 0%, var(--navy-secondary) 100%);
          color: var(--white);
          padding: 15px 20px;
          font-weight: 700;
          font-size: 16px;
          display: flex;
          align-items: center;
          gap: 10px;
        }
        
        .plot-card-body {
          padding: 20px;
        }
        
        .plot-selection-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
          gap: 15px;
          margin-bottom: 20px;
        }
        
        
        .no-plots-message {
          text-align: center;
          padding: 60px 20px;
          background: linear-gradient(135deg, var(--neutral-100) 0%, var(--white) 100%);
          border-radius: 16px;
          border: 2px dashed var(--neutral-300);
          margin: 40px 0;
        }
        
        .no-plots-icon {
          font-size: 48px;
          color: var(--neutral-400);
          margin-bottom: 20px;
        }
        
        /* Responsive plot containers */
        .plot-container-single {
          width: 100%;
          margin-bottom: 30px;
        }
        
        @media (max-width: 768px) {
          .results-tabs .nav-tabs > li > a {
            padding: 10px 15px;
            font-size: 13px;
          }
        }        .plot-option-icon {
          font-size: 24px;
          margin-bottom: 10px;
          color: var(--blue-accent);
        }
        
        .plot-option-card.selected .plot-option-icon {
          color: var(--white);
        }
        
        .no-plots-message {
          text-align: center;
          padding: 60px 20px;
          background: linear-gradient(135deg, var(--neutral-100) 0%, var(--white) 100%);
          border-radius: 16px;
          border: 2px dashed var(--neutral-300);
          margin: 40px 0;
        }
        
        .no-plots-icon {
          font-size: 48px;
          color: var(--neutral-400);
          margin-bottom: 20px;
        }
        
        .layout-controls {
          display: flex;
          gap: 15px;
          align-items: center;
          flex-wrap: wrap;
          margin-bottom: 20px;
          padding: 15px;
          background: var(--neutral-100);
          border-radius: 10px;
        }
        
        .layout-controls .btn-group {
          margin-right: 20px;
        }
        
        /* Responsive plot containers */
        .plot-container-single {
          width: 100%;
          margin-bottom: 30px;
        }
        
        .plot-container-double {
          width: 48%;
          display: inline-block;
          margin: 1%;
          vertical-align: top;
        }
        
        @media (max-width: 768px) {
          .plot-container-double {
            width: 100%;
            margin: 10px 0;
          }
          
          .layout-controls {
            flex-direction: column;
            align-items: stretch;
          }
          
          .plot-selection-grid {
            grid-template-columns: 1fr;
          }
        }
      "))
    ),
    
    # Main content area
    conditionalPanel(
      condition = paste0("!output['", ns("plots_available"), "']"),
      div(class = "no-plots-message",
        div(class = "no-plots-icon", icon("chart-line")),
        h4("No Analysis Results Available", style = "color: var(--neutral-600); margin-bottom: 15px;"),
        p("Complete your bioequivalence analysis to generate interactive plots.",
          style = "color: var(--neutral-500); font-size: 16px; line-height: 1.6;"),
        hr(style = "border-color: var(--neutral-300); margin: 25px 0;"),
        div(style = "display: flex; gap: 10px; justify-content: center; flex-wrap: wrap;",
          tags$span(class = "badge badge-info", style = "font-size: 12px; padding: 8px 12px;", 
                   icon("upload"), " Upload Data"),
          tags$span(class = "badge badge-info", style = "font-size: 12px; padding: 8px 12px;", 
                   icon("cogs"), " Configure Analysis"),
          tags$span(class = "badge badge-info", style = "font-size: 12px; padding: 8px 12px;", 
                   icon("play"), " Run Analysis")
        )
      )
    ),

    # Tabbed plots interface
    conditionalPanel(
      condition = paste0("output['", ns("plots_available"), "']"),
      
      # Main plots tabbed interface
      fluidRow(
        column(12,
          div(class = "results-tabs",
            tabsetPanel(
              id = ns("plots_tabs"),
              type = "tabs",
            
              # Concentration-Time Tab
              tabPanel(tags$span(icon("line-chart"), " Concentration-Time"),
                value = "concentration",
                br(),
                fluidRow(
                  column(12,
                    div(class = "plot-display-area",
                      uiOutput(ns("concentration_plot_display"))
                    )
                  )
                )
              ),
              
              # Cumulative BE Tab
              tabPanel(tags$span(icon("chart-line"), " Cumulative BE"),
                value = "cumulative_pk",
                br(),
                fluidRow(
                  column(12,
                    div(class = "plot-display-area",
                      uiOutput(ns("cumulative_pk_display"))
                    )
                  )
                )
              ),
              
              # BE Assessment Tab
              tabPanel(tags$span(icon("bullseye"), " BE Assessment"),
                value = "be_assessment",
                br(),
                fluidRow(
                  column(12,
                    div(class = "plot-display-area",
                      uiOutput(ns("be_ci_display"))
                    )
                  )
                )
              ),
              
              # Individual Subjects Tab
              tabPanel(tags$span(icon("user"), " Individual Subjects"),
                value = "individual_subjects",
                br(),
                fluidRow(
                  column(12,
                    div(class = "plot-display-area",
                      uiOutput(ns("individual_subjects_display"))
                    )
                  )
                )
              )
              
            ) # close tabsetPanel
          )
        )
      )
    )
  )
}
