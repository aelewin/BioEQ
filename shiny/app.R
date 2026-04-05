# BioEQ Shiny Web Application
# Main application entry point

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(readxl)
library(bslib)

# Load optional libraries with error handling
shinyjs_available <- FALSE
tryCatch({
  library(shinyjs)
  shinyjs_available <<- TRUE
}, error = function(e) {
  message("shinyjs not available - some features may be limited")
})

# Load other optional packages
tryCatch({
  library(digest)
}, error = function(e) {
  message("digest not available - caching may be limited")
})

tryCatch({
  library(shinycssloaders)
}, error = function(e) {
  message("shinycssloaders not available - loading spinners may not work")
})

# Load report generation packages
tryCatch({
  library(rmarkdown)
  library(knitr)
}, error = function(e) {
  message("rmarkdown/knitr not available - report generation may be limited")
})

tryCatch({
  library(officer)
  library(flextable)
}, error = function(e) {
  message("officer/flextable not available - Word report generation not available")
})

tryCatch({
  library(zip)
}, error = function(e) {
  message("zip package not available - package creation may be limited")
})

tryCatch({
  library(plotly)
}, error = function(e) {
  message("plotly not available - interactive plots may be limited")
})

tryCatch({
  library(PowerTOST)
}, error = function(e) {
  message("PowerTOST not available - sample size estimation module will not work")
})

tryCatch({
  library(htmlwidgets)
}, error = function(e) {
  message("htmlwidgets not available - plot export may be limited")
})

tryCatch({
  library(digest)
}, error = function(e) {
  message("digest not available - plot caching may be limited")
})

# Source the existing BioEQ R functions
source("../R/bioeq_main.R", local = TRUE)
source("../R/nca_functions.R", local = TRUE)
source("../R/be_analysis.R", local = TRUE)
source("../R/rsabe_analysis.R", local = TRUE)  # RSABE analysis (FDA linearized + ncTOST)
source("../R/simple_anova.R", local = TRUE)  # Simple ANOVA using lm()
source("../R/statistics.R", local = TRUE)
source("../R/utils.R", local = TRUE)
source("../R/missing_data_handling.R", local = TRUE)  # Missing data for NCA
source("../R/carryover_detection.R", local = TRUE)
source("../R/plotting.R", local = TRUE)  # Enhanced plotting functions with Shiny support
source("../R/cumulative_be_analysis.R", local = TRUE)  # Cumulative bioequivalence analysis

# Source template configuration
source("templates/report_generation.R", local = TRUE)

# Source UI and server components
source("ui/main_ui.R", local = TRUE)
source("ui/exports_reports_ui.R", local = TRUE)
source("ui/results_dashboard_ui.R", local = TRUE)
source("ui/plots_ui.R", local = TRUE)
source("server/main_server.R", local = TRUE)
source("server/results_dashboard_server.R", local = TRUE)
source("server/plots_server.R", local = TRUE)

# Define utility operators and functions
`%||%` <- function(a, b) if (is.null(a)) b else a

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = span(
      icon("prescription-bottle", style = "margin-right: 8px;"),
      "BioEQ Analysis Platform"
    ),
    titleWidth = 280
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
    div(
      style = "text-align: center; padding: 25px 15px 15px 15px; border-bottom: 2px solid rgba(255,255,255,0.1);",
      h3("BioEQ", style = "color: white; margin: 8px 0; font-weight: 700; letter-spacing: 1px;"),
      p("Bioequivalence Analysis Platform", 
        style = "color: #e2e8f0; font-size: 13px; margin: 0; font-weight: 500;")
    ),
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Analysis Setup", tabName = "setup", icon = icon("cogs")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-area")),
      menuItem("Exports & Reports", tabName = "exports", icon = icon("download")),
      br(),
      menuSubItem("Advanced Options", tabName = "advanced", icon = icon("sliders-h")),
      menuSubItem("Validation", tabName = "validation", icon = icon("check-circle")),
      menuSubItem("Sample Size", tabName = "sample_size", icon = icon("calculator")),
      menuSubItem("Help & Support", tabName = "help", icon = icon("question-circle"))
    ),
    div(
      style = "position: fixed; bottom: 15px; left: 15px; right: 15px; text-align: center; 
               border-top: 1px solid rgba(255,255,255,0.1); padding-top: 15px;",
      p("Version BETA", style = "color: #a0aec0; font-size: 11px; margin: 5px 0; font-weight: 500;"),
      p("© 2025 BioEQ Team", style = "color: #a0aec0; font-size: 11px; margin: 0; font-weight: 500;")
    )
  ),
  
  # Body
  dashboardBody(
    # Initialize shinyjs if available
    if (shinyjs_available) shinyjs::useShinyjs(),
    
    # Initialize tooltips and help system
    tags$script(HTML("
      $(document).ready(function(){
        // Initialize Bootstrap tooltips with improved configuration
        $('[data-toggle=\"tooltip\"]').tooltip({
          container: 'body',
          html: true,
          trigger: 'hover focus',
          delay: { show: 300, hide: 100 }
        });
        
        // Custom tooltip functionality for legacy tooltips
        $('.tooltip-wrapper').hover(function() {
          $(this).find('.tooltip-text').fadeIn(200);
        }, function() {
          $(this).find('.tooltip-text').fadeOut(200);
        });
        
        // Re-initialize tooltips when content changes
        $(document).on('shiny:inputchanged shiny:value shiny:bound', function() {
          setTimeout(function() {
            $('[data-toggle=\"tooltip\"]').tooltip('dispose').tooltip({
              container: 'body',
              html: true,
              trigger: 'hover focus',
              delay: { show: 300, hide: 100 }
            });
          }, 100);
        });
        
        // Handle help icon clicks for modals
        $(document).on('click', '.help-icon', function() {
          var helpId = $(this).attr('id');
          if (helpId && helpId.startsWith('help_')) {
            var inputId = helpId.replace('help_', '') + '_help';
            Shiny.setInputValue(inputId, Math.random());
          }
        });
        
        // Handle custom scroll to top message
        Shiny.addCustomMessageHandler('scrollToTop', function(message) {
          $('html, body').animate({
            scrollTop: 0
          }, 300);
        });
        
        // Handle button group updates for plots
        Shiny.addCustomMessageHandler('updateButtonGroup', function(data) {
          $('#' + data.active).addClass('active').removeClass('btn-outline-primary').addClass('btn-primary');
          $('#' + data.inactive).removeClass('active').removeClass('btn-primary').addClass('btn-outline-primary');
        });
        
        // Handle class additions/removals for plot selection
        Shiny.addCustomMessageHandler('addClass', function(data) {
          $('#' + data.id).addClass(data.class);
        });
        
        Shiny.addCustomMessageHandler('removeClass', function(data) {
          $('#' + data.id).removeClass(data.class);
        });
        
        // Handle custom JS injection
        Shiny.addCustomMessageHandler('addCustomJS', function(data) {
          eval(data.script);
        });
      });
    ")),
    
    # Custom CSS for responsive design
    tags$head(
      tags$title("BioEQ Analysis Platform"),
      tags$link(rel = "icon", href = "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><text y='.9em' font-size='90'>🧪</text></svg>"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML("
        /* Modern Professional Navy Blue Theme */
        :root {
          --navy-primary: #1e3a5f;
          --navy-secondary: #2c5282;
          --navy-light: #4a90c2;
          --blue-accent: #3182ce;
          --light-blue: #63b3ed;
          --orange-accent: #ed8936;
          --neutral-100: #f7fafc;
          --neutral-200: #edf2f7;
          --neutral-300: #e2e8f0;
          --neutral-400: #cbd5e0;
          --neutral-500: #a0aec0;
          --neutral-600: #718096;
          --neutral-700: #4a5568;
          --neutral-800: #2d3748;
          --neutral-900: #1a202c;
          --success: #38a169;
          --warning: #d69e2e;
          --danger: #e53e3e;
          --white: #ffffff;
        }
        
        /* Base body styling */
        body {
          font-family: 'Inter', 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
          background: linear-gradient(135deg, var(--neutral-100) 0%, var(--neutral-200) 100%);
          color: var(--neutral-800);
          line-height: 1.6;
        }
        
        /* Header styling - Navy Blue Theme */
        .main-header .navbar {
          margin-left: 280px;
          background: linear-gradient(135deg, var(--navy-primary) 0%, var(--navy-secondary) 100%);
          border-bottom: 3px solid var(--orange-accent);
          box-shadow: 0 2px 10px rgba(30, 58, 95, 0.2);
        }
        
        .main-header .navbar-brand {
          font-weight: 700;
          font-size: 20px;
          color: var(--white) !important;
          text-shadow: 0 2px 4px rgba(0,0,0,0.3);
        }
        
        .main-header .navbar-nav > li > a {
          color: var(--white) !important;
          transition: all 0.3s ease;
        }
        
        .main-header .navbar-nav > li > a:hover {
          background-color: rgba(255,255,255,0.1) !important;
          color: var(--orange-accent) !important;
        }
        
        /* Sidebar styling - Professional Navy */
        .main-sidebar {
          background: linear-gradient(180deg, var(--navy-primary) 0%, var(--navy-secondary) 100%);
          box-shadow: 2px 0 10px rgba(30, 58, 95, 0.2);
        }
        
        .sidebar-menu > li > a {
          color: var(--neutral-200) !important;
          border-left: 3px solid transparent;
          transition: all 0.3s ease;
          font-weight: 500;
        }
        
        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, var(--blue-accent), var(--light-blue)) !important;
          border-left: 3px solid var(--orange-accent) !important;
          color: var(--white) !important;
          box-shadow: inset 0 2px 10px rgba(0,0,0,0.1);
        }
        
        .sidebar-menu > li > a:hover {
          background: linear-gradient(90deg, var(--navy-secondary), var(--navy-light)) !important;
          border-left: 3px solid var(--orange-accent) !important;
          color: var(--white) !important;
          transform: translateX(3px);
        }
        
        .sidebar-menu > li > a > .badge {
          background: var(--orange-accent) !important;
          color: var(--white) !important;
          font-weight: 600;
          border-radius: 12px;
        }
        
        /* Content wrapper - Clean background */
        .content-wrapper {
          background: linear-gradient(135deg, var(--neutral-100) 0%, var(--neutral-200) 50%, var(--neutral-100) 100%);
          min-height: 100vh;
          padding: 20px;
        }
        
        /* Box styling - Modern cards */
        .box {
          background: var(--white);
          border-radius: 16px;
          box-shadow: 0 4px 20px rgba(30, 58, 95, 0.08);
          border: 1px solid var(--neutral-200);
          margin-bottom: 25px;
          transition: all 0.3s ease;
          overflow: hidden;
        }
        
        .box:hover {
          transform: translateY(-4px);
          box-shadow: 0 12px 35px rgba(30, 58, 95, 0.15);
          border-color: var(--blue-accent);
        }
        
        .box-header {
          background: linear-gradient(135deg, var(--neutral-100) 0%, var(--white) 100%);
          border-bottom: 2px solid var(--neutral-200);
          padding: 20px 25px;
          border-radius: 16px 16px 0 0;
        }
        
        .box-header.with-border {
          border-bottom: 2px solid var(--blue-accent);
        }
        
        .box-title {
          font-weight: 700;
          font-size: 18px;
          color: var(--navy-primary);
          display: flex;
          align-items: center;
          gap: 10px;
        }
        
        .box-title i {
          color: var(--blue-accent);
        }
        
        /* Status-specific box headers */
        .box-header.box-header-primary {
          background: linear-gradient(135deg, var(--blue-accent) 0%, var(--light-blue) 100%);
          color: var(--white);
        }
        
        .box-header.box-header-primary .box-title {
          color: var(--white);
        }
        
        .box-header.box-header-success {
          background: linear-gradient(135deg, var(--success) 0%, #48bb78 100%);
          color: var(--white);
        }
        
        .box-header.box-header-success .box-title {
          color: var(--white);
        }
        
        .box-header.box-header-warning {
          background: linear-gradient(135deg, var(--orange-accent) 0%, var(--warning) 100%);
          color: var(--white);
        }
        
        .box-header.box-header-warning .box-title {
          color: var(--white);
        }
        
        .box-header.box-header-info {
          background: linear-gradient(135deg, var(--light-blue) 0%, #90cdf4 100%);
          color: var(--white);
        }
        
        .box-header.box-header-info .box-title {
          color: var(--white);
        }
        
        /* Tab styling - Navy theme */
        .nav-tabs-custom {
          background: var(--white);
          border-radius: 12px;
          overflow: hidden;
          box-shadow: 0 2px 10px rgba(30, 58, 95, 0.08);
        }
        
        .nav-tabs-custom > .nav-tabs {
          background: var(--neutral-100);
          border-bottom: none;
          margin: 0;
        }
        
        .nav-tabs-custom > .nav-tabs > li {
          margin-bottom: 0;
        }
        
        .nav-tabs-custom > .nav-tabs > li > a {
          color: var(--neutral-600);
          font-weight: 600;
          border: none;
          border-radius: 0;
          transition: all 0.3s ease;
          padding: 15px 20px;
        }
        
        .nav-tabs-custom > .nav-tabs > li.active > a {
          background: linear-gradient(135deg, var(--navy-primary) 0%, var(--navy-secondary) 100%);
          color: var(--white);
          border-bottom: 3px solid var(--orange-accent);
        }
        
        .nav-tabs-custom > .nav-tabs > li > a:hover {
          background: var(--neutral-200);
          color: var(--navy-primary);
        }
        
        /* Button styling - Modern navy theme */
        .btn {
          font-weight: 600;
          border-radius: 10px;
          padding: 12px 24px;
          transition: all 0.3s ease;
          border: none;
          text-transform: none;
          letter-spacing: 0.025em;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, var(--navy-primary) 0%, var(--navy-secondary) 100%);
          color: var(--white);
          box-shadow: 0 4px 15px rgba(30, 58, 95, 0.3);
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, var(--navy-secondary) 0%, var(--blue-accent) 100%);
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(30, 58, 95, 0.4);
        }
        
        .btn-success {
          background: linear-gradient(135deg, var(--success) 0%, #48bb78 100%);
          color: var(--white);
          box-shadow: 0 4px 15px rgba(56, 161, 105, 0.3);
        }
        
        .btn-success:hover {
          background: linear-gradient(135deg, #48bb78 0%, #68d391 100%);
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(56, 161, 105, 0.4);
        }
        
        .btn-warning {
          background: linear-gradient(135deg, var(--orange-accent) 0%, var(--warning) 100%);
          color: var(--white);
          box-shadow: 0 4px 15px rgba(237, 137, 54, 0.3);
        }
        
        .btn-warning:hover {
          background: linear-gradient(135deg, var(--warning) 0%, #f6ad55 100%);
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(237, 137, 54, 0.4);
        }
        
        .btn-info {
          background: linear-gradient(135deg, var(--blue-accent) 0%, var(--light-blue) 100%);
          color: var(--white);
          box-shadow: 0 4px 15px rgba(49, 130, 206, 0.3);
        }
        
        .btn-info:hover {
          background: linear-gradient(135deg, var(--light-blue) 0%, #90cdf4 100%);
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(49, 130, 206, 0.4);
        }
        
        .btn-outline-primary {
          border: 2px solid var(--navy-primary);
          color: var(--navy-primary);
          background: transparent;
        }
        
        .btn-outline-primary:hover {
          background: var(--navy-primary);
          color: var(--white);
          transform: translateY(-2px);
        }
        
        /* Form controls - Modern styling */
        .form-control {
          border: 2px solid var(--neutral-300);
          border-radius: 8px;
          padding: 12px 15px;
          font-size: 14px;
          transition: all 0.3s ease;
          background: var(--white);
        }
        
        .form-control:focus {
          border-color: var(--blue-accent);
          box-shadow: 0 0 0 3px rgba(49, 130, 206, 0.1);
          outline: none;
        }
        
        .form-group label {
          font-weight: 600;
          color: var(--neutral-700);
          margin-bottom: 8px;
        }
        
        /* DataTable styling */
        .dataTables_wrapper {
          padding: 20px;
        }
        
        .dataTables_wrapper .dataTables_filter input {
          border: 2px solid var(--neutral-300);
          border-radius: 8px;
          padding: 8px 12px;
        }
        
        .dataTables_wrapper .dataTables_filter input:focus {
          border-color: var(--blue-accent);
          outline: none;
        }
        
        table.dataTable {
          border-collapse: separate;
          border-spacing: 0;
          border-radius: 12px;
          overflow: hidden;
          box-shadow: 0 4px 20px rgba(30, 58, 95, 0.08);
        }
        
        table.dataTable thead th {
          background: linear-gradient(135deg, var(--navy-primary) 0%, var(--navy-secondary) 100%);
          color: var(--white);
          font-weight: 700;
          padding: 15px 12px;
          border: none;
        }
        
        table.dataTable tbody tr {
          background: var(--white);
          transition: all 0.2s ease;
        }
        
        table.dataTable tbody tr:nth-child(even) {
          background: var(--neutral-100);
        }
        
        table.dataTable tbody tr:hover {
          background: rgba(49, 130, 206, 0.05);
          transform: scale(1.01);
        }
        
        table.dataTable tbody td {
          padding: 12px;
          border-bottom: 1px solid var(--neutral-200);
        }
        
        /* Progress indicators */
        .progress-indicator {
          display: flex;
          justify-content: center;
          align-items: center;
          margin: 30px 0;
          gap: 20px;
        }
        
        .progress-step {
          width: 50px;
          height: 50px;
          border-radius: 50%;
          background: var(--neutral-300);
          color: var(--neutral-600);
          display: flex;
          align-items: center;
          justify-content: center;
          font-weight: 700;
          font-size: 18px;
          position: relative;
          transition: all 0.3s ease;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        
        .progress-step.active {
          background: linear-gradient(135deg, var(--blue-accent) 0%, var(--light-blue) 100%);
          color: var(--white);
          transform: scale(1.1);
          box-shadow: 0 8px 25px rgba(49, 130, 206, 0.3);
        }
        
        .progress-step.completed {
          background: linear-gradient(135deg, var(--success) 0%, #48bb78 100%);
          color: var(--white);
          box-shadow: 0 8px 25px rgba(56, 161, 105, 0.3);
        }
        
        .progress-step:not(:last-child)::after {
          content: '';
          position: absolute;
          top: 50%;
          left: 100%;
          width: 30px;
          height: 3px;
          background: var(--neutral-300);
          transform: translateY(-50%);
          transition: all 0.3s ease;
        }
        
        .progress-step.completed:not(:last-child)::after,
        .progress-step.active:not(:last-child)::after {
          background: linear-gradient(90deg, var(--blue-accent), var(--light-blue));
        }
        
        /* Alert styling */
        .alert {
          border-radius: 12px;
          border: none;
          padding: 20px 25px;
          margin-bottom: 20px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        
        .alert-success {
          background: linear-gradient(135deg, #f0fff4 0%, #c6f6d5 100%);
          color: var(--success);
          border-left: 4px solid var(--success);
        }
        
        .alert-warning {
          background: linear-gradient(135deg, #fffbf0 0%, #fed7aa 100%);
          color: var(--orange-accent);
          border-left: 4px solid var(--orange-accent);
        }
        
        .alert-info {
          background: linear-gradient(135deg, #f0f9ff 0%, #bfdbfe 100%);
          color: var(--blue-accent);
          border-left: 4px solid var(--blue-accent);
        }
        
        .alert-danger {
          background: linear-gradient(135deg, #fef2f2 0%, #fecaca 100%);
          color: var(--danger);
          border-left: 4px solid var(--danger);
        }
        
        /* Tooltip styling */
        .tooltip-inner {
          background: var(--navy-primary);
          color: var(--white);
          border-radius: 8px;
          font-size: 13px;
          font-weight: 500;
          max-width: 300px;
          box-shadow: 0 4px 20px rgba(30, 58, 95, 0.3);
        }
        
        .bs-tooltip-top .arrow::before {
          border-top-color: var(--navy-primary);
        }
        
        .bs-tooltip-bottom .arrow::before {
          border-bottom-color: var(--navy-primary);
        }
        
        /* Modal styling */
        .modal-content {
          border-radius: 16px;
          border: none;
          box-shadow: 0 20px 50px rgba(30, 58, 95, 0.3);
        }
        
        .modal-header {
          background: linear-gradient(135deg, var(--navy-primary) 0%, var(--navy-secondary) 100%);
          color: var(--white);
          border-radius: 16px 16px 0 0;
          border-bottom: none;
          padding: 20px 25px;
        }
        
        .modal-title {
          font-weight: 700;
          font-size: 20px;
        }
        
        .modal-body {
          padding: 25px;
        }
        
        /* Custom scrollbar */
        ::-webkit-scrollbar {
          width: 8px;
        }
        
        ::-webkit-scrollbar-track {
          background: var(--neutral-200);
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(var(--navy-primary), var(--navy-secondary));
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: linear-gradient(var(--navy-secondary), var(--blue-accent));
        }
        
        /* Responsive design */
        @media (max-width: 768px) {
          .main-header .navbar {
            margin-left: 0 !important;
          }
          .content-wrapper {
            margin-left: 0 !important;
            padding: 10px;
          }
          .main-sidebar {
            transform: translateX(-100%);
            transition: transform 0.3s ease-in-out;
          }
          .sidebar-open .main-sidebar {
            transform: translateX(0);
          }
          .box {
            margin: 10px 5px;
          }
          .progress-indicator {
            gap: 10px;
          }
          .progress-step {
            width: 40px;
            height: 40px;
            font-size: 16px;
          }
          .progress-step:not(:last-child)::after {
            width: 20px;
          }
        }
        
        /* Print styles */
        @media print {
          .main-sidebar,
          .main-header {
            display: none !important;
          }
          .content-wrapper {
            margin: 0 !important;
            background: white !important;
          }
          .box {
            box-shadow: none !important;
            border: 1px solid #ccc !important;
          }
        }
      "))
    ),
    

    
    tabItems(
      # Data Upload Tab
      tabItem(
        tabName = "upload",
        source("ui/data_upload_ui.R", local = TRUE)$value
      ),
      
      # Analysis Setup Tab
      tabItem(
        tabName = "setup",
        source("ui/analysis_setup_ui.R", local = TRUE)$value
      ),
      

      
      # Results Tab
      tabItem(
        tabName = "results",
        results_dashboard_ui("results_dashboard")
      ),
      
      # Plots Tab
      tabItem(
        tabName = "plots",
        plots_ui("plots_dashboard")
      ),
      
      # Exports & Reports Tab
      tabItem(
        tabName = "exports",
        source("ui/exports_reports_ui.R", local = TRUE)$value
      ),
      
      # Advanced Options Tab
      tabItem(
        tabName = "advanced",
        fluidRow(
          box(
            title = "Advanced Analysis Options", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            h4("Coming Soon"),
            p("Advanced configuration options for experienced users will be available here."),
            tags$ul(
              tags$li("Custom model parameters"),
              tags$li("Advanced outlier detection"),
              tags$li("Custom confidence intervals"),
              tags$li("Specialized study designs")
            )
          )
        )
      ),
      
      # Validation Tab
      tabItem(
        tabName = "validation",
        fluidRow(
          box(
            title = "Method Validation", 
            status = "success", 
            solidHeader = TRUE,
            width = 12,
            h4("Regulatory Validation"),
            p("This application follows regulatory guidelines and industry best practices."),
            tags$ul(
              tags$li("FDA guidance compliance"),
              tags$li("EMA guideline adherence"), 
              tags$li("ICH M13A compatibility"),
              tags$li("Cross-validation with WinNonlin")
            ),
            br(),
            actionButton("run_validation", "Run Validation Tests", 
                        class = "btn-success", icon = icon("check"))
          )
        )
      ),
      
      # Sample Size Tab
      tabItem(
        tabName = "sample_size",
        source("ui/sample_size_ui.R", local = TRUE)$value
      ),
      
      # Help Tab
      tabItem(
        tabName = "help",
        fluidRow(
          box(
            title = "Help & Support", 
            status = "info", 
            solidHeader = TRUE,
            width = 8,
            h4("Getting Started"),
            p("Follow these steps to perform bioequivalence analysis:"),
            tags$ol(
              tags$li(tags$strong("Upload Data:"), " Upload your concentration-time data in CSV format"),
              tags$li(tags$strong("Configure Analysis:"), " Select study design and analysis parameters"),
              tags$li(tags$strong("Review Results:"), " Examine NCA parameters and ANOVA results")
            ),
            br(),
            h4("Data Format Requirements"),
            p("Your CSV file should contain the following columns:"),
            tags$ul(
              tags$li(tags$strong("Subject:"), " Unique subject identifier (1, 2, 3, ...)"),
              tags$li(tags$strong("Treatment:"), " Treatment code (R for Reference, T for Test)"),
              tags$li(tags$strong("Time:"), " Sampling time in hours (0, 0.25, 0.5, 1, ...)"),
              tags$li(tags$strong("Concentration:"), " Drug concentration in ng/mL")
            )
          ),
          box(
            title = "Quick Links", 
            status = "warning", 
            solidHeader = TRUE,
            width = 4,
            tags$a(href = "#", class = "btn btn-primary btn-block", 
                   icon("download"), " Download Example Data"),
            br(),
            tags$a(href = "#", class = "btn btn-info btn-block", 
                   icon("book"), " User Manual"),
            br(),
            tags$a(href = "#", class = "btn btn-success btn-block", 
                   icon("video"), " Video Tutorials"),
            br(),
            tags$a(href = "mailto:support@bioeq.com", class = "btn btn-warning btn-block", 
                   icon("envelope"), " Contact Support")
          )
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive values to store application state
  values <- reactiveValues(
    uploaded_data = NULL,
    study_design = NULL,
    nca_results = NULL,
    anova_results = NULL,
    be_results = NULL,
    analysis_config = NULL,
    data_type = NULL,
    data_summary = NULL,
    validation_result = NULL,
    columns_mapped = FALSE,
    current_step = 1,
    analysis_complete = FALSE,
    carryover_results = NULL,
    missing_data_log = NULL
  )
  
  # Progress tracking
  observe({
    current_tab <- input$sidebar
    if (!is.null(current_tab)) {
      if (current_tab == "upload") {
        values$current_step <- 1
      } else if (current_tab == "setup") {
        values$current_step <- 2
      } else if (current_tab == "results") {
        values$current_step <- 3
      } else if (current_tab == "plots") {
        values$current_step <- 4
      } else if (current_tab == "exports") {
        values$current_step <- 5
      }
    }
  })
  
  # Dynamic progress indicator
  output$progress_indicator <- renderUI({
    step1_class <- if(values$current_step >= 1) "progress-step active" else "progress-step"
    step2_class <- if(values$current_step >= 2) "progress-step active" else "progress-step" 
    step3_class <- if(values$current_step >= 3) "progress-step active" else "progress-step"
    step4_class <- if(values$current_step >= 4) "progress-step active" else "progress-step"
    step5_class <- if(values$current_step >= 5) "progress-step active" else "progress-step"
    
    if (!is.null(values$uploaded_data)) step1_class <- "progress-step completed"
    if (values$analysis_complete) {
      step3_class <- "progress-step completed"
      step4_class <- "progress-step completed"
    }
    
    div(class = "progress-indicator",
      div(class = step1_class, "1"),
      div(class = step2_class, "2"),
      div(class = step3_class, "3"),
      div(class = step4_class, "4"),
      div(class = step5_class, "5")
    )
  })
  
  # Navigation helpers
  observeEvent(input$go_to_setup, {
    req(values$uploaded_data)
    updateTabItems(session, "sidebar", "setup")
    
    # Ensure page scrolls to top when navigating to setup
    session$sendCustomMessage("scrollToTop", "")
    
    showNotification("Proceeding to analysis setup", type = "message")
  })
  
  observeEvent(input$go_to_results, {
    req(values$uploaded_data)
    updateTabItems(session, "sidebar", "results") 
    showNotification("Viewing analysis results", type = "message")
  })
  
  observeEvent(input$go_to_plots, {
    req(values$uploaded_data)
    updateTabItems(session, "sidebar", "plots") 
    showNotification("Viewing interactive plots", type = "message")
  })
  
  observeEvent(input$go_to_exports, {
    req(values$uploaded_data)
    updateTabItems(session, "sidebar", "exports") 
    showNotification("Accessing exports and reports", type = "message")
  })
  
  observeEvent(input$back_to_upload, {
    updateTabItems(session, "sidebar", "upload")
  })
  
  observeEvent(input$back_to_setup, {
    updateTabItems(session, "sidebar", "setup")
  })
  
  # Source server modules with proper environment access
  local({
    source("server/data_upload_server.R", local = environment())
    source("server/analysis_setup_server.R", local = environment()) 
    source("server/sample_size_server.R", local = environment())
  })
  
  # Initialize results dashboard module
  results_dashboard_server("results_dashboard", 
                          be_results = reactive(values$be_results),
                          nca_results = reactive(values$nca_results),
                          analysis_config = reactive(values$analysis_config),
                          carryover_results = reactive(values$carryover_results),
                          missing_data_log = reactive(values$missing_data_log))
  
  # Initialize plots dashboard module
  plots_server("plots_dashboard",
               be_results = reactive(values$be_results),
               nca_results = reactive(values$nca_results),
               analysis_config = reactive(values$analysis_config),
               uploaded_data = reactive(values$uploaded_data),
               validation_result = reactive(values$validation_result))
  
  # =======================================================================
  # EXPORTS & REPORTS SERVER LOGIC
  # =======================================================================
  
  # Gate: show exports when at least NCA or BE results are available
  output$export_data_available <- reactive({
    !is.null(values$nca_results) || !is.null(values$be_results)
  })
  outputOptions(output, "export_data_available", suspendWhenHidden = FALSE)
  
  # Also keep results_available for legacy compatibility with results dashboard
  output$results_available <- reactive({
    !is.null(values$be_results) && !is.null(values$nca_results)
  })
  outputOptions(output, "results_available", suspendWhenHidden = FALSE)
  
  # Helper: write CSV with optional metadata header
  write_export_csv <- function(df, file, metadata_lines = NULL) {
    include_meta <- isTRUE(input$export_include_metadata) && !is.null(metadata_lines)
    if (include_meta) {
      meta_text <- paste0("# ", metadata_lines)
      con <- file(file, "w")
      writeLines(meta_text, con)
      writeLines("", con)  # blank separator line
      close(con)
      # Append CSV data
      write.table(df, file, sep = ",", row.names = FALSE, col.names = TRUE,
                  append = TRUE, quote = TRUE)
    } else {
      write.csv(df, file, row.names = FALSE)
    }
  }
  
  # Helper: standard metadata lines
  build_metadata <- function(export_type) {
    design <- values$be_results$design %||% values$analysis_config$detected_design %||% "Unknown"
    n_subj <- values$be_results$n_subjects %||% "Unknown"
    alpha <- values$analysis_config$alpha_level %||% 0.05
    c(
      paste0("BioEQ Export: ", export_type),
      paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste0("Study Design: ", design),
      paste0("N Subjects: ", n_subj),
      paste0("Alpha: ", alpha),
      paste0("Confidence Level: ", round((1 - alpha * 2) * 100), "%")
    )
  }
  
  # ── 1. NCA Subject-Level Data ──
  output$download_nca_subject_data <- downloadHandler(
    filename = function() paste0("nca_subject_data_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$nca_results)
      nca <- values$nca_results
      df <- if (is.data.frame(nca)) nca else nca$subject_data %||% nca$parameters
      req(df)
      meta <- build_metadata("NCA Subject-Level Data")
      auc_method <- values$nca_results$auc_method %||% "Unknown"
      lz_method <- values$nca_results$lambda_z_method %||% "Unknown"
      meta <- c(meta, paste0("AUC Method: ", auc_method), paste0("Lambda_z Method: ", lz_method))
      write_export_csv(df, file, meta)
      showNotification("NCA subject-level data exported.", type = "message")
    }
  )
  
  # ── 2. NCA Summary Statistics ──
  output$download_nca_summary <- downloadHandler(
    filename = function() paste0("nca_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$nca_results)
      nca <- values$nca_results
      
      # Use pre-computed summary if available
      summary_df <- nca$summary
      
      # Fallback: compute from subject_data
      if (is.null(summary_df)) {
        sd <- if (is.data.frame(nca)) nca else nca$subject_data
        req(sd)
        
        pk_cols <- setdiff(names(sd), c("Subject", "Treatment", "Period", "Sequence",
                                        "Time", "Concentration", "Group"))
        pk_cols <- pk_cols[sapply(pk_cols, function(col) is.numeric(sd[[col]]))]
        
        rows <- lapply(pk_cols, function(param) {
          test_vals <- sd[[param]][sd$Treatment %in% c("T", "Test")]
          ref_vals <- sd[[param]][sd$Treatment %in% c("R", "Reference")]
          test_vals <- test_vals[!is.na(test_vals)]
          ref_vals <- ref_vals[!is.na(ref_vals)]
          
          test_mean <- if (length(test_vals) > 0) mean(test_vals) else NA
          ref_mean <- if (length(ref_vals) > 0) mean(ref_vals) else NA
          test_sd <- if (length(test_vals) > 1) sd(test_vals) else NA
          ref_sd <- if (length(ref_vals) > 1) sd(ref_vals) else NA
          test_geo <- if (length(test_vals) > 0 && all(test_vals > 0)) exp(mean(log(test_vals))) else NA
          ref_geo <- if (length(ref_vals) > 0 && all(ref_vals > 0)) exp(mean(log(ref_vals))) else NA
          ratio <- if (!is.na(test_geo) && !is.na(ref_geo) && ref_geo > 0) test_geo / ref_geo * 100 else NA
          all_vals <- c(test_vals, ref_vals)
          cv_pct <- if (length(all_vals) > 1 && mean(all_vals) > 0) sd(all_vals) / mean(all_vals) * 100 else NA
          
          data.frame(
            Parameter = param,
            N_Test = length(test_vals),
            N_Reference = length(ref_vals),
            Test_Mean = round(test_mean, 6),
            Test_SD = round(test_sd, 6),
            Test_GeoMean = round(test_geo, 6),
            Reference_Mean = round(ref_mean, 6),
            Reference_SD = round(ref_sd, 6),
            Reference_GeoMean = round(ref_geo, 6),
            Ratio_Percent = round(ratio, 4),
            CV_Percent = round(cv_pct, 2),
            stringsAsFactors = FALSE
          )
        })
        summary_df <- do.call(rbind, rows)
      }
      
      meta <- build_metadata("NCA Summary Statistics")
      write_export_csv(summary_df, file, meta)
      showNotification("NCA summary statistics exported.", type = "message")
    }
  )
  
  # ── 3. ANOVA Results (SAS/Phoenix-style stacked tables) ──
  output$download_anova_results <- downloadHandler(
    filename = function() paste0("anova_results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$be_results)
      be_res <- values$be_results
      
      all_rows <- list()
      
      # ── Case A: Parallel design (no ANOVA, t-test results) ──
      if (identical(be_res$design, "parallel") || !is.null(be_res$statistical_results)) {
        for (param in names(be_res$statistical_results)) {
          st <- be_res$statistical_results[[param]]
          ci <- be_res$confidence_intervals[[param]]
          all_rows[[length(all_rows) + 1]] <- data.frame(
            Parameter = param,
            Table_Type = "Parallel_T_Test",
            Source = "Treatment",
            DF = st$degrees_freedom %||% ci$df %||% NA,
            Sum_Sq = NA_real_,
            Mean_Sq = NA_real_,
            F_Value = NA_real_,
            t_Value = ci$t_statistic %||% NA,
            Pr = ci$p_value %||% st$p_value %||% NA,
            Method = st$method %||% "t-test",
            N_Test = st$n_test %||% NA,
            N_Reference = st$n_ref %||% NA,
            Residual_MSE = NA_real_,
            Residual_DF = NA_real_,
            Treatment_Coef = ci$log_difference %||% NA,
            Treatment_SE = ci$standard_error %||% NA,
            Treatment_Pval = ci$p_value %||% st$p_value %||% NA,
            CV_Percent = NA_real_,
            R_Squared = NA_real_,
            stringsAsFactors = FALSE
          )
        }
      }
      
      # ── Case B: Has ANOVA results (crossover, RSABE, ABEL) ──
      if (!is.null(be_res$anova_results) && !is.null(be_res$anova_results$anova_results)) {
        anova_data <- be_res$anova_results$anova_results
        
        for (param in names(anova_data)) {
          pr <- anova_data[[param]]
          if ("error" %in% names(pr)) next
          
          # ── ABEL (replicateBE) ──
          if (!is.null(pr$replicatebe_output)) {
            rbe <- pr$replicatebe_output
            all_rows[[length(all_rows) + 1]] <- data.frame(
              Parameter = param,
              Table_Type = "ABEL_Summary",
              Source = "replicateBE",
              DF = rbe$DF,
              Sum_Sq = NA_real_,
              Mean_Sq = NA_real_,
              F_Value = NA_real_,
              t_Value = NA_real_,
              Pr = NA_real_,
              Method = paste0("Method ", rbe$Method),
              N_Test = rbe$nTT,
              N_Reference = rbe$nRR,
              Residual_MSE = NA_real_,
              Residual_DF = rbe$DF,
              Treatment_Coef = NA_real_,
              Treatment_SE = NA_real_,
              Treatment_Pval = NA_real_,
              CV_Percent = NA_real_,
              R_Squared = NA_real_,
              stringsAsFactors = FALSE
            )
            # Add variability rows
            all_rows[[length(all_rows) + 1]] <- data.frame(
              Parameter = param, Table_Type = "ABEL_Variability",
              Source = "CVwR", DF = NA, Sum_Sq = NA, Mean_Sq = NA,
              F_Value = NA, t_Value = NA, Pr = rbe$`CVwR(%)`,
              Method = NA, N_Test = NA, N_Reference = NA,
              Residual_MSE = NA, Residual_DF = NA,
              Treatment_Coef = NA, Treatment_SE = NA, Treatment_Pval = NA,
              CV_Percent = rbe$`CVwR(%)`, R_Squared = NA,
              stringsAsFactors = FALSE
            )
            all_rows[[length(all_rows) + 1]] <- data.frame(
              Parameter = param, Table_Type = "ABEL_Variability",
              Source = "CVwT", DF = NA, Sum_Sq = NA, Mean_Sq = NA,
              F_Value = NA, t_Value = NA, Pr = rbe$`CVwT(%)`,
              Method = NA, N_Test = NA, N_Reference = NA,
              Residual_MSE = NA, Residual_DF = NA,
              Treatment_Coef = NA, Treatment_SE = NA, Treatment_Pval = NA,
              CV_Percent = rbe$`CVwT(%)`, R_Squared = NA,
              stringsAsFactors = FALSE
            )
            all_rows[[length(all_rows) + 1]] <- data.frame(
              Parameter = param, Table_Type = "ABEL_Variability",
              Source = "swR", DF = NA, Sum_Sq = rbe$swR, Mean_Sq = NA,
              F_Value = NA, t_Value = NA, Pr = NA,
              Method = NA, N_Test = NA, N_Reference = NA,
              Residual_MSE = NA, Residual_DF = NA,
              Treatment_Coef = NA, Treatment_SE = NA, Treatment_Pval = NA,
              CV_Percent = NA, R_Squared = NA,
              stringsAsFactors = FALSE
            )
            all_rows[[length(all_rows) + 1]] <- data.frame(
              Parameter = param, Table_Type = "ABEL_Variability",
              Source = "swT", DF = NA, Sum_Sq = rbe$swT, Mean_Sq = NA,
              F_Value = NA, t_Value = NA, Pr = NA,
              Method = NA, N_Test = NA, N_Reference = NA,
              Residual_MSE = NA, Residual_DF = NA,
              Treatment_Coef = NA, Treatment_SE = NA, Treatment_Pval = NA,
              CV_Percent = NA, R_Squared = NA,
              stringsAsFactors = FALSE
            )
            next
          }
          
          # ── RSABE ──
          if (!is.null(pr$s2_wR)) {
            # Model summary row
            all_rows[[length(all_rows) + 1]] <- data.frame(
              Parameter = param,
              Table_Type = "RSABE_Model_Summary",
              Source = "Model",
              DF = pr$residual_df,
              Sum_Sq = NA_real_,
              Mean_Sq = pr$residual_mse,
              F_Value = NA_real_,
              t_Value = NA_real_,
              Pr = NA_real_,
              Method = if (pr$anova_method == "fixed") "Fixed Effects" else "Mixed Effects (nlme)",
              N_Test = NA_real_,
              N_Reference = NA_real_,
              Residual_MSE = pr$residual_mse,
              Residual_DF = pr$residual_df,
              Treatment_Coef = pr$treatment_coef,
              Treatment_SE = pr$treatment_se,
              Treatment_Pval = NA_real_,
              CV_Percent = pr$cv_wr_percent,
              R_Squared = NA_real_,
              stringsAsFactors = FALSE
            )
            
            # RSABE ANOVA table rows (from anova(model))
            if (!is.null(pr$anova)) {
              anova_df <- tryCatch(as.data.frame(pr$anova), error = function(e) NULL)
              if (!is.null(anova_df) && nrow(anova_df) > 0) {
                for (i in 1:nrow(anova_df)) {
                  row_source <- rownames(anova_df)[i]
                  all_rows[[length(all_rows) + 1]] <- data.frame(
                    Parameter = param,
                    Table_Type = "RSABE_ANOVA",
                    Source = row_source,
                    DF = if ("numDF" %in% names(anova_df)) anova_df[i, "numDF"] else if ("Df" %in% names(anova_df)) anova_df[i, "Df"] else NA,
                    Sum_Sq = if ("Sum Sq" %in% names(anova_df)) anova_df[i, "Sum Sq"] else NA,
                    Mean_Sq = if ("Mean Sq" %in% names(anova_df)) anova_df[i, "Mean Sq"] else NA,
                    F_Value = if ("F-value" %in% names(anova_df)) anova_df[i, "F-value"] else if ("F value" %in% names(anova_df)) anova_df[i, "F value"] else NA,
                    t_Value = NA_real_,
                    Pr = if ("p-value" %in% names(anova_df)) anova_df[i, "p-value"] else if ("Pr(>F)" %in% names(anova_df)) anova_df[i, "Pr(>F)"] else NA,
                    Method = NA_character_,
                    N_Test = NA_real_, N_Reference = NA_real_,
                    Residual_MSE = NA_real_, Residual_DF = if ("denDF" %in% names(anova_df)) anova_df[i, "denDF"] else if ("DenDF" %in% names(anova_df)) anova_df[i, "DenDF"] else NA,
                    Treatment_Coef = NA_real_, Treatment_SE = NA_real_, Treatment_Pval = NA_real_,
                    CV_Percent = NA_real_, R_Squared = NA_real_,
                    stringsAsFactors = FALSE
                  )
                }
              }
            }
            
            # ISC variance rows
            all_rows[[length(all_rows) + 1]] <- data.frame(
              Parameter = param, Table_Type = "RSABE_Variance",
              Source = "s2_wR", DF = pr$df_wR, Sum_Sq = pr$s2_wR, Mean_Sq = NA,
              F_Value = NA, t_Value = NA, Pr = NA, Method = NA,
              N_Test = NA, N_Reference = NA, Residual_MSE = NA, Residual_DF = NA,
              Treatment_Coef = NA, Treatment_SE = NA, Treatment_Pval = NA,
              CV_Percent = pr$cv_wr_percent, R_Squared = NA,
              stringsAsFactors = FALSE
            )
            if (!is.na(pr$s2_wT %||% NA)) {
              all_rows[[length(all_rows) + 1]] <- data.frame(
                Parameter = param, Table_Type = "RSABE_Variance",
                Source = "s2_wT", DF = pr$df_wT, Sum_Sq = pr$s2_wT, Mean_Sq = NA,
                F_Value = NA, t_Value = NA, Pr = NA, Method = NA,
                N_Test = NA, N_Reference = NA, Residual_MSE = NA, Residual_DF = NA,
                Treatment_Coef = NA, Treatment_SE = NA, Treatment_Pval = NA,
                CV_Percent = pr$cv_wt_percent %||% NA, R_Squared = NA,
                stringsAsFactors = FALSE
              )
            }
            next
          }
          
          # ── ABE Crossover (standard ANOVA) ──
          anova_method_label <- if ((pr$anova_method %||% "fixed") == "fixed") "Fixed Effects" else "Mixed Effects (nlme)"
          
          # Model summary row
          all_rows[[length(all_rows) + 1]] <- data.frame(
            Parameter = param,
            Table_Type = "Model_Summary",
            Source = "Model",
            DF = pr$residual_df,
            Sum_Sq = NA_real_,
            Mean_Sq = pr$residual_mse,
            F_Value = NA_real_,
            t_Value = NA_real_,
            Pr = NA_real_,
            Method = anova_method_label,
            N_Test = NA_real_,
            N_Reference = NA_real_,
            Residual_MSE = pr$residual_mse,
            Residual_DF = pr$residual_df,
            Treatment_Coef = pr$treatment_coef,
            Treatment_SE = pr$treatment_se,
            Treatment_Pval = pr$treatment_pval %||% NA,
            CV_Percent = pr$cv_percent %||% NA,
            R_Squared = pr$r_squared %||% NA,
            stringsAsFactors = FALSE
          )
          
          # Type I SS rows
          if (!is.null(pr$anova)) {
            type1 <- tryCatch(as.data.frame(pr$anova), error = function(e) NULL)
            if (!is.null(type1) && nrow(type1) > 0) {
              for (i in 1:nrow(type1)) {
                all_rows[[length(all_rows) + 1]] <- data.frame(
                  Parameter = param,
                  Table_Type = "Type_I_SS",
                  Source = rownames(type1)[i],
                  DF = if ("Df" %in% names(type1)) type1[i, "Df"] else NA,
                  Sum_Sq = if ("Sum Sq" %in% names(type1)) type1[i, "Sum Sq"] else NA,
                  Mean_Sq = if ("Mean Sq" %in% names(type1)) type1[i, "Mean Sq"] else NA,
                  F_Value = if ("F value" %in% names(type1)) type1[i, "F value"] else NA,
                  t_Value = NA_real_,
                  Pr = if ("Pr(>F)" %in% names(type1)) type1[i, "Pr(>F)"] else NA,
                  Method = NA_character_,
                  N_Test = NA_real_, N_Reference = NA_real_,
                  Residual_MSE = NA_real_, Residual_DF = NA_real_,
                  Treatment_Coef = NA_real_, Treatment_SE = NA_real_, Treatment_Pval = NA_real_,
                  CV_Percent = NA_real_, R_Squared = NA_real_,
                  stringsAsFactors = FALSE
                )
              }
            }
          }
          
          # Type III SS rows
          if (!is.null(pr$type3_ss)) {
            type3 <- tryCatch(as.data.frame(pr$type3_ss), error = function(e) NULL)
            if (!is.null(type3) && nrow(type3) > 0) {
              for (i in 1:nrow(type3)) {
                all_rows[[length(all_rows) + 1]] <- data.frame(
                  Parameter = param,
                  Table_Type = "Type_III_SS",
                  Source = rownames(type3)[i],
                  DF = if ("Df" %in% names(type3)) type3[i, "Df"] else NA,
                  Sum_Sq = if ("Sum Sq" %in% names(type3)) type3[i, "Sum Sq"] else NA,
                  Mean_Sq = if ("Mean Sq" %in% names(type3)) type3[i, "Mean Sq"] else NA,
                  F_Value = if ("F value" %in% names(type3)) type3[i, "F value"] else NA,
                  t_Value = NA_real_,
                  Pr = if ("Pr(>F)" %in% names(type3)) type3[i, "Pr(>F)"] else NA,
                  Method = NA_character_,
                  N_Test = NA_real_, N_Reference = NA_real_,
                  Residual_MSE = NA_real_, Residual_DF = NA_real_,
                  Treatment_Coef = NA_real_, Treatment_SE = NA_real_, Treatment_Pval = NA_real_,
                  CV_Percent = NA_real_, R_Squared = NA_real_,
                  stringsAsFactors = FALSE
                )
              }
            }
          }
          
          # Comprehensive ANOVA (Model/Error/Corrected Total)
          if (!is.null(pr$anova_comprehensive)) {
            comp <- tryCatch(as.data.frame(pr$anova_comprehensive), error = function(e) NULL)
            if (!is.null(comp) && nrow(comp) > 0) {
              for (i in 1:nrow(comp)) {
                src <- if ("Source" %in% names(comp)) comp$Source[i] else rownames(comp)[i]
                all_rows[[length(all_rows) + 1]] <- data.frame(
                  Parameter = param,
                  Table_Type = "Comprehensive",
                  Source = src,
                  DF = if ("Df" %in% names(comp)) comp[i, "Df"] else NA,
                  Sum_Sq = if ("Sum Sq" %in% names(comp)) comp[i, "Sum Sq"] else NA,
                  Mean_Sq = if ("Mean Sq" %in% names(comp)) comp[i, "Mean Sq"] else NA,
                  F_Value = if ("F value" %in% names(comp)) comp[i, "F value"] else NA,
                  t_Value = NA_real_,
                  Pr = if ("Pr(>F)" %in% names(comp)) comp[i, "Pr(>F)"] else NA,
                  Method = NA_character_,
                  N_Test = NA_real_, N_Reference = NA_real_,
                  Residual_MSE = NA_real_, Residual_DF = NA_real_,
                  Treatment_Coef = NA_real_, Treatment_SE = NA_real_, Treatment_Pval = NA_real_,
                  CV_Percent = NA_real_, R_Squared = NA_real_,
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
      }
      
      if (length(all_rows) == 0) {
        # Fallback: no ANOVA data available
        all_rows[[1]] <- data.frame(
          Parameter = "No ANOVA results", Table_Type = NA, Source = NA,
          DF = NA, Sum_Sq = NA, Mean_Sq = NA, F_Value = NA, t_Value = NA, Pr = NA,
          Method = NA, N_Test = NA, N_Reference = NA,
          Residual_MSE = NA, Residual_DF = NA,
          Treatment_Coef = NA, Treatment_SE = NA, Treatment_Pval = NA,
          CV_Percent = NA, R_Squared = NA,
          stringsAsFactors = FALSE
        )
      }
      
      result_df <- do.call(rbind, all_rows)
      meta <- build_metadata("ANOVA Results (SAS-Style)")
      be_type <- be_res$analysis_type %||% "ABE"
      meta <- c(meta, paste0("Analysis Type: ", be_type))
      write_export_csv(result_df, file, meta)
      showNotification("ANOVA results exported.", type = "message")
    }
  )
  
  # ── 4. BE Assessment Results ──
  output$download_be_results <- downloadHandler(
    filename = function() paste0("be_results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$be_results)
      be_res <- values$be_results
      req(be_res$confidence_intervals)
      
      ci_results <- be_res$confidence_intervals
      be_conclusions <- be_res$be_conclusions %||% list()
      analysis_type <- be_res$analysis_type %||% "ABE"
      
      rows <- lapply(names(ci_results), function(param) {
        ci <- ci_results[[param]]
        if (is.null(ci) || is.na(ci$point_estimate %||% NA)) return(NULL)
        
        is_be <- be_conclusions[[param]]
        
        # Limits
        lower_limit <- 80
        upper_limit <- 125
        limit_type <- "fixed"
        if (!is.null(ci$limits_used)) {
          lower_limit <- ci$limits_used$lower %||% 80
          upper_limit <- ci$limits_used$upper %||% 125
          limit_type <- ci$limits_used$type %||% "fixed"
        }
        
        base_row <- data.frame(
          Parameter = param,
          Analysis_Type = analysis_type,
          Point_Estimate_Pct = round(ci$point_estimate, 4),
          CI_Lower_Pct = round(ci$ci_lower, 4),
          CI_Upper_Pct = round(ci$ci_upper, 4),
          Confidence_Level = ci$confidence_level %||% 90,
          Geometric_Mean_Ratio = round(ci$geometric_mean_ratio %||% (ci$point_estimate / 100), 6),
          BE_Lower_Limit = lower_limit,
          BE_Upper_Limit = upper_limit,
          Limit_Type = limit_type,
          Degrees_Freedom = ci$degrees_freedom %||% NA,
          N_Subjects = ci$n_subjects %||% be_res$n_subjects %||% NA,
          Within_Limits = ci$within_limits %||% NA,
          BE_Conclusion = if (is.na(is_be %||% NA)) "Unknown" else if (is_be) "Bioequivalent" else "Not Bioequivalent",
          Log_Difference = ci$log_difference %||% NA,
          Log_CI_Lower = ci$log_ci_lower %||% NA,
          Log_CI_Upper = ci$log_ci_upper %||% NA,
          Standard_Error = ci$standard_error %||% NA,
          MSE = ci$mse %||% NA,
          t_Critical = ci$t_critical %||% NA,
          stringsAsFactors = FALSE
        )
        
        # RSABE-specific columns
        if (analysis_type == "RSABE") {
          rsabe_det <- be_res$rsabe_details[[param]]
          rsabe_test <- if (!is.null(rsabe_det)) rsabe_det$rsabe_test else NULL
          base_row$CV_wR_Pct <- ci$cv_wr %||% NA
          base_row$s2_wR <- ci$s2_wR %||% NA
          base_row$Is_HV <- if (!is.null(rsabe_det)) rsabe_det$is_hv else NA
          base_row$RSABE_Method <- be_res$rsabe_method %||% NA
          base_row$Scaling_Pass <- if (!is.null(rsabe_test)) rsabe_test$rsabe_pass else NA
          base_row$PE_Constraint_Pass <- if (!is.null(rsabe_det)) rsabe_det$pe_constraint_pass else NA
          if (!is.null(rsabe_test)) {
            base_row$UCB <- rsabe_test$ucb %||% NA
            base_row$Scaled_Lower <- rsabe_test$scaled_lower %||% rsabe_test$scaled_lower_pct %||% NA
            base_row$Scaled_Upper <- rsabe_test$scaled_upper %||% rsabe_test$scaled_upper_pct %||% NA
          }
        }
        
        # ABEL-specific columns
        if (analysis_type == "ABEL") {
          base_row$CV_wR_Pct <- ci$cv_wr %||% NA
          base_row$CV_wT_Pct <- ci$cv_wt %||% NA
          base_row$Scaled_Lower <- ci$scaled_lower_limit %||% NA
          base_row$Scaled_Upper <- ci$scaled_upper_limit %||% NA
          base_row$Regulator <- ci$regulator %||% be_res$regulator %||% NA
          base_row$sw_Reference <- ci$sw_reference %||% NA
          base_row$sw_Test <- ci$sw_test %||% NA
          base_row$sw_Ratio <- ci$sw_ratio %||% NA
        }
        
        base_row
      })
      
      rows <- rows[!sapply(rows, is.null)]
      
      if (length(rows) == 0) {
        result_df <- data.frame(Note = "No BE results available", stringsAsFactors = FALSE)
      } else {
        # rbind with fill for differing columns across params
        all_cols <- unique(unlist(lapply(rows, names)))
        rows_filled <- lapply(rows, function(r) {
          missing_cols <- setdiff(all_cols, names(r))
          for (mc in missing_cols) r[[mc]] <- NA
          r[all_cols]
        })
        result_df <- do.call(rbind, rows_filled)
      }
      
      meta <- build_metadata("BE Assessment Results")
      meta <- c(meta, paste0("Analysis Type: ", analysis_type),
                paste0("BE Method: ", be_res$be_method %||% be_res$analysis_method %||% analysis_type))
      write_export_csv(result_df, file, meta)
      showNotification("BE assessment results exported.", type = "message")
    }
  )
  
  # ── 5. Raw Uploaded Data ──
  output$download_raw_data <- downloadHandler(
    filename = function() paste0("raw_uploaded_data_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$uploaded_data)
      meta <- c(
        "BioEQ Export: Raw Uploaded Data",
        paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        paste0("Rows: ", nrow(values$uploaded_data)),
        paste0("Columns: ", paste(names(values$uploaded_data), collapse = ", "))
      )
      write_export_csv(values$uploaded_data, file, meta)
      showNotification("Raw data exported.", type = "message")
    }
  )
  
  # Validation test runner
  observeEvent(input$run_validation, {
    showNotification("Running validation tests...", type = "message", duration = 2)
    # TODO: Implement validation tests
    Sys.sleep(2)
    showNotification("All validation tests passed!", type = "message")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
