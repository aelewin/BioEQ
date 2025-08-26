# Plots Server Module
# Server logic for interactive plotly visualizations

# Load required libraries for plotting
if (!require(plotly, quietly = TRUE)) {
  message("plotly not available - installing...")
  install.packages("plotly")
  library(plotly)
}

if (!require(htmlwidgets, quietly = TRUE)) {
  message("htmlwidgets not available - installing...")
  install.packages("htmlwidgets")
  library(htmlwidgets)
}

if (!require(digest, quietly = TRUE)) {
  message("digest not available - installing...")
  install.packages("digest")
  library(digest)
}

# Source plotting functions
source("../R/plotting.R", local = TRUE)
source("../R/cumulative_be_analysis.R", local = TRUE)

plots_server <- function(id, be_results, nca_results, analysis_config, uploaded_data, validation_result = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for plot management
    plot_values <- reactiveValues(
      plot_objects = list(),
      temp_dir = NULL,
      last_analysis_hash = NULL,
      plots_cached = FALSE
    )
    
    # Store temp directory path for cleanup
    temp_dir_path <- NULL
    
    # Initialize session-based temp directory
    observe({
      if (is.null(plot_values$temp_dir)) {
        # Create session-specific temp directory
        temp_dir_path <<- file.path(tempdir(), "bioeq_plots", session$token)
        plot_values$temp_dir <- temp_dir_path
        dir.create(plot_values$temp_dir, recursive = TRUE, showWarnings = FALSE)
        cat("Created temp directory:", plot_values$temp_dir, "\n")
      }
    })
    
    # Check if plots can be generated
    output$plots_available <- reactive({
      !is.null(be_results()) && !is.null(nca_results()) && !is.null(uploaded_data())
    })
    outputOptions(output, "plots_available", suspendWhenHidden = FALSE)
    
    # Generate analysis hash for caching
    analysis_hash <- reactive({
      req(be_results(), nca_results(), uploaded_data())
      
      # Create hash from key analysis components
      hash_components <- list(
        be_results = if(is.list(be_results())) names(be_results()) else "simple",
        nca_summary = if(is.list(nca_results()) && !is.null(nca_results()$subject_data)) {
          paste(nrow(nca_results()$subject_data), ncol(nca_results()$subject_data))
        } else "simple",
        data_summary = paste(nrow(uploaded_data()), ncol(uploaded_data())),
        timestamp = Sys.time()
      )
      
      digest::digest(hash_components, algo = "md5")
    })
    
    # Generate plots when analysis data changes
    observe({
      req(analysis_hash())
      
      current_hash <- analysis_hash()
      
      # Check if we need to regenerate plots
      if (is.null(plot_values$last_analysis_hash) || 
          plot_values$last_analysis_hash != current_hash ||
          !plot_values$plots_cached) {
        
        cat("Generating plots for new analysis...\n")
        generate_all_plots()
        plot_values$last_analysis_hash <- current_hash
        plot_values$plots_cached <- TRUE
      }
    })
    
    # Generate all available plots
    generate_all_plots <- function() {
      req(be_results(), nca_results(), uploaded_data())
      
      cat("Starting plot generation...\n")
      
      plot_objects <- list()
      
      # Generate concentration-time plots
      tryCatch({
        cat("Generating concentration-time plots...\n")
        conc_data <- uploaded_data()
        
        # Debug: Check data structure
        if (!is.null(conc_data)) {
          cat("Concentration data columns:", paste(names(conc_data), collapse = ", "), "\n")
          cat("Concentration data rows:", nrow(conc_data), "\n")
        } else {
          cat("Concentration data is NULL\n")
        }
        
        # Get user-specified units from validation result
        concentration_label <- "Concentration (ng/mL)"
        time_label <- "Time (h)"
        
        if (!is.null(validation_result()) && !is.null(validation_result()$units_settings)) {
          units_settings <- validation_result()$units_settings
          if (!is.null(units_settings$concentration)) {
            concentration_label <- paste0("Concentration (", units_settings$concentration, ")")
          }
          if (!is.null(units_settings$time)) {
            time_label <- paste0("Time (", units_settings$time, ")")
          }
        }
        
        # Set global options for the plotting functions to access
        options(bioeq.concentration.label = concentration_label)
        options(bioeq.time.label = time_label)
        
        # Standardize column names for concentration-time plotting
        if (!is.null(conc_data)) {
          # Create standardized column mapping
          col_mapping <- list(
            "Time" = c("time", "Time", "TIME"),
            "Concentration" = c("concentration", "Concentration", "CONCENTRATION", "conc", "Conc"),
            "Subject" = c("subject", "Subject", "SUBJECT", "subj", "Subj", "ID", "id"),
            "Formulation" = c("treatment", "Treatment", "TREATMENT", "formulation", "Formulation", "FORMULATION", "trt", "Trt")
          )
          
          # Standardize column names
          standardized_data <- conc_data
          for (std_name in names(col_mapping)) {
            possible_names <- col_mapping[[std_name]]
            found_col <- intersect(possible_names, names(conc_data))
            if (length(found_col) > 0) {
              # Rename the first found column to the standard name
              names(standardized_data)[names(standardized_data) == found_col[1]] <- std_name
              cat("Mapped column", found_col[1], "to", std_name, "\n")
            }
          }
          
          # Map treatment values: T -> Test, R -> Reference for proper coloring
          if ("Formulation" %in% names(standardized_data)) {
            cat("Original formulation values:", paste(unique(standardized_data$Formulation), collapse = ", "), "\n")
            standardized_data$Formulation <- ifelse(
              standardized_data$Formulation == "T", "Test",
              ifelse(standardized_data$Formulation == "R", "Reference", 
                     standardized_data$Formulation)
            )
            cat("Mapped formulation values:", paste(unique(standardized_data$Formulation), collapse = ", "), "\n")
          }
          
          cat("Standardized columns:", paste(names(standardized_data), collapse = ", "), "\n")
        } else {
          standardized_data <- NULL
        }
        
        # Linear scale plot
        conc_linear <- shiny_plot_concentration_time(
          data = standardized_data,
          log_scale = FALSE,
          interactive = TRUE
        )
        
        cat("Linear plot result - error:", conc_linear$error, "plot is null:", is.null(conc_linear$plot), "\n")
        
        # Log scale plot
        conc_log <- shiny_plot_concentration_time(
          data = standardized_data,
          log_scale = TRUE,
          interactive = TRUE
        )
        
        cat("Log plot result - error:", conc_log$error, "plot is null:", is.null(conc_log$plot), "\n")
        
        plot_objects$concentration <- list(
          linear = conc_linear,
          log = conc_log
        )
        
        cat("✓ Concentration-time plots generated\n")
      }, error = function(e) {
        cat("✗ Error generating concentration-time plots:", e$message, "\n")
        plot_objects$concentration <- list(error = e$message)
      })
      
      # Prepare individual subjects plot data (placeholder for user selection)
      tryCatch({
        cat("Preparing individual subjects plot data...\n")
        
        # Just store the standardized data for individual subjects plotting
        # The actual plot will be generated when user selects subjects
        plot_objects$individual_subjects <- list(
          data = standardized_data,
          error = NULL
        )
        
        cat("✓ Individual subjects plot data prepared\n")
      }, error = function(e) {
        cat("✗ Error preparing individual subjects plot data:", e$message, "\n")
        plot_objects$individual_subjects <- list(error = e$message)
      })
      
      # Generate PK parameter boxplots
      tryCatch({
        cat("Generating PK boxplots...\n")
        # Use BE results data which contains merged PK parameters with formulation info
        be_data <- be_results()
        
        # Extract unique subject-period-treatment combinations for PK boxplots
        if (!is.null(be_data) && "merged_data" %in% names(be_data)) {
          pk_data <- be_data$merged_data
          # Get unique rows per subject-treatment combination
          pk_data <- pk_data %>%
            dplyr::select(subject, treatment, period, sequence, Cmax, AUC0t, AUC0inf) %>%
            dplyr::distinct() %>%
            dplyr::rename(Formulation = treatment)
        } else {
          pk_data <- nca_results()
          # Add formulation column if missing
          if (!"Formulation" %in% names(pk_data) && "treatment" %in% names(pk_data)) {
            pk_data <- pk_data %>% dplyr::rename(Formulation = treatment)
          }
        }
        
        pk_boxplots <- shiny_plot_pk_boxplots(
          data = pk_data,
          parameters = c("Cmax", "AUC0t", "AUC0inf"),
          interactive = TRUE
        )
        
        plot_objects$pk_boxplots <- pk_boxplots
        
        cat("✓ PK boxplots generated\n")
      }, error = function(e) {
        cat("✗ Error generating PK boxplots:", e$message, "\n")
        plot_objects$pk_boxplots <- list(error = e$message)
      })
      
      # Generate BE confidence interval plot
      tryCatch({
        cat("Generating BE confidence intervals...\n")
        be_data <- be_results()
        
        # Filter BE results to only include user-selected primary PK parameters
        if (!is.null(analysis_config()) && !is.null(analysis_config()$selected_pk_params)) {
          selected_params <- analysis_config()$selected_pk_params
          cat("Selected PK parameters for BE plot:", paste(selected_params, collapse = ", "), "\n")
          
          # Filter the confidence intervals to only include selected parameters
          if (!is.null(be_data$confidence_intervals)) {
            # Get primary parameters (exclude log-transformed versions for cleaner display)
            primary_params <- intersect(selected_params, names(be_data$confidence_intervals))
            cat("Available BE parameters:", paste(names(be_data$confidence_intervals), collapse = ", "), "\n")
            cat("Filtered to primary parameters:", paste(primary_params, collapse = ", "), "\n")
            
            # Check if we have any parameters after filtering
            if (length(primary_params) > 0) {
              # Create filtered BE results object
              filtered_be_data <- be_data
              filtered_be_data$confidence_intervals <- be_data$confidence_intervals[primary_params]
              
              be_ci_plot <- shiny_plot_be_confidence_intervals(
                be_results = filtered_be_data,
                interactive = TRUE
              )
            } else {
              cat("No matching parameters found after filtering\n")
              # Fall back to using all available parameters
              be_ci_plot <- shiny_plot_be_confidence_intervals(
                be_results = be_data,
                interactive = TRUE
              )
            }
          } else {
            cat("No confidence intervals available in BE results\n")
            be_ci_plot <- list(plot = NULL, error = "No confidence intervals available")
          }
        } else {
          cat("No analysis config or selected params available, using all parameters\n")
          be_ci_plot <- shiny_plot_be_confidence_intervals(
            be_results = be_data,
            interactive = TRUE
          )
        }
        
        plot_objects$be_ci <- be_ci_plot
        
        cat("✓ BE confidence intervals generated\n")
      }, error = function(e) {
        cat("✗ Error generating BE confidence intervals:", e$message, "\n")
        plot_objects$be_ci <- list(error = e$message)
      })
      
      # Setup cumulative bioequivalence analysis (data preparation only)
      tryCatch({
        cat("Setting up cumulative bioequivalence analysis...\n")
        be_data <- be_results()
        config <- analysis_config()
        
        # Get the selected ANOVA method and PK parameters
        anova_method <- if (!is.null(config) && !is.null(config$anova_method)) {
          config$anova_method
        } else {
          "fixed_effects"
        }
        
        selected_params <- if (!is.null(config) && !is.null(config$selected_pk_params)) {
          config$selected_pk_params
        } else {
          c("Cmax", "AUC0t", "AUC0inf")
        }
        
        # Detect study design
        study_design <- if (!is.null(config) && !is.null(config$study_design)) {
          config$study_design
        } else {
          "2x2x2"  # Default assumption
        }
        
        cat("Using ANOVA method:", anova_method, "for cumulative analysis\n")
        cat("Selected parameters:", paste(selected_params, collapse = ", "), "\n")
        cat("Study design:", study_design, "\n")
        
        # Check what data is available in BE results
        if (!is.null(be_data)) {
          cat("BE results structure:", paste(names(be_data), collapse = ", "), "\n")
          
          # Try different data sources in order of preference
          pk_data <- NULL
          
          if ("merged_data" %in% names(be_data)) {
            pk_data <- be_data$merged_data
            cat("Using merged_data from BE results\n")
          } else if ("pk_parameters" %in% names(be_data)) {
            pk_data <- be_data$pk_parameters
            cat("Using pk_parameters from BE results\n")
          } else if ("data" %in% names(be_data)) {
            pk_data <- be_data$data
            cat("Using data from BE results\n")
          } else {
            # Try using NCA results directly
            nca_data <- nca_results()
            if (!is.null(nca_data) && "nca_results" %in% names(nca_data)) {
              pk_data <- nca_data$nca_results
              cat("Using NCA results data for cumulative analysis\n")
            }
          }
          
          if (!is.null(pk_data) && nrow(pk_data) > 0) {
            cat("Found PK data with", nrow(pk_data), "rows and", ncol(pk_data), "columns\n")
            cat("Available columns:", paste(names(pk_data), collapse = ", "), "\n")
            
            # Setup cumulative analysis data using the new approach
            cumulative_setup <- generate_cumulative_be_plots(
              data = pk_data,
              parameters = selected_params,
              anova_method = anova_method,
              be_limits = c(0.8, 1.25),
              interactive = TRUE,
              study_design = study_design
            )
            
            plot_objects$cumulative_be <- cumulative_setup
            cat("✓ Cumulative bioequivalence analysis setup complete\n")
          } else {
            cat("No suitable PK data found for cumulative analysis\n")
            plot_objects$cumulative_be <- list(error = "No PK data available for cumulative analysis")
          }
        } else {
          cat("No BE results available for cumulative analysis\n")
          plot_objects$cumulative_be <- list(error = "No BE results available")
        }
        
      }, error = function(e) {
        cat("✗ Error setting up cumulative bioequivalence analysis:", e$message, "\n")
        plot_objects$cumulative_be <- list(error = paste("Setup error:", e$message))
      })
      
      # Store plot objects
      plot_values$plot_objects <- plot_objects
      cat("All plots generation complete\n")
    }
    
    # Individual plot output handlers for each tab
    
    # Concentration-Time Plot Tab
    output$concentration_plot_display <- renderUI({
      req(plot_values$plot_objects)
      plot_data <- plot_values$plot_objects[["concentration"]]
      
      if (is.null(plot_data)) {
        return(div(class = "alert alert-info text-center",
          icon("info-circle"), " Concentration-time plot will appear here once analysis is complete."
        ))
      }
      
      if (!is.null(plot_data$error)) {
        return(div(class = "alert alert-danger",
          strong("Error: "), plot_data$error
        ))
      }
      
      create_concentration_plot_card(plot_data)
    })
    
    # Cumulative PK Plot Tab  
    output$cumulative_pk_display <- renderUI({
      req(plot_values$plot_objects)
      plot_data <- plot_values$plot_objects[["cumulative_be"]]
      
      if (is.null(plot_data)) {
        return(div(class = "alert alert-info text-center",
          icon("info-circle"), " Cumulative bioequivalence plots will appear here once analysis is complete."
        ))
      }
      
      if (!is.null(plot_data$error)) {
        return(div(class = "alert alert-danger",
          strong("Error: "), plot_data$error
        ))
      }
      
      create_cumulative_plot_card(plot_data)
    })
    
    # BE Assessment Plot Tab
    output$be_ci_display <- renderUI({
      req(plot_values$plot_objects)
      plot_data <- plot_values$plot_objects[["be_ci"]]
      
      if (is.null(plot_data)) {
        return(div(class = "alert alert-info text-center",
          icon("info-circle"), " Bioequivalence assessment plot will appear here once analysis is complete."
        ))
      }
      
      if (!is.null(plot_data$error)) {
        return(div(class = "alert alert-danger",
          strong("Error: "), plot_data$error
        ))
      }
      
      create_simple_plot_card(plot_data, "Bioequivalence Assessment", "bullseye")
    })
    
    # Individual Subjects Plot Tab
    output$individual_subjects_display <- renderUI({
      req(plot_values$plot_objects)
      plot_data <- plot_values$plot_objects[["individual_subjects"]]
      
      if (is.null(plot_data)) {
        return(div(class = "alert alert-info text-center",
          icon("info-circle"), " Individual subject plots will appear here once analysis is complete."
        ))
      }
      
      if (!is.null(plot_data$error)) {
        return(div(class = "alert alert-danger",
          strong("Error: "), plot_data$error
        ))
      }
      
      create_individual_subjects_card(plot_data)
    })
    
    # Create concentration plot card (special case with tabs)
    create_concentration_plot_card <- function(plot_data) {
      div(class = "plot-card",
        div(class = "plot-card-header",
          icon("line-chart"), "Concentration-Time Profiles"
        ),
        div(class = "plot-card-body",
          div(class = "mb-3",
            p("Mean concentration-time profiles comparing test and reference formulations.",
              style = "color: var(--neutral-600); font-size: 14px; margin-bottom: 15px;")
          ),
          
          # Tab navigation for linear/log
          div(class = "nav-tabs-custom",
            tags$ul(class = "nav nav-tabs", role = "tablist",
              tags$li(role = "presentation", class = "active",
                tags$a(href = paste0("#", ns("conc_linear_tab")), 
                      `aria-controls` = "linear", role = "tab", 
                      `data-toggle` = "tab", "Linear Scale")
              ),
              tags$li(role = "presentation",
                tags$a(href = paste0("#", ns("conc_log_tab")), 
                      `aria-controls` = "log", role = "tab", 
                      `data-toggle` = "tab", "Log Scale")
              )
            ),
            div(class = "tab-content",
              div(role = "tabpanel", class = "tab-pane active", 
                  id = ns("conc_linear_tab"),
                  if (!is.null(plot_data$linear$plot) && is.null(plot_data$linear$error)) {
                    plotlyOutput(ns("conc_linear_plot"), height = "500px")
                  } else {
                    div(class = "alert alert-warning", 
                        "Linear scale plot not available")
                  }
              ),
              div(role = "tabpanel", class = "tab-pane", 
                  id = ns("conc_log_tab"),
                  if (!is.null(plot_data$log$plot) && is.null(plot_data$log$error)) {
                    plotlyOutput(ns("conc_log_plot"), height = "500px")
                  } else {
                    div(class = "alert alert-warning", 
                        "Log scale plot not available")
                  }
              )
            )
          )
        )
      )
    }
    
    # Create simple plot card
    create_simple_plot_card <- function(plot_data, title, icon_name) {
      output_id <- paste0(gsub("[^a-zA-Z0-9]", "_", tolower(title)), "_plot")
      
      div(class = "plot-card",
        div(class = "plot-card-header",
          icon(icon_name), title
        ),
        div(class = "plot-card-body",
          div(class = "mb-3",
            p(get_plot_description(title),
              style = "color: var(--neutral-600); font-size: 14px; margin-bottom: 15px;")
          ),
          
          if (!is.null(plot_data$plot) && is.null(plot_data$error)) {
            plotlyOutput(ns(output_id), height = "500px")
          } else {
            div(class = "alert alert-warning", 
                paste(title, "not available"))
          }
        )
      )
    }
    
    # Get plot description for info display
    get_plot_description <- function(title) {
      switch(title,
        "PK Parameter Boxplots" = "Distribution of pharmacokinetic parameters by treatment group.",
        "Bioequivalence Assessment" = "90% confidence intervals for bioequivalence ratios.",
        "Interactive visualization of analysis results."
      )
    }
    
    # Create individual subjects card with selection controls
    create_individual_subjects_card <- function(plot_data) {
      # Check if we have data available
      if (is.null(plot_data) || !is.null(plot_data$error)) {
        return(div(class = "alert alert-warning",
          "Individual subjects data not available"
        ))
      }
      
      # Get subject choices from the data
      conc_data <- plot_data$data
      test_subjects <- sort(unique(conc_data$Subject[conc_data$Formulation == "Test"]))
      ref_subjects <- sort(unique(conc_data$Subject[conc_data$Formulation == "Reference"]))
      
      div(class = "plot-card",
        div(class = "plot-card-header",
          icon("user"), "Individual Subject Profiles"
        ),
        div(class = "plot-card-body",
          div(class = "mb-3",
            p("Select specific subjects to view individual concentration-time profiles.",
              style = "color: var(--neutral-600); font-size: 14px; margin-bottom: 15px;")
          ),
          
          # Subject selection controls
          div(class = "subject-selection-controls", style = "margin-bottom: 20px;",
            fluidRow(
              column(6,
                h6("Test Formulation Subjects", style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
                selectInput(ns("test_subjects_select"), 
                           label = NULL,
                           choices = test_subjects,
                           selected = head(test_subjects, 3),
                           multiple = TRUE,
                           width = "100%"),
                checkboxInput(ns("select_all_test"), "Select All Test", value = FALSE)
              ),
              column(6,
                h6("Reference Formulation Subjects", style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
                selectInput(ns("ref_subjects_select"), 
                           label = NULL,
                           choices = ref_subjects,
                           selected = head(ref_subjects, 3),
                           multiple = TRUE,
                           width = "100%"),
                checkboxInput(ns("select_all_ref"), "Select All Reference", value = FALSE)
              )
            ),
            div(style = "text-align: center; margin-top: 15px;",
              actionButton(ns("update_individual_plot"), "Update Plot", 
                          class = "btn btn-primary", icon = icon("sync-alt"))
            )
          ),
          
          # Plot output
          div(id = ns("individual_subjects_plot_container"),
            plotlyOutput(ns("individual_subjects_plot"), height = "500px")
          )
        )
      )
    }
    
    # Create cumulative bioequivalence plot card with parameter selection and subject ordering controls
    create_cumulative_plot_card <- function(plot_data) {
      # Check if we have data available
      if (is.null(plot_data) || !is.null(plot_data$error)) {
        return(div(class = "alert alert-warning",
          ifelse(is.null(plot_data), "Cumulative bioequivalence data not available", plot_data$error)
        ))
      }
      
      # Check if study design supports cumulative analysis
      if (!is.null(plot_data$study_design) && plot_data$study_design == "parallel") {
        return(div(class = "alert alert-info",
          icon("info-circle"), " Cumulative analysis is not supported for parallel study designs. ",
          "This analysis requires crossover or replicate designs where each subject receives both treatments."
        ))
      }
      
      # Extract available parameters and subjects from the setup data
      available_params <- if (!is.null(plot_data$parameters)) {
        plot_data$parameters
      } else {
        c("Cmax", "AUC0t", "AUC0inf")
      }
      
      available_subjects <- if (!is.null(plot_data$available_subjects)) {
        plot_data$available_subjects
      } else {
        1:20  # Fallback
      }
      
      div(class = "plot-card",
        div(class = "plot-card-header",
          icon("line-chart"), "Cumulative Bioequivalence Analysis"
        ),
        div(class = "plot-card-body",
          div(class = "mb-3",
            p("Progressive bioequivalence evaluation as subjects are added sequentially to the analysis. Blue line shows point estimates, black lines show 90% confidence intervals.",
              style = "color: var(--neutral-600); font-size: 14px; margin-bottom: 15px;")
          ),
          
          # Analysis controls
          div(class = "row mb-3",
            column(4,
              h6("PK Parameter", style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
              selectInput(ns("cumulative_parameter"), 
                         label = NULL,
                         choices = setNames(available_params, available_params),
                         selected = available_params[1],
                         width = "100%")
            ),
            column(4,
              h6("Subject Order", style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
              selectInput(ns("subject_order_type"), 
                         label = NULL,
                         choices = list(
                           "Sequential (1, 2, 3...)" = "sequential",
                           "Random Order" = "random",
                           "Custom Order" = "custom"
                         ),
                         selected = "sequential",
                         width = "100%")
            ),
            column(4,
              div(style = "text-align: center; margin-top: 25px;",
                actionButton(ns("run_cumulative_analysis"), "Run Analysis", 
                            class = "btn btn-success", icon = icon("play"))
              )
            )
          ),
          
          # Custom subject order selector (shown when "custom" is selected)
          conditionalPanel(
            condition = paste0("input['", ns("subject_order_type"), "'] == 'custom'"),
            div(class = "row mb-3",
              column(12,
                h6("Custom Subject Order", style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
                p("Drag subjects to reorder, or use the text input below:", 
                  style = "color: var(--neutral-600); font-size: 12px; margin-bottom: 10px;"),
                textInput(ns("custom_subject_order"), 
                         label = "Subject order (comma-separated):",
                         value = paste(available_subjects, collapse = ", "),
                         placeholder = "e.g., 1, 3, 2, 5, 4..."),
                div(class = "text-muted", style = "font-size: 11px;",
                  paste("Available subjects:", paste(available_subjects, collapse = ", "))
                )
              )
            )
          ),
          
          # Analysis status and results
          div(id = ns("cumulative_status"), class = "mb-3"),
          
          # Plot output
          div(id = ns("cumulative_plot_container"),
            plotlyOutput(ns("cumulative_pk_plot"), height = "500px")
          )
        )
      )
    }
    
    # Render individual plot outputs - always available when data exists
    observe({
      req(plot_values$plot_objects)
      
      # Concentration plots
      conc_data <- plot_values$plot_objects$concentration
      if (!is.null(conc_data)) {
        if (!is.null(conc_data$linear$plot) && is.null(conc_data$linear$error)) {
          output$conc_linear_plot <- renderPlotly({
            conc_data$linear$plot
          })
        }
        
        if (!is.null(conc_data$log$plot) && is.null(conc_data$log$error)) {
          output$conc_log_plot <- renderPlotly({
            conc_data$log$plot
          })
        }
      }
      
      # PK boxplots
      pk_data <- plot_values$plot_objects$pk_boxplots
      if (!is.null(pk_data)) {
        if (!is.null(pk_data$plot) && is.null(pk_data$error)) {
          output$pk_parameter_boxplots_plot <- renderPlotly({
            pk_data$plot
          })
        }
      }
      
      # BE confidence intervals
      be_data <- plot_values$plot_objects$be_ci
      if (!is.null(be_data)) {
        if (!is.null(be_data$plot) && is.null(be_data$error)) {
          output$bioequivalence_assessment_plot <- renderPlotly({
            be_data$plot
          })
        }
      }
    })
    
    # Select all observers for individual subjects
    # Handle select all test subjects
    observeEvent(input$select_all_test, {
      req(plot_values$plot_objects$individual_subjects)
      
      individual_data <- plot_values$plot_objects$individual_subjects
      
      if (!is.null(individual_data$error) || is.null(individual_data$data)) {
        return()
      }
      
      conc_data <- individual_data$data
      test_subjects <- unique(conc_data$Subject[conc_data$Formulation == "Test"])
      
      if (input$select_all_test) {
        updateSelectInput(session, "test_subjects_select", selected = test_subjects)
      } else {
        updateSelectInput(session, "test_subjects_select", selected = character(0))
      }
    })
    
    # Handle select all reference subjects
    observeEvent(input$select_all_ref, {
      req(plot_values$plot_objects$individual_subjects)
      
      individual_data <- plot_values$plot_objects$individual_subjects
      
      if (!is.null(individual_data$error) || is.null(individual_data$data)) {
        return()
      }
      
      conc_data <- individual_data$data
      ref_subjects <- unique(conc_data$Subject[conc_data$Formulation == "Reference"])
      
      if (input$select_all_ref) {
        updateSelectInput(session, "ref_subjects_select", selected = ref_subjects)
      } else {
        updateSelectInput(session, "ref_subjects_select", selected = character(0))
      }
    })
    
    # Handle cumulative BE analysis run
    observeEvent(input$run_cumulative_analysis, {
      req(input$cumulative_parameter, input$subject_order_type)
      req(plot_values$plot_objects$cumulative_be)
      
      cumulative_setup <- plot_values$plot_objects$cumulative_be
      
      # Check if setup data is available
      if (is.null(cumulative_setup) || !is.null(cumulative_setup$error)) {
        showNotification("Cumulative analysis setup not available", type = "error")
        return()
      }
      
      # Show progress
      output$cumulative_status <- renderUI({
        div(class = "alert alert-info",
          icon("spinner", class = "fa-spin"), " Running progressive bioequivalence analysis..."
        )
      })
      
      tryCatch({
        # Determine subject order
        available_subjects <- cumulative_setup$available_subjects
        
        subject_order <- switch(input$subject_order_type,
          "sequential" = sort(available_subjects),
          "random" = sample(available_subjects),
          "custom" = {
            if (!is.null(input$custom_subject_order) && nchar(input$custom_subject_order) > 0) {
              # Parse custom order
              custom_order <- trimws(strsplit(input$custom_subject_order, ",")[[1]])
              custom_order <- as.numeric(custom_order[!is.na(as.numeric(custom_order))])
              # Validate subjects exist
              valid_subjects <- intersect(custom_order, available_subjects)
              if (length(valid_subjects) > 0) valid_subjects else sort(available_subjects)
            } else {
              sort(available_subjects)
            }
          }
        )
        
        cat(sprintf("Running cumulative analysis for %s with subject order: %s\n", 
                   input$cumulative_parameter, paste(subject_order, collapse = ", ")))
        
        # Perform progressive analysis
        progressive_results <- perform_progressive_be_analysis(
          data = cumulative_setup$analysis_data,
          parameter = input$cumulative_parameter,
          subject_order = subject_order,
          anova_method = cumulative_setup$anova_method,
          be_limits = cumulative_setup$be_limits
        )
        
        if (!is.null(progressive_results) && nrow(progressive_results) > 0) {
          # Create cumulative plot
          cumulative_plot <- create_cumulative_plot(
            cumulative_data = progressive_results,
            parameter = input$cumulative_parameter,
            be_limits = cumulative_setup$be_limits,
            interactive = TRUE
          )
          
          # Update plot output
          output$cumulative_pk_plot <- renderPlotly({
            cumulative_plot
          })
          
          # Show success status with summary
          successful_analyses <- sum(progressive_results$analysis_successful, na.rm = TRUE)
          total_subjects <- nrow(progressive_results)
          
          output$cumulative_status <- renderUI({
            div(class = "alert alert-success",
              icon("check-circle"), 
              sprintf(" Analysis complete: %d/%d successful analyses for %s", 
                     successful_analyses, total_subjects, input$cumulative_parameter)
            )
          })
          
          cat("Cumulative analysis completed successfully\n")
          showNotification("Cumulative analysis completed", type = "message")
          
        } else {
          output$cumulative_status <- renderUI({
            div(class = "alert alert-warning",
              icon("exclamation-triangle"), " No valid results from progressive analysis"
            )
          })
          showNotification("No valid results from analysis", type = "warning")
        }
        
      }, error = function(e) {
        cat("Error in cumulative analysis:", e$message, "\n")
        
        output$cumulative_status <- renderUI({
          div(class = "alert alert-danger",
            icon("exclamation-circle"), " Error: ", e$message
          )
        })
        
        showNotification(paste("Analysis error:", e$message), type = "error")
        
        # Show error in plot area
        output$cumulative_pk_plot <- renderPlotly({
          plot_ly() %>% 
            add_annotations(
              text = paste("Error:", e$message),
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 16, color = "red")
            ) %>%
            layout(
              xaxis = list(showticklabels = FALSE, showgrid = FALSE),
              yaxis = list(showticklabels = FALSE, showgrid = FALSE)
            )
        })
      })
    })
    
    # Individual subjects plot update handler
    observeEvent(input$update_individual_plot, {
      req(plot_values$plot_objects$individual_subjects)
      req(input$test_subjects_select, input$ref_subjects_select)
      
      individual_data <- plot_values$plot_objects$individual_subjects
      
      if (!is.null(individual_data$error) || is.null(individual_data$data)) {
        showNotification("Individual subjects data not available", type = "error")
        return()
      }
      
      # Get selected subjects
      selected_test <- input$test_subjects_select
      selected_ref <- input$ref_subjects_select
      
      if (length(selected_test) == 0 && length(selected_ref) == 0) {
        showNotification("Please select at least one subject", type = "warning")
        return()
      }
      
      # Generate individual subjects plot
      tryCatch({
        conc_data <- individual_data$data
        
        # Filter data based on which formulation dropdown was used
        plot_data <- data.frame()
        
        # Add test formulation data for selected test subjects
        if (length(selected_test) > 0) {
          test_data <- conc_data[conc_data$Subject %in% selected_test & conc_data$Formulation == "Test", ]
          plot_data <- rbind(plot_data, test_data)
        }
        
        # Add reference formulation data for selected reference subjects  
        if (length(selected_ref) > 0) {
          ref_data <- conc_data[conc_data$Subject %in% selected_ref & conc_data$Formulation == "Reference", ]
          plot_data <- rbind(plot_data, ref_data)
        }
        
        if (nrow(plot_data) == 0) {
          showNotification("No data available for selected subjects", type = "warning")
          return()
        }
        
        # Get all selected subjects for the plot function
        all_selected_subjects <- c(selected_test, selected_ref)
        
        # Create individual subject plot (linear scale only for this view)
        plot_obj <- create_individual_concentration_plot(plot_data, all_selected_subjects, log_scale = FALSE)
        
        output$individual_subjects_plot <- renderPlotly({
          plot_obj
        })
        
        cat("Individual subjects plot updated:\n")
        cat("  Test subjects:", paste(selected_test, collapse = ", "), "\n")
        cat("  Reference subjects:", paste(selected_ref, collapse = ", "), "\n")
        
      }, error = function(e) {
        cat("Error creating individual subjects plot:", e$message, "\n")
        showNotification(paste("Error creating plot:", e$message), type = "error")
      })
    })
    
    # Cleanup temp directory on session end
    session$onSessionEnded(function() {
      if (!is.null(temp_dir_path) && dir.exists(temp_dir_path)) {
        unlink(temp_dir_path, recursive = TRUE)
        cat("Cleaned up temp directory:", temp_dir_path, "\n")
      }
    })
  })
}
