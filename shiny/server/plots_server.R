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

# Source plotting functions (.BIOEQ_R_DIR is set in shiny/app.R)
source(file.path(.BIOEQ_R_DIR, "plotting.R"), local = TRUE)
source(file.path(.BIOEQ_R_DIR, "cumulative_be_analysis.R"), local = TRUE)

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
            "Treatment" = c("treatment", "Treatment", "TREATMENT", "formulation", "Treatment", "FORMULATION", "trt", "Trt")
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
          if ("Treatment" %in% names(standardized_data)) {
            cat("Original formulation values:", paste(unique(standardized_data$Treatment), collapse = ", "), "\n")
            standardized_data$Treatment <- ifelse(
              standardized_data$Treatment == "T", "Test",
              ifelse(standardized_data$Treatment == "R", "Reference",
                     standardized_data$Treatment)
            )
            cat("Mapped formulation values:", paste(unique(standardized_data$Treatment), collapse = ", "), "\n")
          }

          # For replicate designs (>2 periods): create TreatmentPeriod label (T1/T2/R1/R2)
          # Uses per-sequence period ranking so e.g. RTRT period-4 T is always T2
          if ("Period" %in% names(standardized_data) &&
              length(unique(standardized_data$Period)) > 2) {
            cat("Replicate design: creating TreatmentPeriod labels...\n")
            if ("Sequence" %in% names(standardized_data)) {
              tp_map <- standardized_data %>%
                dplyr::distinct(Sequence, Treatment, Period) %>%
                dplyr::arrange(Sequence, Treatment, as.numeric(Period)) %>%
                dplyr::group_by(Sequence, Treatment) %>%
                dplyr::mutate(rep_num = dplyr::row_number()) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(TreatmentPeriod = paste0(
                  ifelse(Treatment == "Test", "T", "R"), rep_num
                )) %>%
                dplyr::select(Sequence, Treatment, Period, TreatmentPeriod)
              standardized_data <- dplyr::left_join(
                standardized_data, tp_map,
                by = c("Sequence", "Treatment", "Period")
              )
            } else {
              tp_map <- standardized_data %>%
                dplyr::distinct(Treatment, Period) %>%
                dplyr::arrange(Treatment, as.numeric(Period)) %>%
                dplyr::group_by(Treatment) %>%
                dplyr::mutate(rep_num = dplyr::row_number()) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(TreatmentPeriod = paste0(
                  ifelse(Treatment == "Test", "T", "R"), rep_num
                )) %>%
                dplyr::select(Treatment, Period, TreatmentPeriod)
              standardized_data <- dplyr::left_join(
                standardized_data, tp_map,
                by = c("Treatment", "Period")
              )
            }
            cat("TreatmentPeriod values:",
                paste(sort(unique(standardized_data$TreatmentPeriod)), collapse = ", "), "\n")
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
        
        # Store data with replicate flag for the individual-subject plot UI
        plot_objects$individual_subjects <- list(
          data         = standardized_data,
          is_replicate = "TreatmentPeriod" %in% names(standardized_data),
          error        = NULL
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
            dplyr::rename(Treatment = treatment)
        } else {
          pk_data <- nca_results()
          # Add formulation column if missing
          if (!"Treatment" %in% names(pk_data) && "treatment" %in% names(pk_data)) {
            pk_data <- pk_data %>% dplyr::rename(Treatment = treatment)
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
        nca_data <- nca_results()
        
        # Get the selected ANOVA method and PK parameters
        anova_method <- if (!is.null(config) && !is.null(config$anova_model)) {
          config$anova_model
        } else {
          "fixed"
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
        
        # Try different data sources for cumulative analysis
        pk_data <- NULL
        
        # For ABEL analysis, use NCA results directly (they contain Subject, Treatment, Period, PK params)
        if (!is.null(nca_data)) {
          if ("subject_data" %in% names(nca_data)) {
            pk_data <- nca_data$subject_data
            cat("Using subject_data from NCA results\n")
          } else if ("nca_results" %in% names(nca_data)) {
            pk_data <- nca_data$nca_results
            cat("Using nca_results from NCA results\n")
          } else if (is.data.frame(nca_data)) {
            pk_data <- nca_data
            cat("Using NCA results data frame directly\n")
          }
        }
        
        # Fallback: try BE results (for older analysis types)
        if (is.null(pk_data) && !is.null(be_data)) {
          cat("BE results structure:", paste(names(be_data), collapse = ", "), "\n")
          
          if ("merged_data" %in% names(be_data)) {
            pk_data <- be_data$merged_data
            cat("Using merged_data from BE results\n")
          } else if ("pk_parameters" %in% names(be_data)) {
            pk_data <- be_data$pk_parameters
            cat("Using pk_parameters from BE results\n")
          } else if ("data" %in% names(be_data)) {
            pk_data <- be_data$data
            cat("Using data from BE results\n")
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
      # Check if PK parameters data was uploaded (not concentration-time data)
      if (!is.null(validation_result()) && !is.null(validation_result()$data_type)) {
        if (validation_result()$data_type == "pk_parameters") {
          return(div(class = "alert alert-warning text-center",
            icon("exclamation-triangle"), 
            strong(" Concentration-Time Data Not Available"), br(),
            "Only PK parameters were uploaded. Concentration-time profiles require raw concentration-time data."
          ))
        }
      }
      
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
    
    # Individual T/R Ratio Plot Tab
    output$be_ci_display <- renderUI({
      req(plot_values$plot_objects)
      
      # Get available parameters from actual data
      available_params <- if (!is.null(plot_values$plot_objects$cumulative_be$analysis_data)) {
        # Get PK parameter columns from the data
        data_cols <- names(plot_values$plot_objects$cumulative_be$analysis_data)
        pk_params <- intersect(data_cols, c("AUC0t", "AUC0inf", "Cmax", "Tmax"))
        if (length(pk_params) > 0) pk_params else c("Cmax")  # Fallback to Cmax
      } else if (!is.null(plot_values$available_parameters)) {
        plot_values$available_parameters
      } else {
        c("AUC0t", "AUC0inf", "Cmax")  # Fallback
      }
      
      available_subjects <- if (!is.null(plot_values$plot_objects$concentration$available_subjects)) {
        plot_values$plot_objects$concentration$available_subjects
      } else {
        1:20  # Fallback
      }
      
      div(class = "plot-card",
        div(class = "plot-card-body",
          div(class = "mb-3",
            p("Individual subject Test/Reference ratios for bioequivalence analysis. Each point represents one subject's T/R ratio. Red dashed lines show bioequivalence limits (80-125%).",
              style = "color: var(--neutral-600); font-size: 14px; margin-bottom: 15px;")
          ),
          
          # Analysis controls
          div(class = "row mb-3",
            column(4,
              h6("PK Parameter", style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
              selectInput(ns("individual_tr_parameter"), 
                         label = NULL,
                         choices = setNames(available_params, available_params),
                         selected = available_params[1],
                         width = "100%")
            ),
            column(4,
              h6("Subject Order", style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
              selectInput(ns("individual_tr_subject_order"), 
                         label = NULL,
                         choices = list(
                           "Sequential (1, 2, 3...)" = "sequential",
                           "Custom Order" = "custom"
                         ),
                         selected = "sequential",
                         width = "100%")
            ),
            column(4,
              div(style = "text-align: center; margin-top: 25px;",
                actionButton(ns("run_individual_tr_analysis"), "Update Plot", 
                            class = "btn btn-primary", icon = icon("sync-alt"))
              )
            )
          ),
          
          # Custom subject order selector (shown when "custom" is selected)
          conditionalPanel(
            condition = paste0("input['", ns("individual_tr_subject_order"), "'] == 'custom'"),
            div(class = "row mb-3",
              column(12,
                h6("Custom Subject Order", style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
                p("Drag subjects to reorder, or use the text input below:", 
                  style = "color: var(--neutral-600); font-size: 12px; margin-bottom: 10px;"),
                textInput(ns("individual_tr_custom_order"), 
                         label = "Subject order (comma-separated):",
                         value = paste(available_subjects, collapse = ", "),
                         placeholder = "e.g., 1, 3, 2, 5, 4..."),
                div(class = "text-muted", style = "font-size: 11px;",
                  paste("Available subjects:", paste(available_subjects, collapse = ", "))
                )
              )
            )
          ),
          
          # Plot output
          div(id = ns("individual_tr_plot_container"),
            plotlyOutput(ns("individual_tr_plot"), height = "500px")
          )
        )
      )
    })
    
    # Individual Subjects Plot Tab
    output$individual_subjects_display <- renderUI({
      # Check if PK parameters data was uploaded (not concentration-time data)
      if (!is.null(validation_result()) && !is.null(validation_result()$data_type)) {
        if (validation_result()$data_type == "pk_parameters") {
          return(div(class = "alert alert-warning text-center",
            icon("exclamation-triangle"), 
            strong(" Concentration-Time Data Not Available"), br(),
            "Only PK parameters were uploaded. Individual subject profiles require raw concentration-time data."
          ))
        }
      }
      
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
    
    # Create individual subjects card with selection controls
    # Replicate designs get per-period selectors (T1/T2/R1/R2);
    # 2x2x2 keeps the original Test/Reference selectors.
    create_individual_subjects_card <- function(plot_data) {
      if (is.null(plot_data) || !is.null(plot_data$error)) {
        return(div(class = "alert alert-warning",
          "Individual subjects data not available"
        ))
      }

      conc_data    <- plot_data$data
      is_replicate <- isTRUE(plot_data$is_replicate) &&
                      "TreatmentPeriod" %in% names(conc_data)

      get_subj_for_tp <- function(tp) {
        sort(as.numeric(unique(
          conc_data$Subject[!is.na(conc_data$TreatmentPeriod) &
                            conc_data$TreatmentPeriod == tp]
        )))
      }

      if (is_replicate) {
        t1_subj <- get_subj_for_tp("T1")
        t2_subj <- get_subj_for_tp("T2")
        r1_subj <- get_subj_for_tp("R1")
        r2_subj <- get_subj_for_tp("R2")
        has_t2  <- length(t2_subj) > 0
        has_r2  <- length(r2_subj) > 0

        # Build selector columns depending on whether T2/R2 exist
        t_selectors <- if (has_t2) {
          tagList(
            column(6,
              h6("T1 Subjects",
                 style = "color: #1F78B4; font-weight: 600; margin-bottom: 5px;"),
              selectInput(ns("t1_subjects_select"), NULL,
                          choices = t1_subj, selected = head(t1_subj, 3),
                          multiple = TRUE, width = "100%"),
              checkboxInput(ns("select_all_t1"), "Select All T1", value = FALSE)
            ),
            column(6,
              h6("T2 Subjects",
                 style = "color: #A6CEE3; font-weight: 600; margin-bottom: 5px;"),
              selectInput(ns("t2_subjects_select"), NULL,
                          choices = t2_subj, selected = head(t2_subj, 3),
                          multiple = TRUE, width = "100%"),
              checkboxInput(ns("select_all_t2"), "Select All T2", value = FALSE)
            )
          )
        } else {
          column(12,
            h6("T Subjects",
               style = "color: #1F78B4; font-weight: 600; margin-bottom: 5px;"),
            selectInput(ns("t1_subjects_select"), NULL,
                        choices = t1_subj, selected = head(t1_subj, 3),
                        multiple = TRUE, width = "100%"),
            checkboxInput(ns("select_all_t1"), "Select All T", value = FALSE)
          )
        }

        r_selectors <- if (has_r2) {
          tagList(
            column(6,
              h6("R1 Subjects",
                 style = "color: #E31A1C; font-weight: 600; margin-bottom: 5px;"),
              selectInput(ns("r1_subjects_select"), NULL,
                          choices = r1_subj, selected = head(r1_subj, 3),
                          multiple = TRUE, width = "100%"),
              checkboxInput(ns("select_all_r1"), "Select All R1", value = FALSE)
            ),
            column(6,
              h6("R2 Subjects",
                 style = "color: #FB9A99; font-weight: 600; margin-bottom: 5px;"),
              selectInput(ns("r2_subjects_select"), NULL,
                          choices = r2_subj, selected = head(r2_subj, 3),
                          multiple = TRUE, width = "100%"),
              checkboxInput(ns("select_all_r2"), "Select All R2", value = FALSE)
            )
          )
        } else {
          column(12,
            h6("R Subjects",
               style = "color: #E31A1C; font-weight: 600; margin-bottom: 5px;"),
            selectInput(ns("r1_subjects_select"), NULL,
                        choices = r1_subj, selected = head(r1_subj, 3),
                        multiple = TRUE, width = "100%"),
            checkboxInput(ns("select_all_r1"), "Select All R", value = FALSE)
          )
        }

        div(class = "plot-card",
          div(class = "plot-card-body",
            div(class = "mb-3",
              p("Select subjects by replicate period to view individual concentration-time profiles.",
                style = "color: var(--neutral-600); font-size: 14px; margin-bottom: 15px;")
            ),
            div(class = "subject-selection-controls", style = "margin-bottom: 20px;",
              fluidRow(t_selectors),
              fluidRow(r_selectors),
              div(style = "text-align: center; margin-top: 15px;",
                actionButton(ns("update_individual_plot"), "Update Plot",
                            class = "btn btn-primary", icon = icon("sync-alt"))
              )
            ),
            div(id = ns("individual_subjects_plot_container"),
              plotlyOutput(ns("individual_subjects_plot"), height = "500px")
            )
          )
        )

      } else {
        # 2x2x2 crossover: original Test / Reference selectors
        test_subjects <- sort(as.numeric(unique(
          conc_data$Subject[conc_data$Treatment == "Test"])))
        ref_subjects  <- sort(as.numeric(unique(
          conc_data$Subject[conc_data$Treatment == "Reference"])))

        div(class = "plot-card",
          div(class = "plot-card-body",
            div(class = "mb-3",
              p("Select specific subjects to view individual concentration-time profiles.",
                style = "color: var(--neutral-600); font-size: 14px; margin-bottom: 15px;")
            ),
            div(class = "subject-selection-controls", style = "margin-bottom: 20px;",
              fluidRow(
                column(6,
                  h6("Test Treatment Subjects",
                     style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
                  selectInput(ns("test_subjects_select"), NULL,
                             choices = test_subjects, selected = head(test_subjects, 3),
                             multiple = TRUE, width = "100%"),
                  checkboxInput(ns("select_all_test"), "Select All Test", value = FALSE)
                ),
                column(6,
                  h6("Reference Treatment Subjects",
                     style = "color: var(--navy-primary); font-weight: 600; margin-bottom: 10px;"),
                  selectInput(ns("ref_subjects_select"), NULL,
                             choices = ref_subjects, selected = head(ref_subjects, 3),
                             multiple = TRUE, width = "100%"),
                  checkboxInput(ns("select_all_ref"), "Select All Reference", value = FALSE)
                )
              ),
              div(style = "text-align: center; margin-top: 15px;",
                actionButton(ns("update_individual_plot"), "Update Plot",
                            class = "btn btn-primary", icon = icon("sync-alt"))
              )
            ),
            div(id = ns("individual_subjects_plot_container"),
              plotlyOutput(ns("individual_subjects_plot"), height = "500px")
            )
          )
        )
      }
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
      
      # Extract available parameters from actual data
      available_params <- if (!is.null(plot_data$analysis_data)) {
        # Get PK parameter columns from the data
        data_cols <- names(plot_data$analysis_data)
        pk_params <- intersect(data_cols, c("AUC0t", "AUC0inf", "Cmax", "Tmax"))
        if (length(pk_params) > 0) pk_params else c("Cmax")  # Fallback to Cmax
      } else if (!is.null(plot_data$parameters)) {
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
                actionButton(ns("run_cumulative_analysis"), "Update Plot", 
                            class = "btn btn-primary", icon = icon("sync-alt"))
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
      
    })
    
    # Auto-generate individual T/R ratio plot when data becomes available
    observe({
      req(plot_values$plot_objects$cumulative_be)
      
      cumulative_setup <- plot_values$plot_objects$cumulative_be
      
      # Only proceed if data is available and no error
      if (!is.null(cumulative_setup) && is.null(cumulative_setup$error)) {
        # Get default parameter (first available)
        available_params <- if (!is.null(plot_values$available_parameters)) {
          plot_values$available_parameters
        } else {
          c("AUC0t", "AUC0inf", "Cmax")
        }
        
        default_param <- available_params[1]
        default_order <- sort(cumulative_setup$available_subjects)
        
        tryCatch({
          # Create initial simple T/R ratio plot
          tr_plot <- create_simple_tr_plot(
            data = cumulative_setup$analysis_data,
            parameter = default_param,
            subject_order = default_order
          )
          
          # Convert to plotly with custom hover
          interactive_plot <- plotly::ggplotly(tr_plot, tooltip = "text") %>%
            plotly::layout(height = 500)
          
          # Render initial plot
          output$individual_tr_plot <- renderPlotly({
            interactive_plot
          })
          
          cat("Initial simple T/R ratio plot created for", default_param, "\n")
          
        }, error = function(e) {
          cat("Error creating initial individual T/R ratio plot:", e$message, "\n")
        })
      }
    })
    
    # Auto-generate cumulative T/R ratio plot when data becomes available
    observe({
      req(plot_values$plot_objects$cumulative_be)
      
      cumulative_setup <- plot_values$plot_objects$cumulative_be
      
      # Only proceed if data is available and no error
      if (!is.null(cumulative_setup) && is.null(cumulative_setup$error)) {
        # Get default parameter (first available)
        available_params <- if (!is.null(plot_values$available_parameters)) {
          plot_values$available_parameters
        } else {
          c("AUC0t", "AUC0inf", "Cmax")
        }
        
        default_param <- available_params[1]
        default_order <- sort(cumulative_setup$available_subjects)
        
        tryCatch({
          # Perform progressive analysis with default settings
          progressive_results <- perform_progressive_be_analysis(
            data = cumulative_setup$analysis_data,
            parameter = default_param,
            subject_order = default_order,
            anova_method = cumulative_setup$anova_method,
            be_limits = cumulative_setup$be_limits
          )
          
          if (!is.null(progressive_results) && nrow(progressive_results) > 0) {
            # Create cumulative plot
            cumulative_plot <- create_cumulative_plot(
              cumulative_data = progressive_results,
              parameter = default_param,
              be_limits = cumulative_setup$be_limits,
              interactive = TRUE
            )
            
            # Update plot output
            output$cumulative_pk_plot <- renderPlotly({
              cumulative_plot
            })
            
            cat("Initial cumulative T/R ratio plot created for", default_param, "\n")
          }
          
        }, error = function(e) {
          cat("Error creating initial cumulative T/R ratio plot:", e$message, "\n")
        })
      }
    })

    # Select all observers for individual subjects
    # Handle select all test subjects
    observeEvent(input$select_all_test, {
      req(plot_values$plot_objects$individual_subjects)
      individual_data <- plot_values$plot_objects$individual_subjects
      if (!is.null(individual_data$error) || is.null(individual_data$data)) return()
      conc_data <- individual_data$data
      test_subjects <- unique(conc_data$Subject[conc_data$Treatment == "Test"])
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
      if (!is.null(individual_data$error) || is.null(individual_data$data)) return()
      conc_data <- individual_data$data
      ref_subjects <- unique(conc_data$Subject[conc_data$Treatment == "Reference"])
      if (input$select_all_ref) {
        updateSelectInput(session, "ref_subjects_select", selected = ref_subjects)
      } else {
        updateSelectInput(session, "ref_subjects_select", selected = character(0))
      }
    })

    # Handle select-all for replicate period selectors (T1/T2/R1/R2)
    for (.tp_lbl in c("t1", "t2", "r1", "r2")) {
      local({
        tp_lbl <- .tp_lbl
        tp     <- toupper(tp_lbl)
        observeEvent(input[[paste0("select_all_", tp_lbl)]], {
          individual_data <- plot_values$plot_objects$individual_subjects
          if (is.null(individual_data) || !is.null(individual_data$error) ||
              is.null(individual_data$data)) return()
          conc_data <- individual_data$data
          if (!"TreatmentPeriod" %in% names(conc_data)) return()
          tp_subj <- unique(conc_data$Subject[
            !is.na(conc_data$TreatmentPeriod) & conc_data$TreatmentPeriod == tp])
          if (isTRUE(input[[paste0("select_all_", tp_lbl)]])) {
            updateSelectInput(session, paste0(tp_lbl, "_subjects_select"),
                              selected = tp_subj)
          } else {
            updateSelectInput(session, paste0(tp_lbl, "_subjects_select"),
                              selected = character(0))
          }
        }, ignoreNULL = TRUE)
      })
    }
    
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
    
    # Handle individual T/R ratio analysis
    observeEvent(input$run_individual_tr_analysis, {
      req(input$individual_tr_parameter, input$individual_tr_subject_order)
      req(plot_values$plot_objects$cumulative_be)  # Use same data as cumulative
      
      cumulative_setup <- plot_values$plot_objects$cumulative_be
      
      # Check if setup data is available
      if (is.null(cumulative_setup) || !is.null(cumulative_setup$error)) {
        showNotification("Analysis data not available for individual T/R ratios", type = "error")
        return()
      }
      
      tryCatch({
        # Determine subject order
        available_subjects <- cumulative_setup$available_subjects
        
        subject_order <- switch(input$individual_tr_subject_order,
          "sequential" = sort(available_subjects),
          "custom" = {
            if (!is.null(input$individual_tr_custom_order) && nchar(input$individual_tr_custom_order) > 0) {
              # Parse custom order
              custom_order <- trimws(strsplit(input$individual_tr_custom_order, ",")[[1]])
              custom_order <- as.numeric(custom_order[!is.na(as.numeric(custom_order))])
              # Validate subjects exist
              valid_subjects <- intersect(custom_order, available_subjects)
              if (length(valid_subjects) > 0) valid_subjects else sort(available_subjects)
            } else {
              sort(available_subjects)
            }
          }
        )
        
        cat(sprintf("Creating simple T/R ratio plot for %s with subject order: %s\n", 
                   input$individual_tr_parameter, paste(subject_order, collapse = ", ")))
        
        # Create simple T/R ratio plot
        tr_plot <- create_simple_tr_plot(
          data = cumulative_setup$analysis_data,
          parameter = input$individual_tr_parameter,
          subject_order = subject_order
        )
        
        # Convert to plotly for interactivity with custom hover
        interactive_plot <- plotly::ggplotly(tr_plot, tooltip = "text") %>%
          plotly::layout(height = 500)
        
        # Update plot output
        output$individual_tr_plot <- renderPlotly({
          interactive_plot
        })
        
        cat("Individual T/R ratio plot updated successfully\n")
        showNotification("Individual T/R ratio plot updated", type = "message")
        
      }, error = function(e) {
        cat("Error creating individual T/R ratio plot:", e$message, "\n")
        
        showNotification(paste("Plot error:", e$message), type = "error")
        
        # Show error in plot area
        output$individual_tr_plot <- renderPlotly({
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

      individual_data <- plot_values$plot_objects$individual_subjects

      if (!is.null(individual_data$error) || is.null(individual_data$data)) {
        showNotification("Individual subjects data not available", type = "error")
        return()
      }

      conc_data    <- individual_data$data
      is_replicate <- isTRUE(individual_data$is_replicate) &&
                      "TreatmentPeriod" %in% names(conc_data)

      tryCatch({
        if (is_replicate) {
          # Collect rows selected for each replicate period
          pieces <- list()
          if (length(input$t1_subjects_select) > 0)
            pieces[["T1"]] <- conc_data[
              !is.na(conc_data$TreatmentPeriod) &
              conc_data$Subject %in% input$t1_subjects_select &
              conc_data$TreatmentPeriod == "T1", ]
          if (length(input$t2_subjects_select) > 0)
            pieces[["T2"]] <- conc_data[
              !is.na(conc_data$TreatmentPeriod) &
              conc_data$Subject %in% input$t2_subjects_select &
              conc_data$TreatmentPeriod == "T2", ]
          if (length(input$r1_subjects_select) > 0)
            pieces[["R1"]] <- conc_data[
              !is.na(conc_data$TreatmentPeriod) &
              conc_data$Subject %in% input$r1_subjects_select &
              conc_data$TreatmentPeriod == "R1", ]
          if (length(input$r2_subjects_select) > 0)
            pieces[["R2"]] <- conc_data[
              !is.na(conc_data$TreatmentPeriod) &
              conc_data$Subject %in% input$r2_subjects_select &
              conc_data$TreatmentPeriod == "R2", ]

          plot_data <- dplyr::bind_rows(pieces)
        } else {
          # 2x2x2 crossover
          selected_test <- input$test_subjects_select
          selected_ref  <- input$ref_subjects_select
          pieces <- list()
          if (length(selected_test) > 0)
            pieces[["Test"]] <- conc_data[
              conc_data$Subject %in% selected_test &
              conc_data$Treatment == "Test", ]
          if (length(selected_ref) > 0)
            pieces[["Ref"]] <- conc_data[
              conc_data$Subject %in% selected_ref &
              conc_data$Treatment == "Reference", ]
          plot_data <- dplyr::bind_rows(pieces)
        }

        if (is.null(plot_data) || nrow(plot_data) == 0) {
          showNotification("Please select at least one subject", type = "warning")
          return()
        }

        all_selected_subjects <- unique(plot_data$Subject)

        plot_obj <- create_individual_concentration_plot(
          plot_data, all_selected_subjects, log_scale = FALSE)

        output$individual_subjects_plot <- renderPlotly({ plot_obj })

        cat("Individual subjects plot updated for",
            length(all_selected_subjects), "subjects\n")

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
    
    # =======================================================================
    # LAMBDA Z REGRESSION PLOTS TAB
    # =======================================================================
    
    # Method display name lookup
    lambda_z_method_names <- c(
      "manual" = "Manual (Fixed Points)",
      "ars" = "ARS (Adjusted R-Squared)",
      "aic" = "AIC (Akaike Information Criterion)",
      "ttt" = "TTT (Two-Times-Tmax)"
    )
    
    # Reactive: available subjects for lambda z tab
    lambda_z_subjects <- reactive({
      req(nca_results())
      nca_res <- nca_results()
      subject_data <- if (is.data.frame(nca_res)) nca_res else nca_res$subject_data
      req(subject_data)
      subjects <- unique(as.character(subject_data$Subject))
      subjects[order(as.numeric(subjects))]
    })
    
    # Reactive: current page of subjects
    lambda_z_page <- reactiveVal(1)
    lambda_z_per_page <- 8  # profiles per page (2 rows of 4)
    
    output$lambda_z_regression_display <- renderUI({
      # Check if PK parameters data was uploaded (not concentration-time data)
      if (!is.null(validation_result()) && !is.null(validation_result()$data_type)) {
        if (validation_result()$data_type == "pk_parameters") {
          return(div(class = "alert alert-warning text-center",
            icon("exclamation-triangle"), 
            strong(" Concentration-Time Data Not Available"), br(),
            "Lambda Z regression plots require raw concentration-time data."
          ))
        }
      }
      
      req(nca_results())
      nca_res <- nca_results()
      subject_data <- if (is.data.frame(nca_res)) nca_res else nca_res$subject_data
      
      if (is.null(subject_data) || !"lambda_z_terminal_times" %in% names(subject_data)) {
        return(div(class = "alert alert-info text-center",
          icon("info-circle"), " Lambda Z regression plots will appear here once analysis is complete."
        ))
      }
      
      # Get method info
      lz_method_code <- if (!is.null(nca_res$lambda_z_method)) nca_res$lambda_z_method else {
        if ("lambda_z_method" %in% names(subject_data)) subject_data$lambda_z_method[1] else "unknown"
      }
      lz_method_display <- lambda_z_method_names[lz_method_code]
      if (is.na(lz_method_display)) lz_method_display <- lz_method_code
      
      all_subjects <- sort(unique(as.character(subject_data$Subject)))
      
      div(class = "plot-card",
        div(class = "plot-card-body",
          # Method label
          div(style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 3px solid #2166AC;",
            tags$span(style = "font-weight: 600; color: #495057;", 
              paste0("Lambda Z Method: ", lz_method_display)),
            tags$span(style = "color: #6c757d; margin-left: 15px; font-size: 0.9em;",
              icon("circle", style = "color: #D6604D; font-size: 8px;"), " Terminal phase points used",
              "    ",
              icon("minus", style = "color: #2166AC;"), " Regression fit"
            )
          ),
          
          # Subject filter and pagination controls
          fluidRow(
            column(6,
              selectInput(ns("lambda_z_subject_filter"),
                label = "Filter Subjects",
                choices = c("All Subjects" = "all", setNames(all_subjects, paste("Subject", all_subjects))),
                selected = "all",
                width = "100%"
              )
            ),
            column(6,
              div(style = "text-align: right; margin-top: 25px;",
                actionButton(ns("lambda_z_prev_page"), icon("arrow-left"), class = "btn btn-sm btn-default"),
                tags$span(id = ns("lambda_z_page_label"), style = "margin: 0 10px; font-weight: 600;",
                  textOutput(ns("lambda_z_page_text"), inline = TRUE)
                ),
                actionButton(ns("lambda_z_next_page"), icon("arrow-right"), class = "btn btn-sm btn-default")
              )
            )
          ),
          
          # Plot output
          div(style = "margin-top: 10px;",
            plotOutput(ns("lambda_z_regression_plot"), height = "700px")
          )
        )
      )
    })
    
    # Page text
    output$lambda_z_page_text <- renderText({
      req(nca_results())
      nca_res <- nca_results()
      subject_data <- if (is.data.frame(nca_res)) nca_res else nca_res$subject_data
      req(subject_data)
      
      # Determine subjects to show based on filter
      filter_val <- input$lambda_z_subject_filter
      if (is.null(filter_val) || filter_val == "all") {
        all_subjects <- unique(as.character(subject_data$Subject))
        all_subjects <- all_subjects[order(as.numeric(all_subjects))]
      } else {
        all_subjects <- filter_val
      }
      
      # Count profiles (subject x treatment x period)
      n_profiles <- nrow(subject_data[subject_data$Subject %in% all_subjects, ])
      total_pages <- max(1, ceiling(n_profiles / lambda_z_per_page))
      current_page <- min(lambda_z_page(), total_pages)
      
      paste0("Page ", current_page, " of ", total_pages)
    })
    
    # Pagination handlers
    observeEvent(input$lambda_z_prev_page, {
      current <- lambda_z_page()
      if (current > 1) lambda_z_page(current - 1)
    })
    
    observeEvent(input$lambda_z_next_page, {
      req(nca_results())
      nca_res <- nca_results()
      subject_data <- if (is.data.frame(nca_res)) nca_res else nca_res$subject_data
      req(subject_data)
      
      filter_val <- input$lambda_z_subject_filter
      if (is.null(filter_val) || filter_val == "all") {
        n_profiles <- nrow(subject_data)
      } else {
        n_profiles <- nrow(subject_data[subject_data$Subject %in% filter_val, ])
      }
      
      total_pages <- max(1, ceiling(n_profiles / lambda_z_per_page))
      current <- lambda_z_page()
      if (current < total_pages) lambda_z_page(current + 1)
    })
    
    # Reset page when filter changes
    observeEvent(input$lambda_z_subject_filter, {
      lambda_z_page(1)
    })
    
    # Render lambda z regression plots
    output$lambda_z_regression_plot <- renderPlot({
      req(nca_results(), uploaded_data())
      
      nca_res <- nca_results()
      subject_data <- if (is.data.frame(nca_res)) nca_res else nca_res$subject_data
      req(subject_data, "lambda_z_terminal_times" %in% names(subject_data))
      
      conc_data <- uploaded_data()
      
      # Filter by subject if specified
      filter_val <- input$lambda_z_subject_filter
      if (!is.null(filter_val) && filter_val != "all") {
        page_subject_data <- subject_data[subject_data$Subject %in% filter_val, ]
      } else {
        page_subject_data <- subject_data
      }
      
      # Sort by numeric Subject
      page_subject_data <- page_subject_data[order(as.numeric(as.character(page_subject_data$Subject))), ]
      
      # Apply pagination
      n_profiles <- nrow(page_subject_data)
      total_pages <- max(1, ceiling(n_profiles / lambda_z_per_page))
      current_page <- min(lambda_z_page(), total_pages)
      
      start_row <- (current_page - 1) * lambda_z_per_page + 1
      end_row <- min(current_page * lambda_z_per_page, n_profiles)
      
      page_subject_data <- page_subject_data[start_row:end_row, ]
      
      # Get the subjects on this page
      page_subjects <- unique(as.character(page_subject_data$Subject))
      
      # Create the plots
      create_lambda_z_regression_plots(
        conc_data = conc_data,
        nca_subject_data = page_subject_data,
        subjects = page_subjects,
        ncol = 4
      )
    }, res = 96)
  })
}
