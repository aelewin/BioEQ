# Analysis Setup Server Logic
# This file handles analysis configuration and parameter setup

# Source help utilities
source("utils/help_utils.R", local = TRUE)

# Source simple ANOVA functions (.BIOEQ_R_DIR is set in shiny/app.R)
source(file.path(.BIOEQ_R_DIR, "simple_anova.R"), local = TRUE)

# Source RSABE analysis functions
source(file.path(.BIOEQ_R_DIR, "rsabe_analysis.R"), local = TRUE)

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

# Upload status for conditional display
output$upload_status <- reactive({
  !is.null(values$uploaded_data)
})
outputOptions(output, "upload_status", suspendWhenHidden = FALSE)

# Study design detection status for conditional display
output$study_design_detected <- reactive({
  !is.null(values$uploaded_data) && !is.null(input$study_design)
})
outputOptions(output, "study_design_detected", suspendWhenHidden = FALSE)

# Check if current study design is parallel (for disabling carryover assessment)
output$is_parallel_design <- reactive({
  if (is.null(input$study_design)) return(FALSE)
  
  # Check manual selection
  if (input$study_design == "parallel") return(TRUE)
  
  # Check auto-detection
  if (input$study_design == "auto" && !is.null(values$uploaded_data)) {
    tryCatch({
      data <- values$uploaded_data
      # Use capitalized column names
      treatments_per_subject <- data %>%
        group_by(Subject) %>%
        summarise(n_treatments = length(unique(Treatment)), .groups = "drop")
      # Crossover/replicate if ANY subject has >1 treatment (robust to missing periods)
      is_crossover <- max(treatments_per_subject$n_treatments, na.rm = TRUE) >= 2
      return(!is_crossover)  # Return TRUE if NOT crossover (i.e., parallel)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  return(FALSE)
})
outputOptions(output, "is_parallel_design", suspendWhenHidden = FALSE)

# Check if the current design is NOT a replicate (for RSABE/ABEL compatibility warnings)
output$is_non_replicate_design <- reactive({
  design <- input$study_design
  if (is.null(design)) return(TRUE)
  
  # If explicitly selected as replicate, it's fine
  if (design %in% c("2x2x3", "2x2x4")) return(FALSE)
  
  # If parallel or 2x2x2, it's non-replicate
  if (design %in% c("parallel", "2x2x2")) return(TRUE)
  
  # If auto-detect, check from data
  if (design == "auto" && !is.null(values$uploaded_data)) {
    tryCatch({
      data <- values$uploaded_data
      treatments_per_subject <- tapply(data$Treatment, data$Subject, function(x) length(unique(x)))
      # Crossover/replicate if ANY subject has >1 treatment (robust to missing periods)
      is_crossover <- max(treatments_per_subject, na.rm = TRUE) >= 2

      if (!is_crossover) return(TRUE)  # parallel
      
      # Check periods to distinguish 2x2x2 from replicate
      if ("Period" %in% names(data)) {
        n_periods <- length(unique(data$Period))
        if (n_periods <= 2) return(TRUE)  # 2x2x2
        return(FALSE)  # replicate (3+ periods)
      }
      return(TRUE)  # conservative: assume non-replicate if no Period info
    }, error = function(e) {
      return(TRUE)
    })
  }
  
  return(TRUE)
})
outputOptions(output, "is_non_replicate_design", suspendWhenHidden = FALSE)

# F5: Drive the PK-parameter selection from the data actually uploaded.
# For pre-calculated PK data the only analysable parameters are those the user
# mapped, so restrict both the choices and the selection to those (prevents
# e.g. a Cmax-only dataset from having AUC0-t pre-checked). For concentration-
# time data the NCA generates the full standard set, so the static defaults
# (Cmax + AUC0-t) are left untouched.
observeEvent(list(values$uploaded_data, values$data_type, values$pk_parameter_info), {
  if (is.null(values$uploaded_data)) return()
  if (!identical(values$data_type, "pk_parameters")) return()

  primary_choices   <- c("Cmax" = "Cmax", "AUC0-t" = "AUC0t")
  secondary_choices <- c("AUC0-inf" = "AUC0inf", "pAUC" = "pAUC", "Tmax" = "Tmax")

  # Available parameters = mapped PK params, falling back to matching columns.
  avail <- names(values$pk_parameter_info %||% list())
  if (length(avail) == 0) {
    avail <- intersect(c(primary_choices, secondary_choices), names(values$uploaded_data))
  }

  p_avail <- primary_choices[primary_choices %in% avail]
  s_avail <- secondary_choices[secondary_choices %in% avail]

  updateCheckboxGroupInput(session, "primary_pk_params",
                           choices = p_avail, selected = unname(p_avail))
  updateCheckboxGroupInput(session, "secondary_pk_params",
                           choices = s_avail, selected = character(0))
}, ignoreInit = TRUE)

# Check if groups are detected in the data
output$groups_detected <- reactive({
  if (is.null(values$uploaded_data)) return(FALSE)
  
  tryCatch({
    data <- values$uploaded_data
    has_group <- "group" %in% names(data)
    
    if (has_group) {
      # Check if there are actually multiple groups
      n_groups <- length(unique(data$group[!is.na(data$group)]))
      return(n_groups > 1)
    }
    
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  })
})
outputOptions(output, "groups_detected", suspendWhenHidden = FALSE)

# Observer for BE analysis type selection - show notifications for RSABE/ABEL
# Detected design output
output$detected_design <- renderUI({
  req(values$uploaded_data)
  
  tryCatch({
    data <- values$uploaded_data
    
    # Verify required columns exist
    if (!all(c("Subject", "Treatment") %in% names(data))) {
      return(p("Unable to detect design - missing required columns", 
               style = "color: #dc3545;"))
    }
    
    # Use capitalized column names
    n_treatments <- length(unique(data$Treatment))
    
    # Safely calculate treatments per subject
    treatments_per_subject <- tryCatch({
      data %>%
        group_by(Subject) %>%
        summarise(n_treatments = length(unique(Treatment)), .groups = "drop")
    }, error = function(e) {
      # Fallback if dplyr fails
      aggregate(Treatment ~ Subject, data = data, 
                FUN = function(x) length(unique(x)))
    })
    
    # A design is crossover/replicate if ANY subject receives more than one
    # treatment. Using max(...) >= 2 (rather than requiring EVERY subject to have
    # the full treatment set) is robust to subjects with missing periods, which
    # otherwise cause a replicate study to be misclassified as parallel.
    is_crossover <- isTRUE(max(treatments_per_subject$n_treatments, na.rm = TRUE) >= 2)

    # Detect replicate design using the BE analysis function
    replicate_result <- NULL
    if (isTRUE(is_crossover) && all(c("Subject", "Period", "Treatment") %in% names(data))) {
      replicate_result <- tryCatch({
        detect_replicate_design(data)
      }, error = function(e) {
        bioeq_log(sprintf("detect_replicate_design failed: %s", e$message), "WARNING")
        NULL
      })
    }

    # Use period count as authoritative fallback — works even when detect_replicate_design
    # fails or returns is_replicate=FALSE (e.g. on concentration-time data with many rows)
    n_p_data <- if ("Period" %in% names(data)) length(unique(data$Period)) else 2L

    # Determine detected design
    detected_design <- if (!is.null(replicate_result) && isTRUE(replicate_result$is_replicate)) {
      paste0(replicate_result$design_type, " (", replicate_result$n_periods, " periods)")
    } else if (is_crossover && n_p_data >= 4) {
      paste0("2\u00d72\u00d7", n_p_data, " Replicate Design (", n_p_data, " periods)")
    } else if (is_crossover && n_p_data == 3) {
      "2\u00d72\u00d73 Partial Replicate Design (3 periods)"
    } else if (is_crossover && n_treatments == 2) {
      "2\u00d72\u00d72 Crossover Design"
    } else if (is_crossover) {
      paste0(n_treatments, "\u00d7", n_treatments, " Crossover Design")
    } else {
      "Parallel Group Design"
    }
    
    n_subjects <- length(unique(data$Subject))
    n_observations <- nrow(data)
    
    div(
      p(strong("Design: "), detected_design, style = "margin: 8px 0;"),
      p(strong("Treatments: "), n_treatments, style = "margin: 8px 0;"),
      p(strong("Subjects: "), n_subjects, style = "margin: 8px 0;"),
      p(strong("Observations: "), n_observations, style = "margin: 8px 0;"),
      if (!is.null(replicate_result) && isTRUE(replicate_result$is_replicate)) {
        tagList(
          p(strong("Periods: "), replicate_result$n_periods, style = "margin: 8px 0;"),
          p(strong("Sequences: "), paste(replicate_result$sequences, collapse = ", "), style = "margin: 8px 0;")
        )
      }
    )
    
  }, error = function(e) {
    p("Error detecting study design:", e$message, style = "color: #dc3545; margin: 8px 0;")
  })
})

# Detected design type output for conditional UI
output$detected_design_type <- reactive({
  req(values$uploaded_data)
  
  tryCatch({
    data <- values$uploaded_data
    
    # Use capitalized column names
    treatments_per_subject <- data %>%
      group_by(Subject) %>%
      summarise(n_treatments = length(unique(Treatment)), .groups = "drop")
    # Crossover/replicate if ANY subject receives >1 treatment (robust to
    # subjects with missing periods). True parallel designs give every subject a
    # single treatment. See detected_design above for the matching logic.
    is_crossover <- isTRUE(max(treatments_per_subject$n_treatments, na.rm = TRUE) >= 2)

    if (is_crossover) {
      return("crossover")
    } else {
      return("parallel")
    }
  }, error = function(e) {
    return("unknown")
  })
})
outputOptions(output, "detected_design_type", suspendWhenHidden = FALSE)

# RSABE's ANOVA model is auto-selected by replicate design (partial -> Fixed,
# full -> Mixed), per FDA guidance — see R/rsabe_analysis.R::perform_rsabe().
# Single-choice selectInput (same style as the other analysis types) so the
# value is displayed but not user-editable. IMPORTANT: the value must be one
# perform_simple_anova() recognizes ("fixed"/"nlme"/"satterthwaite"/
# "kenward-roger") — this same analysis_config$anova_model feeds the
# ANOVA Results tab's primary perform_simple_anova() call, not just
# perform_rsabe() (which does its own separate, internal auto-selection and
# ignores this value). Using an unrecognized value here silently breaks that
# ANOVA table instead of erroring.
#
# NOTE: this selectInput deliberately uses its OWN id
# ("rsabe_anova_model_display"), NOT "anova_model" — it used to reuse
# "anova_model" (the same id as the ABE/ABEL/parallel model pickers below),
# with suspendWhenHidden = FALSE forcing it to render on every uploaded_data
# change regardless of which be_analysis_type tab was actually active. That
# silently overwrote input$anova_model with RSABE's auto-choice (e.g. "nlme"
# for a full-replicate dataset) even while the user was on the ABE tab with
# "Fixed Effects (lm)" visibly selected, so the ABE analysis actually ran
# mixed-effects until the user manually re-touched the ABE dropdown. Giving
# this its own id removes the collision; the RSABE-specific value is read out
# explicitly (input$rsabe_anova_model_display) when assembling analysis_config
# below, only when be_analysis_type == "RSABE".
output$rsabe_anova_model_ui <- renderUI({
  rtype <- tryCatch({
    req(values$uploaded_data)
    rr <- detect_replicate_design(values$uploaded_data)
    if (isTRUE(rr$is_partial_replicate)) "partial" else "full"
  }, error = function(e) "partial")

  choice <- if (rtype == "full") {
    list("Mixed Effects - nlme (REML)" = "nlme")
  } else {
    list("Fixed Effects (PROC GLM)" = "fixed")
  }
  selectInput("rsabe_anova_model_display", label = NULL, choices = choice, selected = choice[[1]])
})
outputOptions(output, "rsabe_anova_model_ui", suspendWhenHidden = FALSE)

# Dynamic alpha display text based on BE analysis type
output$alpha_display_text <- renderText({
  alpha <- input$alpha_level %||% 0.05
  ci <- (1 - 2 * alpha) * 100
  sprintf("%.0f%% CI (\u03b1 = %.2f one-sided for TOST)", ci, alpha)
})

# Help modal handlers
create_help_modal(session, input, "study_design", help_texts$study_design$title, help_texts$study_design$content)
create_help_modal(session, input, "auc_method", help_texts$auc_method$title, help_texts$auc_method$content)
create_help_modal(session, input, "pk_parameters", help_texts$pk_parameters$title, help_texts$pk_parameters$content)
create_help_modal(session, input, "lambda_z", help_texts$lambda_z$title, help_texts$lambda_z$content)
create_help_modal(session, input, "missing_data", help_texts$missing_data$title, help_texts$missing_data$content)
create_help_modal(session, input, "carryover_effect", help_texts$carryover_effect$title, help_texts$carryover_effect$content)
create_help_modal(session, input, "analysis_model", help_texts$analysis_model$title, help_texts$analysis_model$content)
create_help_modal(session, input, "confidence_level", help_texts$confidence_level$title, help_texts$confidence_level$content)
create_help_modal(session, input, "alpha_level_abe", help_texts$alpha_level_abe$title, help_texts$alpha_level_abe$content)
create_help_modal(session, input, "alpha_level_sabe", help_texts$alpha_level_sabe$title, help_texts$alpha_level_sabe$content)
create_help_modal(session, input, "be_limits", help_texts$be_limits$title, help_texts$be_limits$content)
create_help_modal(session, input, "reference_scaling", help_texts$reference_scaling$title, help_texts$reference_scaling$content)
create_help_modal(session, input, "abel_method", help_texts$abel_method$title, help_texts$abel_method$content)
create_help_modal(session, input, "abel_upper_cap", help_texts$abel_upper_cap$title, help_texts$abel_upper_cap$content)
create_help_modal(session, input, "rsabe_method", help_texts$rsabe_method$title, help_texts$rsabe_method$content)

# Observer to disable carryover assessment for parallel designs and PK parameter datasets
observe({
  # Check if study design indicates parallel design
  is_parallel <- FALSE
  is_pk_data <- FALSE
  
  if (!is.null(input$study_design)) {
    # Check manual selection
    if (input$study_design == "parallel") {
      is_parallel <- TRUE
    }
    
    # Check auto-detection
    if (input$study_design == "auto" && !is.null(values$uploaded_data)) {
      tryCatch({
        data <- values$uploaded_data
        n_treatments <- length(unique(data$Treatment))
        treatments_per_subject <- tapply(data$Treatment, data$Subject, function(x) length(unique(x)))
        is_crossover <- all(treatments_per_subject == n_treatments)
        is_parallel <- !is_crossover  # TRUE if NOT crossover (i.e., parallel)
      }, error = function(e) {
        is_parallel <- FALSE
      })
    }
  }
  
  # Check if this is PK parameter data (no Time/Concentration columns)
  if (!is.null(values$data_type) && values$data_type == "pk_parameters") {
    is_pk_data <- TRUE
  } else if (!is.null(values$uploaded_data)) {
    # Double-check by looking for Time/Concentration columns
    has_time <- any(c("Time", "time", "Time.Point", "timepoint") %in% names(values$uploaded_data))
    has_conc <- any(c("Concentration", "concentration", "Conc", "DV") %in% names(values$uploaded_data))
    is_pk_data <- !(has_time && has_conc)
  }
  
  # Disable carryover checkbox if parallel design or PK parameter data
  if (is_parallel || is_pk_data) {
    updateCheckboxInput(session, "test_carryover", value = FALSE)
    shinyjs::disable("test_carryover")
  } else {
    shinyjs::enable("test_carryover")
  }
})

# Current settings summary - updated for both data types
output$settings_summary <- renderUI({
  
  study_design <- if (!is.null(input$study_design)) input$study_design else "auto"
  confidence <- 90  # Fixed: 90% CI standard for BE (corresponds to alpha=0.05 two one-sided)
  be_lower <- 80    # Standard ABE lower limit (%)
  be_upper <- 125   # Standard ABE upper limit (%)
  log_transform <- TRUE  # Log transformation always applied per regulatory requirements
  ref_scaling <- FALSE   # Reference scaling handled by RSABE/ABEL analysis types
  
  # Data type specific settings
  data_type <- values$data_type %||% "concentration"
  
  if (data_type == "concentration") {
    auc_method <- if (!is.null(input$auc_method)) input$auc_method else "mixed"
    lambda_method <- if (!is.null(input$lambda_z_method)) input$lambda_z_method else "ttt"
    
    # Set standard PK parameters (calculated automatically)
    # Use log-transformed parameters for BE analysis as per regulatory requirements
    standard_params <- c("lnAUC0t", "lnAUC0inf", "lnCmax", "AUC0t", "AUC0inf", "Cmax", "Tmax", "half_life", "CL_F", "Vd_F", "lambda_z", "MRT")
    
    # Add optional parameters based on user selection
    optional_params <- c()
    
    # Add pAUC if selected
    if (isTRUE(input$calculate_pAUC)) {
      optional_params <- c(optional_params, "pAUC")
    }
    
    all_pk_params <- c(standard_params, optional_params)
    pk_params <- length(all_pk_params)
  } else {
    # For PK parameters data
    available_params <- if (!is.null(input$available_pk_parameters)) length(input$available_pk_parameters) else 0
    be_params <- if (!is.null(input$pk_parameters_for_be)) length(input$pk_parameters_for_be) else 0
  }
  
  # Display current design if auto-detected
  detected_design <- if (study_design == "auto" && !is.null(values$uploaded_data)) {
    tryCatch({
      data <- values$uploaded_data
      n_treatments <- length(unique(data$Treatment))
      treatments_per_subject <- tapply(data$Treatment, data$Subject, function(x) length(unique(x)))
      is_crossover <- all(treatments_per_subject == n_treatments)
      
      if (!is_crossover) {
        "Parallel"
      } else {
        rr <- tryCatch(detect_replicate_design(data), error = function(e) NULL)
        if (!is.null(rr) && isTRUE(rr$is_replicate)) {
          paste0("Replicate (", rr$design_name, ")")
        } else if ("Period" %in% names(data)) {
          n_p <- length(unique(data$Period))
          if (n_p >= 4) "2×2×4 Full Replicate"
          else if (n_p == 3) "2×2×3 Partial Replicate"
          else "2×2×2"
        } else {
          "2×2×2"
        }
      }
    }, error = function(e) {
      "Error detecting design"
    })
  } else {
    study_design
  }
  
  div(
    div(style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
      h5(icon("cog"), " Analysis Configuration", style = "color: #495057; margin-top: 0;"),
      
      div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
        span("Data Type:", style = "font-weight: 500;"),
        span(if(data_type == "concentration") "Concentration-Time" else "PK Parameters", 
             style = "color: #007bff;")
      ),
      div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
        span("Study Design:", style = "font-weight: 500;"),
        span(detected_design, style = "color: #007bff;")
      ),
      
      if (data_type == "concentration") {
        tagList(
          div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
            span("AUC Method:", style = "font-weight: 500;"),
            span(auc_method, style = "color: #007bff;")
          ),
          div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
            span("Lambda_z:", style = "font-weight: 500;"),
            span(toupper(lambda_method), style = "color: #007bff;")
          )
        )
      } else {
        tagList(
          div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
            span("Available Parameters:", style = "font-weight: 500;"),
            span(available_params, style = "color: #007bff;")
          ),
          div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
            span("BE Analysis Parameters:", style = "font-weight: 500;"),
            span(be_params, style = "color: #007bff;")
          )
        )
      }
    ),
    
    div(style = "background: #e8f5e8; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
      h5(icon("check-circle"), " Bioequivalence Criteria", style = "color: #28a745; margin-top: 0;"),
      
      div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
        span("Limits:", style = "font-weight: 500;"),
        span(paste0(be_lower, "% - ", be_upper, "%"), style = "color: #28a745;")
      ),
      div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
        span("Confidence:", style = "font-weight: 500;"),
        span(paste0(confidence, "%"), style = "color: #28a745;")
      ),
      if(study_design == "parallel" || (study_design == "auto" && detected_design == "Parallel Group Design")) {
        welch_setting <- as.logical(input$welch_correction_be %||% TRUE)
        div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
          span("Welch Correction:", style = "font-weight: 500;"),
          span(if(welch_setting) "Enabled" else "Disabled", style = "color: #28a745;")
        )
      },
      div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
        span("Log Transform:", style = "font-weight: 500;"),
        span(if(log_transform) "Yes" else "No", style = "color: #28a745;")
      ),
      if (data_type == "concentration") {
        div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
          span("PK Parameters:", style = "font-weight: 500;"),
          span(pk_params, style = "color: #28a745;")
        )
      } else {
        div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
          span("BE Parameters:", style = "font-weight: 500;"),
          span(be_params, style = "color: #28a745;")
        )
      },
      if(ref_scaling) {
        div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
          span("Ref. Scaling:", style = "font-weight: 500;"),
          span("Enabled", style = "color: #ffc107;")
        )
      }
    ),
    
    div(style = "border-top: 1px solid #dee2e6; padding-top: 10px; text-align: center;",
      tags$small(
        icon("clock"), " Updated: ", format(Sys.time(), "%H:%M:%S"), 
        style = "color: #6c757d;"
      )
    )
  )
})

# Debug output to help diagnose analysis setup issues
output$debug_info <- renderUI({
  div(style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #17a2b8;",
    h6("Debug Information:", style = "color: #0c5460; margin-top: 0;"),
    p(strong("Uploaded data exists: "), 
      if(!is.null(values$uploaded_data)) "YES" else "NO", 
      style = "margin: 5px 0; font-size: 12px;"),
    p(strong("Data type: "), 
      values$data_type %||% "Not set", 
      style = "margin: 5px 0; font-size: 12px;"),
    if (!is.null(values$uploaded_data)) {
      p(strong("Data dimensions: "), 
        paste(nrow(values$uploaded_data), "rows,", ncol(values$uploaded_data), "cols"),
        style = "margin: 5px 0; font-size: 12px;")
    },
    if (!is.null(values$uploaded_data)) {
      p(strong("Column names: "), 
        paste(names(values$uploaded_data), collapse = ", "),
        style = "margin: 5px 0; font-size: 12px;")
    }
  )
})

# Analysis execution
observeEvent(input$run_analysis, {
  req(values$uploaded_data)
  .bioeq_t0 <- proc.time()[["elapsed"]]

  # Validate that at least one PK parameter is selected for ANOVA
  selected_primary <- input$primary_pk_params %||% c()
  selected_secondary <- input$secondary_pk_params %||% c()

  if (length(selected_primary) == 0 && length(selected_secondary) == 0) {
    showNotification(
      "Please select at least one PK parameter for ANOVA analysis.",
      type = "error",
      duration = 5
    )
    return()
  }
  
  # Set standard PK parameters (calculated automatically)
  # Use log-transformed parameters for BE analysis as per regulatory requirements
  standard_params <- c("lnAUC0t", "lnAUC0inf", "lnCmax", "AUC0t", "AUC0inf", "Cmax", "Tmax", "half_life", "CL_F", "Vd_F", "lambda_z", "MRT")
  
  # Add optional parameters based on user selection
  optional_params <- c()
  
  # Add pAUC if selected with time points
  if (isTRUE(input$calculate_pAUC)) {
    optional_params <- c(optional_params, "pAUC")
  }
  
  all_pk_params <- c(standard_params, optional_params)
  
  # Collect analysis configuration
  analysis_config <- list(
    study_design = input$study_design %||% "auto",
    be_analysis_type = input$be_analysis_type %||% "ABE",  # NEW: BE analysis type
    auc_method = input$auc_method %||% "mixed",
    lambda_z_method = input$lambda_z_method %||% "ttt",
    lambda_z_points = input$lambda_z_points %||% 3,
    confidence_level = 90,  # Standard 90% CI for BE (alpha=0.05 two one-sided)
    alpha_level = {
      be_type <- input$be_analysis_type %||% "ABE"
      if (be_type == "RSABE") {
        input$alpha_level_rsabe %||% 0.05
      } else if (be_type == "ABEL") {
        input$alpha_level_abel %||% 0.05
      } else {
        input$alpha_level %||% 0.05
      }
    },
    # BE limits: standard 80-125% (per-parameter overrides below for ABE)
    be_limits = list(lower = 80, upper = 125),
    be_lower = 80,
    be_upper = 125,
    # Per-parameter BE limits (for ABE advanced options)
    be_limits_per_param = list(
      Cmax = list(lower = input$be_lower_cmax %||% 80, upper = input$be_upper_cmax %||% 125),
      AUC  = list(lower = input$be_lower_auc  %||% 80, upper = input$be_upper_auc  %||% 125)
    ),
    # RSABE method selection
    rsabe_method = input$rsabe_method %||% "fda_linearized",
    # ABEL: which PK parameters are evaluated for expanded limits (rest stay at 80–125%)
    abel_eligible_params = input$abel_eligible_params %||% c("Cmax"),
    pk_parameters = all_pk_params,
    # ANOVA Configuration
    # The "Analysis Model" picker is one of four different selectInputs
    # depending on design/BE type (parallel-locked / ABE / ABEL / RSABE-
    # auto), each on its OWN input id - never read input$anova_model here
    # without checking which picker is actually the one on screen. This used
    # to be a single shared "anova_model" id reused by all four, which meant
    # whichever one last fired a change event (including ones the user never
    # saw, e.g. RSABE's auto-selected value re-rendering on every data
    # change) silently won, regardless of what was visibly selected. See the
    # comment on output$rsabe_anova_model_ui above and on each selectInput in
    # analysis_setup_ui.R's "Step 3: ANOVA Configuration" block.
    anova_model = {
      be_type <- input$be_analysis_type %||% "ABE"
      is_parallel <- tryCatch({
        req(values$uploaded_data)
        data <- values$uploaded_data
        treatments_per_subject <- data %>%
          group_by(Subject) %>%
          summarise(n_treatments = length(unique(Treatment)), .groups = "drop")
        !isTRUE(max(treatments_per_subject$n_treatments, na.rm = TRUE) >= 2)
      }, error = function(e) FALSE)

      if (is_parallel) {
        input$anova_model_parallel %||% "fixed"
      } else if (be_type == "RSABE") {
        input$rsabe_anova_model_display %||% "fixed"
      } else if (be_type == "ABEL") {
        input$anova_model_abel %||% "fixed"
      } else {
        input$anova_model %||% "fixed"
      }
    },
    random_effects = input$random_effects %||% "(1|subject)",
    # Group Effects Configuration
    include_group_fixed = input$include_group_fixed %||% FALSE,
    include_group_random = input$include_group_random %||% FALSE,
    include_group_treatment_interaction = input$include_group_treatment_interaction %||% FALSE,
    # Parallel Design Configuration
    welch_correction = as.logical(input$welch_correction_be %||% TRUE),
    # PK Parameter Selection for ANOVA
    selected_pk_params = {
      primary_selected <- input$primary_pk_params %||% c("Cmax", "AUC0t", "AUC0inf")
      secondary_selected <- input$secondary_pk_params %||% c()
      combined_params <- c(primary_selected, secondary_selected)
      
      # Safety check - ensure we always have at least some parameters
      if (length(combined_params) == 0) {
        bioeq_log("No PK parameters selected - using defaults", "WARNING")
        combined_params <- c("Cmax", "AUC0t", "AUC0inf")
      }
      
      combined_params
    },
    outlier_test = TRUE,  # Outlier detection always enabled (ABEL has its own toggle)
    # pAUC configuration - automatically enable if pAUC is in selected parameters
    calculate_pAUC = {
      manual_pAUC <- isTRUE(input$calculate_pAUC)
      auto_pAUC <- "pAUC" %in% c(input$primary_pk_params, input$secondary_pk_params)
      result <- manual_pAUC || auto_pAUC
      bioeq_log(sprintf("pAUC Configuration: Manual=%s, Auto=%s, Final=%s", manual_pAUC, auto_pAUC, result), "DEBUG")
      result
    },
    pAUC_start = input$pAUC_start %||% 0,
    pAUC_end = input$pAUC_end %||% 2,
    log_transform = TRUE,  # Always TRUE — log transformation per regulatory requirements
    model_effects = c("sequence", "period", "treatment", "subject"),  # Standard ANOVA model terms
    missing_data_middle = input$missing_data_middle %||% "complete",
    missing_data_terminal = input$missing_data_terminal %||% "complete",
    reference_scaling = FALSE,  # Handled by RSABE/ABEL analysis types
    scaling_threshold = 30,     # CVwR threshold for reference scaling (%)
    scaling_cap = c(0.8, 1.25), # Scaling cap limits (ratio)
    # ICH M13A Carryover Detection
    test_carryover = input$test_carryover %||% FALSE,
    carryover_threshold = input$carryover_threshold %||% 5,
    exclude_carryover_subjects = input$exclude_carryover_subjects %||% TRUE,
    extrap_limit = 20  # AUC extrapolation limit (%)
  )
  
  # Store configuration for results display
  values$analysis_config <- analysis_config

  # Console phase lines mirror this progress bar 1:1 (see bioeq_phase(),
  # R/analysis_summary.R) so the console and the UI never diverge. Always 5:
  # Validation / NCA / Carryover / ANOVA / BE assessment - Carryover always
  # gets a line, even when not enabled (detail says so), rather than
  # changing the total depending on what ran.
  .n_phases <- 5L
  .phase <- 0L

  # Show progress
  shinyjs::show("analysis_progress")

  tryCatch({
    withProgress(message = 'Running bioequivalence analysis...', value = 0, {

    incProgress(0.1, detail = "Validating data and configuration...")

    incProgress(0.2, detail = "Detecting study design...")
    if (analysis_config$study_design == "auto") {
      # Auto-detect design logic here
      data <- values$uploaded_data

      # Use capitalized column names (Subject, Treatment) as per standardization
      n_treatments <- length(unique(data$Treatment))

      # Count treatments per subject without dplyr
      treatments_per_subject <- tapply(data$Treatment, data$Subject, function(x) length(unique(x)))
      is_crossover <- all(treatments_per_subject == n_treatments)

      detected_design <- if (!is_crossover) {
        "parallel"
      } else {
        # Use detect_replicate_design() for accurate period/replicate detection
        replicate_result <- tryCatch(
          detect_replicate_design(data),
          error = function(e) NULL
        )
        # Always use period count as the authoritative source for replicate detection
        n_p <- if (!is.null(replicate_result) && !is.null(replicate_result$n_periods)) {
          replicate_result$n_periods
        } else if ("Period" %in% names(data)) {
          length(unique(data$Period))
        } else {
          2L
        }
        if (n_p >= 4) "2x2x4" else if (n_p == 3) "2x2x3" else "2x2x2"
      }

      analysis_config$detected_design <- detected_design
      bioeq_log(sprintf("Auto-detected design: %s (%d treatments, crossover=%s)",
                        detected_design, n_treatments, is_crossover), "DEBUG")
    }

    # Header prints first (design, subjects, sequences, NCA/method config),
    # THEN the phase lines start as one contiguous block below it - Validation
    # is the first phase line, not printed earlier, so the reader sees the
    # full run configuration before any phase ticks by, and the [1/5]..[5/5]
    # list is never split across the header.
    header_design_info <- tryCatch(detect_replicate_design(values$uploaded_data), error = function(e) NULL)
    print_analysis_header(analysis_config, values$uploaded_data, header_design_info)

    .phase <- .phase + 1L
    bioeq_phase(.phase, .n_phases, "Validation", "ok")

    incProgress(0.3, detail = paste("Running NCA analysis using", analysis_config$auc_method, "method..."))
    # Here you would call the actual NCA functions from your R package
    # nca_results <- calculate_pk_parameters_batch(values$uploaded_data, analysis_config)
    
    # Perform carryover detection if enabled (per ICH M13A Section 2.2.3.3)
    analysis_data <- values$uploaded_data
    carryover_results <- NULL
    
    if (analysis_config$test_carryover) {
      # Check if data contains required columns for carryover detection
      required_carryover_cols <- c("Time", "Concentration")
      # Also check common alternative column names
      alt_time_cols <- c("time", "Time.Point", "timepoint")
      alt_conc_cols <- c("concentration", "Conc", "DV")
      
      has_time <- any(c(required_carryover_cols[1], alt_time_cols) %in% names(analysis_data))
      has_concentration <- any(c(required_carryover_cols[2], alt_conc_cols) %in% names(analysis_data))
      
      if (has_time && has_concentration) {
        incProgress(0.1, detail = "Performing carryover detection (ICH M13A)...")
        
        carryover_results <- detect_carryover(
          data = analysis_data,
          threshold = analysis_config$carryover_threshold
        )
        
        # Store carryover results for reporting
        values$carryover_results <- carryover_results
        
        # Exclude subjects with carryover if requested
        if (analysis_config$exclude_carryover_subjects && nrow(carryover_results$flagged_subjects) > 0) {
          excluded_subjects <- carryover_results$flagged_subjects$Subject  # Capital S to match carryover function output
          
          # Data should ALWAYS have Subject (capitalized) from data upload
          analysis_data <- analysis_data[!analysis_data$Subject %in% excluded_subjects, ]
          
          # Carryover notification removed; summary is now only in Results view
        } # No pop-up for carryover detection

        .phase <- .phase + 1L
        bioeq_phase(.phase, .n_phases, "Carryover",
                    sprintf("%d subject(s) flagged", nrow(carryover_results$flagged_subjects)))
      } else {
        # Data does not contain concentration-time data required for carryover detection
        bioeq_log("Skipping carryover detection: pre-calculated PK parameter data detected (requires Time and Concentration columns)", "DEBUG")

        # Create a minimal carryover results object to indicate it was skipped
        values$carryover_results <- list(
          data = analysis_data,
          carryover_summary = "Skipped - Pre-calculated PK data (Time/Concentration columns not available)",
          excluded_subjects = character(),
          flagged_subjects = data.frame(),
          carryover_data = data.frame(Message = "Carryover detection skipped for pre-calculated PK data"),
          carryover_details = data.frame(),
          threshold = analysis_config$carryover_threshold / 100,
          n_original = nrow(analysis_data),
          n_cleaned = nrow(analysis_data),
          n_excluded_subjects = 0
        )
        .phase <- .phase + 1L
        bioeq_phase(.phase, .n_phases, "Carryover", "skipped (pre-calculated PK data)")
      }
    } else {
      .phase <- .phase + 1L
      bioeq_phase(.phase, .n_phases, "Carryover", "skipped (not enabled)")
    }

    # Get the selected AUC method
    auc_method <- input$auc_method %||% "mixed"
    
    # Check if data contains concentration-time data or pre-calculated PK parameters
    has_time <- any(c("time", "Time", "Time.Point", "timepoint") %in% names(analysis_data))
    has_concentration <- any(c("concentration", "Concentration", "Conc", "DV") %in% names(analysis_data))
    
    # Check for common PK parameters that indicate pre-calculated data
    pk_parameter_patterns <- c("AUC", "Cmax", "Tmax", "CL", "Vd", "lambda", "MRT", "half", "t_half")
    has_pk_params <- any(sapply(pk_parameter_patterns, function(pattern) {
      any(grepl(pattern, names(analysis_data), ignore.case = TRUE))
    }))
    
    if (has_time && has_concentration && !has_pk_params) {
      # This is concentration-time data - perform NCA analysis
      bioeq_log("Detected concentration-time data - performing NCA analysis", "DEBUG")

      # ── Handle missing data before NCA ──
      middle_method <- analysis_config$missing_data_middle %||% "complete"
      terminal_method <- analysis_config$missing_data_terminal %||% "complete"

      # handle_missing_data() (R/missing_data_handling.R) logs its own
      # WARNING-level summary when it actually imputes/excludes anything -
      # not duplicated here.
      missing_result <- handle_missing_data(
        data = analysis_data,
        middle_method = middle_method,
        terminal_method = terminal_method,
        group_cols = c("Subject", "Treatment"),
        time_col = "Time",
        conc_col = "Concentration",
        period_col = "Period"
      )
      analysis_data <- missing_result$data
      values$missing_data_log <- missing_result$log
      
      # "group" (dosing/facility cohort) is optional design metadata — carry it
      # through the NCA step like Subject/Treatment/Period/Sequence when present,
      # so it remains available for the group-effect ANOVA option downstream.
      nca_id_cols <- c("Subject", "Treatment", "Period", "Sequence")
      if ("group" %in% names(analysis_data)) nca_id_cols <- c(nca_id_cols, "group")

      nca_results <- perform_nca_analysis(
        data = analysis_data,  # Use processed data with missing values handled
        id_cols = nca_id_cols,
        time_col = "Time",
        conc_col = "Concentration",
        lambda_z_method = analysis_config$lambda_z_method,
        auc_method = auc_method,
        lambda_z_points = analysis_config$lambda_z_points,  # Pass the manual points
        calculate_pAUC = analysis_config$calculate_pAUC,
        pAUC_start = analysis_config$pAUC_start,
        pAUC_end = analysis_config$pAUC_end
      )

      # Add missing design variables for ANOVA analysis
      # The NCA analysis might not preserve all design variables, so we add them back
      if (!("Sequence" %in% names(nca_results)) || !("Period" %in% names(nca_results)) ||
          ("group" %in% names(analysis_data) && !("group" %in% names(nca_results)))) {

        # Create unique identifier for merging - use CAPITALIZED column names
        analysis_data$merge_id <- paste(analysis_data$Subject, analysis_data$Treatment, sep = "_")
        nca_results$merge_id <- paste(nca_results$Subject, nca_results$Treatment, sep = "_")

        # Get design variables from original data
        design_cols <- c("merge_id", "Subject", "Sequence", "Period", "Treatment")
        if ("group" %in% names(analysis_data)) design_cols <- c(design_cols, "group")
        design_vars <- analysis_data[!duplicated(analysis_data$merge_id), design_cols]

        # Merge design variables with NCA results
        nca_results <- merge(nca_results, design_vars, by = "merge_id", all.x = TRUE, suffixes = c("", ".design"))
        nca_results$merge_id <- NULL  # Remove temporary merge column
      }
      
    } else if (has_pk_params && !has_time && !has_concentration) {
      # This is pre-calculated PK parameter data - format it as NCA results
      bioeq_log(sprintf("Detected pre-calculated PK parameter data - skipping NCA analysis (available: %s)",
                        paste(names(analysis_data)[grepl(paste(pk_parameter_patterns, collapse = "|"),
                                                          names(analysis_data), ignore.case = TRUE)], collapse = ", ")), "DEBUG")


      # Use the uploaded data directly as "NCA results"
      nca_results <- analysis_data
      
      # Add log-transformed versions of PK parameters if they don't exist.
      # Uses fuzzy case-insensitive matching to handle column name variants from
      # different software exports (e.g., AUCt, AUClast, AUCT all map to lnAUC0t).
      pk_log_mappings <- list(
        "lnAUC0t"   = c("AUC0t",   "AUCt",    "AUC0-t",  "AUC_t",   "AUClast",
                        "AUC_last","auct",    "auc0t",   "auclast", "AUC0T",   "AUCT"),
        "lnAUC0inf" = c("AUC0inf", "AUCinf",  "AUC0-inf","AUC_inf", "AUCinfinity",
                        "aucinf",  "auc0inf", "AUC0INF", "AUCINF"),
        "lnCmax"    = c("Cmax",    "CMAX",    "cmax",    "CMax",    "C_max",   "c_max")
      )
      col_names_lower <- tolower(names(nca_results))
      for (ln_name in names(pk_log_mappings)) {
        if (!ln_name %in% names(nca_results)) {
          # Case-insensitive search for first matching candidate column
          candidates <- pk_log_mappings[[ln_name]]
          match_idx  <- match(tolower(candidates), col_names_lower)
          match_idx  <- match_idx[!is.na(match_idx)]
          if (length(match_idx) > 0) {
            match_col    <- names(nca_results)[match_idx[1]]
            param_values <- nca_results[[match_col]]
            if (is.numeric(param_values) && all(param_values > 0, na.rm = TRUE)) {
              nca_results[[ln_name]] <- log(param_values)
              bioeq_log(sprintf("Added log-transformed parameter: %s (from column '%s')", ln_name, match_col), "DEBUG")
            } else if (!is.numeric(param_values)) {
              bioeq_log(sprintf("Skipping log transformation for %s: non-numeric data in '%s' (type: %s)",
                                ln_name, match_col, class(param_values)[1]), "WARNING")
            } else {
              n_nonpos <- sum(param_values <= 0, na.rm = TRUE)
              bioeq_log(sprintf("Skipping log transformation for %s: %d non-positive values in '%s'",
                                ln_name, n_nonpos, match_col), "WARNING")
            }
          }
        }
      }
      
    } else {
      # Unclear data type - attempt NCA but handle gracefully
      bioeq_log(sprintf("Uncertain data type - attempting NCA analysis (has_time=%s, has_concentration=%s, has_pk_params=%s)",
                        has_time, has_concentration, has_pk_params), "WARNING")

      nca_results <- tryCatch({
        perform_nca_analysis(
          data = analysis_data,
          id_cols = c("Subject", "Treatment", "Period", "Sequence"),
          time_col = "Time",
          conc_col = "Concentration",
          lambda_z_method = analysis_config$lambda_z_method,
          auc_method = auc_method,
          lambda_z_points = analysis_config$lambda_z_points,
          calculate_pAUC = analysis_config$calculate_pAUC,
          pAUC_start = analysis_config$pAUC_start,
          pAUC_end = analysis_config$pAUC_end
        )
      }, error = function(e) {
        bioeq_log(sprintf("NCA analysis failed: %s - treating as pre-calculated PK data", e$message), "ERROR")
        return(analysis_data)  # Use original data if NCA fails
      })
    }

    .phase <- .phase + 1L
    bioeq_phase(.phase, .n_phases, "NCA",
                sprintf("%d profiles", if (!is.null(nca_results)) nrow(nca_results) else 0L))

    incProgress(0.3, detail = "Performing ANOVA and bioequivalence assessment...")
    
    # Perform simple ANOVA analysis using lm() on selected parameters
    anova_results <- list()
    
    if (!is.null(nca_results)) {
      
      # Get all primary and log-transformed PK parameters for ANOVA
      primary_params <- c("Cmax", "AUC0t", "AUC0inf")
      log_params <- c("lnCmax", "lnAUC0t", "lnAUC0inf")
      
      # Create mapping of parameters to their log versions
      param_mapping <- list(
        "Cmax" = c("Cmax", "lnCmax"),
        "AUC0t" = c("AUC0t", "lnAUC0t"),
        "AUC0inf" = c("AUC0inf", "lnAUC0inf"),
        "Tmax" = c("Tmax", "lnTmax"),  # Now includes log-transformed Tmax
        "pAUC" = c("pAUC", "lnpAUC")  # pAUC and its log version
      )
      
      # Determine which parameters to analyze based on user selection and availability
      selected_params <- analysis_config$selected_pk_params
      
      # Check if selected_params is empty
      if (length(selected_params) == 0) {
        bioeq_log("No parameters selected for analysis", "ERROR")
        anova_results <- list(
          error = "No PK parameters were selected for ANOVA analysis. Please select at least one parameter in Step 2."
        )
        values$anova_results <- anova_results
        
        showNotification(
          "No PK parameters selected for analysis. Please check Step 2 parameter selection.",
          type = "error",
          duration = 10
        )
        return()
      }
      
      # Expand selected parameters to include their log-transformed versions
      expanded_params <- c()
      for (param in selected_params) {
        if (param %in% names(param_mapping)) {
          expanded_params <- c(expanded_params, param_mapping[[param]])
        } else {
          expanded_params <- c(expanded_params, param)
        }
      }
      
      # Remove duplicates and filter to only include those available in the data
      expanded_params <- unique(expanded_params)
      available_selected_params <- intersect(expanded_params, names(nca_results))
      
      # Filter to only include numeric parameters for ANOVA
      numeric_params <- c()
      for (param in available_selected_params) {
        param_values <- nca_results[[param]]
        if (is.numeric(param_values)) {
          # Check if we have enough non-missing numeric values
          non_missing_count <- sum(!is.na(param_values))
          if (non_missing_count >= 4) {  # Need at least 4 observations for ANOVA
            numeric_params <- c(numeric_params, param)
          } else {
            bioeq_log(sprintf("Skipping %s: insufficient non-missing values (%d, need at least 4)", param, non_missing_count), "WARNING")
          }
        } else {
          bioeq_log(sprintf("Skipping %s: non-numeric data (type: %s, first values: %s)",
                            param, class(param_values)[1],
                            if (!is.null(param_values) && length(param_values) > 0) paste(head(param_values, 3), collapse = ", ") else "none"), "WARNING")
        }
      }
      
      # Check if this is a replicate design with ABEL selected
      # If so, skip separate ANOVA and let replicateBE handle it
      # Detect design from data or use configured design
      detected_study_design <- analysis_config$detected_design %||% analysis_config$study_design
      
      # Also try to detect from data if still "auto"
      if (is.null(detected_study_design) || detected_study_design == "auto") {
        tryCatch({
          replicate_check <- detect_replicate_design(nca_results)
          if (replicate_check$is_replicate) {
            detected_study_design <- replicate_check$design_type
          }
        }, error = function(e) {
          detected_study_design <- "2x2x2"  # Default fallback
        })
      }
      
      is_replicate_design <- detected_study_design %in% c("2x2x3", "2x2x4", "replicate") || 
                            grepl("replicate", detected_study_design, ignore.case = TRUE)
      is_abel_analysis <- analysis_config$be_analysis_type == "ABEL"
      is_rsabe_analysis <- analysis_config$be_analysis_type == "RSABE"
      
      if (length(numeric_params) > 0) {

        if (is_replicate_design && (is_abel_analysis || is_rsabe_analysis)) {
          bioeq_log(sprintf(
            "Running ANOVA for replicate %s design (%d parameters): %s - non-eligible params (e.g. AUC0t under EMA) need ANOVA for fixed ABE CIs",
            analysis_config$be_analysis_type, length(numeric_params), paste(numeric_params, collapse = ", ")), "DEBUG")
        } else {
          bioeq_log(sprintf("Running ANOVA for %d parameters: %s",
                            length(numeric_params), paste(numeric_params, collapse = ", ")), "DEBUG")
        }
        bioeq_log(sprintf("Model: %s, Design: %s", analysis_config$anova_model, detected_study_design), "DEBUG")

        tryCatch({
          
          # Use the ANOVA function with the selected model type and random effects
          # alpha for ANOVA CI: for TOST alpha=0.05 (one-sided) -> CI alpha=0.10 (two-sided 90% CI)
          anova_alpha <- (analysis_config$alpha_level %||% 0.05) * 2
          simple_anova_results <- perform_simple_anova(
            nca_results, 
            numeric_params,  # Use validated numeric parameters
            analysis_config$anova_model,
            analysis_config$random_effects,
            analysis_config$include_group_fixed,
            analysis_config$include_group_random,
            analysis_config$include_group_treatment_interaction,
            alpha = anova_alpha
          )
          
          # Wrap results in expected structure for the UI
          anova_results <- list(
            anova_results = simple_anova_results,
            design = if (is_replicate_design && (is_abel_analysis || is_rsabe_analysis)) detected_study_design else "simple_anova",
            parameters = available_selected_params
          )

        }, error = function(e) {
          bioeq_log(sprintf("ANOVA failed: %s", e$message), "ERROR")
          anova_results <<- list(
            anova_results = list(),  # Empty list instead of error
            error = paste("ANOVA failed:", e$message)
          )
        })
      } else {
        # Provide specific error message about why no parameters are available
        error_msg <- "No valid numeric parameters available for ANOVA analysis."
        if (length(available_selected_params) == 0) {
          error_msg <- paste(error_msg, "Selected parameters not found in data:", paste(selected_params, collapse = ", "))
        } else {
          error_msg <- paste(error_msg, "All selected parameters contain non-numeric data or insufficient observations.")
        }
        
        anova_results <- list(
          error = error_msg
        )
        
        # Show user-friendly notification
        showNotification(
          paste("Analysis Error:", error_msg, "Please verify your data contains numeric values for the selected PK parameters."),
          type = "error",
          duration = 10
        )
      }
    }
    
    # Store ANOVA results from perform_simple_anova (available for all design types now).
    # For ABEL/RSABE, this will be overwritten later by the BE analysis results which
    # incorporate both the fixed-ABE ANOVA (from here) and the replicateBE output.
    if (!is.null(anova_results)) {
      values$anova_results <- anova_results
    }

    .phase <- .phase + 1L
    bioeq_phase(.phase, .n_phases, "ANOVA",
                sprintf("%d parameters",
                        length(if (exists("numeric_params")) numeric_params else character())))

    incProgress(0.1, detail = "Generating comprehensive results...")

    # For demonstration, create enhanced mock results that reflect the configuration
    values$analysis_complete <- TRUE
    
    # Build NCA results structure from real computed data
    # Compute real summary statistics from subject-level NCA results
    nca_summary <- tryCatch({
      # Core PK parameters to summarize (if present in the data)
      summary_params <- c("AUC0t", "AUC0inf", "Cmax", "Tmax", "t_half")
      available_params <- intersect(summary_params, colnames(nca_results))
      
      if (length(available_params) > 0 && "Treatment" %in% colnames(nca_results)) {
        summary_rows <- lapply(available_params, function(param) {
          vals <- nca_results[[param]]
          if (!is.numeric(vals)) return(NULL)
          
          test_vals <- vals[nca_results$Treatment %in% c("T", "Test")]
          ref_vals <- vals[nca_results$Treatment %in% c("R", "Reference")]
          
          test_vals <- test_vals[!is.na(test_vals)]
          ref_vals <- ref_vals[!is.na(ref_vals)]
          
          if (length(test_vals) == 0 || length(ref_vals) == 0) return(NULL)
          
          # Compute %CV from pooled data
          all_vals <- c(test_vals, ref_vals)
          cv_pct <- if (mean(all_vals) != 0) (sd(all_vals) / mean(all_vals)) * 100 else NA
          
          # Determine method label
          method_label <- if (param %in% c("Tmax")) {
            "Non-parametric"
          } else if (param %in% c("AUC0t", "AUC0inf")) {
            analysis_config$auc_method %||% "Linear-Log Trapezoidal"
          } else if (param == "t_half") {
            analysis_config$lambda_z_method %||% "OLS"
          } else {
            "Standard"
          }
          
          data.frame(
            Parameter = param,
            Test_Mean = round(mean(test_vals), 2),
            Reference_Mean = round(mean(ref_vals), 2),
            CV_percent = round(cv_pct, 2),
            Method = method_label,
            stringsAsFactors = FALSE
          )
        })
        
        summary_rows <- summary_rows[!sapply(summary_rows, is.null)]
        if (length(summary_rows) > 0) do.call(rbind, summary_rows) else NULL
      } else {
        NULL
      }
    }, error = function(e) {
      bioeq_log(sprintf("Could not compute NCA summary statistics: %s", e$message), "WARNING")
      NULL
    })
    
    values$nca_results <- list(
      # Real NCA results data (all 19 parameters)
      subject_data = nca_results,
      
      # Alias for subject_data (referenced as fallback in some server modules)
      parameters = nca_results,
      
      # Real computed summary statistics (NULL if computation fails)
      summary = nca_summary,
      
      # Method information from real analysis config
      lambda_z_method = analysis_config$lambda_z_method,
      auc_method = analysis_config$auc_method
    )
    
    # =======================================================================
    # PERFORM REAL BIOEQUIVALENCE ANALYSIS
    # =======================================================================
    
    tryCatch({
      # Determine study design
      study_design <- analysis_config$detected_design %||% analysis_config$study_design
      
      # Get BE limits from config (with proper fallback)
      cfg_be_lower <- analysis_config$be_limits$lower %||% (analysis_config$be_lower %||% 80)
      cfg_be_upper <- analysis_config$be_limits$upper %||% (analysis_config$be_upper %||% 125)
      be_limits <- c(cfg_be_lower / 100, cfg_be_upper / 100)
      # Alpha (one-sided) from user input
      # For TOST: alpha = 0.05 -> 90% CI, alpha = 0.025 -> 95% CI
      alpha <- analysis_config$alpha_level %||% 0.05
      
      # Prepare data for BE analysis 
      # For PK parameter data, uploaded_data already contains everything we need
      # For concentration-time data, use NCA results directly (one row per subject-period
      # with PK columns) — DO NOT join PK values onto raw concentration-time rows, as that
      # causes a massive Cartesian explosion (replicate designs have multiple NCA rows per
      # Subject+Treatment, multiplying every conc-time row by 2 for every parameter merged).
      if (values$data_type == "pk_parameters") {
        # PK parameter data: use uploaded data directly (already has Subject, Treatment, Period, Sequence, PK params)
        be_data <- values$uploaded_data
      } else if (is.data.frame(nca_results) && nrow(nca_results) > 0) {
        # Concentration-time data: use NCA results table directly as the BE input
        be_data <- nca_results
      } else {
        # Fallback: no NCA results — use uploaded data
        be_data <- values$uploaded_data
        bioeq_log("No NCA results - falling back to uploaded data for BE analysis", "WARNING")
      }
      
      # Ensure proper column names for BE analysis functions
      # BE analysis functions expect: Subject, Treatment, Period, Sequence
      if ("subject" %in% names(be_data)) {
        names(be_data)[names(be_data) == "subject"] <- "Subject"
      }
      if ("treatment" %in% names(be_data)) {
        names(be_data)[names(be_data) == "treatment"] <- "Treatment"
      }
      if ("period" %in% names(be_data)) {
        names(be_data)[names(be_data) == "period"] <- "Period"
      }
      if ("sequence" %in% names(be_data)) {
        names(be_data)[names(be_data) == "sequence"] <- "Sequence"
      }
      
      bioeq_log(sprintf("BE data: %d rows, %d cols, %d unique subjects (columns: %s)",
                        nrow(be_data), ncol(be_data), length(unique(be_data$Subject)),
                        paste(names(be_data), collapse = ", ")), "DEBUG")

      # NOTE: Previously we merged NCA results onto raw concentration-time rows here, which
      # caused a Cartesian explosion for replicate designs (joining on (Subject, Treatment)
      # multiplied every conc-time row by the count of NCA rows per pair). be_data is now
      # set above to either uploaded_data (PK data) or nca_results (concentration data),
      # so no further merging is required.


      valid_params <- c()
      
      # Reconstruct the same parameter selection logic used for ANOVA
      selected_params <- analysis_config$selected_pk_params
      
      # Create mapping of parameters to their log versions (same as ANOVA section)
      param_mapping <- list(
        "Cmax" = c("Cmax", "lnCmax"),
        "AUC0t" = c("AUC0t", "lnAUC0t"),
        "AUC0inf" = c("AUC0inf", "lnAUC0inf"),
        "Tmax" = c("Tmax", "lnTmax"),
        "pAUC" = c("pAUC", "lnpAUC")
      )
      
      # Expand selected parameters to include their log-transformed versions
      expanded_params <- c()
      for (param in selected_params) {
        if (param %in% names(param_mapping)) {
          expanded_params <- c(expanded_params, param_mapping[[param]])
        } else {
          expanded_params <- c(expanded_params, param)
        }
      }
      
      # Remove duplicates and filter to only include those available in the data
      expanded_params <- unique(expanded_params)
      selected_be_params <- intersect(expanded_params, names(be_data))
      
      bioeq_log(sprintf(
        "BE Analysis parameters - selected: %s | expanded: %s | available: %s",
        paste(selected_params, collapse = ", "), paste(expanded_params, collapse = ", "),
        paste(selected_be_params, collapse = ", ")), "DEBUG")

      for (param in selected_be_params) {
        if (param %in% names(be_data)) {
          param_values <- be_data[[param]]

          # Convert to numeric if needed
          if (!is.numeric(param_values)) {
            be_data[[param]] <- as.numeric(as.character(param_values))
            param_values <- be_data[[param]]
          }

          # Check if we have valid numeric data
          if (is.numeric(param_values)) {
            if (sum(!is.na(param_values) & param_values > 0) >= 4) {
              valid_params <- c(valid_params, param)
            } else {
              bioeq_log(sprintf("%s has insufficient valid data for BE analysis", param), "WARNING")
            }
          } else {
            bioeq_log(sprintf("%s could not be converted to numeric", param), "WARNING")
          }
        } else {
          bioeq_log(sprintf("%s not found in merged data", param), "WARNING")
        }
      }
      
      if (length(valid_params) == 0) {
        stop("No valid parameters found for BE analysis. Check NCA results and parameter names.")
      }
      
      bioeq_log(sprintf("Analyzing %s design with %d subjects, %d valid parameters",
                        study_design, length(unique(be_data$Subject)), length(valid_params)), "DEBUG")

      # Use the new BE analysis routing system
      be_analysis_result <- perform_be_analysis_by_type(
        data = be_data,
        analysis_type = analysis_config$be_analysis_type,
        design = study_design,
        params = list(
          alpha_level = alpha,
          be_limits = list(lower = be_limits[1] * 100, upper = be_limits[2] * 100),
          be_limits_per_param = analysis_config$be_limits_per_param,
          pk_parameters = valid_params,
          confidence_level = (1 - alpha * 2) * 100,
          anova_model = analysis_config$anova_model,
          welch_correction = analysis_config$welch_correction,
          anova_results = anova_results$anova_results,  # Pass the ANOVA results
          # ABEL-specific parameters
          abel_eligible_params = analysis_config$abel_eligible_params %||% c("Cmax"),
          abel_upper_cap = if (!is.null(input$abel_upper_cap)) input$abel_upper_cap else "50",
          abel_adjust_tie = if (!is.null(input$abel_adjust_tie)) input$abel_adjust_tie else FALSE,
          abel_outlier_analysis = if (!is.null(input$abel_outlier_analysis)) input$abel_outlier_analysis else FALSE,
          abel_outlier_fence = if (!is.null(input$abel_outlier_fence)) input$abel_outlier_fence else 2,
          # RSABE-specific parameters
          rsabe_method = analysis_config$rsabe_method %||% "fda_linearized"
        )
      )
      
      # Store the real BE analysis results and merge ANOVA results
      values$be_results <- be_analysis_result

      # Preserve the BE-engine's own ANOVA/scaling output (RSABE ISC variance /
      # replicateBE Method A/B) BEFORE the simple-ANOVA overwrite below. The
      # ANOVA tab uses this for RSABE/ABEL so it can show the purpose-built
      # scaled formatters (variance components + scaled limits + correct
      # decision) instead of a fixed-limit ABE table. Keyed by base parameter
      # name (e.g. "Cmax").
      values$be_results$scaled_anova <- be_analysis_result$anova_results

      # ANOVA tab always shows the user's selected ANOVA model results from perform_simple_anova()
      # (keyed by log-transformed parameter names: lnCmax, lnAUC0t, etc., with full Type III SS).
      # replicateBE runs its own internal ANOVA for ABEL scaling, but that's an implementation
      # detail of the package — we don't display it. Both fit the same linear model with the
      # user's data, so the BE point estimates and CIs are consistent with what's shown here.
      if (!is.null(anova_results) && !is.null(anova_results$anova_results) &&
          length(anova_results$anova_results) > 0) {
        values$anova_results <- anova_results
        values$be_results$anova_results <- anova_results
        bioeq_log(sprintf("ANOVA tab will display simple_anova results (%d parameters)",
                          length(anova_results$anova_results)), "DEBUG")
      } else if (!is.null(be_analysis_result$anova_results) &&
                 length(be_analysis_result$anova_results) > 0) {
        # Fallback: use whatever the BE engine returned (e.g. replicateBE-derived)
        values$anova_results <- be_analysis_result$anova_results
        bioeq_log(sprintf("Falling back to BE-engine ANOVA results (%s)", analysis_config$be_analysis_type), "WARNING")
      } else {
        bioeq_log("No ANOVA results available from either source", "WARNING")
      }

      .phase <- .phase + 1L
      bioeq_phase(.phase, .n_phases, "BE assessment",
                  sprintf("%d parameters", length(be_analysis_result$confidence_intervals %||% list())))

      # Best-effort Reference/Test intra-subject CV% for the summary's one
      # line display. compute_reference_anova_variance() (R/simple_anova.R)
      # requires Treatment coded as literal "R"/"T" (not "Reference"/"Test"),
      # so recode a throwaway copy of be_data just for this - never touches
      # be_data/be_analysis_result itself. This is a reporting nicety, not
      # part of the analysis, so any failure here just means the summary
      # falls back to its own pooled-MSE approximation - never surfaced as
      # an analysis error.
      ref_test_cv <- tryCatch({
        first_param <- names(be_analysis_result$confidence_intervals)[1]
        if (!is.null(first_param) && !is.null(be_data) &&
            all(c("Treatment", first_param) %in% names(be_data))) {
          rt_data <- be_data
          rt_data$Treatment <- ifelse(rt_data$Treatment %in% c("Reference", "R"), "R",
                                ifelse(rt_data$Treatment %in% c("Test", "T"), "T", rt_data$Treatment))
          rv <- compute_reference_anova_variance(rt_data, first_param)
          if (!is.null(rv$cv_wR) && !is.na(rv$cv_wR)) list(ref = rv$cv_wR, test = rv$cv_wT) else NULL
        } else {
          NULL
        }
      }, error = function(e) NULL)

      # The one user-facing report for this whole run - see
      # R/analysis_summary.R for why this uses cat() rather than bioeq_log().
      print_be_analysis_summary(
        result      = be_analysis_result,
        elapsed_sec = proc.time()[["elapsed"]] - .bioeq_t0,
        config      = analysis_config,
        ref_test_cv = ref_test_cv
      )

    }, error = function(e) {
      bioeq_log(sprintf("Error in BE Analysis: %s", e$message), "ERROR")

      # Create fallback mock results in case of error - PRESERVE ANOVA RESULTS
      values$be_results <- list(
        error = paste("BE Analysis failed:", e$message),
        confidence_intervals = list(),
        be_conclusions = list(),
        design = study_design,
        n_subjects = length(unique(values$uploaded_data$Subject)),
        parameters = analysis_config$pk_parameters,
        alpha = alpha,
        be_limits = be_limits,
        anova_results = anova_results  # PRESERVE ANOVA RESULTS EVEN WHEN BE FAILS
      )
      
      # Mark analysis as incomplete
      values$analysis_complete <- FALSE
      
      showNotification(
        paste("BE Analysis Error:", e$message), 
        type = "error", 
        duration = 10
      )
    })
    
  })
  
  }, error = function(e) {
    # Handle any unexpected errors during analysis
    bioeq_log(sprintf("Unexpected error during analysis: %s", e$message), "ERROR")
    
    # Hide progress indicator
    shinyjs::hide("analysis_progress")
    
    # Show error notification with specific guidance
    error_message <- paste("Analysis failed:", e$message)
    if (grepl("log.*non-numeric", e$message, ignore.case = TRUE)) {
      error_message <- "Analysis failed: Unable to perform log transformation on non-numeric data. Please ensure your data contains only numeric values for PK parameters."
    } else if (grepl("no package called", e$message, ignore.case = TRUE)) {
      error_message <- paste("Analysis failed:", e$message, "- Please install required packages.")
    }
    
    showNotification(
      error_message,
      type = "error",
      duration = 15
    )
    
    # Store minimal error results
    values$analysis_complete <- FALSE
    values$anova_results <- list(error = error_message)
    values$be_results <- list(error = error_message)
    
    return()
  })
  
  shinyjs::hide("analysis_progress")
  updateTabItems(session, "sidebar", "results")

  # No separate "Analysis completed successfully!" toast here — the
  # withProgress() bar above already walks through each step and reaches
  # 100% ("Generating comprehensive results...") right before this point,
  # and navigating to the Results tab is itself a clear completion signal.
  # A second toast on top of that was pure duplication (two overlapping
  # "done" popups for one action).
})

# Custom template saving
observeEvent(input$save_custom_template, {
  showModal(modalDialog(
    title = "Save Custom Template",
    
    textInput("template_name", "Template Name:", value = "My Custom Template"),
    textAreaInput("template_description", "Description:", 
                  value = "Custom analysis parameters", rows = 3),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_save_template", "Save Template", class = "btn-primary")
    )
  ))
})

observeEvent(input$confirm_save_template, {
  # Here you would save the current parameters as a custom template
  showNotification("Custom template saved successfully!", type = "message")
  removeModal()
})

# Update available PK parameters based on data type and uploaded data
observe({
  if (!is.null(values$data_type) && values$data_type == "pk_parameters" && 
      !is.null(values$validation_result) && !is.null(values$validation_result$mapped_pk_parameters)) {
    
    # Get available PK parameters from uploaded data
    available_params <- names(values$validation_result$mapped_pk_parameters)
    
    # Create choices list with proper labels
    param_choices <- list()
    param_labels <- list(
      "AUC0t" = "AUC0-t (primary)",
      "AUC0inf" = "AUC0-inf (primary)", 
      "Cmax" = "Cmax (primary)",
      "Tmax" = "Tmax (secondary)",
      "half_life" = "T1/2 (secondary)",
      "clearance" = "CL/F (secondary)",
      "volume" = "Vd/F (secondary)",
      "AUC_extrap_percent" = "AUC%extrap (secondary)"
    )
    
    for (param in available_params) {
      label <- param_labels[[param]] %||% paste(param, "(detected)")
      param_choices[[label]] <- param
    }
    
    # Update the checkbox group for available parameters
    updateCheckboxGroupInput(session, "available_pk_parameters",
                           choices = param_choices,
                           selected = available_params)
    
    # Update the checkbox group for BE analysis (subset of available)
    primary_params <- intersect(available_params, c("AUC0t", "AUC0inf", "Cmax"))
    updateCheckboxGroupInput(session, "pk_parameters_for_be",
                           choices = param_choices,
                           selected = primary_params)
  }
})

# Update PK parameters for BE analysis based on available parameters selection
observeEvent(input$available_pk_parameters, {
  if (!is.null(input$available_pk_parameters) && length(input$available_pk_parameters) > 0) {
    
    # Get the current choices from available parameters
    available_params <- input$available_pk_parameters
    
    # Create choices list
    param_choices <- list()
    param_labels <- list(
      "AUC0t" = "AUC0-t (primary)",
      "AUC0inf" = "AUC0-inf (primary)", 
      "Cmax" = "Cmax (primary)",
      "Tmax" = "Tmax (secondary)",
      "half_life" = "T1/2 (secondary)",
      "clearance" = "CL/F (secondary)",
      "volume" = "Vd/F (secondary)",
      "AUC_extrap_percent" = "AUC%extrap (secondary)"
    )
    
    for (param in available_params) {
      label <- param_labels[[param]] %||% paste(param, "(detected)")
      param_choices[[label]] <- param
    }
    
    # Update BE analysis parameters choices
    current_selection <- input$pk_parameters_for_be %||% c()
    valid_selection <- intersect(current_selection, available_params)
    
    updateCheckboxGroupInput(session, "pk_parameters_for_be",
                           choices = param_choices,
                           selected = valid_selection)
  }
})
