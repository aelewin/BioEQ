# BioEQ - Bioequivalence Analysis Functions
# Reorganized and improved version with better structure

# =============================================================================
# SECTION 1: MAIN ANALYSIS FUNCTIONS (High-level interfaces)
# =============================================================================

#' Main Bioequivalence Analysis Function
#'
#' @param data Validated BE study data
#' @param design Study design ("2x2x2", "parallel", "replicate", "auto")
#' @param alpha Significance level (default 0.05)
#' @param be_limits Bioequivalence limits (default c(0.8, 1.25))
#' @param parameters PK parameters to analyze
#' @param regulatory_standard Regulatory standard ("FDA", "EMA", "ICH")
#' @param custom_config Custom configuration overrides
#' @return BE analysis results
#' @export
perform_be_analysis <- function(data, 
                               design = "auto",
                               alpha = 0.05,
                               be_limits = c(0.8, 1.25),
                               parameters = c("lnAUC0t", "lnAUC0inf", "lnCmax"),
                               regulatory_standard = "FDA",
                               custom_config = NULL) {
  
  cat("🔬 Starting bioequivalence analysis...\n")
  
  # Create analysis configuration
  config <- create_analysis_config(regulatory_standard, custom_config)
  config$alpha <- alpha
  config$be_limits <- be_limits
  
  # Validate and prepare data
  validated_data <- validate_be_data(data, design)
  
  # Auto-detect design if needed
  if (design == "auto") {
    design <- detect_study_design(validated_data)
    cat("📊 Auto-detected study design:", design, "\n")
  }
  
  # Perform analysis by design
  results <- switch(design,
    "2x2x2" = be_crossover_2x2x2(validated_data, alpha, be_limits, parameters, "fixed"),
    "parallel" = be_parallel(validated_data, alpha, be_limits, parameters),
    "replicate" = be_replicate(validated_data, alpha, be_limits, parameters),
    stop("Unsupported study design: ", design)
  )
  
  # Apply regulatory evaluation
  results <- apply_ich_m13a_evaluation(results, config)
  
  cat("✅ Bioequivalence analysis completed!\n")
  return(results)
}

#' Perform bioequivalence analysis based on selected analysis type
#' 
#' Routes analysis to appropriate BE method (ABE, RSABE, or ABEL)
#' 
#' @param data Data frame with PK parameters
#' @param analysis_type Type of BE analysis: "ABE", "RSABE", or "ABEL"
#' @param design Study design ("2x2x2", "parallel", "replicate", etc.)
#' @param params Analysis parameters including limits and confidence level
#' @return BE analysis results with analysis type metadata
#' @export
perform_be_analysis_by_type <- function(data, analysis_type = "ABE", design = "auto", params = list()) {
  
  cat(sprintf("🔬 Performing %s analysis...\n", analysis_type))
  
  # Route to appropriate analysis function
  results <- switch(analysis_type,
    "ABE" = perform_average_be(data, design, params),
    "RSABE" = perform_rsabe_placeholder(data, design, params),
    "ABEL" = perform_abel_placeholder(data, design, params),
    # Default to ABE if unknown type
    perform_average_be(data, design, params)
  )
  
  # Add analysis type metadata to results
  results$analysis_type <- analysis_type
  results$analysis_method <- switch(analysis_type,
    "ABE" = "Average Bioequivalence",
    "RSABE" = "Reference-Scaled Average Bioequivalence", 
    "ABEL" = "Average Bioequivalence with Expanding Limits"
  )
  
  return(results)
}

#' Perform standard Average Bioequivalence (ABE) analysis
#' 
#' Standard BE analysis with fixed limits (typically 80-125%)
#' 
#' @param data Data frame with PK parameters
#' @param design Study design
#' @param params Analysis parameters
#' @return ABE analysis results
#' @export
perform_average_be <- function(data, design = "auto", params = list()) {
  
  cat("📊 Performing Average Bioequivalence (ABE) analysis...\n")
  
  # Extract parameters with defaults
  alpha <- params$alpha_level %||% 0.05
  be_limits <- c(
    (params$be_limits$lower %||% 80) / 100,
    (params$be_limits$upper %||% 125) / 100
  )
  parameters <- params$pk_parameters %||% c("lnAUC0t", "lnAUC0inf", "lnCmax")
  anova_model <- params$anova_model %||% "fixed"
  anova_results <- params$anova_results %||% NULL
  
  # Use existing BE analysis functions
  results <- switch(design,
    "2x2x2" = be_crossover_2x2x2(data, alpha, be_limits, parameters, anova_model, anova_results),
    "parallel" = be_parallel(data, alpha, be_limits, parameters),
    "replicate" = be_replicate(data, alpha, be_limits, parameters),
    "2x2x3" = be_replicate(data, alpha, be_limits, parameters, design = "2x2x3"),
    "2x2x4" = be_replicate(data, alpha, be_limits, parameters, design = "2x2x4"),
    stop("Unsupported study design for ABE: ", design)
  )
  
  # Add ABE-specific metadata
  results$be_method <- "ABE"
  results$limits_type <- "fixed"
  results$limits_justification <- "Standard regulatory limits (80.00% - 125.00%)"
  
  return(results)
}

#' Placeholder for RSABE analysis
#' 
#' Currently uses ABE methodology with informative messaging
#' 
#' @param data Data frame with PK parameters
#' @param design Study design
#' @param params Analysis parameters
#' @return Placeholder results using ABE methodology with RSABE metadata
#' @export
perform_rsabe_placeholder <- function(data, design = "auto", params = list()) {
  
  cat("⚠️ RSABE analysis requested but not yet implemented.\n")
  cat("📊 Using standard ABE methodology as placeholder.\n")
  cat("🔧 Full RSABE implementation coming soon...\n")
  
  # For now, perform standard ABE analysis
  results <- perform_average_be(data, design, params)
  
  # Add RSABE-specific metadata and placeholders
  results$be_method <- "RSABE (Placeholder)"
  results$limits_type <- "fixed (pending scaling implementation)"
  results$limits_justification <- "Currently using standard ABE limits. RSABE scaling pending."
  
  # Add RSABE-specific information structure
  results$rsabe_info <- list(
    implemented = FALSE,
    status = "Placeholder - using ABE methodology",
    message = "RSABE analysis requested but using ABE methodology until implementation complete",
    planned_features = list(
      scaling_formula = "exp(±0.893 × sWR) where sWR is within-subject SD of reference",
      cv_threshold = "Applied when CV > 30% (sWR > 0.294)",
      point_constraint = "Point estimate must be within 80.00% - 125.00%",
      regulatory_basis = "FDA guidance for industry - Bioequivalence Studies with PK Endpoints for Drugs Submitted Under an ANDA"
    ),
    implementation_notes = list(
      "Calculate within-subject variability for reference formulation",
      "Apply scaled limits based on sWR",
      "Maintain point estimate constraint",
      "Handle mixed scaling (CV < 30% uses standard limits)"
    )
  )
  
  return(results)
}

#' Placeholder for ABEL analysis
#' 
#' Currently uses ABE methodology with informative messaging
#' 
#' @param data Data frame with PK parameters
#' @param design Study design  
#' @param params Analysis parameters
#' @return Placeholder results using ABE methodology with ABEL metadata
#' @export
perform_abel_placeholder <- function(data, design = "auto", params = list()) {
  
  cat("⚠️ ABEL analysis requested but not yet implemented.\n")
  cat("📊 Using standard ABE methodology as placeholder.\n")
  cat("🔧 Full ABEL implementation coming soon...\n")
  
  # For now, perform standard ABE analysis
  results <- perform_average_be(data, design, params)
  
  # Add ABEL-specific metadata and placeholders
  results$be_method <- "ABEL (Placeholder)"
  results$limits_type <- "fixed (pending expansion implementation)"
  results$limits_justification <- "Currently using standard ABE limits. ABEL expansion pending."
  
  # Add ABEL-specific information structure
  results$abel_info <- list(
    implemented = FALSE,
    status = "Placeholder - using ABE methodology", 
    message = "ABEL analysis requested but using ABE methodology until implementation complete",
    planned_features = list(
      expansion_formula = "exp(±0.76 × sWR) up to maximum of 69.84% - 143.19%",
      cv_threshold = "Applied when CV > 30% for Cmax",
      gmr_constraint = "Geometric mean ratio must be within 80.00% - 125.00%",
      max_expansion = "Upper limit capped at 143.19%, lower at 69.84%",
      regulatory_basis = "EMA guideline on the investigation of bioequivalence (CPMP/EWP/QWP/1401/98 Rev. 1)"
    ),
    implementation_notes = list(
      "Calculate within-subject variability for reference formulation",
      "Apply EMA expansion formula with maximum caps",
      "Ensure GMR constraint satisfaction",
      "Handle switching conditions based on CV"
    )
  )
  
  return(results)
}

#' Analyze 2x2x2 Crossover Bioequivalence Study
#'
#' @param data Validated BE study data
#' @param alpha Significance level (default 0.05)
#' @param be_limits Bioequivalence limits (default c(0.8, 1.25))
#' @param parameters PK parameters to analyze
#' @return BE analysis results
#' @export
be_crossover_2x2x2 <- function(data, alpha = 0.05, be_limits = c(0.8, 1.25), 
                               parameters = c("lnAUC0t", "lnAUC0inf", "lnCmax"),
                               anova_model = "fixed", anova_results = NULL) {
  
  cat("🔬 Analyzing 2x2x2 crossover bioequivalence study...\n")
  
  # Check if ANOVA results are provided
  if (is.null(anova_results)) {
    stop("ANOVA results must be provided. Use the ANOVA module to generate results first.")
  }
  
  # Validate PK parameters against available ANOVA results
  available_params <- names(anova_results)
  pk_params <- parameters[parameters %in% available_params]
  
  if (length(pk_params) == 0) {
    stop("No valid parameters found in ANOVA results. Available: ", paste(available_params, collapse = ", "))
  }
  
  cat(sprintf("📊 Using ANOVA results for %d parameters: %s\n", 
              length(pk_params), paste(pk_params, collapse = ", ")))
  
  # Extract BE results from ANOVA
  be_results <- extract_be_from_anova(anova_results, alpha, be_limits)
  
  # Instead of filtering based on input parameters, use all parameters that were successfully analyzed
  # This ensures that all user-selected parameters that had valid ANOVA results are included
  filtered_ci <- be_results$confidence_intervals
  filtered_anova <- be_results$anova_results
  
  # Remove any NULL or NA entries
  filtered_ci <- filtered_ci[!sapply(filtered_ci, is.null)]
  filtered_anova <- filtered_anova[!sapply(filtered_anova, is.null)]
  
  cat(sprintf("🔗 Including all valid CI parameters: %s\n", 
              paste(names(filtered_ci), collapse = ", ")))
  
  if (length(filtered_ci) == 0) {
    stop("No valid confidence intervals available for the requested parameters")
  }
  
  # Generate bioequivalence conclusions
  be_conclusions <- evaluate_bioequivalence(filtered_ci, be_limits)
  
  # Compile results
  result <- create_be_results(
    design = "2x2x2",
    data = data,
    anova_results = filtered_anova,
    confidence_intervals = filtered_ci,
    be_conclusions = be_conclusions,
    parameters = pk_params,
    alpha = alpha,
    be_limits = be_limits
  )
  
  # Add ANOVA method information to the results for display
  # Get the method from the first available ANOVA result
  if (length(filtered_anova) > 0) {
    result$anova_method <- filtered_anova[[1]]$anova_method %||% anova_model
    result$anova_method_description <- if (result$anova_method == "mixed") "Mixed Effects Model (REML)" else "Fixed Effects Model"
  } else {
    result$anova_method <- anova_model
    result$anova_method_description <- if (anova_model == "mixed") "Mixed Effects Model (REML)" else "Fixed Effects Model"
  }
  
  cat("✅ 2x2x2 crossover analysis completed!\n")
  return(result)
}

#' Analyze Parallel Group Bioequivalence Study
#'
#' @param data Validated BE study data
#' @param alpha Significance level (default 0.05)
#' @param be_limits Bioequivalence limits (default c(0.8, 1.25))
#' @param parameters PK parameters to analyze
#' @return BE analysis results
#' @export
be_parallel <- function(data, alpha = 0.05, be_limits = c(0.8, 1.25), 
                       parameters = c("lnAUC0t", "lnAUC0inf", "lnCmax")) {
  
  cat("🔬 Analyzing parallel group bioequivalence study...\n")
  
  # Define appropriate parameters for parallel BE analysis
  # Exclude time-based parameters (Tmax, Tlast) and parameters with low variability
  appropriate_params <- c("lnAUC0t", "lnAUC0inf", "lnCmax", "AUC0t", "AUC0inf", "Cmax", 
                         "CL_F", "Vd_F", "lambda_z", "MRT")
  
  # Filter requested parameters to only include appropriate ones
  filtered_parameters <- intersect(parameters, appropriate_params)
  
  if (length(filtered_parameters) == 0) {
    cat("⚠️ No appropriate parameters found for parallel BE analysis.\n")
    cat("Appropriate parameters: ", paste(appropriate_params, collapse = ", "), "\n")
    cat("Requested parameters: ", paste(parameters, collapse = ", "), "\n")
    return(NULL)
  }
  
  # Validate PK parameters
  pk_params <- validate_pk_parameters(data, filtered_parameters)
  
  # Perform analysis for each parameter
  confidence_intervals <- list()
  statistical_results <- list()
  
  for (param in pk_params) {
    cat("  Analyzing", param, "...\n")
    
    # Perform parallel group analysis using geometric mean ratio method with error handling
    tryCatch({
      param_results <- analyze_parallel_parameter(data, param, alpha)
      
      if (!is.null(param_results)) {
        confidence_intervals[[param]] <- param_results$ci
        statistical_results[[param]] <- param_results$stats
        
        cat("    ", param, ": ", sprintf("%.2f%% (%.2f%% - %.2f%%)", 
                                        param_results$ci$point_estimate, 
                                        param_results$ci$ci_lower, 
                                        param_results$ci$ci_upper), "\n")
      } else {
        cat("    ⚠️ ", param, ": Analysis returned NULL\n")
      }
    }, error = function(e) {
      cat("    ❌ ", param, ": Error -", e$message, "\n")
      # Continue with other parameters
    })
  }
  
  # Generate bioequivalence conclusions
  be_conclusions <- evaluate_bioequivalence(confidence_intervals, be_limits)
  
  # Compile results
  result <- create_be_results(
    design = "parallel",
    data = data,
    statistical_results = statistical_results,
    confidence_intervals = confidence_intervals,
    be_conclusions = be_conclusions,
    parameters = pk_params,
    alpha = alpha,
    be_limits = be_limits
  )
  
  cat("✅ Parallel group analysis completed!\n")
  return(result)
}

#' Analyze Replicate Crossover Bioequivalence Study
#'
#' @param data Validated BE study data
#' @param alpha Significance level (default 0.05)
#' @param be_limits Bioequivalence limits (default c(0.8, 1.25))
#' @param parameters PK parameters to analyze
#' @param scaling Whether to use scaled average bioequivalence
#' @return BE analysis results
#' @export
be_replicate <- function(data, alpha = 0.05, be_limits = c(0.8, 1.25), 
                        parameters = c("lnAUC0t", "lnAUC0inf", "lnCmax"),
                        scaling = TRUE) {
  
  cat("🔬 Analyzing replicate crossover bioequivalence study...\n")
  
  # Validate PK parameters
  pk_params <- validate_pk_parameters(data, parameters)
  
  # Add sequence information
  data_with_seq <- add_sequence_info_replicate(data)
  
  # Detect replicate design structure
  design_info <- detect_replicate_design(data_with_seq)
  cat("📋 Detected design:", design_info$design_name, "\n")
  cat("   Sequences:", paste(design_info$sequences, collapse = ", "), "\n")
  cat("   Periods:", design_info$n_periods, "\n")
  if (design_info$is_partial_replicate) {
    cat("   Type: Partial replicate (reference replicated only)\n")
  } else {
    cat("   Type: Full replicate (both formulations replicated)\n")
  }
  cat("\n")
  
  # Perform analysis for each parameter
  confidence_intervals <- list()
  variability_results <- list()
  scaling_decisions <- list()
  
  for (param in pk_params) {
    cat("  Analyzing", param, "...\n")
    
    # Perform replicate analysis with potential scaling
    param_results <- analyze_replicate_parameter(data_with_seq, param, alpha, scaling)
    
    if (!is.null(param_results)) {
      confidence_intervals[[param]] <- param_results$ci
      variability_results[[param]] <- param_results$variability
      scaling_decisions[[param]] <- list(
        use_scaling = param_results$scaling_applied,
        cv_wr = param_results$variability$cv_wr,
        threshold = 30
      )
      
      cat("    Within-subject CV:", round(param_results$variability$cv_wr, 1), "%\n")
      cat("    Scaling applied:", ifelse(param_results$scaling_applied, "YES", "NO"), "\n")
      cat("    ", param, ": ", sprintf("%.1f%% (%.1f%% - %.1f%%)", 
                                      param_results$ci$point_estimate, 
                                      param_results$ci$ci_lower, 
                                      param_results$ci$ci_upper), "\n")
    }
  }
  
  # Generate bioequivalence conclusions
  be_conclusions <- evaluate_bioequivalence(confidence_intervals, be_limits)
  
  # Compile results
  result <- create_be_results(
    design = "replicate",
    data = data_with_seq,
    variability_results = variability_results,
    confidence_intervals = confidence_intervals,
    be_conclusions = be_conclusions,
    parameters = pk_params,
    alpha = alpha,
    be_limits = be_limits,
    scaling_decisions = scaling_decisions,
    design_info = design_info
  )
  
  cat("✅ Replicate crossover analysis completed!\n")
  return(result)
}

# =============================================================================
# SECTION 2: PARAMETER-SPECIFIC ANALYSIS FUNCTIONS
# =============================================================================

#' Analyze Single Parameter for Crossover Design
#'
#' @param data Study data
#' @param parameter Parameter name
#' @param alpha Significance level
#' @param be_limits Bioequivalence limits
#' @return Parameter analysis results
#' Extract BE confidence intervals from ANOVA results
#'
#' @param anova_results ANOVA results from perform_simple_anova
#' @param alpha Significance level (default 0.05) 
#' @param be_limits Bioequivalence limits (default c(0.8, 1.25))
#' @return BE analysis results extracted from ANOVA
extract_be_from_anova <- function(anova_results, alpha = 0.05, be_limits = c(0.8, 1.25)) {
  
  cat("🔗 Extracting bioequivalence results from ANOVA analysis...\n")
  
  # Filter to only log-transformed parameters for BE analysis
  log_params <- names(anova_results)[grepl("^ln", names(anova_results))]
  
  if (length(log_params) == 0) {
    stop("No log-transformed parameters found in ANOVA results. BE analysis requires log-transformed data.")
  }
  
  cat(sprintf("📊 Using ANOVA results for %d log-transformed parameters: %s\n", 
              length(log_params), paste(log_params, collapse = ", ")))
  
  confidence_intervals <- list()
  anova_be_results <- list()
  
  for (param in log_params) {
    cat(sprintf("  Processing %s...\n", param))
    
    # Wrap each parameter processing in error handling
    tryCatch({
      anova_result <- anova_results[[param]]
    
    # Extract key ANOVA outputs for BE calculation
    model <- anova_result$model
    anova_table <- anova_result$anova
    mse <- anova_result$residual_mse
    df <- anova_result$residual_df
    n_observations <- anova_result$n_observations
    
    # Validate extracted values
    if (is.na(mse) || is.null(mse) || mse <= 0) {
      cat(sprintf("  ❌ Error: Invalid MSE (%s) for %s\n", mse, param))
      next
    }
    
    if (is.na(df) || is.null(df) || df <= 0) {
      cat(sprintf("  ❌ Error: Invalid degrees of freedom (%s) for %s\n", df, param))
      next
    }
    
    # Extract pre-calculated treatment difference from ANOVA results
    # This ensures consistent T/R ratio calculation
    treatment_diff <- anova_result$treatment_coef
    cat(sprintf("  [DEBUG] Using pre-calculated treatment_diff: %s\n", 
                if(is.na(treatment_diff)) "NA" else format(treatment_diff, digits=6)))
    
    # Check if treatment_diff is valid
    if (is.na(treatment_diff) || is.null(treatment_diff)) {
      cat(sprintf("  ⚠️  Warning: Invalid treatment effect for %s\n", param))
      next
    }
    
    # Use the pre-calculated standard error from ANOVA for imbalanced designs
    # This is more accurate than the simplified formula SE = sqrt(2 * MSE / n_subjects)
    treatment_se <- anova_result$treatment_se
    
    if (is.na(treatment_se) || is.null(treatment_se)) {
      cat(sprintf("  ⚠️  Warning: Invalid treatment SE for %s, using fallback calculation\n", param))
      
      # Fallback: Calculate standard error for balanced design
      # Extract number of subjects from the data structure
      tryCatch({
        if (anova_result$anova_method == "nlme") {
          # For nlme models, extract subjects from the grouping structure
          groups_info <- model$groups
          if (!is.null(groups_info) && "subject" %in% names(groups_info)) {
            n_subjects <- length(unique(groups_info$subject))
          } else {
            # Fallback: get from model frame
            model_frame <- model.frame(model)
            if ("subject" %in% names(model_frame)) {
              n_subjects <- length(unique(model_frame$subject))
            } else {
              # Last fallback: estimate from total observations
              n_subjects <- n_observations / 2
            }
          }
        } else {
          # For other model types, use model.frame approach
          model_frame <- model.frame(model)
          subject_var <- NULL
          
          # Look for subject variable in model frame
          if ("subject" %in% names(model_frame)) {
            subject_var <- "subject"
          } else if ("subj" %in% names(model_frame)) {
            subject_var <- "subj"
          } else {
            # Look for variables that might be subject IDs
            for (var_name in names(model_frame)) {
              if (grepl("subj", var_name, ignore.case = TRUE)) {
                subject_var <- var_name
                break
              }
            }
          }
          
          if (!is.null(subject_var)) {
            n_subjects <- length(unique(model_frame[[subject_var]]))
          } else {
            # Fallback: estimate from total observations
            n_subjects <- n_observations / 2
          }
        }
        
        # Validate n_subjects
        if (is.null(n_subjects) || is.na(n_subjects) || n_subjects <= 0) {
          n_subjects <- n_observations / 2
          cat(sprintf("  ⚠️  Warning: Using fallback subject count for %s: %d\n", param, n_subjects))
        }
        
      }, error = function(e) {
        cat(sprintf("  ⚠️  Warning: Error extracting subject count for %s: %s\n", param, e$message))
        n_subjects <- n_observations / 2
        cat(sprintf("  ⚠️  Using fallback subject count: %d\n", n_subjects))
      })
      
      # Fallback SE calculation (only accurate for balanced designs)
      treatment_se <- sqrt(2 * mse / n_subjects)
      cat(sprintf("  ⚠️  Using fallback SE calculation: %f\n", treatment_se))
    } else {
      cat(sprintf("  ✓ Using pre-calculated treatment SE from ANOVA: %f\n", treatment_se))
    }
    
    # Extract subject count for results reporting
    model <- anova_result$model
    n_subjects <- NA
    tryCatch({
      model_frame <- model.frame(model)
      subject_var <- NULL
      
      # Look for subject variable in model frame
      if ("subject" %in% names(model_frame)) {
        subject_var <- "subject"
      } else if ("subj" %in% names(model_frame)) {
        subject_var <- "subj"
      } else {
        # Look for variables that might be subject IDs
        for (var_name in names(model_frame)) {
          if (grepl("subj", var_name, ignore.case = TRUE)) {
            subject_var <- var_name
            break
          }
        }
      }
      
      if (!is.null(subject_var)) {
        n_subjects <- length(unique(model_frame[[subject_var]]))
      } else {
        # Fallback: estimate from total observations
        n_subjects <- n_observations / 2
      }
    }, error = function(e) {
      n_subjects <- n_observations / 2
    })
    
    # Calculate 90% confidence interval for the difference in log means
    # CI = (Ln(Mean_T) - Ln(Mean_R)) ± t * (standard error of the difference)
    confidence_level <- 1 - alpha
    
    # Validate degrees of freedom
    if (is.na(df) || is.null(df) || df <= 0) {
      cat(sprintf("  ❌ Error: Invalid degrees of freedom (%s) for %s\n", df, param))
      next
    }
    
    t_critical <- qt(1 - alpha/2, df)
    ci_lower_log <- treatment_diff - t_critical * treatment_se
    ci_upper_log <- treatment_diff + t_critical * treatment_se
    
    # Convert to geometric scale (ratio scale) and express as percentages
    # Point estimate = exp(treatment_diff) = Geometric Mean Ratio
    point_estimate <- exp(treatment_diff) * 100
    ci_lower <- exp(ci_lower_log) * 100
    ci_upper <- exp(ci_upper_log) * 100
    
    # Debug output for BE calculation values
    cat(sprintf("  [DEBUG] %s BE calculation values:\n", param))
    cat(sprintf("    treatment_diff: %s\n", if(is.na(treatment_diff)) "NA" else format(treatment_diff, digits=6)))
    cat(sprintf("    mse: %s\n", if(is.na(mse)) "NA" else format(mse, digits=6)))
    cat(sprintf("    treatment_se: %s\n", if(is.na(treatment_se)) "NA" else format(treatment_se, digits=6)))
    cat(sprintf("    df: %s\n", if(is.na(df) || df <= 0) "NA" else as.character(round(df))))
    cat(sprintf("    t_critical: %s\n", if(is.na(t_critical)) "NA" else format(t_critical, digits=6)))
    cat(sprintf("    point_estimate: %s\n", if(is.na(point_estimate)) "NA" else format(point_estimate, digits=2)))
    cat(sprintf("    ci_lower: %s\n", if(is.na(ci_lower)) "NA" else format(ci_lower, digits=2)))
    cat(sprintf("    ci_upper: %s\n", if(is.na(ci_upper)) "NA" else format(ci_upper, digits=2)))
    
    # Check for invalid values before evaluating bioequivalence
    if (is.na(ci_lower) || is.na(ci_upper) || is.infinite(ci_lower) || is.infinite(ci_upper)) {
      cat(sprintf("  ⚠️  Warning: Invalid confidence interval values for %s - skipping BE evaluation\n", param))
      within_limits <- FALSE
    } else {
      # Validate BE limits before evaluation
      if (is.null(be_limits) || length(be_limits) < 2 || 
          is.na(be_limits[1]) || is.na(be_limits[2])) {
        cat(sprintf("  ⚠️  Warning: Invalid BE limits - using default 80-125%%\n"))
        lower_limit <- 80.0
        upper_limit <- 125.0
      } else {
        # Convert decimal limits to percentage if needed
        if (max(be_limits, na.rm = TRUE) <= 10) {
          lower_limit <- be_limits[1] * 100
          upper_limit <- be_limits[2] * 100
        } else {
          lower_limit <- be_limits[1]
          upper_limit <- be_limits[2]
        }
      }
      
      # Evaluate bioequivalence with robust error handling
      tryCatch({
        condition1 <- ci_lower >= lower_limit
        condition2 <- ci_upper <= upper_limit
        
        if (is.na(condition1) || is.na(condition2)) {
          cat(sprintf("  ⚠️  Warning: NA conditions in BE evaluation for %s\n", param))
          within_limits <- FALSE
        } else {
          within_limits <- condition1 && condition2
        }
      }, error = function(e) {
        cat(sprintf("  ❌ Error in BE evaluation for %s: %s\n", param, e$message))
        within_limits <- FALSE
      })
    }
    
    # Store confidence interval with correct BE calculations
    confidence_intervals[[param]] <- list(
      parameter = param,
      point_estimate = point_estimate,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      confidence_level = confidence_level * 100,
      log_difference = treatment_diff,        # Corrected: Ln(Mean_T) - Ln(Mean_R)
      log_ci_lower = ci_lower_log,
      log_ci_upper = ci_upper_log,
      standard_error = treatment_se,            # Corrected: SE from ANOVA model
      degrees_freedom = df,
      within_limits = within_limits,
      # Additional BE-specific information
      geometric_mean_ratio = exp(treatment_diff),  # Raw GMR (not as percentage)
      mse = mse,                            # MSE from ANOVA
      n_subjects = n_subjects,              # Number of subjects
      t_critical = t_critical,              # t-value used
      anova_method = anova_result$anova_method %||% "fixed"
    )
    
    # Store ANOVA-based results (point to original ANOVA results)
    anova_be_results[[param]] <- anova_result
    
    cat(sprintf("  ✅ %s: %.1f%% (%.1f%% - %.1f%%) [%s]\n", 
                param, point_estimate, ci_lower, ci_upper,
                if(within_limits) "BE" else "Not BE"))
    
    }, error = function(e) {
      cat(sprintf("  ❌ Error in BE Analysis for %s: %s\n", param, e$message))
      # Store error result
      confidence_intervals[[param]] <<- list(
        parameter = param,
        error = e$message,
        point_estimate = NA,
        ci_lower = NA,
        ci_upper = NA,
        within_limits = FALSE
      )
    })
  }
  
  # Generate bioequivalence conclusions
  be_conclusions <- evaluate_bioequivalence(confidence_intervals, be_limits)
  
  return(list(
    confidence_intervals = confidence_intervals,
    anova_results = anova_be_results,
    be_conclusions = be_conclusions,
    method = "ANOVA-based analysis",
    design = "2x2x2 crossover",
    limits = be_limits,
    alpha = alpha
  ))
}

#' Simplified analyze_crossover_parameter that uses ANOVA module results
#' This function is now just a placeholder for backward compatibility
#' The actual analysis is done by extract_be_from_anova
analyze_crossover_parameter <- function(data, parameter, alpha, be_limits, anova_model = "fixed") {
  # This function is now deprecated in favor of using ANOVA module results
  # It's kept for backward compatibility but should not be used directly
  stop("analyze_crossover_parameter is deprecated. Use extract_be_from_anova with ANOVA module results instead.")
}

# =============================================================================
# SECTION 3: ANALYSIS FUNCTIONS FOR SPECIFIC DESIGNS  
# =============================================================================

#' Analyze Single Parameter for Parallel Design (Geometric Mean Ratio Method)
#'
#' @param data Study data
#' @param parameter Parameter name
#' @param alpha Significance level
#' @return Parameter analysis results
analyze_parallel_parameter <- function(data, parameter, alpha) {
  
  # Prepare parameter data
  param_data <- prepare_parameter_data(data, parameter)
  
  if (nrow(param_data) < 4) {
    warning("Insufficient data for parameter: ", parameter)
    return(NULL)
  }
  
  # Separate test and reference data
  test_data <- subset(param_data, Formulation == "Test" | Formulation == "T")
  ref_data <- subset(param_data, Formulation == "Reference" | Formulation == "R")
  
  if (nrow(test_data) == 0 || nrow(ref_data) == 0) {
    warning("Missing test or reference data for parameter: ", parameter)
    return(NULL)
  }
  
  # Geometric Mean Ratio approach (same as 2x2 crossover but with parallel data)
  # Use pooled variance approach for parallel design
  
  # Calculate geometric means
  test_geom_mean <- exp(mean(test_data$log_param, na.rm = TRUE))
  ref_geom_mean <- exp(mean(ref_data$log_param, na.rm = TRUE))
  
  # Calculate log geometric mean difference (point estimate on log scale)
  log_pe <- mean(test_data$log_param, na.rm = TRUE) - mean(ref_data$log_param, na.rm = TRUE)
  
  # Calculate pooled variance for parallel design
  n_test <- nrow(test_data)
  n_ref <- nrow(ref_data)
  
  var_test <- var(test_data$log_param, na.rm = TRUE)
  var_ref <- var(ref_data$log_param, na.rm = TRUE)
  
  # Pooled variance for parallel design
  pooled_var <- ((n_test - 1) * var_test + (n_ref - 1) * var_ref) / (n_test + n_ref - 2)
  
  # Standard error for parallel design
  se <- sqrt(pooled_var * (1/n_test + 1/n_ref))
  
  # Degrees of freedom
  df <- n_test + n_ref - 2
  
  # t-value for 90% CI
  t_val <- qt(1 - alpha, df)
  
  # Confidence interval on log scale
  log_ci_lower <- log_pe - t_val * se
  log_ci_upper <- log_pe + t_val * se
  
  # Transform to geometric scale (as percentages)
  point_estimate <- exp(log_pe) * 100
  ci_lower <- exp(log_ci_lower) * 100
  ci_upper <- exp(log_ci_upper) * 100
  
  # Calculate t-statistic for testing
  t_statistic <- log_pe / se
  p_value <- 2 * (1 - pt(abs(t_statistic), df))
  
  # Prepare results with both scales
  ci_result <- list(
    # Geometric scale (traditional BE presentation)
    point_estimate = point_estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    confidence_level = (1 - 2 * alpha) * 100,
    
    # Log scale (regulatory assessment scale)
    log_point_estimate = log_pe,
    log_ci_lower = log_ci_lower,
    log_ci_upper = log_ci_upper,
    
    # Statistical details
    t_statistic = t_statistic,
    df = df,
    p_value = p_value,
    
    # Scale identification
    scale = "both"
  )
  
  stats_result <- list(
    pooled_variance = pooled_var,
    standard_error = se,
    p_value = p_value,
    degrees_freedom = df,
    n_test = n_test,
    n_ref = n_ref,
    test_geom_mean = test_geom_mean,
    ref_geom_mean = ref_geom_mean
  )
  
  return(list(
    ci = ci_result,
    stats = stats_result
  ))
}

#' Analyze Single Parameter for Replicate Design
#'
#' @param data Study data with sequence information
#' @param parameter Parameter name
#' @param alpha Significance level
#' @param scaling Whether to use scaled bioequivalence
#' @return Parameter analysis results
analyze_replicate_parameter <- function(data, parameter, alpha, scaling) {
  
  # Prepare parameter data
  param_data <- prepare_parameter_data(data, parameter)
  
  if (nrow(param_data) < 8) {
    warning("Insufficient data for replicate analysis: ", parameter)
    return(NULL)
  }
  
  # Detect replicate design structure
  design_info <- detect_replicate_design(param_data)
  
  # Perform mixed-effects analysis
  mixed_result <- perform_replicate_mixed_effects(param_data, parameter)
  
  # Calculate within-subject variability with design-specific handling
  variability <- calculate_within_subject_variability(mixed_result, design_info)
  
  # Determine if scaling should be applied
  use_scaling <- scaling && (variability$cv_wr > 30)
  
  # Calculate confidence interval (scaled or unscaled)
  ci_result <- if (use_scaling) {
    calculate_scaled_ci(mixed_result, variability, alpha)
  } else {
    calculate_unscaled_ci(mixed_result, alpha)
  }
  
  return(list(
    ci = ci_result,
    variability = variability,
    mixed_effects = mixed_result,
    scaling_applied = use_scaling,
    design_info = design_info
  ))
}

# =============================================================================
# SECTION 3: STATISTICAL COMPUTATION FUNCTIONS
# =============================================================================

#' Perform ANOVA for Crossover Design
#'
#' Calculate Confidence Interval for Crossover Design
#'
#' @param anova_result ANOVA results
#' @param alpha Significance level
#' @return Confidence interval
calculate_crossover_ci <- function(anova_result, alpha) {
  
  # Calculate t-value
  t_value <- qt(0.95, anova_result$df)
  
  # Calculate confidence interval on log scale
  ci_lower_log <- anova_result$formulation_effect - t_value * anova_result$standard_error
  ci_upper_log <- anova_result$formulation_effect + t_value * anova_result$standard_error
  
  # Convert to ratio scale (percentage)
  point_estimate <- exp(anova_result$formulation_effect) * 100
  ci_lower <- exp(ci_lower_log) * 100
  ci_upper <- exp(ci_upper_log) * 100
  
  return(list(
    point_estimate = point_estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    confidence_level = (1 - alpha) * 100
  ))
}

#' Perform Mixed-Effects Analysis for Replicate Design  
#'
#' @param param_data Parameter data
#' @param parameter Parameter name
#' @return Mixed-effects model results
perform_replicate_mixed_effects <- function(param_data, parameter) {
  
  # Require nlme for mixed-effects modeling
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("Package 'nlme' is required for replicate design analysis")
  }
  
  # Ensure proper factor levels
  param_data$Subject <- as.factor(param_data$Subject)
  param_data$Period <- as.factor(param_data$Period)
  param_data$Formulation <- as.factor(param_data$Formulation)
  
  # Add sequence information if not present
  if (!"Sequence" %in% names(param_data)) {
    param_data <- add_sequence_info_replicate(param_data)
  }
  param_data$Sequence <- as.factor(param_data$Sequence)
  
  # Fit mixed-effects model
  model <- nlme::lme(
    log_param ~ Formulation + Period + Sequence,
    random = ~ 1 | Subject,
    data = param_data,
    method = "REML"
  )
  
  return(list(
    model = model,
    fixed_effects = nlme::fixed.effects(model),
    random_effects = nlme::random.effects(model),
    variance_components = nlme::VarCorr(model)
  ))
}

# =============================================================================
# SECTION 4: DATA VALIDATION AND PREPARATION FUNCTIONS
# =============================================================================

#' Validate BE Study Data
#'
#' @param data Raw study data
#' @param design Expected study design
#' @return Validated and standardized data
validate_be_data <- function(data, design) {
  
  # Check basic data structure
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Data must be a non-empty data frame")
  }
  
  # Standardize column names
  data <- standardize_column_names(data)
  
  # Check required columns
  required_cols <- get_required_columns(design)
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns for ", design, " design: ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # Validate data quality
  data <- validate_data_quality(data, design)
  
  return(data)
}

#' Validate Data Quality - Simplified
#'
#' @param data Study data
#' @param design Study design
#' @param missing_data_method Method for handling missing data (simplified to complete cases only)
#' @return Quality-validated data
validate_data_quality <- function(data, design, missing_data_method = "complete") {
  
  # Use complete cases approach - remove rows with missing critical data
  critical_cols <- get_required_columns(design)
  data <- data[complete.cases(data[critical_cols]), ]
  
  if (missing_data_method != "complete") {
    cat("ℹ️ Missing data method simplified to complete cases approach\n")
  }
  
  # Validate subject consistency
  data <- validate_subject_consistency(data, design)
  
  # Validate formulation coding
  data <- validate_formulation_coding(data)
  
  # Validate numeric parameters
  data <- validate_numeric_parameters(data)
  
  return(data)
}

#' Validate PK Parameters
#'
#' @param data Study data
#' @param parameters Requested parameters
#' @return Available validated parameters
validate_pk_parameters <- function(data, parameters) {
  
  pk_params <- intersect(parameters, names(data))
  
  if (length(pk_params) == 0) {
    stop("No PK parameters found in data. Available columns: ", 
         paste(names(data), collapse = ", "))
  }
  
  cat("Found PK parameters:", paste(pk_params, collapse = ", "), "\n")
  return(pk_params)
}

#' Prepare Parameter Data for Analysis
#'
#' @param data Study data
#' @param parameter Parameter name
#' @return Prepared parameter data
prepare_parameter_data <- function(data, parameter) {
  
  # Ensure parameter exists
  if (!parameter %in% names(data)) {
    stop("Parameter '", parameter, "' not found in data")
  }
  
  # Convert parameter to numeric if needed
  param_values <- data[[parameter]]
  if (!is.numeric(param_values)) {
    cat("  Converting", parameter, "to numeric...\n")
    param_values <- as.numeric(as.character(param_values))
    data[[parameter]] <- param_values
  }
  
  # Filter data for the parameter (remove NA and non-positive values)
  param_data <- data[!is.na(param_values) & param_values > 0, ]
  
  if (nrow(param_data) == 0) {
    stop("No valid data for parameter '", parameter, "' (all values are NA or <= 0)")
  }
  
  # Add log-transformed parameter (now safe since we validated numeric > 0)
  param_data$log_param <- log(param_data[[parameter]])
  
  # Ensure proper factor levels
  param_data$Subject <- as.factor(param_data$Subject)
  param_data$Formulation <- as.factor(param_data$Formulation)
  
  if ("Period" %in% names(param_data)) {
    param_data$Period <- as.factor(param_data$Period)
  }
  
  cat("  Prepared", nrow(param_data), "observations for", parameter, "\n")
  return(param_data)
}

#' Prepare Log Parameter Data for Analysis (No Additional Transformation)
#'
#' @param data Study data
#' @param parameter Log parameter name (e.g., lnCmax, lnAUC0t)
#' @return Prepared parameter data
prepare_log_parameter_data <- function(data, parameter) {
  
  # Ensure parameter exists
  if (!parameter %in% names(data)) {
    stop("Log parameter '", parameter, "' not found in data")
  }
  
  # Filter data for the parameter (remove NA values)
  param_values <- data[[parameter]]
  param_data <- data[!is.na(param_values), ]
  
  if (nrow(param_data) == 0) {
    stop("No valid data for log parameter '", parameter, "' (all values are NA)")
  }
  
  # Ensure proper factor levels
  param_data$Subject <- as.factor(param_data$Subject)
  param_data$Formulation <- as.factor(param_data$Formulation)
  
  if ("Period" %in% names(param_data)) {
    param_data$Period <- as.factor(param_data$Period)
  }
  
  cat(sprintf("  Prepared %d observations for log parameter %s\n", nrow(param_data), parameter))
  return(param_data)
}

# =============================================================================
# SECTION 5: UTILITY AND HELPER FUNCTIONS
# =============================================================================

#' Detect Study Design from Data
#'
#' @param data Study data
#' @return Detected design
detect_study_design <- function(data) {
  
  # Count observations per subject
  obs_per_subject <- table(data$Subject)
  unique_obs_counts <- unique(obs_per_subject)
  
  # Check for period column existence
  has_period <- "Period" %in% names(data)
  
  if (has_period) {
    max_periods <- max(as.numeric(as.character(data$Period)), na.rm = TRUE)
    
    if (max_periods == 2 && all(obs_per_subject == 2)) {
      return("2x2x2")
    } else if (max_periods > 2) {
      return("replicate")
    }
  }
  
  # If no period or single observation per subject
  if (all(obs_per_subject == 1)) {
    return("parallel")
  }
  
  # Default fallback
  warning("Could not auto-detect design, defaulting to parallel")
  return("parallel")
}

#' Create Analysis Configuration
#'
#' @param regulatory_standard Regulatory standard
#' @param custom_config Custom configuration
#' @return Analysis configuration
create_analysis_config <- function(regulatory_standard, custom_config) {
  
  base_configs <- list(
    FDA = list(
      alpha = 0.05,
      be_limits = c(0.8, 1.25),
      confidence_level = 90,
      primary_params = c("AUC0t", "AUC0inf", "Cmax"),
      scaling_threshold = 30,
      scaling_cap = c(0.8, 1.25),
      tmax_required = FALSE
    ),
    EMA = list(
      alpha = 0.05,
      be_limits = c(0.8, 1.25),
      confidence_level = 90,
      primary_params = c("AUC0t", "AUC0inf", "Cmax"),
      scaling_threshold = 30,
      scaling_cap = c(0.69, 1.43),
      tmax_required = FALSE
    ),
    ICH = list(
      alpha = 0.05,
      be_limits = c(0.8, 1.25),
      confidence_level = 90,
      primary_params = c("AUC0t", "AUC0inf", "Cmax"),
      scaling_threshold = 30,
      scaling_cap = c(0.8, 1.25),
      tmax_required = FALSE
    )
  )
  
  config <- base_configs[[regulatory_standard]]
  
  # Override with custom settings
  if (!is.null(custom_config)) {
    config <- modifyList(config, custom_config)
  }
  
  config$regulatory_standard <- regulatory_standard
  return(config)
}

#' Evaluate Bioequivalence
#'
#' @param confidence_intervals CI results for all parameters
#' @param be_limits Bioequivalence limits
#' @return Bioequivalence conclusions
evaluate_bioequivalence <- function(confidence_intervals, be_limits) {
  
  # Filter out invalid parameter names (NA, NULL, empty)
  valid_params <- names(confidence_intervals)
  valid_params <- valid_params[!is.na(valid_params) & !is.null(valid_params) & valid_params != ""]
  confidence_intervals <- confidence_intervals[valid_params]
  
  cat("🔍 Evaluating bioequivalence for", length(confidence_intervals), "parameters...\n")
  
  be_conclusions <- list()
  
  # Handle BE limits - check if they're already in percentage or decimal format
  # Default to standard ABE limits if be_limits is null or invalid
  if (is.null(be_limits) || all(is.na(be_limits)) || length(be_limits) == 0) {
    cat("⚠️ BE limits not provided or invalid. Using default ABE limits: 80.00% - 125.00%\n")
    lower_limit <- 80.0
    upper_limit <- 125.0
  } else if (is.list(be_limits) && "lower" %in% names(be_limits)) {
    # Already in percentage format from the analysis setup
    lower_limit <- be_limits$lower
    upper_limit <- be_limits$upper
  } else if (is.numeric(be_limits) && max(be_limits, na.rm = TRUE) > 10) {
    # Already in percentage format
    lower_limit <- ifelse(length(be_limits) >= 1, be_limits[1], 80.0)
    upper_limit <- ifelse(length(be_limits) >= 2, be_limits[2], 125.0)
  } else if (is.numeric(be_limits) && length(be_limits) >= 2) {
    # Decimal format - convert to percentage
    lower_limit <- be_limits[1] * 100
    upper_limit <- be_limits[2] * 100
  } else {
    # Fallback to default limits
    cat("⚠️ Invalid BE limits format. Using default ABE limits: 80.00% - 125.00%\n")
    lower_limit <- 80.0
    upper_limit <- 125.0
  }
  
  cat(sprintf("📏 BE limits: %.1f%% - %.1f%%\n", lower_limit, upper_limit))
  
  for (param in names(confidence_intervals)) {
    ci <- confidence_intervals[[param]]
    
    # Debug the CI structure
    cat(sprintf("🔍 Evaluating %s:\n", param))
    cat(sprintf("  CI structure: %s\n", paste(names(ci), collapse = ", ")))
    cat(sprintf("  ci_lower value: %s (is.na: %s)\n", ci$ci_lower, is.na(ci$ci_lower)))
    cat(sprintf("  ci_upper value: %s (is.na: %s)\n", ci$ci_upper, is.na(ci$ci_upper)))
    if ("log_ci_lower" %in% names(ci)) {
      cat(sprintf("  log_ci_lower value: %s (is.na: %s)\n", ci$log_ci_lower, is.na(ci$log_ci_lower)))
      cat(sprintf("  log_ci_upper value: %s (is.na: %s)\n", ci$log_ci_upper, is.na(ci$log_ci_upper)))
    }
    
    # Handle different CI structure possibilities
    ci_lower <- NULL
    ci_upper <- NULL
    point_est <- NULL
    
    # Try to extract confidence interval bounds
    if ("ci_lower" %in% names(ci) && !is.na(ci$ci_lower)) {
      ci_lower <- ci$ci_lower
      ci_upper <- ci$ci_upper
      point_est <- ci$point_estimate
    } else if ("log_ci_lower" %in% names(ci) && !is.na(ci$log_ci_lower)) {
      # Use log-scale CIs if regular CIs are NA (for log-transformed parameters)
      ci_lower <- exp(ci$log_ci_lower) * 100  # Convert from log scale to percentage
      ci_upper <- exp(ci$log_ci_upper) * 100
      point_est <- exp(ci$log_point_estimate) * 100
      cat(sprintf("  📊 Using log-scale CI values converted to %%: lower=%.2f, upper=%.2f\n", 
                  ci_lower, ci_upper))
    } else if (length(ci) >= 2 && is.numeric(ci)) {
      # Handle vector format
      ci_lower <- ci[1]
      ci_upper <- ci[2]
      point_est <- mean(c(ci_lower, ci_upper))
    } else {
      cat(sprintf("  ⚠️ Unknown CI structure for %s\n", param))
      cat(sprintf("  ⚠️ Available fields: %s\n", paste(names(ci), collapse = ", ")))
      be_conclusions[[param]] <- NA
      next
    }
    
    # Validate CI values before BE evaluation
    if (is.null(ci_lower) || is.null(ci_upper) || is.na(ci_lower) || is.na(ci_upper)) {
      cat(sprintf("  ⚠️ Invalid CI values for %s: lower=%s, upper=%s\n", 
                  param, ci_lower, ci_upper))
      be_conclusions[[param]] <- NA
      next
    }
    
    # Evaluate bioequivalence with robust error handling
    # Validate all values before logical operations
    if (is.null(lower_limit) || is.null(upper_limit) || is.na(lower_limit) || is.na(upper_limit)) {
      cat(sprintf("  ⚠️ Invalid BE limits for %s: lower=%s, upper=%s\n", 
                  param, lower_limit, upper_limit))
      be_conclusions[[param]] <- NA
      next
    }
    
    tryCatch({
      condition1 <- ci_lower >= lower_limit
      condition2 <- ci_upper <= upper_limit
      
      if (is.na(condition1) || is.na(condition2)) {
        cat(sprintf("  ⚠️ NA conditions in BE evaluation for %s\n", param))
        is_be <- FALSE
      } else {
        is_be <- condition1 && condition2
      }
      
      cat(sprintf("  ✅ %s: CI [%.1f%%, %.1f%%] vs Limits [%.1f%%, %.1f%%] → %s\n", 
                  param, ci_lower, ci_upper, lower_limit, upper_limit,
                  ifelse(is_be, "Bioequivalent", "Not Bioequivalent")))
      
    }, error = function(e) {
      cat(sprintf("  ❌ Error in BE evaluation for %s: %s\n", param, e$message))
      is_be <- FALSE
    })
    
    be_conclusions[[param]] <- is_be
    
    status <- ifelse(is_be, "✅ BIOEQUIVALENT", "❌ NOT BIOEQUIVALENT")
    cat(sprintf("  %s: %.1f%% (%.1f%% - %.1f%%) %s\n", 
                param, point_est, ci_lower, ci_upper, status))
  }
  
  # Summary
  valid_conclusions <- be_conclusions[!is.na(be_conclusions)]
  if (length(valid_conclusions) > 0) {
    be_count <- sum(unlist(valid_conclusions), na.rm = TRUE)
    total_count <- length(valid_conclusions)
    cat(sprintf("📊 Summary: %d of %d parameters are bioequivalent\n", 
                be_count, total_count))
  }
  
  # Debug: Print all BE conclusions
  cat("🔍 BE Conclusions Debug:\n")
  for (param_name in names(be_conclusions)) {
    conclusion_value <- be_conclusions[[param_name]]
    cat(sprintf("  %s: %s (class: %s)\n", param_name, conclusion_value, class(conclusion_value)))
  }
  
  return(be_conclusions)
}

#' Create BE Results Object
#'
#' @param design Study design
#' @param data Study data
#' @param ... Additional result components
#' @return Standardized BE results object
create_be_results <- function(design, data, ...) {
  
  result <- list(
    design = design,
    data = data,
    n_subjects = length(unique(data$Subject)),
    ...
  )
  
  class(result) <- c("bioeq", "list")
  return(result)
}

# =============================================================================
# SECTION 8: ORIGINAL LEGACY AND PLACEHOLDER FUNCTIONS
# =============================================================================

#' Prepare NCA Data for Statistical Analysis
#'
#' @param nca_results NCA results data frame
#' @param parameters Parameters to include
#' @return Prepared data for statistical analysis
#' @export
prepare_nca_data <- function(nca_results, parameters) {
  # Select relevant columns
  id_cols <- c("subj", "tmt")
  param_cols <- intersect(parameters, names(nca_results))
  
  stats_data <- nca_results[, c(id_cols, param_cols), drop = FALSE]
  
  # Add log-transformed parameters for multiplicative model
  for (param in param_cols) {
    if (is.numeric(stats_data[[param]])) {
      log_param_name <- paste0("ln_", param)
      stats_data[[log_param_name]] <- ifelse(stats_data[[param]] > 0, 
                                            log(stats_data[[param]]), NA)
    }
  }
  
  # Add sequence and period information (reconstruct from crossover design)
  # In 2x2x2: subjects 1-n/2 get TR sequence, n/2+1-n get RT sequence
  n_subjects <- length(unique(stats_data$subj))
  
  # Create sequence assignment
  stats_data$seq <- NA
  for (subj in unique(stats_data$subj)) {
    subj_data <- stats_data[stats_data$subj == subj, ]
    treatments <- sort(unique(subj_data$tmt))
    
    if (length(treatments) == 2 && all(treatments == c(1, 2))) {
      # Determine sequence based on subject ID (simplified)
      # In practice, this should come from the original data
      if (as.numeric(subj) <= n_subjects/2) {
        stats_data[stats_data$subj == subj & stats_data$tmt == 2, "period"] <- 1
        stats_data[stats_data$subj == subj & stats_data$tmt == 1, "period"] <- 2
        stats_data[stats_data$subj == subj, "seq"] <- 1  # TR sequence
      } else {
        stats_data[stats_data$subj == subj & stats_data$tmt == 1, "period"] <- 1
        stats_data[stats_data$subj == subj & stats_data$tmt == 2, "period"] <- 2
        stats_data[stats_data$subj == subj, "seq"] <- 2  # RT sequence
      }
    }
  }
  
  # Convert to factors
  stats_data$subj <- as.factor(stats_data$subj)
  stats_data$tmt <- as.factor(stats_data$tmt)
  stats_data$seq <- as.factor(stats_data$seq)
  stats_data$period <- as.factor(stats_data$period)
  
  return(stats_data)
}

#' Analyze Crossover Parameter (Legacy Implementation)
#'
#' @param data Study data
#' @param parameter Parameter name
#' @param alpha Significance level
#' @param be_limits Bioequivalence limits
#' @return Parameter analysis results
analyze_crossover_parameter_legacy <- function(data, parameter, alpha, be_limits) {
  
  # Create analysis-ready data
  param_data <- data[!is.na(data[[parameter]]) & data[[parameter]] > 0, ]
  
  if (nrow(param_data) >= 4) {
    # Ensure parameter is numeric before log transformation
    if (!is.numeric(param_data[[parameter]])) {
      cat("  Converting", parameter, "to numeric in legacy function...\n")
      param_data[[parameter]] <- as.numeric(as.character(param_data[[parameter]]))
      # Re-filter after conversion
      param_data <- param_data[!is.na(param_data[[parameter]]) & param_data[[parameter]] > 0, ]
    }
    
    if (nrow(param_data) == 0) {
      warning("No valid numeric data for parameter: ", parameter)
      return(NULL)
    }
    
    # Log-transform for multiplicative model (now safe)
    param_data$log_param <- log(param_data[[parameter]])
    
    # Simple ANOVA model (crossover design)
    formula_str <- "log_param ~ Formulation + Subject + Period"
    
    # Check if we have required variables
    if (all(c("Formulation", "Subject", "Period") %in% names(param_data))) {
      
      # Convert to factors
      param_data$Formulation <- as.factor(param_data$Formulation)
      param_data$Subject <- as.factor(param_data$Subject)
      param_data$Period <- as.factor(param_data$Period)
      
      # Fit model
      model <- lm(log_param ~ Formulation + Subject + Period, data = param_data)
      
      # Extract treatment effect (Test vs Reference)
      formulation_coef <- coef(model)["FormulationTest"]
      if (is.na(formulation_coef)) {
        # Try the other way around
        formulation_coef <- -coef(model)["FormulationReference"]
      }
      
      if (!is.na(formulation_coef)) {
        # Get standard error
        model_summary <- summary(model)
        se <- model_summary$coefficients["FormulationTest", "Std. Error"]
        if (is.na(se)) {
          se <- model_summary$coefficients["FormulationReference", "Std. Error"]
        }
        
        # Calculate confidence interval
        df <- model$df.residual
        t_value <- qt(0.95, df)
        
        ci_lower_log <- formulation_coef - t_value * se
        ci_upper_log <- formulation_coef + t_value * se
        
        # Convert to ratio scale (percentage)
        point_estimate <- exp(formulation_coef) * 100
        ci_lower <- exp(ci_lower_log) * 100
        ci_upper <- exp(ci_upper_log) * 100
        
        # Store results
        return(list(
          point_estimate = point_estimate,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          confidence_level = (1 - alpha) * 100,
          anova_model = model,
          formulation_effect = formulation_coef,
          standard_error = se,
          df = df,
          n_subjects = length(unique(param_data$Subject))
        ))
      }
    }
  }
  
  return(NULL)
}

# =============================================================================
# SECTION 7: HELPER FUNCTIONS FOR STATISTICAL COMPUTATIONS
# =============================================================================

#' Extract Formulation Effect from Model
#'
#' @param model Linear model object
#' @return List with formulation effect value and name
extract_formulation_effect <- function(model) {
  coefs <- coef(model)
  
  # Try different formulation coefficient names
  formulation_names <- c("FormulationTest", "FormulationT", "FormulationReference", "FormulationR")
  
  for (name in formulation_names) {
    if (name %in% names(coefs) && !is.na(coefs[name])) {
      value <- coefs[name]
      # Adjust sign if needed (we want Test - Reference)
      if (grepl("Reference|R$", name)) {
        value <- -value
      }
      return(list(value = value, name = name))
    }
  }
  
  stop("Could not find formulation effect in model coefficients")
}

#' Get Standard Error from Model Summary
#'
#' @param model_summary Model summary object
#' @param coef_name Coefficient name
#' @return Standard error
get_standard_error <- function(model_summary, coef_name) {
  se <- model_summary$coefficients[coef_name, "Std. Error"]
  if (is.na(se)) {
    stop("Could not extract standard error for coefficient: ", coef_name)
  }
  return(se)
}

#' Validate Subject Consistency
#'
#' @param data Study data
#' @param design Study design
#' @return Validated data
validate_subject_consistency <- function(data, design) {
  # Check for proper subject-period combinations
  if (design == "2x2x2") {
    # Each subject should have exactly 2 observations
    obs_per_subject <- table(data$Subject)
    invalid_subjects <- names(obs_per_subject)[obs_per_subject != 2]
    
    if (length(invalid_subjects) > 0) {
      warning("Removing subjects with != 2 observations: ", paste(invalid_subjects, collapse = ", "))
      data <- data[!data$Subject %in% invalid_subjects, ]
    }
  } else if (design == "replicate") {
    # Check for replicate design requirements
    design_info <- detect_replicate_design(data)
    
    if (design_info$is_partial_replicate) {
      # For partial replicate, check reference formulation replication
      formulations <- unique(data$Formulation)
      ref_formulation <- if ("Reference" %in% formulations) {
        "Reference"
      } else if ("R" %in% formulations) {
        "R"
      } else {
        formulations[1]
      }
      
      ref_counts <- data %>%
        filter(Formulation == ref_formulation) %>%
        group_by(Subject) %>%
        summarise(n_obs = n(), .groups = "drop")
      
      subjects_with_ref_replicates <- sum(ref_counts$n_obs >= 2)
      
      if (subjects_with_ref_replicates == 0) {
        warning("Partial replicate design requires subjects with multiple reference observations")
      }
    }
  }
  
  return(data)
}

#' Get Required Columns by Design
get_required_columns <- function(design) {
  switch(design,
    "2x2x2" = c("Subject", "Period", "Formulation"),
    "parallel" = c("Subject", "Formulation"),
    "replicate" = c("Subject", "Period", "Formulation"),
    "auto" = c("Subject", "Formulation")  # Minimum for auto-detection
  )
}

#' Standardize Column Names
#' 
#' Handles common variations in column naming
standardize_column_names <- function(data) {
  
  # Column name mappings
  name_mappings <- list(
    Subject = c("SUBJECT", "ID", "USUBJID", "subj"),
    Period = c("PERIOD", "PER", "period"),
    Formulation = c("FORMULATION", "TRT", "TREATMENT", "tmt", "Treatment"),
    Sequence = c("SEQUENCE", "SEQ", "seq")
  )
  
  # Apply mappings
  for (std_name in names(name_mappings)) {
    for (alt_name in name_mappings[[std_name]]) {
      if (alt_name %in% names(data) && !std_name %in% names(data)) {
        names(data)[names(data) == alt_name] <- std_name
        break
      }
    }
  }
  
  return(data)
}

#' Validate Numeric Parameters
#'
#' @param data Study data
#' @return Data with validated numeric parameters
validate_numeric_parameters <- function(data) {
  # Identify potential PK parameters (numeric columns excluding ID variables)
  id_cols <- c("Subject", "Period", "Formulation", "Sequence")
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  pk_cols <- setdiff(numeric_cols, id_cols)
  
  # Check for negative or zero values in PK parameters
  for (col in pk_cols) {
    if (any(data[[col]] <= 0, na.rm = TRUE)) {
      warning("Found non-positive values in ", col, " - these will be excluded from analysis")
    }
  }
  
  return(data)
}

#' Validate Formulation Coding
#'
#' @param data Study data
#' @return Data with validated formulation coding
validate_formulation_coding <- function(data) {
  # Standardize formulation levels
  if ("Formulation" %in% names(data)) {
    # Map common formulation codes
    data$Formulation <- as.character(data$Formulation)
    data$Formulation[data$Formulation %in% c("T", "Test", "1")] <- "Test"
    data$Formulation[data$Formulation %in% c("R", "Reference", "2")] <- "Reference"
    data$Formulation <- as.factor(data$Formulation)
    
    # Check that we have both levels
    levels_present <- levels(data$Formulation)
    if (!"Test" %in% levels_present || !"Reference" %in% levels_present) {
      warning("Missing Test or Reference formulation in data")
    }
  }
  
  return(data)
}

# =============================================================================
# SECTION 8: ADVANCED REPLICATE DESIGN FUNCTIONS
# =============================================================================

#' Detect Replicate Design Type and Structure
#'
#' @param data Study data with Subject, Period, Formulation columns
#' @return List with design information
detect_replicate_design <- function(data) {
  
  # Check if we have required columns
  if (!all(c("Subject", "Period", "Formulation") %in% names(data))) {
    stop("Data must contain Subject, Period, and Formulation columns")
  }
  
  # Analyze the structure without dplyr dependency
  subjects <- unique(data$Subject)
  subject_patterns <- data.frame(
    Subject = character(),
    pattern = character(),
    n_periods = numeric(),
    formulations = character(),
    stringsAsFactors = FALSE
  )
  
  # Build subject patterns manually
  for (subj in subjects) {
    subj_data <- data[data$Subject == subj, ]
    subj_data <- subj_data[order(subj_data$Period), ]
    
    pattern <- paste(subj_data$Formulation, collapse = "")
    n_periods <- nrow(subj_data)
    formulations <- paste(unique(subj_data$Formulation), collapse = ",")
    
    subject_patterns <- rbind(subject_patterns, data.frame(
      Subject = subj,
      pattern = pattern,
      n_periods = n_periods,
      formulations = formulations,
      stringsAsFactors = FALSE
    ))
  }
  
  # Get unique patterns
  unique_patterns <- unique(subject_patterns$pattern)
  n_periods <- max(subject_patterns$n_periods)
  
  # Determine design type
  design_info <- list()
  
  if (n_periods == 3) {
    # Check for 2x2x3 designs (TRR, RTT, etc.)
    if (any(grepl("TRR|RTT", unique_patterns))) {
      design_info$design_name <- "2x2x3 (Partial Replicate)"
      design_info$is_partial_replicate <- TRUE
      design_info$sequences <- unique_patterns
    } else if (any(grepl("TRT|RTR", unique_patterns))) {
      design_info$design_name <- "2x3x3 (Full Replicate)"
      design_info$is_partial_replicate <- FALSE
      design_info$sequences <- unique_patterns
    } else {
      design_info$design_name <- "3-Period Crossover"
      design_info$is_partial_replicate <- FALSE
      design_info$sequences <- unique_patterns
    }
  } else if (n_periods == 4) {
    # Check for 2x2x4 designs
    if (any(grepl("TRTR|RTRT", unique_patterns))) {
      design_info$design_name <- "2x2x4 (Full Replicate)"
      design_info$is_partial_replicate <- FALSE
      design_info$sequences <- unique_patterns
    } else {
      design_info$design_name <- "4-Period Crossover"
      design_info$is_partial_replicate <- FALSE
      design_info$sequences <- unique_patterns
    }
  } else {
    design_info$design_name <- paste0(n_periods, "-Period Crossover")
    design_info$is_partial_replicate <- FALSE
    design_info$sequences <- unique_patterns
  }
  
  design_info$n_periods <- n_periods
  design_info$n_subjects <- nrow(subject_patterns)
  design_info$sequence_distribution <- table(subject_patterns$pattern)
  
  return(design_info)
}

#' Add Sequence Information for Replicate Design
#'
#' @param data Study data
#' @return Data with sequence information added
add_sequence_info_replicate <- function(data) {
  
  # Determine sequence pattern from the data
  subject_patterns <- data %>%
    arrange(Subject, Period) %>%
    group_by(Subject) %>%
    summarise(
      pattern = paste(Formulation, collapse = ""),
      .groups = "drop"
    )
  
  # Create sequence mapping
  unique_patterns <- unique(subject_patterns$pattern)
  sequence_map <- setNames(1:length(unique_patterns), unique_patterns)
  
  subject_patterns$Sequence <- sequence_map[subject_patterns$pattern]
  
  # Merge back to original data
  data <- merge(data, subject_patterns[, c("Subject", "Sequence")], by = "Subject")
  
  return(data)
}

#' Calculate Within-Subject Variability for Replicate Design
#'
#' @param mixed_result Mixed-effects model results
#' @param design_info Design information
#' @return Variability measures including CV and variance components
calculate_within_subject_variability <- function(mixed_result, design_info = NULL) {
  
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("Package 'nlme' is required for replicate design analysis")
  }
  
  # Extract variance components from mixed-effects model
  var_corr <- nlme::VarCorr(mixed_result$model)
  
  # Within-subject variance (residual)
  sigma2_wr <- as.numeric(mixed_result$model$sigma^2)
  
  # Between-subject variance
  sigma2_br <- as.numeric(var_corr[1, "Variance"])
  if (is.na(sigma2_br)) sigma2_br <- 0
  
  # Calculate within-subject coefficient of variation
  swr <- sqrt(sigma2_wr)
  cv_wr <- sqrt(exp(sigma2_wr) - 1) * 100
  
  # Calculate between-subject coefficient of variation
  sbr <- sqrt(sigma2_br)
  cv_br <- sqrt(exp(sigma2_br) - 1) * 100
  
  # Total CV
  cv_total <- sqrt(exp(sigma2_wr + sigma2_br) - 1) * 100
  
  return(list(
    sigma2_wr = sigma2_wr,
    sigma2_br = sigma2_br,
    swr = swr,
    sbr = sbr,
    cv_wr = cv_wr,
    cv_br = cv_br,
    cv_total = cv_total,
    scaling_threshold_met = cv_wr > 30
  ))
}

#' Calculate Scaled Confidence Interval
#'
#' @param mixed_result Mixed-effects results
#' @param variability Variability measures
#' @param alpha Significance level
#' @param regulatory_cap Regulatory scaling cap (default EMA: c(0.69, 1.43))
#' @return Scaled confidence interval
calculate_scaled_ci <- function(mixed_result, variability, alpha, regulatory_cap = c(0.69, 1.43)) {
  
  # Extract formulation effect from mixed model
  fixed_effects <- nlme::fixed.effects(mixed_result$model)
  
  # Find formulation effect coefficient
  formulation_coef <- 0
  formulation_se <- 0
  
  coef_names <- names(fixed_effects)
  formulation_names <- c("FormulationTest", "FormulationT", "FormulationReference", "FormulationR")
  
  for (name in formulation_names) {
    if (name %in% coef_names) {
      formulation_coef <- fixed_effects[name]
      # Get standard error from summary
      model_summary <- summary(mixed_result$model)
      formulation_se <- model_summary$tTable[name, "Std.Error"]
      
      # Adjust sign if needed (we want Test - Reference)
      if (grepl("Reference|R$", name)) {
        formulation_coef <- -formulation_coef
      }
      break
    }
  }
  
  # Calculate degrees of freedom
  df <- mixed_result$model$fixDF$X[length(mixed_result$model$fixDF$X)]
  
  # Calculate scaled bioequivalence limits
  swr <- variability$swr
  scaled_lower <- exp(-log(1.25) * sqrt(swr^2 / 0.1))  # Scaled lower limit
  scaled_upper <- exp(log(1.25) * sqrt(swr^2 / 0.1))   # Scaled upper limit
  
  # Apply regulatory cap
  scaled_lower <- max(scaled_lower, regulatory_cap[1])
  scaled_upper <- min(scaled_upper, regulatory_cap[2])
  
  # Calculate confidence interval
  t_critical <- qt(0.95, df)
  ci_lower_log <- formulation_coef - t_critical * formulation_se
  ci_upper_log <- formulation_coef + t_critical * formulation_se
  
  # Back-transform to ratio scale
  point_estimate <- exp(formulation_coef) * 100
  ci_lower <- exp(ci_lower_log) * 100
  ci_upper <- exp(ci_upper_log) * 100
  
  # Check scaled bioequivalence
  is_bioequivalent <- (ci_lower >= scaled_lower * 100) && (ci_upper <= scaled_upper * 100)
  
  return(list(
    point_estimate = point_estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    confidence_level = (1 - alpha) * 100,
    scaled_limits = c(scaled_lower, scaled_upper),
    regulatory_cap = regulatory_cap,
    scaling_applied = TRUE,
    is_bioequivalent = is_bioequivalent,
    df = df,
    t_critical = t_critical
  ))
}

#' Calculate Unscaled Confidence Interval
#'
#' @param mixed_result Mixed-effects results
#' @param alpha Significance level
#' @param be_limits Standard bioequivalence limits
#' @return Unscaled confidence interval
calculate_unscaled_ci <- function(mixed_result, alpha, be_limits = c(0.8, 1.25)) {
  
  # Extract formulation effect from mixed model
  fixed_effects <- nlme::fixed.effects(mixed_result$model)
  
  # Find formulation effect coefficient
  formulation_coef <- 0
  formulation_se <- 0
  
  coef_names <- names(fixed_effects)
  formulation_names <- c("FormulationTest", "FormulationT", "FormulationReference", "FormulationR")
  
  for (name in formulation_names) {
    if (name %in% coef_names) {
      formulation_coef <- fixed_effects[name]
      # Get standard error from summary
      model_summary <- summary(mixed_result$model)
      formulation_se <- model_summary$tTable[name, "Std.Error"]
      
      # Adjust sign if needed (we want Test - Reference)
      if (grepl("Reference|R$", name)) {
        formulation_coef <- -formulation_coef
      }
      break
    }
  }
  
  # Calculate degrees of freedom
  df <- mixed_result$model$fixDF$X[length(mixed_result$model$fixDF$X)]
  
  # Calculate confidence interval
  t_critical <- qt(0.95, df)
  ci_lower_log <- formulation_coef - t_critical * formulation_se
  ci_upper_log <- formulation_coef + t_critical * formulation_se
  
  # Back-transform to ratio scale
  point_estimate <- exp(formulation_coef) * 100
  ci_lower <- exp(ci_lower_log) * 100
  ci_upper <- exp(ci_upper_log) * 100
  
  # Check standard bioequivalence
  is_bioequivalent <- (ci_lower >= be_limits[1] * 100) && (ci_upper <= be_limits[2] * 100)
  
  return(list(
    point_estimate = point_estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    confidence_level = (1 - alpha) * 100,
    be_limits = be_limits,
    scaling_applied = FALSE,
    is_bioequivalent = is_bioequivalent,
    df = df,
    t_critical = t_critical
  ))
}

# =============================================================================
# SECTION 9: COMPREHENSIVE ICH M13A REGULATORY EVALUATION
# =============================================================================

#' Apply ICH M13A Regulatory Evaluation
#'
#' @param results Statistical results
#' @param config Analysis configuration
#' @return Results with comprehensive regulatory assessment
apply_ich_m13a_evaluation <- function(results, config) {
  
  cat("🏛️ Applying ICH M13A regulatory evaluation...\n")
  
  # Create regulatory assessment for each parameter
  regulatory_assessment <- list()
  
  for (param in names(results$confidence_intervals)) {
    ci <- results$confidence_intervals[[param]]
    
    # ICH M13A specific evaluations
    param_assessment <- list(
      bioequivalence_conclusion = evaluate_bioequivalence_ich_m13a(ci, param, config),
      confidence_interval_evaluation = evaluate_ci_ich_m13a(ci, param, config),
      point_estimate_evaluation = evaluate_pe_ich_m13a(ci, param, config),
      variability_assessment = if (results$design == "replicate") {
        evaluate_variability_ich_m13a(results$variability_results[[param]], param, config)
      } else NULL,
      scaling_justification = if (results$design == "replicate" && !is.null(results$scaling_decisions[[param]])) {
        evaluate_scaling_decision_ich_m13a(results$scaling_decisions[[param]], param, config)
      } else NULL
    )
    
    # Parameter-specific assessments
    if (param %in% c("AUC0t", "AUC0inf")) {
      param_assessment$exposure_assessment <- "Primary exposure parameter - requires BE demonstration"
    }
    if (param == "Cmax") {
      param_assessment$cmax_assessment <- "Primary rate parameter - requires BE demonstration"
    }
    if (param == "Tmax") {
      param_assessment$tmax_assessment <- "Non-parametric analysis recommended for Tmax"
    }
    
    regulatory_assessment[[param]] <- param_assessment
  }
  
  # Overall study assessment
  overall_assessment <- list(
    study_design_adequacy = evaluate_design_adequacy_ich_m13a(results, config),
    sample_size_adequacy = evaluate_sample_size_adequacy_ich_m13a(results, config),
    primary_parameters_assessed = evaluate_primary_parameters_ich_m13a(results, config),
    regulatory_compliance = determine_overall_compliance_ich_m13a(regulatory_assessment, config),
    recommendations = generate_regulatory_recommendations_ich_m13a(regulatory_assessment, results, config)
  )
  
  # Add regulatory section to results
  results$regulatory_assessment <- list(
    standard = "ICH M13A",
    parameter_assessments = regulatory_assessment,
    overall_assessment = overall_assessment,
    evaluation_date = Sys.Date(),
    evaluator = "BioEQ Automated Assessment"
  )
  
  cat("✅ ICH M13A regulatory evaluation completed!\n")
  return(results)
}

#' Evaluate Bioequivalence Conclusion per ICH M13A
#'
#' @param ci Confidence interval results
#' @param parameter Parameter name
#' @param config Analysis configuration
#' @return ICH M13A bioequivalence evaluation
evaluate_bioequivalence_ich_m13a <- function(ci, parameter, config) {
  
  # ICH M13A specific criteria
  be_limits <- config$be_limits
  
  # Check if confidence interval is within acceptance limits
  ci_within_limits <- (ci$ci_lower >= be_limits[1] * 100) && (ci$ci_upper <= be_limits[2] * 100)
  
  # Check point estimate criteria (should be reasonably close to 100%)
  pe_acceptable <- (ci$point_estimate >= 80) && (ci$point_estimate <= 125)
  
  # ICH M13A conclusion
  conclusion <- if (ci_within_limits && pe_acceptable) {
    "BIOEQUIVALENT - Meets ICH M13A criteria"
  } else if (ci_within_limits && !pe_acceptable) {
    "QUESTIONABLE - CI within limits but point estimate concerning"
  } else {
    "NOT BIOEQUIVALENT - Does not meet ICH M13A criteria"
  }
  
  return(list(
    conclusion = conclusion,
    ci_within_limits = ci_within_limits,
    point_estimate_acceptable = pe_acceptable,
    ich_compliant = ci_within_limits && pe_acceptable
  ))
}

#' Evaluate Confidence Interval per ICH M13A
#'
#' @param ci Confidence interval results
#' @param parameter Parameter name
#' @param config Analysis configuration
#' @return CI evaluation
evaluate_ci_ich_m13a <- function(ci, parameter, config) {
  
  # Check CI width
  ci_width <- ci$ci_upper - ci$ci_lower
  
  # Check if CI is appropriately narrow (< 45% is generally good)
  ci_narrow <- ci_width < 45
  
  # Check if CI is symmetric around point estimate
  lower_distance <- ci$point_estimate - ci$ci_lower
  upper_distance <- ci$ci_upper - ci$point_estimate
  asymmetry_ratio <- max(lower_distance, upper_distance) / min(lower_distance, upper_distance)
  ci_symmetric <- asymmetry_ratio < 1.5
  
  return(list(
    ci_width = ci_width,
    ci_narrow = ci_narrow,
    ci_symmetric = ci_symmetric,
    asymmetry_ratio = asymmetry_ratio,
    evaluation = if (ci_narrow && ci_symmetric) "Acceptable" else "Review recommended"
  ))
}

#' Evaluate Point Estimate per ICH M13A
#'
#' @param ci Confidence interval results
#' @param parameter Parameter name
#' @param config Analysis configuration
#' @return Point estimate evaluation
evaluate_pe_ich_m13a <- function(ci, parameter, config) {
  
  # Distance from 100%
  pe_deviation <- abs(ci$point_estimate - 100)
  
  # ICH M13A guidance on point estimate
  pe_close_to_unity <- pe_deviation < 10  # Within 10% of unity
  pe_acceptable_range <- (ci$point_estimate >= 90) && (ci$point_estimate <= 111)
  
  return(list(
    point_estimate = ci$point_estimate,
    deviation_from_unity = pe_deviation,
    close_to_unity = pe_close_to_unity,
    acceptable_range = pe_acceptable_range,
    evaluation = if (pe_close_to_unity) "Excellent" else if (pe_acceptable_range) "Acceptable" else "Concerning"
  ))
}

#' Evaluate Variability per ICH M13A
#'
#' @param variability Variability results
#' @param parameter Parameter name
#' @param config Analysis configuration
#' @return Variability evaluation
evaluate_variability_ich_m13a <- function(variability, parameter, config) {
  
  if (is.null(variability)) return(NULL)
  
  cv_wr <- variability$cv_wr
  
  # ICH M13A variability categories
  variability_category <- if (cv_wr < 20) {
    "Low variability"
  } else if (cv_wr < 30) {
    "Moderate variability"
  } else if (cv_wr < 50) {
    "High variability - scaling may be considered"
  } else {
    "Very high variability - scaling recommended"
  }
  
  # Scaling threshold per ICH M13A
  scaling_threshold_met <- cv_wr > config$scaling_threshold
  
  return(list(
    cv_wr = cv_wr,
    variability_category = variability_category,
    scaling_threshold_met = scaling_threshold_met,
    scaling_recommended = cv_wr > 30,
    evaluation = variability_category
  ))
}

#' Evaluate Scaling Decision per ICH M13A
#'
#' @param scaling_decision Scaling decision results
#' @param parameter Parameter name
#' @param config Analysis configuration
#' @return Scaling evaluation
evaluate_scaling_decision_ich_m13a <- function(scaling_decision, parameter, config) {
  
  cv_wr <- scaling_decision$cv_wr
  use_scaling <- scaling_decision$use_scaling
  threshold <- scaling_decision$threshold
  
  # ICH M13A scaling justification
  scaling_justified <- cv_wr > threshold
  decision_appropriate <- (scaling_justified && use_scaling) || (!scaling_justified && !use_scaling)
  
  justification <- if (use_scaling && scaling_justified) {
    paste0("Scaling applied appropriately (CV = ", round(cv_wr, 1), "% > ", round(threshold, 1), "%)")
  } else if (!use_scaling && !scaling_justified) {
    paste0("Standard limits applied appropriately (CV = ", round(cv_wr, 1), "% ≤ ", round(threshold, 1), "%)")
  } else if (use_scaling && !scaling_justified) {
    "WARNING: Scaling applied despite low variability"
  } else {
    "WARNING: Standard limits used despite high variability"
  }
  
  return(list(
    scaling_justified = scaling_justified,
    decision_appropriate = decision_appropriate,
    justification = justification,
    cv_wr = cv_wr,
    threshold = threshold
  ))
}

# =============================================================================
# SECTION 10: ENHANCED SUMMARY GENERATION
# =============================================================================

#' Generate Comprehensive BE Summary
#'
#' @param results BE analysis results
#' @param include_regulatory Include regulatory assessment
#' @return Comprehensive summary data frame
generate_be_summary <- function(results, include_regulatory = TRUE) {
  
  if (is.null(results$confidence_intervals) || length(results$confidence_intervals) == 0) {
    return(data.frame(message = "No parameters analyzed"))
  }
  
  # Create summary table
  summary_data <- data.frame(
    Parameter = character(),
    N_Subjects = numeric(),
    Point_Estimate = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    CI_Width = numeric(),
    Bioequivalent = logical(),
    stringsAsFactors = FALSE
  )
  
  # Add design-specific columns
  if (results$design == "replicate") {
    summary_data$CV_WR <- numeric(0)
    summary_data$Scaling_Applied <- logical(0)
  }
  
  if (include_regulatory && !is.null(results$regulatory_assessment)) {
    summary_data$Regulatory_Status <- character(0)
    summary_data$ICH_Compliant <- logical(0)
  }
  
  # Fill summary data
  for (param in names(results$confidence_intervals)) {
    ci <- results$confidence_intervals[[param]]
    
    # Base data
    row_data <- data.frame(
      Parameter = param,
      N_Subjects = results$n_subjects,
      Point_Estimate = round(ci$point_estimate, 2),
      CI_Lower = round(ci$ci_lower, 2),
      CI_Upper = round(ci$ci_upper, 2),
      CI_Width = round(ci$ci_upper - ci$ci_lower, 2),
      Bioequivalent = results$be_conclusions[[param]],
      stringsAsFactors = FALSE
    )
    
    # Add replicate-specific data
    if (results$design == "replicate") {
      cv_wr <- if (!is.null(results$variability_results[[param]])) {
        results$variability_results[[param]]$cv_wr
      } else NA
      
      scaling_applied <- if (!is.null(results$scaling_decisions[[param]])) {
        results$scaling_decisions[[param]]$use_scaling
      } else FALSE
      
      row_data$CV_WR <- round(cv_wr, 1)
      row_data$Scaling_Applied <- scaling_applied
    }
    
    # Add regulatory data
    if (include_regulatory && !is.null(results$regulatory_assessment)) {
      param_assessment <- results$regulatory_assessment$parameter_assessments[[param]]
      
      if (!is.null(param_assessment)) {
        row_data$Regulatory_Status <- param_assessment$bioequivalence_conclusion$conclusion
        row_data$ICH_Compliant <- param_assessment$bioequivalence_conclusion$ich_compliant
      } else {
        row_data$Regulatory_Status <- "Not assessed"
        row_data$ICH_Compliant <- NA
      }
    }
    
    summary_data <- rbind(summary_data, row_data)
  }
  
  # Add metadata
  attr(summary_data, "design") <- results$design
  attr(summary_data, "n_subjects") <- results$n_subjects
  attr(summary_data, "be_limits") <- results$be_limits
  attr(summary_data, "alpha") <- results$alpha
  attr(summary_data, "analysis_date") <- Sys.Date()
  
  if (include_regulatory && !is.null(results$regulatory_assessment)) {
    attr(summary_data, "regulatory_standard") <- results$regulatory_assessment$standard
    attr(summary_data, "overall_compliant") <- results$regulatory_assessment$overall_assessment$regulatory_compliance
  }
  
  return(summary_data)
}

# =============================================================================
# SECTION 11: ADDITIONAL HELPER FUNCTIONS FOR REGULATORY EVALUATION
# =============================================================================

#' Evaluate Design Adequacy per ICH M13A
evaluate_design_adequacy_ich_m13a <- function(results, config) {
  list(
    design_type = results$design,
    adequate = TRUE,
    notes = paste("Design", results$design, "is appropriate for bioequivalence assessment")
  )
}

#' Evaluate Sample Size Adequacy per ICH M13A
evaluate_sample_size_adequacy_ich_m13a <- function(results, config) {
  n_subjects <- results$n_subjects
  
  adequate <- n_subjects >= 12  # Minimum recommended by ICH M13A
  
  list(
    n_subjects = n_subjects,
    adequate = adequate,
    notes = if (adequate) {
      "Sample size adequate"
    } else {
      "Sample size may be insufficient for reliable BE assessment"
    }
  )
}

#' Evaluate Primary Parameters per ICH M13A
evaluate_primary_parameters_ich_m13a <- function(results, config) {
  analyzed_params <- names(results$confidence_intervals)
  required_params <- config$primary_params
  
  missing_params <- setdiff(required_params, analyzed_params)
  
  list(
    analyzed_parameters = analyzed_params,
    required_parameters = required_params,
    missing_parameters = missing_params,
    adequate = length(missing_params) == 0
  )
}

#' Determine Overall Compliance per ICH M13A
determine_overall_compliance_ich_m13a <- function(regulatory_assessment, config) {
  
  # Check if all assessed parameters are compliant
  param_compliance <- sapply(regulatory_assessment, function(x) {
    x$bioequivalence_conclusion$ich_compliant
  })
  
  all_compliant <- all(param_compliance, na.rm = TRUE)
  
  return(all_compliant)
}

#' Generate Regulatory Recommendations per ICH M13A
generate_regulatory_recommendations_ich_m13a <- function(regulatory_assessment, results, config) {
  
  recommendations <- c()
  
  # Check each parameter for specific recommendations
  for (param in names(regulatory_assessment)) {
    assessment <- regulatory_assessment[[param]]
    
    if (!assessment$bioequivalence_conclusion$ich_compliant) {
      recommendations <- c(recommendations, 
                          paste("Consider additional studies for", param, "bioequivalence"))
    }
    
    if (!is.null(assessment$variability_assessment) && 
        assessment$variability_assessment$cv_wr > 50) {
      recommendations <- c(recommendations,
                          paste("High variability observed for", param, "- consider formulation optimization"))
    }
  }
  
  if (length(recommendations) == 0) {
    recommendations <- "No specific recommendations - study meets ICH M13A criteria"
  }
  
  return(recommendations)
}
