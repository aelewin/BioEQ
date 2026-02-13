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
                               custom_config = NULL,
                               welch_correction = TRUE) {
  
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
    "parallel" = be_parallel(validated_data, alpha, be_limits, parameters, welch_correction),
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
  welch_correction <- params$welch_correction %||% TRUE
  
  # Extract per-parameter BE limits (from ABE Advanced Options)
  be_limits_per_param <- params$be_limits_per_param %||% NULL
  
  # Use existing BE analysis functions
  results <- switch(design,
    "2x2x2" = be_crossover_2x2x2(data, alpha, be_limits, parameters, anova_model, anova_results, be_limits_per_param),
    "parallel" = be_parallel(data, alpha, be_limits, parameters, welch_correction, be_limits_per_param),
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

#' Perform RSABE analysis
#' 
#' FDA Reference Scaled Average Bioequivalence
#' Supports two methods:
#'   - FDA Linearized Scaled Criterion (Howe UCB) — default
#'   - Non-Central TOST (ncTOST) — exact method
#' 
#' @param data Data frame with PK parameters
#' @param design Study design
#' @param params Analysis parameters (includes rsabe_method: "fda_linearized" or "nctost")
#' @return RSABE analysis results
#' @export
perform_rsabe_placeholder <- function(data, design = "auto", params = list()) {
  
  # Source the RSABE analysis module if perform_rsabe is not yet available
  if (!exists("perform_rsabe", mode = "function")) {
    # Try multiple paths (from different working directories)
    possible_paths <- c(
      "R/rsabe_analysis.R",
      "../R/rsabe_analysis.R",
      file.path(getwd(), "R", "rsabe_analysis.R")
    )
    
    sourced <- FALSE
    for (path in possible_paths) {
      if (file.exists(path)) {
        source(path)
        sourced <- TRUE
        break
      }
    }
    
    if (!sourced) {
      stop("RSABE analysis module not found. Expected at: R/rsabe_analysis.R")
    }
  }
  
  # Delegate to the RSABE engine
  perform_rsabe(data, design, params)
}

#' Perform ABEL analysis
#' 
#' EMA Average Bioequivalence with Expanding Limits using replicateBE package
#' Supports both Method A (ANOVA) and Method B (mixed model)
#' 
#' @param data Data frame with PK parameters (expects capitalized columns: Subject, Treatment, Period, Sequence)
#' @param design Study design  
#' @param params Analysis parameters (including abel_method: "A" or "B")
#' @return ABEL analysis results
#' @export
perform_abel_placeholder <- function(data, design = "auto", params = list()) {
  
  # Derive replicateBE method from ANOVA model selection
  # Fixed Effects → Method A (linear model)
  # Mixed Effects (nlme/Satterthwaite/Kenward-Roger) → Method B with appropriate DF
  anova_model <- params$anova_model %||% "fixed"
  
  # Determine method and DF approximation
  # replicateBE::method.B() option parameter:
  #   1 = lmerTest with Satterthwaite DF
  #   2 = nlme::lme (SAS DDFM=CONTAIN equivalent) — default
  #   3 = lmerTest with Kenward-Roger DF
  if (anova_model == "fixed") {
    use_method_a <- TRUE
    method_label <- "Method A (ANOVA/Linear Model)"
    df_method <- NULL  # Not used for Method A
  } else {
    use_method_a <- FALSE
    if (grepl("satterthwaite", anova_model, ignore.case = TRUE)) {
      df_method <- 1  # lmerTest Satterthwaite DF
      method_label <- "Method B (Satterthwaite DF, option=1)"
    } else if (grepl("kenward", anova_model, ignore.case = TRUE)) {
      df_method <- 3  # lmerTest Kenward-Roger DF
      method_label <- "Method B (Kenward-Roger DF, option=3)"
    } else {
      # Default for "nlme" or other mixed models
      df_method <- 2  # nlme::lme (SAS CONTAIN equivalent)
      method_label <- "Method B (nlme, SAS CONTAIN DF, option=2)"
    }
  }
  
  cat(sprintf("🔬 Performing ABEL analysis using replicateBE::%s...\n", method_label))
  cat(sprintf("   ANOVA Model: %s\n", anova_model))
  
  # Extract parameters
  alpha <- params$alpha_level %||% 0.05
  parameters <- params$pk_parameters %||% c("Cmax", "AUC0t", "AUC0inf")
  
  # Regulatory authority determines which parameters get expanded limits
  abel_authority <- params$abel_regulator %||% "EMA"
  cat(sprintf("   Regulatory Authority: %s\n", abel_authority))
  
  # Helper: determine if a parameter is eligible for ABEL scaling
  # EMA: Only Cmax gets expanded limits
  # HC (Health Canada): Cmax and AUC0t (steady-state) get expanded limits
  is_abel_eligible <- function(param_name, regulator) {
    # Normalize: strip ln/log prefix to get base parameter
    base <- sub("^(ln|log)", "", param_name, ignore.case = TRUE)
    base_upper <- toupper(base)
    
    if (regulator == "EMA") {
      # EMA: Only Cmax
      return(grepl("^CMAX$", base_upper))
    } else if (regulator == "HC") {
      # Health Canada: Cmax and AUC0t (AUCss for steady-state)
      return(grepl("^CMAX$", base_upper) || grepl("^AUC0T$", base_upper) || grepl("^AUCSS$", base_upper))
    } else {
      # Default (GCC etc.): Same as EMA
      return(grepl("^CMAX$", base_upper))
    }
  }
  
  # Detect replicate design
  design_info <- detect_replicate_design(data)
  
  if (!design_info$is_replicate) {
    stop("ABEL analysis requires a replicate design (2x2x3 or 2x2x4). Detected: ", design_info$design_type)
  }
  
  # Verify required columns exist (capitalized)
  required_cols <- c("Subject", "Period", "Sequence", "Treatment")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Analyze each parameter
  all_results <- list()
  ci_list <- list()
  conclusion_list <- list()
  processed_params <- c()  # Track which base parameters we've already processed
  
  for (param in parameters) {
    
    # Determine whether to use non-log or log-transformed data
    # Priority: Non-log data > Log-transformed data
    # Detection: Check if parameter starts with "ln" or "log"
    
    is_log_param <- grepl("^(ln|log)", param, ignore.case = TRUE)
    
    # Extract base parameter name for tracking duplicates
    if (is_log_param) {
      base_param_name <- sub("^(ln|log)", "", param, ignore.case = TRUE)
    } else {
      base_param_name <- param
    }
    
    # Check if we've already processed this base parameter
    if (base_param_name %in% processed_params) {
      cat(sprintf("⏭️  Skipping '%s' - already processed as '%s'\n", param, base_param_name))
      next
    }
    
    # If this is a log parameter, check if non-log version exists
    if (is_log_param) {
      # Extract base parameter name (e.g., "Cmax" from "lnCmax")
      base_param <- sub("^(ln|log)", "", param, ignore.case = TRUE)
      
      if (base_param %in% names(data)) {
        # Non-log version exists - use that instead
        cat(sprintf("ℹ️  Found non-log version '%s' for '%s' - using non-log data (replicateBE will log-transform)\n", 
                    base_param, param))
        param_to_use <- base_param
        data_is_logged <- FALSE
      } else {
        # Only log version available - use it
        cat(sprintf("ℹ️  Only log-transformed '%s' available - using pre-logged data\n", param))
        param_to_use <- param
        data_is_logged <- TRUE
      }
    } else {
      # This is a non-log parameter
      # Check if log version exists and warn if both present
      log_versions <- c(paste0("ln", param), paste0("log", param), paste0("Log", param))
      found_log <- log_versions[log_versions %in% names(data)]
      
      if (length(found_log) > 0 && param %in% names(data)) {
        # Both exist - prefer non-log
        cat(sprintf("ℹ️  Using non-log '%s' (ignoring log version: %s)\n", 
                    param, paste(found_log, collapse = ", ")))
        param_to_use <- param
        data_is_logged <- FALSE
      } else if (length(found_log) > 0) {
        # Only log version exists
        cat(sprintf("ℹ️  Non-log '%s' not found, using log-transformed version '%s'\n", 
                    param, found_log[1]))
        param_to_use <- found_log[1]
        data_is_logged <- TRUE
      } else if (param %in% names(data)) {
        # Only non-log exists
        param_to_use <- param
        data_is_logged <- FALSE
      } else {
        # Neither exists
        cat("⚠️  Parameter", param, "not found in data, skipping...\n")
        next
      }
    }
    
    # Check if the parameter exists in data
    if (!param_to_use %in% names(data)) {
      cat("⚠️  Parameter", param_to_use, "not found in data, skipping...\n")
      next
    }
    
    # Mark this base parameter as processed
    processed_params <- c(processed_params, base_param_name)
    
    # =====================================================================
    # Per-parameter routing: Determine if this param gets ABEL or fixed ABE
    # =====================================================================
    use_abel_for_param <- is_abel_eligible(base_param_name, abel_authority)
    
    if (!use_abel_for_param) {
      # ---------------------------------------------------------------
      # NON-ELIGIBLE: Use standard ABE with fixed 80-125% limits
      # (e.g., AUC parameters under EMA, AUC0inf under HC)
      # ---------------------------------------------------------------
      cat(sprintf("  📋 %s: Not eligible for ABEL scaling under %s — using fixed ABE limits (80-125%%)\n",
                  base_param_name, abel_authority))
      
      tryCatch({
        # Use the ANOVA results already computed for this parameter
        anova_res_for_param <- params$anova_results
        
        # Try to find results for this param (log-transformed)
        ln_param <- paste0("ln", base_param_name)
        param_anova <- NULL
        if (!is.null(anova_res_for_param)) {
          if (!is.null(anova_res_for_param[[ln_param]])) {
            param_anova <- anova_res_for_param[[ln_param]]
          } else if (!is.null(anova_res_for_param[[base_param_name]])) {
            param_anova <- anova_res_for_param[[base_param_name]]
          }
        }
        
        if (!is.null(param_anova) && !is.null(param_anova$pe_estimate)) {
          # Extract from existing ANOVA results
          pe <- param_anova$pe_estimate
          ci_lo_val <- param_anova$ci_lower
          ci_hi_val <- param_anova$ci_upper
          df_val <- param_anova$residual_df
          
          # Fixed limits for non-scaling parameters
          fixed_lower <- 80.0
          fixed_upper <- 125.0
          
          be_pass <- (ci_lo_val >= fixed_lower) && (ci_hi_val <= fixed_upper)
          
          ci_list[[base_param_name]] <- list(
            parameter = base_param_name,
            point_estimate = pe,
            ci_lower = ci_lo_val,
            ci_upper = ci_hi_val,
            confidence_level = (1 - alpha) * 100,
            geometric_mean_ratio = pe / 100,
            within_limits = be_pass,
            # Fixed limits - no scaling
            cv_wr = NA,
            scaled_lower_limit = fixed_lower,
            scaled_upper_limit = fixed_upper,
            limits_used = list(lower = fixed_lower, upper = fixed_upper, type = "fixed"),
            method = paste0("ABE (fixed limits, ", abel_authority, " guidance)"),
            regulator = abel_authority,
            degrees_freedom = df_val,
            cv_wt = NA,
            n_subjects = length(unique(data$Subject)),
            sw_test = NA,
            sw_reference = NA,
            sw_ratio = NA
          )
          
          all_results[[base_param_name]] <- list(
            model = NULL,
            anova = param_anova$anova,
            treatment_coef = param_anova$treatment_coef,
            treatment_se = param_anova$treatment_se,
            residual_mse = param_anova$residual_mse,
            residual_df = df_val,
            n_observations = param_anova$n_observations,
            anova_method = param_anova$anova_method %||% "fixed",
            cv_wr_percent = NA,
            cv_wt_percent = NA,
            data_was_logged = TRUE,
            abel_routing = "fixed_abe"
          )
          
          conclusion_list[[base_param_name]] <- be_pass
          
          cat(sprintf("  ✓ %s (ABE): PE=%.2f%%, CI [%.2f%%, %.2f%%], Limits [%.1f%%, %.1f%%], BE=%s\n",
                      base_param_name, pe, ci_lo_val, ci_hi_val, fixed_lower, fixed_upper,
                      ifelse(be_pass, "Pass", "Fail")))
        } else {
          cat(sprintf("  ⚠️  %s: No ANOVA results available, skipping\n", base_param_name))
        }
      }, error = function(e) {
        cat(sprintf("  ❌ Error processing %s (ABE fallback): %s\n", base_param_name, e$message))
      })
      
      next  # Skip to next parameter
    }
    
    # =====================================================================
    # ABEL-ELIGIBLE: Route through replicateBE for scaled limits
    # =====================================================================
    cat(sprintf("  📋 %s: Eligible for ABEL scaling under %s\n", base_param_name, abel_authority))
    
    tryCatch({
      # Prepare data for replicateBE (expects lowercase: subject, period, sequence, treatment, PK)
      replicate_data <- data.frame(
        subject = as.factor(data$Subject),
        period = as.factor(data$Period),
        sequence = as.factor(data$Sequence),
        treatment = as.factor(data$Treatment),
        PK = as.numeric(data[[param_to_use]]),
        stringsAsFactors = FALSE
      )
      
      # Remove any rows with missing PK values
      replicate_data <- replicate_data[!is.na(replicate_data$PK), ]
      
      if (nrow(replicate_data) == 0) {
        stop("No valid data for parameter ", param_to_use)
      }
      
      # Call replicateBE method (A or B) for ABEL (EMA method)
      # Method A: ANOVA-based approach (default)
      # Method B: Mixed model approach with subjects as random effect
      # NOTE: Write to temp CSV and read from file (required for file-based params)
      temp_dir <- tempdir()
      temp_file <- file.path(temp_dir, paste0("abel_temp_", param_to_use))
      write.csv(replicate_data, paste0(temp_file, ".csv"), row.names = FALSE, quote = FALSE)
      
      cat(sprintf("  - Wrote temp file: %s.csv\n", temp_file))
      cat(sprintf("  - Data structure: %d rows, PK range: [%.2f, %.2f]\n", 
                  nrow(replicate_data), min(replicate_data$PK, na.rm=TRUE), max(replicate_data$PK, na.rm=TRUE)))
      cat(sprintf("  - Column types: subject=%s, period=%s, sequence=%s, treatment=%s, PK=%s\n",
                  class(replicate_data$subject)[1], class(replicate_data$period)[1], 
                  class(replicate_data$sequence)[1], class(replicate_data$treatment)[1], 
                  class(replicate_data$PK)[1]))
      cat(sprintf("  - Using %s data (logtrans = %s)\n", 
                  ifelse(data_is_logged, "pre-logged", "non-log"), 
                  ifelse(data_is_logged, "FALSE", "TRUE")))
      
      # Get ABEL-specific parameters from params (with defaults)
      # Map UI values to replicateBE regulator codes
      abel_cap_input <- if (!is.null(params$abel_upper_cap)) params$abel_upper_cap else "50"
      
      # Map to replicateBE regulator parameter
      # Health Canada uses "HC" regulator code in replicateBE (cap at CVwR = 57.4%)
      # EMA uses "EMA" regulator code (50% cap: 69.84% - 143.19%)
      # GCC uses "GCC" regulator code (fixed widened limits: 75% - 133.33%)
      if (abel_authority == "HC") {
        abel_regulator_code <- "HC"  # Health Canada: cap at CVwR = 57.4% (limits: 66.7% - 150.0%)
      } else {
        abel_regulator_code <- switch(abel_cap_input,
          "none" = "EMA",   # Use EMA but effectively no cap
          "50" = "EMA",     # EMA with 50% cap (69.84% - 143.19%)
          "fixed" = "GCC",  # GCC with fixed widened limits (75% - 133.33%)
          "EMA"             # Default to EMA
        )
      }
      
      abel_adjust <- if (!is.null(params$abel_adjust_tie)) params$abel_adjust_tie else FALSE
      abel_ola <- if (!is.null(params$abel_outlier_analysis)) params$abel_outlier_analysis else FALSE
      abel_fence <- if (!is.null(params$abel_outlier_fence)) params$abel_outlier_fence else 2
      
      cat(sprintf("  - ABEL Settings: Authority=%s, Cap=%s -> Regulator=%s (adjust=%s, ola=%s, fence=%.1f)\n", 
                  abel_authority, abel_cap_input, abel_regulator_code, abel_adjust, abel_ola, abel_fence))
      
      # Call appropriate replicateBE method based on ANOVA model selection
      if (use_method_a) {
        # Method A: Linear model (ANOVA-based)
        abel_result <- tryCatch({
          replicateBE::method.A(
            path.in = temp_dir,
            file = basename(temp_file),
            ext = "csv",
            print = FALSE,
            details = TRUE,
            alpha = alpha,
            regulator = abel_regulator_code,  # Regulator: EMA, HC, or GCC
            logtrans = !data_is_logged,  # Only log-transform if data is NOT already logged
            adjust = abel_adjust,        # TIE adjustment
            ola = abel_ola,              # Outlier analysis
            fence = abel_fence           # Outlier detection fence
          )
        }, error = function(e) {
          cat(sprintf("[ERROR] replicateBE::method.A() failed for %s: %s\n", param, e$message))
          print(traceback())
          stop(e)
        })
      } else {
        # Method B: Mixed effects model with specified DF approximation
        # NOTE: method.B() does NOT support 'adjust' (TIE adjustment) — only method.A() does.
        # The 'option' parameter must be numeric: 1=Satterthwaite, 2=nlme/SAS, 3=Kenward-Roger
        cat(sprintf("  - Method B option=%d (%s)\n", df_method,
                    switch(as.character(df_method), "1"="Satterthwaite", "2"="nlme/SAS CONTAIN", "3"="Kenward-Roger")))
        abel_result <- tryCatch({
          replicateBE::method.B(
            path.in = temp_dir,
            file = basename(temp_file),
            ext = "csv",
            print = FALSE,
            details = TRUE,
            alpha = alpha,
            regulator = abel_regulator_code,  # Regulator: EMA, HC, or GCC
            logtrans = !data_is_logged,  # Only log-transform if data is NOT already logged
            option = df_method,          # DF approximation: 1=Satterthwaite, 2=nlme, 3=KR
            ola = abel_ola,              # Outlier analysis (boolean)
            fence = abel_fence           # Outlier detection fence
          )
        }, error = function(e) {
          cat(sprintf("[ERROR] replicateBE::method.B(option=%d) failed for %s: %s\n", df_method, param, e$message))
          print(traceback())
          stop(e)
        })
      }
      
      # Clean up temp file
      unlink(paste0(temp_file, ".csv"))
      
      # Extract key results from the data frame (single row)
      # Use row/column indexing: result[1, "column_name"]
      pe <- abel_result[1, "PE(%)"]  # Point estimate as percentage
      ci_lower <- abel_result[1, "CL.lo(%)"]  # Lower CI limit as percentage
      ci_upper <- abel_result[1, "CL.hi(%)"]  # Upper CI limit as percentage
      
      # ABEL-specific values
      cv_wr <- abel_result[1, "CVwR(%)"]  # CV% for reference (already in %)
      
      # Scaled limits (already in %)
      scaled_lower <- abel_result[1, "L(%)"]
      scaled_upper <- abel_result[1, "U(%)"]
      
      # BE conclusion
      be_pass <- abel_result[1, "BE"] == "pass"
      
      # Extract ANOVA-related information from replicateBE output
      # These will be used to populate ANOVA results display
      anova_df <- abel_result[1, "DF"]  # Degrees of freedom
      cv_wt <- abel_result[1, "CVwT(%)"]  # CV% for test
      n_total <- abel_result[1, "n"]  # Total subjects
      n_tt <- abel_result[1, "nTT"]  # Subjects with both test treatments
      n_rr <- abel_result[1, "nRR"]  # Subjects with both reference treatments
      sw_t <- abel_result[1, "swT"]  # Within-subject SD for test
      sw_r <- abel_result[1, "swR"]  # Within-subject SD for reference
      sw_ratio <- abel_result[1, "sw.ratio"]  # Ratio of within-subject SDs
      
      # Convert percentages back to ratios for display consistency
      gmr <- pe / 100
      ci_lo <- ci_lower / 100
      ci_hi <- ci_upper / 100
      limit_lo <- scaled_lower / 100
      limit_hi <- scaled_upper / 100
      
      # Store confidence interval as LIST (matching ABE format)
      # Must match structure from extract_be_from_anova for compatibility with results display
      # Use method_label from ANOVA model determination above
      # Store under BASE parameter name to avoid duplicates (e.g., "Cmax" not "lnCmax")
      ci_list[[base_param_name]] <- list(
        parameter = base_param_name,  # Use base name for display
        point_estimate = pe,  # Already as percentage from replicateBE
        ci_lower = ci_lower,  # Already as percentage
        ci_upper = ci_upper,  # Already as percentage
        confidence_level = (1 - alpha) * 100,
        geometric_mean_ratio = gmr,  # As ratio
        within_limits = be_pass,
        # ABEL-specific fields
        cv_wr = cv_wr,  # Within-subject CV for reference
        scaled_lower_limit = scaled_lower,  # Scaled lower limit (%)
        scaled_upper_limit = scaled_upper,  # Scaled upper limit (%)
        limits_used = list(lower = scaled_lower, upper = scaled_upper, type = "scaled"),
        method = method_label,
        regulator = abel_authority,
        # ANOVA-related fields from replicateBE
        degrees_freedom = anova_df,
        cv_wt = cv_wt,  # CV for test
        n_subjects = n_total,
        sw_test = sw_t,
        sw_reference = sw_r,
        sw_ratio = sw_ratio
      )
      
      # Store ANOVA-like results for this parameter
      # Create structure similar to standard ANOVA output for results display
      # Store under BASE parameter name to avoid duplicates
      all_results[[base_param_name]] <- list(
        model = NULL,  # replicateBE doesn't return model object
        anova = data.frame(
          Source = c("Treatment", "Subject", "Period", "Residual"),
          DF = c(1, n_total - 1, design_info$n_periods - 1, anova_df),
          MS = c(NA, NA, NA, sw_r^2),  # Only residual MS available from sw_r
          stringsAsFactors = FALSE
        ),
        treatment_coef = log(gmr),  # Log of GMR
        treatment_se = (log(ci_hi) - log(gmr)) / qt(1 - alpha/2, anova_df),  # Back-calculate SE
        residual_mse = sw_r^2,
        residual_df = anova_df,
        n_observations = n_total * design_info$n_periods,
        anova_method = if(use_method_a) "lm" else switch(as.character(df_method), "1"="lmerTest", "3"="lmerTest", "nlme"),
        df_method = if(use_method_a) NA else df_method,
        df_method_label = if(use_method_a) NA else switch(as.character(df_method), "1"="Satterthwaite", "2"="nlme/SAS CONTAIN", "3"="Kenward-Roger"),
        cv_wr_percent = cv_wr,
        cv_wt_percent = cv_wt,
        replicatebe_output = abel_result[1, ],  # Store full replicateBE output row
        data_was_logged = data_is_logged  # Track whether we used pre-logged data
      )
      
      conclusion_list[[base_param_name]] <- be_pass
      
      cat(sprintf("  ✓ %s: GMR=%.4f, 90%% CI [%.4f, %.4f], CV_WR=%.2f%%, BE=%s\n",
                  base_param_name, gmr, ci_lo, ci_hi, cv_wr, ifelse(be_pass, "Pass", "Fail")))
      
    }, error = function(e) {
      cat("❌ Error analyzing", param, ":", e$message, "\n")
    })
  }
  
  if (length(ci_list) == 0) {
    stop("No parameters could be analyzed successfully")
  }
  
  # Convert conclusion_list to proper format (TRUE/FALSE values)
  be_conclusions_formatted <- list()
  for (param in names(conclusion_list)) {
    be_conclusions_formatted[[param]] <- conclusion_list[[param]]
  }
  
  # Combine into results structure matching BioEQ format
  # NOTE: Keep confidence_intervals as a LIST (not data frame) to match ABE/standard format
  # Each element should be: confidence_intervals[[param]] = list(parameter, point_estimate, ci_lower, ci_upper, ...)
  # IMPORTANT: Wrap ANOVA results in nested structure to match simple ANOVA format
  # Display expects: be_res$anova_results$anova_results[[param]]
  results <- list(
    confidence_intervals = ci_list,  # Keep as list, NOT data frame
    be_conclusions = be_conclusions_formatted,
    anova_results = list(
      anova_results = all_results,  # Nested structure for display compatibility
      design = design_info$design_type,
      parameters = names(all_results),
      note = sprintf("ANOVA performed by replicateBE %s", method_label)
    ),
    design_type = design_info$design_type,
    n_subjects = length(unique(data$Subject)),
    n_periods = design_info$n_periods,
    analysis_type = "ABEL",  # CRITICAL: Required for UI to recognize ABEL analysis
    analysis_method = sprintf("ABEL %s (%s)", method_label, abel_authority),
    be_method = sprintf("Average Bioequivalence with Expanding Limits (ABEL) - %s [%s]", method_label, abel_authority),
    anova_model = anova_model,
    replicatebe_method = if(use_method_a) "A" else "B",
    df_approximation = if(use_method_a) NA else df_method,
    alpha_level = alpha,
    regulator = abel_authority,
    limits_justification = sprintf("ABEL per %s guidance: scaled limits for eligible parameters, fixed 80-125%% for others", abel_authority)
  )
  
  cat("✅ ABEL analysis completed!\n")
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
                               anova_model = "fixed", anova_results = NULL,
                               be_limits_per_param = NULL) {
  
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
  be_results <- extract_be_from_anova(anova_results, alpha, be_limits, be_limits_per_param)
  
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
  
  # Generate bioequivalence conclusions (uses limits_used from each CI)
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
                       parameters = c("AUC0t", "AUC0inf", "Cmax"), 
                       welch_correction = TRUE,
                       be_limits_per_param = NULL) {
  
  cat("🔬 Analyzing parallel group bioequivalence study...\n")
  
  # For parallel BE analysis, we need raw parameters because log transformation 
  # happens inside the analysis function (prepare_parameter_data)
  # Filter to only raw parameters (not log-transformed ones)
  available_params <- intersect(parameters, names(data))
  
  # Remove any log-transformed parameters to avoid double log transformation
  raw_params <- available_params[!startsWith(available_params, "ln")]
  
  # Remove Tmax as it requires non-parametric analysis, not confidence intervals
  # Tmax should be analyzed using median differences and Wilcoxon tests per regulatory guidance
  if ("Tmax" %in% raw_params) {
    cat("ℹ️  Note: Tmax excluded from parametric BE analysis (requires non-parametric methods)\n")
    cat("   For Tmax assessment, use median differences and Wilcoxon signed-rank tests\n")
    cat("   as recommended by FDA and EMA guidance documents.\n")
    raw_params <- raw_params[raw_params != "Tmax"]
  }
  
  if (length(raw_params) == 0) {
    cat("⚠️ No suitable raw parameters found for parallel BE analysis.\n")
    cat("Available parameters in data: ", paste(names(data), collapse = ", "), "\n")
    cat("Original requested parameters: ", paste(parameters, collapse = ", "), "\n")
    cat("Note: Parallel analysis requires raw parameters for internal log transformation.\n")
    return(NULL)
  }
  
  cat("Using raw parameters for parallel analysis:", paste(raw_params, collapse = ", "), "\n")
  
  # Validate PK parameters
  pk_params <- validate_pk_parameters(data, raw_params)
  
  # Perform analysis for each parameter
  confidence_intervals <- list()
  statistical_results <- list()
  
  for (param in pk_params) {
    cat("  Analyzing", param, "...\n")
    
    # Perform parallel group analysis
    tryCatch({
      param_results <- analyze_parallel_parameter(data, param, alpha, welch_correction)
      
      if (!is.null(param_results)) {
        ci <- param_results$ci
        
        # Add per-parameter limits_used metadata
        if (!is.null(be_limits_per_param)) {
          param_upper_name <- toupper(param)
          if (grepl("CMAX", param_upper_name) && !is.null(be_limits_per_param$Cmax)) {
            ci$limits_used <- list(
              lower = be_limits_per_param$Cmax$lower %||% 80,
              upper = be_limits_per_param$Cmax$upper %||% 125,
              type = "fixed"
            )
          } else if (grepl("AUC|AUMC", param_upper_name) && !is.null(be_limits_per_param$AUC)) {
            ci$limits_used <- list(
              lower = be_limits_per_param$AUC$lower %||% 80,
              upper = be_limits_per_param$AUC$upper %||% 125,
              type = "fixed"
            )
          }
        }
        
        confidence_intervals[[param]] <- ci
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
extract_be_from_anova <- function(anova_results, alpha = 0.05, be_limits = c(0.8, 1.25), be_limits_per_param = NULL) {
  
  cat("🔗 Extracting bioequivalence results from ANOVA analysis...\n")
  
  # Helper: resolve BE limits for a specific parameter
  # Maps parameter names like lnCmax -> Cmax category, lnAUC0t -> AUC category
  resolve_param_limits <- function(param_name, global_limits, per_param) {
    if (is.null(per_param)) return(global_limits)
    
    # Determine category: Cmax or AUC
    param_upper <- toupper(param_name)
    if (grepl("CMAX", param_upper)) {
      cat_limits <- per_param$Cmax
    } else if (grepl("AUC|AUMC", param_upper)) {
      cat_limits <- per_param$AUC
    } else {
      # Unknown parameter category — use global limits
      return(global_limits)
    }
    
    if (is.null(cat_limits)) return(global_limits)
    
    lower <- (cat_limits$lower %||% 80) / 100
    upper <- (cat_limits$upper %||% 125) / 100
    
    # Only use per-param limits if they differ from default 80/125
    # (i.e. user actually changed them in Advanced Options)
    return(c(lower, upper))
  }
  
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
    
    # Resolve per-parameter limits (falls back to global be_limits)
    param_be_limits <- resolve_param_limits(param, be_limits, be_limits_per_param)
    
    # Pre-compute limits for storage regardless of CI validity
    if (is.null(param_be_limits) || length(param_be_limits) < 2 || 
        is.na(param_be_limits[1]) || is.na(param_be_limits[2])) {
      lower_limit <- 80.0
      upper_limit <- 125.0
    } else {
      # Convert decimal limits to percentage if needed
      if (max(param_be_limits, na.rm = TRUE) <= 10) {
        lower_limit <- param_be_limits[1] * 100
        upper_limit <- param_be_limits[2] * 100
      } else {
        lower_limit <- param_be_limits[1]
        upper_limit <- param_be_limits[2]
      }
    }
    
    cat(sprintf("  📏 %s limits: %.2f%% - %.2f%%\n", param, lower_limit, upper_limit))
    
    if (is.na(ci_lower) || is.na(ci_upper) || is.infinite(ci_lower) || is.infinite(ci_upper)) {
      cat(sprintf("  ⚠️  Warning: Invalid confidence interval values for %s - skipping BE evaluation\n", param))
      within_limits <- FALSE
    } else {
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
      # Per-parameter limits used for this evaluation
      limits_used = list(lower = lower_limit, upper = upper_limit, type = "fixed"),
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
analyze_parallel_parameter <- function(data, parameter, alpha, welch_correction = TRUE) {
  
  # Prepare parameter data
  param_data <- prepare_parameter_data(data, parameter)
  
  if (nrow(param_data) < 4) {
    warning("Insufficient data for parameter: ", parameter)
    return(NULL)
  }
  
  # Separate test and reference data - handle all possible column names and values
  treatment_col <- NULL
  
  # Check all possible treatment column names in order of preference
  possible_cols <- c("Treatment", "Treatment", "treatment", "Treat")
  for (col in possible_cols) {
    if (col %in% names(param_data)) {
      treatment_col <- col
      break
    }
  }
  
  if (is.null(treatment_col)) {
    stop("Cannot find treatment column in data. Available columns: ", paste(names(param_data), collapse = ", "))
  }
  
  # Get unique treatment values
  unique_treatments <- unique(param_data[[treatment_col]])
  
  # Handle all possible test/reference value mappings
  test_data <- subset(param_data, param_data[[treatment_col]] %in% c("Test", "T", "test"))
  ref_data <- subset(param_data, param_data[[treatment_col]] %in% c("Reference", "R", "ref", "reference"))
  
  if (nrow(test_data) == 0 || nrow(ref_data) == 0) {
    warning("Missing test or reference data for parameter: ", parameter)
    return(NULL)
  }
  
  # EXACTLY like reference code: t.test on log data
  test_result <- t.test(
    test_data$log_param, 
    ref_data$log_param, 
    conf.level = 1 - alpha,  # Correct confidence level calculation for two-sided test
    var.equal = !welch_correction    # Use welch_correction parameter
  )
  
  
  # EXACTLY like reference code: calculate results
  logPE <- as.numeric(test_result$estimate[1] - test_result$estimate[2])
  point_estimate <- 100 * exp(logPE)
  ci_lower <- 100 * exp(test_result$conf.int[1])
  ci_upper <- 100 * exp(test_result$conf.int[2])
  
  # Return simple results structure
  ci_result <- list(
    point_estimate = point_estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    confidence_level = (1 - alpha) * 100,  # Correct confidence level
    t_statistic = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value
  )
  
  stats_result <- list(
    method = test_result$method,
    degrees_freedom = test_result$parameter,
    p_value = test_result$p.value,
    n_test = nrow(test_data),
    n_ref = nrow(ref_data)
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
  param_data$Treatment <- as.factor(param_data$Treatment)
  
  # Add sequence information if not present
  if (!"Sequence" %in% names(param_data)) {
    param_data <- add_sequence_info_replicate(param_data)
  }
  param_data$Sequence <- as.factor(param_data$Sequence)
  
  # Fit mixed-effects model
  model <- nlme::lme(
    log_param ~ Treatment + Period + Sequence,
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
  
  # Data should already have standardized column names from Shiny upload process
  # No need to call standardize_column_names() - removed to avoid redundancy
  
  # Check required columns
  required_cols <- get_required_columns(design)
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns for ", design, " design: ", 
         paste(missing_cols, collapse = ", "),
         "\nAvailable columns: ", paste(names(data), collapse = ", "))
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
  param_data$Treatment <- as.factor(param_data$Treatment)
  
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
  param_data$Treatment <- as.factor(param_data$Treatment)
  
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
    
    # Use per-parameter limits_used if already set on the CI result (from extract_be_from_anova)
    if (!is.null(ci$limits_used)) {
      param_lower <- ci$limits_used$lower %||% lower_limit
      param_upper <- ci$limits_used$upper %||% upper_limit
    } else {
      param_lower <- lower_limit
      param_upper <- upper_limit
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
    if (is.null(param_lower) || is.null(param_upper) || is.na(param_lower) || is.na(param_upper)) {
      cat(sprintf("  ⚠️ Invalid BE limits for %s: lower=%s, upper=%s\n", 
                  param, param_lower, param_upper))
      be_conclusions[[param]] <- NA
      next
    }
    
    tryCatch({
      condition1 <- ci_lower >= param_lower
      condition2 <- ci_upper <= param_upper
      
      if (is.na(condition1) || is.na(condition2)) {
        cat(sprintf("  ⚠️ NA conditions in BE evaluation for %s\n", param))
        is_be <- FALSE
      } else {
        is_be <- condition1 && condition2
      }
      
      cat(sprintf("  ✅ %s: CI [%.1f%%, %.1f%%] vs Limits [%.1f%%, %.1f%%] → %s\n", 
                  param, ci_lower, ci_upper, param_lower, param_upper,
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
    formula_str <- "log_param ~ Treatment + Subject + Period"
    
    # Check if we have required variables
    if (all(c("Treatment", "Subject", "Period") %in% names(param_data))) {
      
      # Convert to factors
      param_data$Treatment <- as.factor(param_data$Treatment)
      param_data$Subject <- as.factor(param_data$Subject)
      param_data$Period <- as.factor(param_data$Period)
      
      # Fit model
      model <- lm(log_param ~ Treatment + Subject + Period, data = param_data)
      
      # Extract treatment effect (Test vs Reference)
      formulation_coef <- coef(model)["TreatmentTest"]
      if (is.na(formulation_coef)) {
        # Try the other way around
        formulation_coef <- -coef(model)["TreatmentReference"]
      }
      
      if (!is.na(formulation_coef)) {
        # Get standard error
        model_summary <- summary(model)
        se <- model_summary$coefficients["TreatmentTest", "Std. Error"]
        if (is.na(se)) {
          se <- model_summary$coefficients["TreatmentReference", "Std. Error"]
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

#' Extract Treatment Effect from Model
#'
#' @param model Linear model object
#' @return List with formulation effect value and name
extract_formulation_effect <- function(model) {
  coefs <- coef(model)
  
  # Try different formulation coefficient names
  formulation_names <- c("TreatmentTest", "TreatmentT", "TreatmentReference", "TreatmentR")
  
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
      formulations <- unique(data$Treatment)
      ref_formulation <- if ("Reference" %in% formulations) {
        "Reference"
      } else if ("R" %in% formulations) {
        "R"
      } else {
        formulations[1]
      }
      
      # Count reference observations per subject without dplyr
      ref_data <- data[data$Treatment == ref_formulation, ]
      ref_counts <- tapply(rep(1, nrow(ref_data)), ref_data$Subject, sum)
      
      subjects_with_ref_replicates <- sum(ref_counts >= 2, na.rm = TRUE)
      
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
    "2x2x2" = c("Subject", "Period", "Treatment"),
    "parallel" = c("Subject", "Treatment"),
    "replicate" = c("Subject", "Period", "Treatment"),
    "auto" = c("Subject", "Treatment")  # Minimum for auto-detection
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
    Treatment = c("FORMULATION", "TRT", "TREATMENT", "tmt", "Treatment"),
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
  id_cols <- c("Subject", "Period", "Treatment", "Sequence")
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

#' Validate Treatment Coding
#'
#' @param data Study data
#' @return Data with validated formulation coding
validate_formulation_coding <- function(data) {
  # Standardize formulation levels
  if ("Treatment" %in% names(data)) {
    # Map common formulation codes
    data$Treatment <- as.character(data$Treatment)
    data$Treatment[data$Treatment %in% c("T", "Test", "1")] <- "Test"
    data$Treatment[data$Treatment %in% c("R", "Reference", "2")] <- "Reference"
    data$Treatment <- as.factor(data$Treatment)
    
    # Check that we have both levels
    levels_present <- levels(data$Treatment)
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
#' @param data Study data with Subject, Period, Treatment columns
#' @return List with design information
detect_replicate_design <- function(data) {
  
  # Data should already have standardized capitalized column names from Shiny upload
  # PREFER Sequence column if available (most accurate), otherwise infer from Period/Treatment
  
  # Check minimum required columns
  if (!"Subject" %in% colnames(data)) {
    stop("Data must contain Subject column. Found: ", paste(colnames(data), collapse = ", "))
  }
  
  # PREFERRED METHOD: Use Sequence column if available
  if ("Sequence" %in% colnames(data)) {
    cat("ℹ️  Using Sequence column for design detection (most accurate)\n")
    
    # Extract unique sequences
    sequences <- unique(data$Sequence)
    sequences <- sequences[!is.na(sequences)]
    
    if (length(sequences) == 0) {
      stop("Sequence column exists but contains no valid data")
    }
    
    # Determine periods from sequence length
    n_periods <- nchar(as.character(sequences[1]))
    
    # Count subjects per sequence
    sequence_dist <- table(sapply(unique(data$Subject), function(s) {
      as.character(data$Sequence[data$Subject == s][1])
    }))
    
    # Build design info
    design_info <- list()
    design_info$sequences <- as.character(sequences)
    design_info$n_periods <- n_periods
    design_info$sequence_distribution <- sequence_dist
    design_info$n_subjects_total <- length(unique(data$Subject))
    design_info$n_subjects_complete <- length(unique(data$Subject))
    design_info$subjects_with_missing_periods <- character(0)
    design_info$n_subjects_incomplete <- 0
    
    # Detect design type from sequences
    if (n_periods == 4) {
      if (any(grepl("TRTR|RTRT", sequences))) {
        design_info$design_name <- "2x2x4 (Full Replicate)"
        design_info$design_type <- "2x2x4 Full Replicate"
        design_info$is_replicate <- TRUE
        design_info$is_partial_replicate <- FALSE
      } else {
        design_info$design_name <- "4-Period Crossover"
        design_info$design_type <- "4-Period Crossover"
        design_info$is_replicate <- FALSE
        design_info$is_partial_replicate <- FALSE
      }
    } else if (n_periods == 3) {
      if (any(grepl("TRR|RTT|RRT|TTR", sequences))) {
        design_info$design_name <- "2x2x3 (Partial Replicate)"
        design_info$design_type <- "2x2x3 Partial Replicate"
        design_info$is_replicate <- TRUE
        design_info$is_partial_replicate <- TRUE
      } else if (any(grepl("TRT|RTR", sequences))) {
        design_info$design_name <- "2x3x3 (Full Replicate)"
        design_info$design_type <- "2x3x3 Full Replicate"
        design_info$is_replicate <- TRUE
        design_info$is_partial_replicate <- FALSE
      } else {
        design_info$design_name <- "3-Period Crossover"
        design_info$design_type <- "3-Period Crossover"
        design_info$is_replicate <- FALSE
        design_info$is_partial_replicate <- FALSE
      }
    } else if (n_periods == 2) {
      design_info$design_name <- "2x2x2 Crossover"
      design_info$design_type <- "2x2x2 Crossover"
      design_info$is_replicate <- FALSE
      design_info$is_partial_replicate <- FALSE
    } else {
      design_info$design_name <- paste0(n_periods, "-Period Crossover")
      design_info$design_type <- paste0(n_periods, "-Period Crossover")
      design_info$is_replicate <- FALSE
      design_info$is_partial_replicate <- FALSE
    }
    
    return(design_info)
  }
  
  # FALLBACK METHOD: Infer from Period and Treatment columns
  cat("ℹ️  Sequence column not found - inferring design from Period and Treatment\n")
  
  required_cols <- c("Period", "Treatment")
  if (!all(required_cols %in% colnames(data))) {
    stop("Data must contain Period and Treatment columns (or Sequence column). Found: ", 
         paste(colnames(data), collapse = ", "))
  }
  
  # Prefer Sequence column if available, otherwise use Treatment
  use_sequence <- "Sequence" %in% colnames(data)
  
  if (use_sequence) {
    cat("ℹ️  Using Sequence column for design detection\n")
    # Extract pattern from Sequence column directly
    subjects <- unique(data$Subject)
    subject_patterns <- data.frame(
      Subject = character(),
      pattern = character(),
      n_periods = numeric(),
      has_missing_periods = logical(),
      stringsAsFactors = FALSE
    )
    
    for (subj in subjects) {
      subj_data <- data[data$Subject == subj, ]
      
      # Get sequence from first row (should be same for all rows of same subject)
      pattern <- as.character(subj_data$Sequence[1])
      n_periods <- nchar(pattern)  # RTRT = 4, TRR = 3, etc.
      
      # Check for missing periods
      expected_periods <- 1:n_periods
      actual_periods <- unique(subj_data$Period)
      has_missing <- !all(expected_periods %in% actual_periods)
      
      subject_patterns <- rbind(subject_patterns, data.frame(
        Subject = subj,
        pattern = pattern,
        n_periods = n_periods,
        has_missing_periods = has_missing,
        stringsAsFactors = FALSE
      ))
    }
    
  } else {
    # Fallback: use Treatment column to build pattern
    if (!"Treatment" %in% colnames(data)) {
      stop("Data must contain either Sequence or Treatment column for design detection")
    }
    
    cat("ℹ️  Using Treatment column to infer design (Sequence column preferred)\n")
    
    subjects <- unique(data$Subject)
    subject_patterns <- data.frame(
      Subject = character(),
      pattern = character(),
      n_periods = numeric(),
      has_missing_periods = logical(),
      stringsAsFactors = FALSE
    )
    
    for (subj in subjects) {
      subj_data <- data[data$Subject == subj, ]
      subj_data <- subj_data[order(subj_data$Period), ]
      
      # Check for missing periods
      expected_periods <- 1:max(subj_data$Period)
      actual_periods <- subj_data$Period
      has_missing <- !all(expected_periods %in% actual_periods)
      
      pattern <- paste(subj_data$Treatment, collapse = "")
      n_periods <- nrow(subj_data)
      
      subject_patterns <- rbind(subject_patterns, data.frame(
        Subject = subj,
        pattern = pattern,
        n_periods = n_periods,
        has_missing_periods = has_missing,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Identify subjects with missing periods
  subjects_with_missing <- subject_patterns$Subject[subject_patterns$has_missing_periods]
  
  # Filter to complete subjects only for design detection
  complete_patterns <- subject_patterns[!subject_patterns$has_missing_periods, ]
  
  # Get unique patterns from complete subjects
  unique_patterns <- unique(complete_patterns$pattern)
  n_periods <- max(complete_patterns$n_periods)
  
  # Determine design type based on complete subjects only
  design_info <- list()
  
  if (n_periods == 3) {
    # Check for 2x2x3 designs (TRR, RTT, etc.)
    if (any(grepl("TRR|RTT", unique_patterns))) {
      design_info$design_name <- "2x2x3 (Partial Replicate)"
      design_info$design_type <- "2x2x3 Partial Replicate"
      design_info$is_replicate <- TRUE
      design_info$is_partial_replicate <- TRUE
      design_info$sequences <- unique_patterns
    } else if (any(grepl("TRT|RTR", unique_patterns))) {
      design_info$design_name <- "2x3x3 (Full Replicate)"
      design_info$design_type <- "2x3x3 Full Replicate"
      design_info$is_replicate <- TRUE
      design_info$is_partial_replicate <- FALSE
      design_info$sequences <- unique_patterns
    } else {
      design_info$design_name <- "3-Period Crossover"
      design_info$design_type <- "3-Period Crossover"
      design_info$is_replicate <- FALSE
      design_info$is_partial_replicate <- FALSE
      design_info$sequences <- unique_patterns
    }
  } else if (n_periods == 4) {
    # Check for 2x2x4 designs
    if (any(grepl("TRTR|RTRT", unique_patterns))) {
      design_info$design_name <- "2x2x4 (Full Replicate)"
      design_info$design_type <- "2x2x4 Full Replicate"
      design_info$is_replicate <- TRUE
      design_info$is_partial_replicate <- FALSE
      design_info$sequences <- unique_patterns
    } else {
      design_info$design_name <- "4-Period Crossover"
      design_info$design_type <- "4-Period Crossover"
      design_info$is_replicate <- FALSE
      design_info$is_partial_replicate <- FALSE
      design_info$sequences <- unique_patterns
    }
  } else if (n_periods == 2) {
    # Standard 2x2x2 crossover
    design_info$design_name <- "2x2x2 Crossover"
    design_info$design_type <- "2x2x2 Crossover"
    design_info$is_replicate <- FALSE
    design_info$is_partial_replicate <- FALSE
    design_info$sequences <- unique_patterns
  } else {
    design_info$design_name <- paste0(n_periods, "-Period Crossover")
    design_info$design_type <- paste0(n_periods, "-Period Crossover")
    design_info$is_replicate <- FALSE
    design_info$is_partial_replicate <- FALSE
    design_info$sequences <- unique_patterns
  }
  
  design_info$n_periods <- n_periods
  design_info$n_subjects_complete <- nrow(complete_patterns)
  design_info$n_subjects_total <- nrow(subject_patterns)
  design_info$sequence_distribution <- table(complete_patterns$pattern)
  design_info$subjects_with_missing_periods <- subjects_with_missing
  design_info$n_subjects_incomplete <- length(subjects_with_missing)
  
  # Add warnings if there are subjects with missing data
  if (length(subjects_with_missing) > 0) {
    design_info$warnings <- paste0(
      "WARNING: ", length(subjects_with_missing), 
      " subject(s) have missing periods and were excluded from design detection: ",
      paste(head(subjects_with_missing, 10), collapse = ", "),
      if (length(subjects_with_missing) > 10) " ..." else ""
    )
  }
  
  return(design_info)
}