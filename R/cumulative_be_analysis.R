#' Cumulative Bioequivalence Analysis Functions
#' 
#' Functions to perform progressive cumulative BE analysis
#' These functions are called during the main analysis pipeline
#'
#' @author BioEQ Team
#' @date 2025-08-21

# Load required packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("dplyr package is required for cumulative analysis")
}

#' Generate Cumulative BE Analysis Setup
#' 
#' Called from the main analysis pipeline to setup cumulative plots
#' Only supports crossover and replicate designs (not parallel)
#' 
#' @param data Merged PK data frame from BE analysis
#' @param parameters PK parameters to analyze (e.g., c("Cmax", "AUC0t"))
#' @param anova_method ANOVA method to use ("fixed", "nlme", etc.)
#' @param be_limits BE limits as vector c(lower, upper)
#' @param interactive Whether to create interactive plots
#' @param study_design Study design ("2x2x2", "replicate", etc.)
#' @return List containing analysis data setup, or error message
generate_cumulative_be_plots <- function(data, parameters = c("Cmax", "AUC0t", "AUC0inf"), 
                                        anova_method = "fixed", be_limits = c(0.8, 1.25),
                                        interactive = TRUE, study_design = NULL) {
  
  cat("\n📊 Setting up Cumulative BE Analysis...\n")
  
  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    return(list(error = "No data available for cumulative analysis"))
  }
  
  # Check if study design supports cumulative analysis
  if (!is.null(study_design) && study_design == "parallel") {
    return(list(error = "Cumulative analysis not supported for parallel studies"))
  }
  
  cat("Available columns:", paste(names(data), collapse = ", "), "\n")
  cat("Requested parameters:", paste(parameters, collapse = ", "), "\n")
  cat("ANOVA method:", anova_method, "\n")
  cat("Study design:", study_design, "\n")
  
  # Prepare data structure for cumulative analysis
  tryCatch({
    # Standardize and validate data structure
    analysis_data <- prepare_cumulative_data(data)
    
    if (is.null(analysis_data)) {
      return(list(error = "Failed to prepare data for cumulative analysis"))
    }
    
    # Get subject information for ordering
    subjects <- sort(unique(analysis_data$subject))
    n_subjects <- length(subjects)
    
    cat(sprintf("Found %d subjects for cumulative analysis\n", n_subjects))
    
    # Filter parameters to only those available in data
    available_params <- intersect(parameters, names(analysis_data))
    if (length(available_params) == 0) {
      # Try with ln prefix for log-transformed parameters
      ln_params <- paste0("ln", parameters)
      available_params <- intersect(ln_params, names(analysis_data))
      if (length(available_params) == 0) {
        return(list(error = "No valid PK parameters found in data"))
      }
    }
    
    # Return data structure needed for the UI
    return(list(
      analysis_data = analysis_data,
      available_subjects = subjects,
      default_order = subjects,
      parameters = available_params,
      anova_method = anova_method,
      be_limits = be_limits,
      study_design = study_design,
      error = NULL
    ))
    
  }, error = function(e) {
    cat("Error in cumulative analysis setup:", e$message, "\n")
    return(list(error = paste("Setup error:", e$message)))
  })
}

#' Prepare Data for Cumulative Analysis
#' 
#' Standardizes column names and validates data structure
#' 
#' @param data Raw PK data
#' @return Standardized data frame or NULL if invalid
prepare_cumulative_data <- function(data) {
  
  # Standardize column names - handle different naming conventions
  analysis_data <- data
  
  # Map common column name variations to standard names
  col_mappings <- list(
    "subject" = c("subject", "Subject", "SUBJECT", "subj", "Subj", "ID", "id"),
    "treatment" = c("treatment", "Treatment", "TREATMENT", "Formulation", "formulation", "trt", "Trt"),
    "period" = c("period", "Period", "PERIOD"),
    "sequence" = c("sequence", "Sequence", "SEQUENCE", "seq", "Seq")
  )
  
  for (std_name in names(col_mappings)) {
    possible_names <- col_mappings[[std_name]]
    found_col <- intersect(possible_names, names(analysis_data))
    if (length(found_col) > 0 && !std_name %in% names(analysis_data)) {
      names(analysis_data)[names(analysis_data) == found_col[1]] <- std_name
    }
  }
  
  # Validate required columns
  required_cols <- c("subject", "treatment")
  missing_cols <- setdiff(required_cols, names(analysis_data))
  
  if (length(missing_cols) > 0) {
    cat("Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
    return(NULL)
  }
  
  # Standardize treatment values
  if ("treatment" %in% names(analysis_data)) {
    analysis_data$treatment <- ifelse(
      analysis_data$treatment %in% c("T", "Test", "test"),
      "Test",
      ifelse(analysis_data$treatment %in% c("R", "Reference", "reference"), 
             "Reference", analysis_data$treatment)
    )
  }
  
  # Add period/sequence if missing (for crossover studies)
  if (!"period" %in% names(analysis_data)) {
    # Simple assumption: each subject appears twice, once per treatment
    if (requireNamespace("dplyr", quietly = TRUE)) {
      analysis_data <- analysis_data %>%
        dplyr::group_by(subject) %>%
        dplyr::mutate(period = dplyr::row_number()) %>%
        dplyr::ungroup()
    } else {
      # Manual grouping without dplyr
      subjects <- unique(analysis_data$subject)
      analysis_data$period <- 1
      for (subj in subjects) {
        subj_rows <- which(analysis_data$subject == subj)
        analysis_data$period[subj_rows] <- seq_along(subj_rows)
      }
    }
  }
  
  if (!"sequence" %in% names(analysis_data)) {
    # Default sequence assumption
    analysis_data$sequence <- "TR"
  }
  
  return(analysis_data)
}

#' Perform Progressive Cumulative BE Analysis
#' 
#' Runs complete BE analysis for each subset of subjects in specified order
#' 
#' @param data Standardized PK data
#' @param parameter PK parameter to analyze
#' @param subject_order Vector specifying order of subject inclusion
#' @param anova_method ANOVA method to use
#' @param be_limits BE limits as vector c(lower, upper)
#' @return Data frame with progressive analysis results
perform_progressive_be_analysis <- function(data, parameter, subject_order, 
                                          anova_method = "fixed", be_limits = c(0.8, 1.25)) {
  
  cat(sprintf("\n🔍 Starting Progressive BE Analysis for %s\n", parameter))
  cat("Subject order:", paste(subject_order, collapse = ", "), "\n")
  
  # Ensure we use log-transformed parameters for BE analysis
  log_parameter <- if (startsWith(parameter, "ln")) {
    parameter
  } else {
    paste0("ln", parameter)
  }
  
  # Check if log parameter exists in data
  if (!log_parameter %in% names(data)) {
    if (parameter %in% names(data)) {
      cat(sprintf("Creating log-transformed parameter: %s\n", log_parameter))
      data[[log_parameter]] <- log(data[[parameter]])
    } else {
      cat(sprintf("Error: Neither %s nor %s found in data\n", parameter, log_parameter))
      return(data.frame())
    }
  }
  
  cat(sprintf("Using parameter: %s for progressive analysis\n", log_parameter))
  
  # Initialize results
  cumulative_results <- data.frame(
    n_subjects = integer(),
    subjects_included = character(),
    point_estimate = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    within_be_limits = logical(),
    analysis_successful = logical(),
    error_message = character(),
    stringsAsFactors = FALSE
  )
  
  # Progressive analysis starting from n=1
  for (i in 1:length(subject_order)) {
    current_subjects <- subject_order[1:i]
    subset_data <- data[data$subject %in% current_subjects, ]
    
    cat(sprintf("  Analyzing subset %d/%d (subjects: %s)...\n", 
               i, length(subject_order), paste(current_subjects, collapse = ", ")))
    
    tryCatch({
      # Perform complete BE analysis for this subset
      be_result <- perform_subset_be_analysis(
        data = subset_data,
        parameter = log_parameter,
        anova_method = anova_method
      )
      
      if (!is.null(be_result) && !is.null(be_result$point_estimate) && 
          !is.na(be_result$point_estimate)) {
        # Convert BE limits to percentage if needed
        be_lower <- if (be_limits[1] < 1) be_limits[1] * 100 else be_limits[1]
        be_upper <- if (be_limits[2] < 10) be_limits[2] * 100 else be_limits[2]
        
        # Add results
        cumulative_results <- rbind(cumulative_results, data.frame(
          n_subjects = i,
          subjects_included = paste(current_subjects, collapse = ","),
          point_estimate = be_result$point_estimate,
          ci_lower = be_result$ci_lower,
          ci_upper = be_result$ci_upper,
          within_be_limits = (be_result$ci_lower >= be_lower && be_result$ci_upper <= be_upper),
          analysis_successful = TRUE,
          error_message = "",
          stringsAsFactors = FALSE
        ))
        
        cat(sprintf("    ✓ PE: %.2f%%, CI: [%.2f%%, %.2f%%]\n", 
                   be_result$point_estimate, be_result$ci_lower, be_result$ci_upper))
      } else {
        # Record failed analysis
        cumulative_results <- rbind(cumulative_results, data.frame(
          n_subjects = i,
          subjects_included = paste(current_subjects, collapse = ","),
          point_estimate = NA,
          ci_lower = NA,
          ci_upper = NA,
          within_be_limits = FALSE,
          analysis_successful = FALSE,
          error_message = "Analysis failed - insufficient data",
          stringsAsFactors = FALSE
        ))
        
        cat("    ✗ Analysis failed\n")
      }
      
    }, error = function(e) {
      # Record error
      cumulative_results <<- rbind(cumulative_results, data.frame(
        n_subjects = i,
        subjects_included = paste(current_subjects, collapse = ","),
        point_estimate = NA,
        ci_lower = NA,
        ci_upper = NA,
        within_be_limits = FALSE,
        analysis_successful = FALSE,
        error_message = paste("Error:", e$message),
        stringsAsFactors = FALSE
      ))
      
      cat(sprintf("    ✗ Error: %s\n", e$message))
    })
  }
  
  successful_analyses <- sum(cumulative_results$analysis_successful, na.rm = TRUE)
  cat(sprintf("✓ Completed progressive analysis: %d/%d successful\n", 
             successful_analyses, nrow(cumulative_results)))
  
  return(cumulative_results)
}

#' Perform BE Analysis for Data Subset
#' 
#' Runs complete bioequivalence analysis for a subset of subjects
#' 
#' @param data Subset of PK data
#' @param parameter PK parameter to analyze (should be log-transformed)
#' @param anova_method ANOVA method to use
#' @return List with point estimate and confidence intervals
perform_subset_be_analysis <- function(data, parameter, anova_method = "fixed") {
  
  # Validate that we have the parameter
  if (!parameter %in% names(data)) {
    cat(sprintf("Parameter %s not found in data\n", parameter))
    return(NULL)
  }
  
  # Remove any missing or invalid values
  valid_data <- data[!is.na(data[[parameter]]) & is.finite(data[[parameter]]), ]
  
  if (nrow(valid_data) < 2) {
    cat("Insufficient data rows after cleaning\n")
    return(NULL)
  }
  
  # Check that we have both treatments
  treatments <- unique(valid_data$treatment)
  if (length(treatments) < 2 || !all(c("Test", "Reference") %in% treatments)) {
    cat("Missing treatment groups - need both Test and Reference\n")
    return(NULL)
  }
  
  # Get unique subjects
  subjects <- unique(valid_data$subject)
  n_subjects <- length(subjects)
  
  cat(sprintf("    Subset has %d subjects, %d observations\n", n_subjects, nrow(valid_data)))
  
  # For single subject, calculate simple ratio (no CI possible)
  if (n_subjects == 1) {
    test_data <- valid_data[valid_data$treatment == "Test", ]
    ref_data <- valid_data[valid_data$treatment == "Reference", ]
    
    if (nrow(test_data) > 0 && nrow(ref_data) > 0) {
      log_ratio <- mean(test_data[[parameter]]) - mean(ref_data[[parameter]])
      pe <- exp(log_ratio) * 100
      
      cat(sprintf("    Single subject ratio: %.2f%%\n", pe))
      
      return(list(
        point_estimate = pe,
        ci_lower = pe,  # No CI for single subject
        ci_upper = pe
      ))
    } else {
      cat("    Single subject missing treatment data\n")
      return(NULL)
    }
  }
  
  # For multiple subjects, we need to create NCA-style data
  # The ANOVA function expects NCA results format with one row per subject-treatment
  
  # Check if we already have NCA-style data (one row per subject-treatment)
  subject_treatment_combinations <- valid_data %>%
    group_by(subject, treatment) %>%
    summarise(n_rows = n(), .groups = 'drop')
  
  # If we have multiple rows per subject-treatment, this is concentration-time data
  # We need to extract the PK parameter values (already calculated)
  if (any(subject_treatment_combinations$n_rows > 1)) {
    # Extract unique parameter values for each subject-treatment combination
    anova_data <- valid_data %>%
      group_by(subject, treatment) %>%
      summarise(
        !!parameter := first(.data[[parameter]]),  # PK parameter should be constant per subject-treatment
        sequence = first(sequence),
        period = first(period),
        .groups = 'drop'
      ) %>%
      ungroup()
  } else {
    # Data is already in NCA format
    anova_data <- valid_data
  }
  
  # Check if we have enough data for ANOVA
  if (nrow(anova_data) < 4) {  # Need at least 2 subjects x 2 treatments
    cat("    Insufficient data for ANOVA analysis\n")
    return(NULL)
  }
  
  # Check for balanced design (each subject should have both treatments)
  subject_treatment_counts <- anova_data %>%
    group_by(subject) %>%
    summarise(n_treatments = n(), .groups = 'drop')
  
  if (any(subject_treatment_counts$n_treatments < 2)) {
    cat("    Unbalanced design - some subjects missing treatments\n")
    return(NULL)
  }
  
  cat(sprintf("    Prepared ANOVA data: %d rows for %d subjects\n", 
             nrow(anova_data), n_subjects))
  
  # Try to use existing ANOVA function
  tryCatch({
    cat(sprintf("    Attempting ANOVA with %d rows, columns: %s\n", 
               nrow(anova_data), paste(names(anova_data), collapse = ", ")))
    
    anova_result <- perform_simple_anova(
      nca_data = anova_data,
      parameters = parameter,
      anova_model = anova_method
    )
    
    if (!is.null(anova_result[[parameter]]) && is.null(anova_result[[parameter]]$error)) {
      result <- anova_result[[parameter]]
      
      cat(sprintf("    ANOVA success: PE=%.2f%%, CI=[%.2f%%, %.2f%%]\n", 
                 result$pe_estimate, result$ci_lower, result$ci_upper))
      
      return(list(
        point_estimate = result$pe_estimate,
        ci_lower = result$ci_lower,
        ci_upper = result$ci_upper
      ))
    } else {
      cat("    ANOVA analysis returned null or error result\n")
      # Fall through to manual calculation
    }
    
  }, error = function(e) {
    cat(sprintf("    ANOVA error: %s\n", e$message))
    # Fall through to manual calculation
  })
  
  # Manual BE calculation as fallback
  cat("    Using manual BE calculation...\n")
  
  tryCatch({
    # Get Test and Reference values for each subject
    test_data <- anova_data[anova_data$treatment == "Test", ]
    ref_data <- anova_data[anova_data$treatment == "Reference", ]
    
    # Ensure we have matching subjects
    test_subjects <- sort(test_data$subject)
    ref_subjects <- sort(ref_data$subject)
    
    if (!identical(test_subjects, ref_subjects)) {
      cat("    Subject mismatch between treatments\n")
      return(NULL)
    }
    
    # Order data by subject to ensure matching
    test_data <- test_data[order(test_data$subject), ]
    ref_data <- ref_data[order(ref_data$subject), ]
    
    # Calculate log differences for each subject (crossover design)
    test_values <- test_data[[parameter]]
    ref_values <- ref_data[[parameter]]
    
    if (length(test_values) != length(ref_values) || length(test_values) < 2) {
      cat("    Insufficient paired data for calculation\n")
      return(NULL)
    }
    
    # Paired differences (Test - Reference) on log scale
    log_differences <- test_values - ref_values
    
    # Calculate statistics
    n <- length(log_differences)
    mean_diff <- mean(log_differences)
    se_diff <- sd(log_differences) / sqrt(n)
    df <- n - 1
    
    # 90% confidence interval
    t_critical <- qt(0.95, df)  # 90% CI = 5% in each tail
    
    ci_lower_log <- mean_diff - t_critical * se_diff
    ci_upper_log <- mean_diff + t_critical * se_diff
    
    # Convert to percentage scale
    pe <- exp(mean_diff) * 100
    ci_lower <- exp(ci_lower_log) * 100
    ci_upper <- exp(ci_upper_log) * 100
    
    cat(sprintf("    Manual calculation: PE=%.2f%%, CI=[%.2f%%, %.2f%%] (n=%d)\n", 
               pe, ci_lower, ci_upper, n))
    
    return(list(
      point_estimate = pe,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    ))
    
  }, error = function(e2) {
    cat(sprintf("    Manual calculation also failed: %s\n", e2$message))
    return(NULL)
  })
}

#' Create Cumulative BE Plot from Progressive Analysis
#' 
#' Creates simple ggplot visualization of cumulative BE results
#' 
#' @param cumulative_data Data frame with progressive analysis results
#' @param parameter Parameter name for title
#' @param be_limits BE acceptance limits as vector c(lower, upper)
#' @param interactive Whether to create interactive plot (for consistency)
#' @return ggplot object
create_cumulative_plot <- function(cumulative_data, parameter, be_limits, interactive = TRUE) {
  
  # Suppress global variable warnings
  n_subjects <- point_estimate <- ci_lower <- ci_upper <- NULL
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for cumulative plots")
  }
  
  # Filter out failed analyses
  plot_data <- cumulative_data[cumulative_data$analysis_successful & 
                              !is.na(cumulative_data$point_estimate), ]
  
  if (nrow(plot_data) == 0) {
    # Return empty plot with message
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, 
                       label = "No successful analyses for plotting",
                       size = 6, color = "gray") +
      ggplot2::theme_void()
    return(p)
  }
  
  # Convert BE limits to percentage if needed
  be_lower <- if (be_limits[1] < 1) be_limits[1] * 100 else be_limits[1]
  be_upper <- if (be_limits[2] < 10) be_limits[2] * 100 else be_limits[2]
  
  # Create the plot with simplified styling
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = n_subjects)) +
    
    # Add BE limit lines (simple dashed lines)
    ggplot2::geom_hline(yintercept = be_lower, color = "red", linetype = "dashed", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = be_upper, color = "red", linetype = "dashed", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 100, color = "gray", linetype = "solid", alpha = 0.5) +
    
    # Add confidence interval lines (black, simple)
    ggplot2::geom_line(ggplot2::aes(y = ci_lower), color = "black", linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(y = ci_upper), color = "black", linewidth = 1) +
    
    # Add point estimate line and points (blue)
    ggplot2::geom_line(ggplot2::aes(y = point_estimate), color = "blue", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = point_estimate), color = "blue", size = 2) +
    
    # Simple scaling
    ggplot2::scale_x_continuous(
      name = "Number of Subjects",
      breaks = pretty(plot_data$n_subjects)
    ) +
    
    ggplot2::scale_y_continuous(
      name = "Ratio (%) with 90% CI",
      limits = c(
        min(70, min(plot_data$ci_lower, na.rm = TRUE) * 0.95),
        max(140, max(plot_data$ci_upper, na.rm = TRUE) * 1.05)
      )
    ) +
    
    ggplot2::labs(
      title = paste('Cumulative Bioequivalence Analysis:', parameter),
      subtitle = "Red dashed lines show bioequivalence limits (80-125%)"
    ) +
    
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  # Return static plot (non-interactive by default for simplicity)
  return(p)
}
