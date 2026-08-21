# BioEQ - Non-Compartmental Analysis Functions
# Modernized NCA functions for bioequivalence analysis

# NOTE: The original hand-written NCA math (calculate_auc_linear,
# estimate_lambda_z, calculate_pk_parameters, calculate_auc_trap,
# calculate_lambda_z_fixed_points, calculate_pAUC,
# perform_enhanced_nca_analysis) was removed in the 2026-08 PKNCA
# migration - see R/nca_pknca.R for the replacement engine and
# bioeq-dead-code-audit-2026-08 / the NCA audit for why (the old code
# had a real defect: aic/ars/manual all included Cmax in the terminal
# regression). perform_nca_analysis() below is unchanged except that it
# now calls calculate_pk_parameters_pknca() instead of the removed
# calculate_pk_parameters().

#' Perform Complete NCA Analysis
#'
#' @param data Data frame with time and concentration data
#' @param id_cols Column names for subject/treatment identification
#' @param time_col Column name for time
#' @param conc_col Column name for concentration
#' @param dose Administered dose
#' @param lambda_z_method Method for lambda_z estimation
#' @return Data frame with NCA parameters for each profile
#' @export
perform_nca_analysis <- function(data, id_cols = c("subject", "treatment"), 
                                time_col = "time", conc_col = "concentration",
                                dose_col = NULL, lambda_z_method = "manual", 
                                auc_method = "mixed", lambda_z_points = 3,
                                calculate_pAUC = FALSE, pAUC_start = 0, pAUC_end = 2) {
  
  cat("🧮 Performing NCA analysis...\n")
  
  # Check which id_cols are actually available
  available_id_cols <- intersect(id_cols, names(data))
  missing_id_cols <- setdiff(id_cols, names(data))
  
  # Check if required time and concentration columns exist
  if (!time_col %in% names(data)) {
    cat(sprintf("❌ Time column '%s' not found in data\n", time_col))
    cat(sprintf("Available columns: %s\n", paste(names(data), collapse = ", ")))
    return(data.frame(error = "Missing time column"))
  }
  
  if (!conc_col %in% names(data)) {
    cat(sprintf("❌ Concentration column '%s' not found in data\n", conc_col))
    cat(sprintf("Available columns: %s\n", paste(names(data), collapse = ", ")))
    return(data.frame(error = "Missing concentration column"))
  }
  
  # Use only available id columns
  id_cols <- available_id_cols
  
  # Group by ID columns and calculate PK parameters for each profile
  results_list <- list()
  
  # Create grouping variable
  data$group_id <- do.call(paste, c(data[id_cols], sep = "_"))
  unique_groups <- unique(data$group_id)
  
  cat("  Analyzing", length(unique_groups), "concentration profiles...\n")
  
  for (i in seq_along(unique_groups)) {
    group_data <- data[data$group_id == unique_groups[i], ]
    
    # Extract ID information
    id_info <- group_data[1, id_cols, drop = FALSE]
    
    # Extract dose information if dose_col is provided, otherwise use default
    dose_value <- if (!is.null(dose_col) && dose_col %in% names(group_data)) {
      group_data[[dose_col]][1]  # Use first dose value for the group
    } else {
      1  # Default dose value
    }
    
    # Calculate PK parameters - PKNCA-backed engine (2026-08 migration;
    # see R/nca_pknca.R header for rationale). calculate_pk_parameters()
    # (the original hand-written implementation) is retained below,
    # unused, only as a documented reference of the prior behavior.
    pk_params <- calculate_pk_parameters_pknca(
      time = group_data[[time_col]],
      conc = group_data[[conc_col]],
      dose = dose_value,
      lambda_z_method = lambda_z_method,
      auc_method = auc_method,
      lambda_z_points = lambda_z_points,
      calculate_pAUC = calculate_pAUC,
      pAUC_start = pAUC_start,
      pAUC_end = pAUC_end
    )
    
    # Combine ID info with PK parameters
    result_row <- cbind(id_info, as.data.frame(pk_params))
    results_list[[i]] <- result_row
  }
  
  # Combine all results
  nca_results <- do.call(rbind, results_list)
  rownames(nca_results) <- NULL
  
  cat("✅ NCA analysis completed\n")
  cat("  Parameters calculated for", nrow(nca_results), "profiles\n\n")
  
  # Add class for S3 methods
  class(nca_results) <- c("nca_results", "data.frame")
  
  return(nca_results)
}

#' Print NCA Results
#'
#' @param x NCA results object
#' @export
print.nca_results <- function(x, ...) {
  cat("🧮 NCA Analysis Results\n")
  cat("======================\n\n")
  
  # Summary statistics
  cat("Number of profiles:", nrow(x), "\n")
  
  if ("tmt" %in% names(x)) {
    tmt_counts <- table(x$tmt)
    cat("Treatment groups:\n")
    for (i in seq_along(tmt_counts)) {
      tmt_name <- ifelse(names(tmt_counts)[i] == "1", "Reference", "Test")
      cat("  ", tmt_name, ":", tmt_counts[i], "profiles\n")
    }
  }
  
  cat("\nKey Parameters Summary:\n")
  
  # Select key parameters for summary
  key_params <- c("Cmax", "Tmax", "AUC0t", "AUC0inf", "t_half", "lambda_z_r_squared")
  available_params <- intersect(key_params, names(x))
  
  if (length(available_params) > 0) {
    summary_data <- x[available_params]
    print(summary(summary_data))
  }
  
  invisible(x)
}
