# BioEQ - Non-Compartmental Analysis Functions
# Modernized NCA functions for bioequivalence analysis

#' Calculate AUC using Linear Trapezoidal Rule
#'
#' @param time Vector of time points
#' @param conc Vector of concentrations
#' @param method Method for AUC calculation ("linear", "log", "mixed", "linear_log")
#' @return AUC value
#' @export
calculate_auc_linear <- function(time, conc, method = "linear") {
  # Remove NA values and sort by time
  valid_idx <- !is.na(time) & !is.na(conc) & conc >= 0
  time <- time[valid_idx]
  conc <- conc[valid_idx]
  
  if (length(time) < 2) {
    return(NA)
  }
  
  # Sort by time
  order_idx <- order(time)
  time <- time[order_idx]
  conc <- conc[order_idx]
  
  auc <- 0
  
  # Find Cmax index for linear_log method
  cmax_idx <- which.max(conc)
  
  for (i in 2:length(time)) {
    dt <- time[i] - time[i-1]
    
    if (method == "linear" || conc[i-1] == 0 || conc[i] == 0) {
      # Linear trapezoidal
      auc <- auc + dt * (conc[i-1] + conc[i]) / 2
    } else if (method == "log") {
      # Log-linear trapezoidal
      if (conc[i] != conc[i-1]) {
        auc <- auc + dt * (conc[i] - conc[i-1]) / log(conc[i] / conc[i-1])
      } else {
        auc <- auc + dt * conc[i]
      }
    } else if (method == "mixed") {
      # Mixed: linear up, log down
      if (conc[i] >= conc[i-1]) {
        # Linear
        auc <- auc + dt * (conc[i-1] + conc[i]) / 2
      } else {
        # Log-linear
        if (conc[i] != conc[i-1]) {
          auc <- auc + dt * (conc[i] - conc[i-1]) / log(conc[i] / conc[i-1])
        } else {
          auc <- auc + dt * conc[i]
        }
      }
    } else if (method == "linear_log") {
      # Linear up to Cmax, log after Cmax
      if (i <= cmax_idx) {
        # Before or at Cmax - use linear (absorption phase)
        auc <- auc + dt * (conc[i-1] + conc[i]) / 2
      } else {
        # After Cmax - use log if possible (elimination phase)
        if (conc[i] != conc[i-1]) {
          auc <- auc + dt * (conc[i] - conc[i-1]) / log(conc[i] / conc[i-1])
        } else {
          auc <- auc + dt * conc[i]
        }
      }
    }
  }
  
  return(auc)
}

#' Estimate Lambda_z using Different Methods
#'
#' @param time Vector of time points
#' @param conc Vector of concentrations
#' @param method Method for lambda_z estimation ("aic", "ars", "ttt", "manual")
#' @param n_points Number of points to use (for manual method)
#' @return List with lambda_z estimate and related parameters
#' @export
estimate_lambda_z <- function(time, conc, method = "aic", n_points = 3, tmax = NULL) {
  # Filter out zero concentrations and ensure we have enough points
  valid_idx <- which(conc > 0)
  if (length(valid_idx) < 3) {
    return(list(
      lambda_z = NA,
      t_half = NA,
      r_squared = NA,
      n_points_used = 0,
      start_idx = NA,
      end_idx = NA
    ))
  }
  
  time_valid <- time[valid_idx]
  conc_valid <- conc[valid_idx]
  log_conc <- log(conc_valid)
  
  # Ensure we have at least 3 points after Cmax
  cmax_idx <- which.max(conc_valid)
  post_cmax_idx <- which(valid_idx > cmax_idx)
  
  if (length(post_cmax_idx) < 3) {
    return(list(
      lambda_z = NA,
      t_half = NA,
      r_squared = NA,
      n_points_used = 0,
      start_idx = NA,
      end_idx = NA
    ))
  }
  
  # Method-specific selection of points
  if (method == "manual") {
    # Manual: User specifies exact number of terminal points
    n_available <- length(time_valid)
    n_use <- min(n_points, n_available)
    if (n_use < 3) {
      return(list(
        lambda_z = NA,
        t_half = NA,
        r_squared = NA,
        n_points_used = 0,
        start_idx = NA,
        end_idx = NA
      ))
    }
    
    # Use the last n_use points
    start_idx <- n_available - n_use + 1
    end_idx <- n_available
    selected_idx <- start_idx:end_idx
    
  } else if (method == "ars") {
    # ARS: Maximize adjusted R-squared
    n_available <- length(time_valid)
    best_adj_r_sq <- -Inf
    best_start_idx <- NA
    
    # Try different starting points (must have at least 3 points)
    for (start in 1:(n_available - 2)) {
      # Fit linear regression
      x <- time_valid[start:n_available]
      y <- log_conc[start:n_available]
      n <- length(x)
      
      if (n >= 3 && all(is.finite(y))) {
        fit <- lm(y ~ x)
        r_sq <- summary(fit)$r.squared
        # Calculate adjusted R-squared
        adj_r_sq <- 1 - (1 - r_sq) * (n - 1) / (n - 2)
        
        if (adj_r_sq > best_adj_r_sq) {
          best_adj_r_sq <- adj_r_sq
          best_start_idx <- start
        }
      }
    }
    
    if (is.na(best_start_idx)) {
      return(list(
        lambda_z = NA,
        t_half = NA,
        r_squared = NA,
        n_points_used = 0,
        start_idx = NA,
        end_idx = NA
      ))
    }
    
    start_idx <- best_start_idx
    end_idx <- n_available
    selected_idx <- start_idx:end_idx
    
  } else if (method == "aic") {
    # AIC: Already implemented
    n_available <- length(time_valid)
    best_aic <- Inf
    best_start_idx <- NA
    
    # Try different starting points (must have at least 3 points)
    for (start in 1:(n_available - 2)) {
      # Fit linear regression
      x <- time_valid[start:n_available]
      y <- log_conc[start:n_available]
      
      if (all(is.finite(y))) {
        fit <- lm(y ~ x)
        n <- length(x)
        rss <- sum(residuals(fit)^2)
        # AIC = n * ln(RSS/n) + 2k where k=2 (intercept and slope)
        aic <- n * log(rss/n) + 2 * 2
        
        if (aic < best_aic) {
          best_aic <- aic
          best_start_idx <- start
        }
      }
    }
    
    if (is.na(best_start_idx)) {
      return(list(
        lambda_z = NA,
        t_half = NA,
        r_squared = NA,
        n_points_used = 0,
        start_idx = NA,
        end_idx = NA
      ))
    }
    
    start_idx <- best_start_idx
    end_idx <- n_available
    selected_idx <- start_idx:end_idx
    
  } else if (method == "ttt") {
    # TTT: Two-Times-Tmax - use all points from 2*Tmax onwards
    if (is.null(tmax)) {
      # Find Tmax from the data if not provided
      tmax <- time_valid[which.max(conc_valid)]
    }
    
    two_times_tmax <- 2 * tmax
    
    # Find points after 2*Tmax
    eligible_idx <- which(time_valid >= two_times_tmax)
    
    if (length(eligible_idx) < 3) {
      # If not enough points after 2*Tmax, fall back to using points after Tmax
      eligible_idx <- which(time_valid > tmax)
    }
    
    if (length(eligible_idx) < 3) {
      return(list(
        lambda_z = NA,
        t_half = NA,
        r_squared = NA,
        n_points_used = 0,
        start_idx = NA,
        end_idx = NA
      ))
    }
    
    selected_idx <- eligible_idx
    start_idx <- min(selected_idx)
    end_idx <- max(selected_idx)
    
  } else {
    stop(paste("Unknown lambda_z estimation method:", method))
  }
  
  # Perform regression on selected points
  x <- time_valid[selected_idx]
  y <- log_conc[selected_idx]
  
  if (length(x) < 3 || !all(is.finite(y))) {
    return(list(
      lambda_z = NA,
      t_half = NA,
      r_squared = NA,
      n_points_used = 0,
      start_idx = NA,
      end_idx = NA
    ))
  }
  
  fit <- lm(y ~ x)
  summary_fit <- summary(fit)
  lambda_z <- -coef(fit)[2]  # Negative of slope
  t_half <- log(2) / lambda_z
  r_squared <- summary_fit$r.squared
  lambda_z_p_value <- summary_fit$coefficients[2, 4]  # P-value for slope
  lambda_z_t_value <- summary_fit$coefficients[2, 3]  # T-statistic
  lambda_z_se <- summary_fit$coefficients[2, 2]       # Standard error
  
  return(list(
    lambda_z = lambda_z,
    t_half = t_half,
    r_squared = r_squared,
    lambda_z_p_value = lambda_z_p_value,
    lambda_z_t_value = lambda_z_t_value,
    lambda_z_se = lambda_z_se,
    n_points_used = length(selected_idx),
    start_idx = valid_idx[start_idx],
    end_idx = valid_idx[end_idx],
    method = method
  ))
}

#' Calculate AUC to a specific time point
#'
#' @param time Vector of time points
#' @param conc Vector of concentrations  
#' @param target_time Target time for AUC calculation
#' @param method Method for AUC calculation
#' @return List with AUC value and method used
#' @export
calculate_auc_to_time <- function(time, conc, target_time, method = "mixed") {
  # Remove NA values and sort by time
  valid_idx <- !is.na(time) & !is.na(conc) & conc >= 0
  time <- time[valid_idx]
  conc <- conc[valid_idx]
  
  if (length(time) < 2) {
    return(list(auc = NA, method = "insufficient_data"))
  }
  
  # Sort by time
  order_idx <- order(time)
  time <- time[order_idx]
  conc <- conc[order_idx]
  
  # Find the index for target_time
  if (target_time <= max(time)) {
    # Target time is within data range
    if (target_time %in% time) {
      # Exact time point exists
      target_idx <- which(time == target_time)
      return(list(
        auc = calculate_auc_linear(time[1:target_idx], conc[1:target_idx], method = method),
        method = paste0(method, "_exact")
      ))
    } else {
      # Need to interpolate
      upper_idx <- which(time > target_time)[1]
      if (is.na(upper_idx) || upper_idx == 1) {
        return(list(auc = NA, method = "interpolation_failed"))
      }
      
      lower_idx <- upper_idx - 1
      
      # Linear interpolation for concentration at target_time
      time_diff <- time[upper_idx] - time[lower_idx]
      conc_diff <- conc[upper_idx] - conc[lower_idx]
      target_conc <- conc[lower_idx] + (target_time - time[lower_idx]) * conc_diff / time_diff
      
      # Calculate AUC to interpolated point
      time_subset <- c(time[1:lower_idx], target_time)
      conc_subset <- c(conc[1:lower_idx], target_conc)
      
      return(list(
        auc = calculate_auc_linear(time_subset, conc_subset, method = method),
        method = paste0(method, "_interpolated")
      ))
    }
  } else {
    # Target time is beyond data range
    return(list(auc = NA, method = "beyond_data_range"))
  }
}

#' Calculate Pharmacokinetic Parameters
#'
#' @param time Vector of time points
#' @param conc Vector of concentrations
#' @param dose Administered dose
#' @param lambda_z_method Method for lambda_z estimation
#' @return List of PK parameters
#' @export
calculate_pk_parameters <- function(time, conc, dose = 1, lambda_z_method = "manual", 
                                  auc_method = "mixed", lambda_z_points = 3, 
                                  calculate_pAUC = FALSE, pAUC_start = 0, pAUC_end = 2) {
  # Basic data validation
  valid_idx <- !is.na(time) & !is.na(conc)
  time_clean <- time[valid_idx]
  conc_clean <- conc[valid_idx]
  
  if (length(time_clean) < 2) {
    return(list(error = "Insufficient data points"))
  }
  
  # Sort by time
  order_idx <- order(time_clean)
  time_clean <- time_clean[order_idx]
  conc_clean <- conc_clean[order_idx]
  
  # Initialize results list
  pk_params <- list()
  
  # Cmax and Tmax
  cmax_idx <- which.max(conc_clean)
  pk_params$Cmax <- conc_clean[cmax_idx]
  pk_params$Tmax <- time_clean[cmax_idx]
  
  # AUC0-t (to last measurable concentration)
  last_measurable_idx <- max(which(conc_clean > 0))
  if (last_measurable_idx > 1) {
    pk_params$AUC0t <- calculate_auc_linear(
      time_clean[1:last_measurable_idx], 
      conc_clean[1:last_measurable_idx], 
      method = auc_method
    )
    pk_params$Tlast <- time_clean[last_measurable_idx]
    pk_params$Clast <- conc_clean[last_measurable_idx]
  } else {
    pk_params$AUC0t <- NA
    pk_params$Tlast <- NA
    pk_params$Clast <- NA
  }
  
  # Calculate pAUC if requested
  if (calculate_pAUC) {
    pk_params$pAUC <- calculate_pAUC(
      time_clean, 
      conc_clean, 
      start_time = pAUC_start, 
      end_time = pAUC_end, 
      method = auc_method
    )
    pk_params$pAUC_start <- pAUC_start
    pk_params$pAUC_end <- pAUC_end
  }
  
  # Calculate AUC0-72 for long half-life drugs
  # Always calculate this as it may be needed for ANOVA
  auc072_result <- calculate_auc_to_time(time_clean, conc_clean, target_time = 72, method = auc_method)
  pk_params$AUC072 <- auc072_result$auc
  pk_params$AUC072_method <- auc072_result$method
  
  # Lambda_z estimation
  # Find Tmax for TTT method
  tmax <- time[which.max(conc)]
  
  # Estimate lambda_z and half-life with the selected method
  lambda_z_result <- estimate_lambda_z(
    time, 
    conc, 
    method = lambda_z_method,
    n_points = lambda_z_points,  # Pass manual points parameter
    tmax = tmax  # Pass Tmax for TTT method
  )
  pk_params$lambda_z <- lambda_z_result$lambda_z
  pk_params$lambda_z_r_squared <- lambda_z_result$r_squared
  pk_params$lambda_z_p_value <- lambda_z_result$lambda_z_p_value
  pk_params$lambda_z_t_value <- lambda_z_result$lambda_z_t_value
  pk_params$lambda_z_se <- lambda_z_result$lambda_z_se
  pk_params$lambda_z_points <- lambda_z_result$n_points_used  # Use n_points_used not points_used
  pk_params$lambda_z_method <- lambda_z_method  # This is already correct
  
  # Derived parameters
  if (!is.na(pk_params$lambda_z) && pk_params$lambda_z > 0) {
    # Half-life
    pk_params$t_half <- log(2) / pk_params$lambda_z
    
    # AUC0-inf
    if (!is.na(pk_params$Clast) && pk_params$Clast > 0) {
      auc_extrapolated <- pk_params$Clast / pk_params$lambda_z
      pk_params$AUC0inf <- pk_params$AUC0t + auc_extrapolated
      pk_params$AUC_percent_extrap <- (auc_extrapolated / pk_params$AUC0inf) * 100
    } else {
      pk_params$AUC0inf <- NA
      pk_params$AUC_percent_extrap <- NA
    }
    
    # Clearance and Volume of distribution
    if (!is.na(pk_params$AUC0inf)) {
      pk_params$CL_F <- dose / pk_params$AUC0inf
      pk_params$Vd_F <- pk_params$CL_F / pk_params$lambda_z
    } else {
      pk_params$CL_F <- NA
      pk_params$Vd_F <- NA
    }
    
    # Mean Residence Time
    # Simplified calculation - could be enhanced
    if (!is.na(pk_params$AUC0inf)) {
      # AUMC calculation would be needed for accurate MRT
      pk_params$MRT <- 1 / pk_params$lambda_z  # Simplified
    } else {
      pk_params$MRT <- NA
    }
  } else {
    pk_params$t_half <- NA
    pk_params$AUC0inf <- NA
    pk_params$AUC_percent_extrap <- NA
    pk_params$CL_F <- NA
    pk_params$Vd_F <- NA
    pk_params$MRT <- NA
  }
  
  # Add metadata
  pk_params$dose <- dose
  pk_params$lambda_z_method <- lambda_z_method
  pk_params$analysis_time <- Sys.time()
  
  # Add log-transformed parameters for BE analysis
  # These are the actual parameters used for bioequivalence assessment
  
  # Standard log-transformed parameters
  if (!is.na(pk_params$Cmax) && pk_params$Cmax > 0) {
    pk_params$lnCmax <- log(pk_params$Cmax)
  } else {
    pk_params$lnCmax <- NA
  }
  
  if (!is.na(pk_params$AUC0t) && pk_params$AUC0t > 0) {
    pk_params$lnAUC0t <- log(pk_params$AUC0t)
  } else {
    pk_params$lnAUC0t <- NA
  }
  
  if (!is.na(pk_params$AUC0inf) && pk_params$AUC0inf > 0) {
    pk_params$lnAUC0inf <- log(pk_params$AUC0inf)
  } else {
    pk_params$lnAUC0inf <- NA
  }
  
  # Additional log-transformed parameters for optional PK endpoints
  
  # lnTmax - Always calculate since Tmax is always available
  if (!is.na(pk_params$Tmax) && pk_params$Tmax > 0) {
    pk_params$lnTmax <- log(pk_params$Tmax)
  } else {
    pk_params$lnTmax <- NA
  }
  
  # lnpAUC - Only if pAUC was calculated
  if (!is.null(pk_params$pAUC) && !is.na(pk_params$pAUC) && pk_params$pAUC > 0) {
    pk_params$lnpAUC <- log(pk_params$pAUC)
  } else {
    pk_params$lnpAUC <- NA
  }
  
  # lnAUC072 - Always calculate since AUC072 is always calculated
  if (!is.na(pk_params$AUC072) && pk_params$AUC072 > 0) {
    pk_params$lnAUC072 <- log(pk_params$AUC072)
  } else {
    pk_params$lnAUC072 <- NA
  }
  
  return(pk_params)
}

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
  cat(sprintf("[DEBUG] Requested id_cols: %s\n", paste(id_cols, collapse = ", ")))
  cat(sprintf("[DEBUG] Available data columns: %s\n", paste(names(data), collapse = ", ")))
  
  # Check which id_cols are actually available
  available_id_cols <- intersect(id_cols, names(data))
  missing_id_cols <- setdiff(id_cols, names(data))
  
  cat(sprintf("[DEBUG] Available id_cols: %s\n", paste(available_id_cols, collapse = ", ")))
  if (length(missing_id_cols) > 0) {
    cat(sprintf("[DEBUG] Missing id_cols: %s\n", paste(missing_id_cols, collapse = ", ")))
  }
  
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
    
    # Calculate PK parameters
    pk_params <- calculate_pk_parameters(
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

#' Alias for calculate_auc_linear for backward compatibility
#'
#' @param time Vector of time points
#' @param conc Vector of concentrations
#' @return AUC value using linear trapezoidal rule
#' @export
calculate_auc_trap <- function(time, conc) {
  calculate_auc_linear(time, conc, method = "linear")
}

#' Calculate lambda_z using user-specified terminal points
#'
#' @param times Vector of time points
#' @param concentrations Vector of concentrations
#' @param n_points Number of terminal points to use (default: 3)
#' @param min_points Minimum points required (default: 3)
#' @return List with lambda_z, r_squared, and terminal points used
#' @export
calculate_lambda_z_fixed_points <- function(times, concentrations, n_points = 3, min_points = 3) {
  
  # Remove zero concentrations and time = 0
  valid_idx <- which(concentrations > 0 & times > 0)
  
  if (length(valid_idx) < min_points) {
    return(list(
      lambda_z = NA,
      r_squared = NA,
      n_points = 0,
      terminal_points = NULL,
      slope = NA,
      intercept = NA,
      method = "insufficient_points"
    ))
  }
  
  valid_times <- times[valid_idx]
  valid_concs <- concentrations[valid_idx]
  
  # Use last n_points for terminal phase
  n_available <- length(valid_times)
  n_use <- min(n_points, n_available)
  
  # Select terminal points (last n_use points)
  terminal_idx <- (n_available - n_use + 1):n_available
  terminal_times <- valid_times[terminal_idx]
  terminal_concs <- valid_concs[terminal_idx]
  
  # Log-linear regression
  ln_concs <- log(terminal_concs)
  
  # Perform linear regression: ln(C) = intercept - lambda_z * time
  fit <- lm(ln_concs ~ terminal_times)
  summary_fit <- summary(fit)
  
  # Extract parameters
  lambda_z <- -coef(fit)[2]  # Negative of slope
  intercept <- coef(fit)[1]
  r_squared <- summary_fit$r.squared
  lambda_z_p_value <- summary_fit$coefficients[2, 4]  # P-value for slope
  lambda_z_t_value <- summary_fit$coefficients[2, 3]  # T-statistic
  lambda_z_se <- summary_fit$coefficients[2, 2]       # Standard error
  
  # Calculate AUC extrapolation from last observed point
  last_time <- max(terminal_times)
  last_conc <- terminal_concs[which.max(terminal_times)]
  auc_extrapolated <- last_conc / lambda_z
  
  return(list(
    lambda_z = as.numeric(lambda_z),
    r_squared = r_squared,
    lambda_z_p_value = as.numeric(lambda_z_p_value),
    lambda_z_t_value = as.numeric(lambda_z_t_value),
    lambda_z_se = as.numeric(lambda_z_se),
    n_points = n_use,
    terminal_points = data.frame(
      time = terminal_times,
      conc = terminal_concs,
      ln_conc = ln_concs
    ),
    slope = -as.numeric(lambda_z),
    intercept = as.numeric(intercept),
    last_time = last_time,
    last_conc = last_conc,
    auc_extrapolated = as.numeric(auc_extrapolated),
    method = "fixed_points"
  ))
}

#' Calculate partial AUC (pAUC) between specified time points
#'
#' @param time Vector of time points
#' @param conc Vector of concentrations
#' @param start_time Start time for partial AUC calculation
#' @param end_time End time for partial AUC calculation
#' @param method AUC calculation method ("linear", "log", "mixed")
#' @return Partial AUC value between start_time and end_time
#' @export
calculate_pAUC <- function(time, conc, start_time = 0, end_time = 2, method = "linear") {
  # Basic validation
  if (length(time) != length(conc)) {
    return(NA)
  }
  
  if (start_time >= end_time) {
    warning("Start time must be less than end time")
    return(NA)
  }
  
  # Remove invalid data points
  valid_idx <- !is.na(time) & !is.na(conc) & conc >= 0
  time_clean <- time[valid_idx]
  conc_clean <- conc[valid_idx]
  
  if (length(time_clean) < 2) {
    return(NA)
  }
  
  # Sort by time
  order_idx <- order(time_clean)
  time_clean <- time_clean[order_idx]
  conc_clean <- conc_clean[order_idx]
  
  # Find time points within the specified range
  in_range <- time_clean >= start_time & time_clean <= end_time
  
  if (sum(in_range) < 2) {
    # Need to interpolate if we don't have enough points in range
    range_times <- c(start_time, time_clean[time_clean > start_time & time_clean < end_time], end_time)
    range_concs <- c()
    
    for (t in range_times) {
      if (t %in% time_clean) {
        # Exact time point exists
        range_concs <- c(range_concs, conc_clean[time_clean == t])
      } else {
        # Interpolate concentration at this time point
        if (t < min(time_clean)) {
          # Extrapolate backwards (usually zero)
          range_concs <- c(range_concs, 0)
        } else if (t > max(time_clean)) {
          # Extrapolate forwards (usually zero or last value)
          range_concs <- c(range_concs, 0)
        } else {
          # Linear interpolation
          interpolated_conc <- approx(time_clean, conc_clean, xout = t, method = "linear")$y
          range_concs <- c(range_concs, interpolated_conc)
        }
      }
    }
    
    time_subset <- range_times
    conc_subset <- range_concs
  } else {
    # Use points within range and add boundaries if needed
    time_subset <- time_clean[in_range]
    conc_subset <- conc_clean[in_range]
    
    # Add start time if not present
    if (!start_time %in% time_subset) {
      start_conc <- if (start_time < min(time_clean)) {
        0  # Assume zero concentration before first measurement
      } else {
        approx(time_clean, conc_clean, xout = start_time, method = "linear")$y
      }
      time_subset <- c(start_time, time_subset)
      conc_subset <- c(start_conc, conc_subset)
    }
    
    # Add end time if not present
    if (!end_time %in% time_subset) {
      end_conc <- if (end_time > max(time_clean)) {
        0  # Assume zero concentration after last measurement
      } else {
        approx(time_clean, conc_clean, xout = end_time, method = "linear")$y
      }
      time_subset <- c(time_subset, end_time)
      conc_subset <- c(conc_subset, end_conc)
    }
    
    # Sort again
    order_idx <- order(time_subset)
    time_subset <- time_subset[order_idx]
    conc_subset <- conc_subset[order_idx]
  }
  
  # Calculate AUC using the specified method
  if (length(time_subset) >= 2) {
    pAUC <- calculate_auc_linear(time_subset, conc_subset, method = method)
    return(as.numeric(pAUC))
  } else {
    return(NA)
  }
}

#' Enhanced NCA analysis with detailed lambda_z reporting
#'
#' @param data Data frame with columns: subj, tmt, time, conc
#' @param lambda_points Number of terminal points for lambda_z (default: 3)
#' @return Data frame with NCA parameters and detailed lambda_z info
#' @export
perform_enhanced_nca_analysis <- function(data, lambda_points = 3) {
  
  subjects <- unique(data$subj)
  treatments <- unique(data$tmt)
  
  results <- data.frame()
  lambda_details <- list()
  
  for (subj in subjects) {
    for (trt in treatments) {
      # Get subject-treatment data
      subj_data <- data[data$subj == subj & data$tmt == trt, ]
      
      if (nrow(subj_data) > 0) {
        # Sort by time
        subj_data <- subj_data[order(subj_data$time), ]
        
        times <- subj_data$time
        concentrations <- subj_data$conc
        
        # Calculate basic NCA parameters
        auc0t <- calculate_auc_linear(times, concentrations)
        cmax <- max(concentrations)
        tmax <- times[which.max(concentrations)]
        
        # Calculate lambda_z with fixed points
        lambda_result <- calculate_lambda_z_fixed_points(times, concentrations, 
                                                        n_points = lambda_points)
        
        # Calculate AUC0inf
        auc0inf <- if (!is.na(lambda_result$lambda_z) && lambda_result$lambda_z > 0) {
          auc0t + lambda_result$auc_extrapolated
        } else {
          NA
        }
        
        # Calculate half-life
        t_half <- if (!is.na(lambda_result$lambda_z) && lambda_result$lambda_z > 0) {
          log(2) / lambda_result$lambda_z
        } else {
          NA
        }
        
        # Store detailed lambda_z information
        lambda_key <- paste0("S", subj, "_T", trt)
        lambda_details[[lambda_key]] <- list(
          subject = subj,
          treatment = trt,
          lambda_z = lambda_result$lambda_z,
          r_squared = lambda_result$r_squared,
          n_points = lambda_result$n_points,
          terminal_points = lambda_result$terminal_points,
          auc_extrapolated = lambda_result$auc_extrapolated,
          method = lambda_result$method
        )
        
        # Create result row
        result_row <- data.frame(
          subj = subj,
          tmt = trt,
          Cmax = cmax,
          Tmax = tmax,
          AUC0t = auc0t,
          AUC0inf = auc0inf,
          t_half = t_half,
          Lambda_z = lambda_result$lambda_z,
          Lambda_z_r2 = lambda_result$r_squared,
          Lambda_z_points = lambda_result$n_points,
          stringsAsFactors = FALSE
        )
        
        results <- rbind(results, result_row)
      }
    }
  }
  
  # Attach lambda details as attribute
  attr(results, "lambda_details") <- lambda_details
  
  return(results)
}
