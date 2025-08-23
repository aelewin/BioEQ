# BioEQ - Statistical Functions
# Modernized statistical functions for bioequivalence analysis

#' Calculate Sample Size for Bioequivalence Study
#'
#' @param alpha Significance level (default 0.05)
#' @param power Target power (default 0.80)
#' @param theta0 True ratio (default 0.95)
#' @param theta1 Lower bioequivalence limit (default 0.80)
#' @param theta2 Upper bioequivalence limit (default 1.25)
#' @param cv Coefficient of variation (within-subject for crossover)
#' @param design Study design ("2x2x2", "parallel", "replicate")
#' @return Sample size calculation results
#' @export
calculate_sample_size <- function(alpha = 0.05, power = 0.80, theta0 = 0.95,
                                 theta1 = 0.80, theta2 = 1.25, cv = 0.25,
                                 design = "2x2x2") {
  
  cat("📊 Calculating sample size for bioequivalence study...\n")
  cat("Design:", design, "\n")
  cat("Power:", power * 100, "%\n")
  cat("CV:", cv * 100, "%\n")
  cat("True ratio:", theta0, "\n")
  cat("BE limits:", theta1, "-", theta2, "\n\n")
  
  if (design == "2x2x2") {
    # Sample size calculation for 2x2x2 crossover design
    result <- sample_size_crossover_2x2x2(alpha, power, theta0, theta1, theta2, cv)
  } else if (design == "parallel") {
    # Sample size calculation for parallel design
    result <- sample_size_parallel(alpha, power, theta0, theta1, theta2, cv)
  } else {
    stop("Sample size calculation not implemented for design: ", design)
  }
  
  # Add metadata
  result$parameters <- list(
    alpha = alpha,
    power = power,
    theta0 = theta0,
    theta1 = theta1,
    theta2 = theta2,
    cv = cv,
    design = design
  )
  
  class(result) <- c("sample_size", "list")
  
  cat("✅ Sample size calculation completed\n")
  return(result)
}

#' Sample Size for 2x2x2 Crossover Design
#'
#' @param alpha Significance level
#' @param power Target power
#' @param theta0 True ratio
#' @param theta1 Lower BE limit
#' @param theta2 Upper BE limit
#' @param cv Within-subject CV
#' @return Sample size results
sample_size_crossover_2x2x2 <- function(alpha, power, theta0, theta1, theta2, cv) {
  
  # Convert to log scale
  delta <- log(theta0)
  delta1 <- log(theta1)
  delta2 <- log(theta2)
  
  # Standard error for difference in crossover design
  # SE = sqrt(2 * sigma^2 / n), where sigma^2 = log(cv^2 + 1)
  sigma2 <- log(cv^2 + 1)
  
  # Non-centrality parameters for TOST
  # We need to solve for n such that both one-sided tests have sufficient power
  
  # Function to calculate power for given sample size
  power_function <- function(n) {
    if (n <= 0) return(0)
    
    se <- sqrt(2 * sigma2 / n)
    df <- n - 2
    
    # Two one-sided tests
    t1 <- (delta - delta1) / se  # Test if ratio > lower limit
    t2 <- (delta2 - delta) / se  # Test if ratio < upper limit
    
    # Power is minimum of both tests
    power1 <- pt(qt(alpha, df), df, ncp = t1, lower.tail = FALSE)
    power2 <- pt(qt(alpha, df), df, ncp = t2, lower.tail = FALSE)
    
    return(min(power1, power2))
  }
  
  # Search for required sample size
  n_min <- 2
  n_max <- 1000
  
  # Binary search for sample size
  while (n_max - n_min > 1) {
    n_mid <- floor((n_min + n_max) / 2)
    power_mid <- power_function(n_mid)
    
    if (power_mid < power) {
      n_min <- n_mid
    } else {
      n_max <- n_mid
    }
  }
  
  n_required <- n_max
  achieved_power <- power_function(n_required)
  
  return(list(
    n_subjects = n_required,
    achieved_power = achieved_power,
    target_power = power,
    design = "2x2x2 crossover",
    within_cv = cv * 100
  ))
}

#' Sample Size for Parallel Design
#'
#' @param alpha Significance level
#' @param power Target power
#' @param theta0 True ratio
#' @param theta1 Lower BE limit
#' @param theta2 Upper BE limit
#' @param cv Total CV (pooled within and between subject)
#' @return Sample size results
sample_size_parallel <- function(alpha, power, theta0, theta1, theta2, cv) {
  
  # For parallel design, we need larger sample sizes due to between-subject variability
  delta <- log(theta0)
  delta1 <- log(theta1)
  delta2 <- log(theta2)
  
  # Standard error for parallel design (equal allocation)
  # SE = sqrt(2 * sigma^2 / n_per_group)
  sigma2 <- log(cv^2 + 1)
  
  # Function to calculate power for given sample size per group
  power_function <- function(n_per_group) {
    if (n_per_group <= 1) return(0)
    
    se <- sqrt(2 * sigma2 / n_per_group)
    df <- 2 * (n_per_group - 1)
    
    # Two one-sided tests
    t1 <- (delta - delta1) / se
    t2 <- (delta2 - delta) / se
    
    power1 <- pt(qt(alpha, df), df, ncp = t1, lower.tail = FALSE)
    power2 <- pt(qt(alpha, df), df, ncp = t2, lower.tail = FALSE)
    
    return(min(power1, power2))
  }
  
  # Search for required sample size per group
  n_min <- 2
  n_max <- 2000
  
  while (n_max - n_min > 1) {
    n_mid <- floor((n_min + n_max) / 2)
    power_mid <- power_function(n_mid)
    
    if (power_mid < power) {
      n_min <- n_mid
    } else {
      n_max <- n_mid
    }
  }
  
  n_per_group <- n_max
  total_n <- 2 * n_per_group
  achieved_power <- power_function(n_per_group)
  
  return(list(
    n_per_group = n_per_group,
    n_total = total_n,
    achieved_power = achieved_power,
    target_power = power,
    design = "parallel",
    total_cv = cv * 100
  ))
}

#' Calculate Power for Given Sample Size
#'
#' @param n Sample size (total for crossover, per group for parallel)
#' @param n_per_group Alternative parameter name for sample size per group  
#' @param alpha Significance level
#' @param theta0 True ratio
#' @param theta1 Lower BE limit
#' @param theta2 Upper BE limit
#' @param cv Coefficient of variation
#' @param design Study design
#' @return Power calculation results
#' @export
calculate_power <- function(n = NULL, n_per_group = NULL, alpha = 0.05, theta0 = 0.95, theta1 = 0.80,
                           theta2 = 1.25, cv = 0.25, design = "2x2x2") {
  
  # Handle parameter aliases
  if (!is.null(n_per_group)) {
    n <- n_per_group
  }
  if (is.null(n)) {
    stop("Must provide either 'n' or 'n_per_group' parameter")
  }
  
  cat("⚡ Calculating power for bioequivalence study...\n")
  cat("Design:", design, "\n")
  cat("Sample size:", n, "\n")
  cat("CV:", cv * 100, "%\n\n")
  
  delta <- log(theta0)
  delta1 <- log(theta1)
  delta2 <- log(theta2)
  sigma2 <- log(cv^2 + 1)
  
  if (design == "2x2x2") {
    # Crossover design
    se <- sqrt(2 * sigma2 / n)
    df <- n - 2
    
    t1 <- (delta - delta1) / se
    t2 <- (delta2 - delta) / se
    
    power1 <- pt(qt(alpha, df), df, ncp = t1, lower.tail = FALSE)
    power2 <- pt(qt(alpha, df), df, ncp = t2, lower.tail = FALSE)
    
    power <- min(power1, power2)
    
  } else if (design == "parallel") {
    # Parallel design (n per group)
    se <- sqrt(2 * sigma2 / n)
    df <- 2 * (n - 1)
    
    t1 <- (delta - delta1) / se
    t2 <- (delta2 - delta) / se
    
    power1 <- pt(qt(alpha, df), df, ncp = t1, lower.tail = FALSE)
    power2 <- pt(qt(alpha, df), df, ncp = t2, lower.tail = FALSE)
    
    power <- min(power1, power2)
    
  } else {
    stop("Power calculation not implemented for design: ", design)
  }
  
  result <- list(
    power = power,
    sample_size = n,
    design = design,
    cv = cv * 100,
    true_ratio = theta0,
    be_limits = c(theta1, theta2),
    alpha = alpha
  )
  
  class(result) <- c("power_calculation", "list")
  
  cat("✅ Power calculation completed\n")
  cat("Power:", round(power * 100, 1), "%\n\n")
  
  return(result)
}

#' Calculate 90% Confidence Intervals for BE Parameters
#'
#' @param test_values Test treatment values (log-transformed)
#' @param ref_values Reference treatment values (log-transformed)
#' @param alpha Significance level (default 0.05 for 90% CI)
#' @param design Study design for appropriate error calculation
#' @return Confidence interval results
#' @export
calculate_confidence_intervals <- function(test_values, ref_values, alpha = 0.05,
                                         design = "2x2x2") {
  
  # Calculate difference and standard error
  diff_mean <- mean(test_values, na.rm = TRUE) - mean(ref_values, na.rm = TRUE)
  
  if (design == "2x2x2") {
    # For crossover design, calculate within-subject differences
    if (length(test_values) != length(ref_values)) {
      stop("Test and reference values must have same length for crossover design")
    }
    
    differences <- test_values - ref_values
    se <- sd(differences, na.rm = TRUE) / sqrt(length(differences))
    df <- length(differences) - 1
    
  } else if (design == "parallel") {
    # For parallel design, use pooled standard error
    n1 <- length(test_values)
    n2 <- length(ref_values)
    
    s1 <- sd(test_values, na.rm = TRUE)
    s2 <- sd(ref_values, na.rm = TRUE)
    
    pooled_var <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
    se <- sqrt(pooled_var * (1/n1 + 1/n2))
    df <- n1 + n2 - 2
    
  } else {
    stop("CI calculation not implemented for design: ", design)
  }
  
  # Calculate confidence interval
  t_critical <- qt(0.95, df)
  
  ci_lower_log <- diff_mean - t_critical * se
  ci_upper_log <- diff_mean + t_critical * se
  
  # Convert to ratio scale
  ratio_estimate <- exp(diff_mean)
  ci_lower <- exp(ci_lower_log)
  ci_upper <- exp(ci_upper_log)
  
  result <- list(
    ratio_estimate = ratio_estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    confidence_level = (1 - alpha) * 100,
    log_difference = diff_mean,
    standard_error = se,
    degrees_freedom = df,
    design = design
  )
  
  return(result)
}

#' Perform Two One-Sided Tests (TOST) for Bioequivalence
#'
#' @param test_values Test treatment values
#' @param ref_values Reference treatment values
#' @param be_limits Bioequivalence limits
#' @param alpha Significance level
#' @param design Study design
#' @return TOST results
#' @export
perform_tost <- function(test_values, ref_values, be_limits = c(0.8, 1.25),
                        alpha = 0.05, design = "2x2x2") {
  
  # Calculate confidence interval
  ci_result <- calculate_confidence_intervals(test_values, ref_values, alpha, design)
  
  # Log-scale limits
  log_lower <- log(be_limits[1])
  log_upper <- log(be_limits[2])
  
  # Two one-sided tests
  t1 <- (ci_result$log_difference - log_lower) / ci_result$standard_error
  t2 <- (log_upper - ci_result$log_difference) / ci_result$standard_error
  
  p1 <- pt(t1, ci_result$degrees_freedom, lower.tail = FALSE)
  p2 <- pt(t2, ci_result$degrees_freedom, lower.tail = FALSE)
  
  tost_p_value <- max(p1, p2)
  
  # Bioequivalence conclusion
  ci_within_limits <- ci_result$ci_lower >= be_limits[1] && ci_result$ci_upper <= be_limits[2]
  p_significant <- tost_p_value < alpha
  bioequivalent <- ci_within_limits && p_significant
  
  result <- list(
    ratio_estimate = ci_result$ratio_estimate,
    ci_lower = ci_result$ci_lower,
    ci_upper = ci_result$ci_upper,
    be_limits = be_limits,
    tost_p_value = tost_p_value,
    p1 = p1,
    p2 = p2,
    bioequivalent = bioequivalent,
    alpha = alpha,
    confidence_level = ci_result$confidence_level
  )
  
  return(result)
}

#' Print Sample Size Results
#'
#' @param x Sample size calculation result
#' @export
print.sample_size <- function(x, ...) {
  cat("📊 Sample Size Calculation\n")
  cat("==========================\n\n")
  
  cat("Design:", x$design, "\n")
  
  if ("n_subjects" %in% names(x)) {
    cat("Required sample size:", x$n_subjects, "subjects\n")
  } else if ("n_per_group" %in% names(x)) {
    cat("Required sample size:", x$n_per_group, "per group (", x$n_total, "total)\n")
  }
  
  cat("Target power:", x$target_power * 100, "%\n")
  cat("Achieved power:", round(x$achieved_power * 100, 1), "%\n")
  
  if ("within_cv" %in% names(x)) {
    cat("Within-subject CV:", x$within_cv, "%\n")
  } else if ("total_cv" %in% names(x)) {
    cat("Total CV:", x$total_cv, "%\n")
  }
  
  invisible(x)
}

#' Print Power Calculation Results
#'
#' @param x Power calculation result
#' @export
print.power_calculation <- function(x, ...) {
  cat("⚡ Power Calculation\n")
  cat("===================\n\n")
  
  cat("Design:", x$design, "\n")
  cat("Sample size:", x$sample_size, "\n")
  cat("Power:", round(x$power * 100, 1), "%\n")
  cat("CV:", x$cv, "%\n")
  cat("True ratio:", x$true_ratio, "\n")
  cat("BE limits:", paste(x$be_limits, collapse = " - "), "\n")
  cat("Alpha:", x$alpha, "\n")
  
  invisible(x)
}

# Enhanced BioEQ with scaling capability
perform_be_analysis_enhanced <- function(data, design, regulatory_standard, 
                                       scaling = "none", cv_threshold = 0.3) {
  
  if (scaling == "reference" && cv_within_ref > cv_threshold) {
    # Use reference-scaled limits (similar to replicateBE)
    scaled_limits <- calculate_scaled_limits(cv_within_ref, regulatory_standard)
    return(perform_scaled_be_analysis(data, scaled_limits))
  } else {
    # Use standard fixed limits
    return(perform_standard_be_analysis(data, c(0.8, 1.25)))
  }
}
