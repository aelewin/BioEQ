# BioEQ - Utility Functions
# Modernized utility functions for bioequivalence analysis

#' Null Coalescing Operator
#'
#' @param x First value
#' @param y Default value if x is NULL or NA
#' @return x if not NULL/NA, otherwise y
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

#' Validate Bioequivalence Study Data
#'
#' @param data Data frame containing BE study data
#' @return Validated and formatted data frame
#' @export
validate_be_data <- function(data) {
  cat("🔍 Validating bioequivalence data...\n")
  
  # Required columns
  required_cols <- c("subj", "seq", "prd", "tmt", "time", "conc")
  
  # Check for required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check for missing values in key columns
  key_cols <- c("subj", "seq", "prd", "tmt", "time")
  for (col in key_cols) {
    if (any(is.na(data[[col]]))) {
      stop("Missing values found in column: ", col)
    }
  }
  
  # Validate sequence coding (should be 1 or 2)
  unique_seq <- unique(data$seq)
  if (!all(unique_seq %in% c(1, 2))) {
    stop("Sequence (seq) should be coded as 1 or 2")
  }
  
  # Validate treatment coding (should be 1 or 2)
  unique_tmt <- unique(data$tmt)
  if (!all(unique_tmt %in% c(1, 2))) {
    stop("Treatment (tmt) should be coded as 1 (Reference) or 2 (Test)")
  }
  
  # Validate time values (should be non-negative)
  if (any(data$time < 0, na.rm = TRUE)) {
    stop("Time values should be non-negative")
  }
  
  # Validate concentration values (should be non-negative, allow NA)
  if (any(data$conc < 0, na.rm = TRUE)) {
    stop("Concentration values should be non-negative")
  }
  
  # Convert factors to appropriate types
  data$subj <- as.factor(data$subj)
  data$seq <- as.factor(data$seq)
  data$prd <- as.factor(data$prd)
  data$tmt <- as.factor(data$tmt)
  data$time <- as.numeric(data$time)
  data$conc <- as.numeric(data$conc)
  
  # Add treatment labels
  data$treatment <- factor(data$tmt, levels = c(1, 2), labels = c("Reference", "Test"))
  data$sequence <- factor(data$seq, levels = c(1, 2), labels = c("TR", "RT"))
  
  n_subjects <- length(unique(data$subj))
  n_timepoints <- length(unique(data$time))
  
  cat("✅ Data validation completed\n")
  cat("  Subjects:", n_subjects, "\n")
  cat("  Time points:", n_timepoints, "\n")
  cat("  Observations:", nrow(data), "\n\n")
  
  return(data)
}

#' Format BE Data for Analysis
#'
#' @param data Validated BE data
#' @return Formatted data ready for analysis
#' @export
format_be_data <- function(data) {
  # Ensure proper ordering
  data <- data[order(data$subj, data$prd, data$time), ]
  
  # Add derived variables
  data$log_conc <- ifelse(data$conc > 0, log(data$conc), NA)
  
  return(data)
}

#' Summarize Data
#'
#' @param data Data frame
#' @param measurevar Column name to summarize
#' @param groupvars Grouping variables
#' @param na.rm Remove NA values
#' @param conf.interval Confidence interval
#' @return Summary statistics
#' @export
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE) {
  
  # Length function that handles NA
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  
  # Calculate summary statistics
  datac <- data %>%
    group_by(across(all_of(groupvars))) %>%
    summarise(
      N = length2(.data[[measurevar]], na.rm = na.rm),
      mean = mean(.data[[measurevar]], na.rm = na.rm),
      sd = sd(.data[[measurevar]], na.rm = na.rm),
      .groups = "drop"
    )
  
  # Rename mean column
  names(datac)[names(datac) == "mean"] <- measurevar
  
  # Calculate standard error and confidence intervals
  datac$se <- datac$sd / sqrt(datac$N)
  
  # T-statistic for confidence interval
  ciMult <- qt(conf.interval/2 + .5, datac$N - 1)
  datac$ci <- datac$se * ciMult
  
  return(as.data.frame(datac))
}

#' Check for Data Errors
#'
#' @param data BE study data
#' @return List of potential data issues
#' @export
check_data_errors <- function(data) {
  errors <- list()
  warnings <- list()
  
  # Check for duplicate time points within subject/period/treatment
  duplicates <- data %>%
    group_by(subj, prd, tmt, time) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(duplicates) > 0) {
    errors$duplicates <- "Duplicate time points found for same subject/period/treatment"
  }
  
  # Check for missing baseline (time = 0) measurements
  baseline_missing <- data %>%
    group_by(subj, prd, tmt) %>%
    summarise(has_baseline = any(time == 0), .groups = "drop") %>%
    filter(!has_baseline)
  
  if (nrow(baseline_missing) > 0) {
    warnings$baseline <- "Some profiles missing time = 0 measurements"
  }
  
  # Check for non-zero baseline concentrations
  nonzero_baseline <- data %>%
    filter(time == 0, conc > 0, !is.na(conc))
  
  if (nrow(nonzero_baseline) > 0) {
    warnings$nonzero_baseline <- "Non-zero baseline concentrations detected"
  }
  
  # Check for monotonically decreasing concentrations after Cmax
  # (simplified check - could be enhanced)
  
  return(list(errors = errors, warnings = warnings))
}

#' Load Example Dataset
#'
#' @param dataset_name Name of example dataset
#' @return Example BE data
#' @export
load_example_data <- function(dataset_name = "crossover_2x2x2") {
  
  if (dataset_name == "crossover_2x2x2") {
    # Generate a simple 2x2x2 crossover example
    set.seed(123)
    
    n_subjects <- 24
    time_points <- c(0, 0.5, 1, 2, 4, 6, 8, 12, 24)
    
    # Create data structure
    data <- expand.grid(
      subj = 1:n_subjects,
      prd = 1:2,
      time = time_points
    )
    
    # Assign sequences (half TR, half RT)
    data$seq <- rep(c(1, 2), each = n_subjects/2 * 2 * length(time_points))
    
    # Assign treatments based on sequence and period
    data$tmt <- ifelse(
      (data$seq == 1 & data$prd == 1) | (data$seq == 2 & data$prd == 2), 
      2, 1  # Test = 2, Reference = 1
    )
    
    # Generate realistic PK concentrations
    data$conc <- generate_pk_concentrations(data)
    
    # Add some missing values at random
    missing_idx <- sample(nrow(data), size = floor(0.02 * nrow(data)))
    data$conc[missing_idx] <- NA
    
    cat("📊 Generated example 2x2x2 crossover dataset\n")
    cat("  Subjects:", n_subjects, "\n")
    cat("  Time points:", length(time_points), "\n")
    
    # Convert to standard format and return structured result
    validated_data <- validate_be_data(data)
    
    # Transform to plotting format (Subject, Time, Concentration, Formulation)
    conc_data <- validated_data %>%
      select(
        Subject = subj,
        Time = time,
        Concentration = conc,
        Formulation = treatment,
        Period = prd,
        Sequence = sequence
      ) %>%
      filter(!is.na(Concentration))
    
    # Calculate PK parameters using NCA analysis
    # Need to prepare data for perform_nca_analysis (expects subj, tmt, time, conc)
    nca_input <- validated_data %>%
      filter(!is.na(conc))
    
    pk_params <- perform_nca_analysis(
      data = nca_input,
      id_cols = c("subj", "tmt"),
      time_col = "time", 
      conc_col = "conc"
    )
    
    # Transform PK data to expected format (Subject, Formulation, PK parameters)
    pk_data <- pk_params %>%
      mutate(
        Subject = subj,
        Formulation = factor(tmt, levels = c(1, 2), labels = c("Reference", "Test"))
      ) %>%
      select(-subj, -tmt)
    
    # Return in format expected by plotting functions
    return(list(
      concentration_data = conc_data,
      pk_parameters = pk_data
    ))
  }
  
  stop("Unknown dataset: ", dataset_name)
}

#' Generate Realistic PK Concentrations (helper for examples)
#'
#' @param data Data frame with study structure
#' @return Vector of concentrations
generate_pk_concentrations <- function(data) {
  conc <- numeric(nrow(data))
  
  for (i in 1:nrow(data)) {
    time <- data$time[i]
    tmt <- data$tmt[i]
    subj <- data$subj[i]
    
    if (time == 0) {
      conc[i] <- 0
    } else {
      # Simple 1-compartment model with absorption
      ka <- rnorm(1, 1.5, 0.3)
      ke <- rnorm(1, 0.1, 0.02)
      
      # Treatment effect (test vs reference)
      bioavail <- ifelse(tmt == 2, rnorm(1, 0.95, 0.15), 1.0)
      
      # Inter-subject variability
      subj_factor <- exp(rnorm(1, 0, 0.25))
      
      # PK equation
      dose <- 100
      conc[i] <- dose * bioavail * subj_factor * ka / (ka - ke) * 
                 (exp(-ke * time) - exp(-ka * time))
      
      # Add residual error
      conc[i] <- conc[i] * exp(rnorm(1, 0, 0.1))
      
      # Ensure non-negative
      conc[i] <- max(0, conc[i])
    }
  }
  
  return(conc)
}

#' Print BioEQ Object
#'
#' @param x BioEQ analysis result
#' @export
print.bioeq <- function(x, ...) {
  cat("🧬 BioEQ Analysis Result\n")
  cat("========================\n\n")
  
  if (!is.null(x$metadata)) {
    cat("Study Design:", x$metadata$design, "\n")
    cat("Analysis Date:", format(x$metadata$analysis_date), "\n")
    cat("BE Limits:", paste(x$metadata$be_limits, collapse = " - "), "\n\n")
  }
  
  if (!is.null(x$summary)) {
    cat("Summary:\n")
    print(x$summary)
  }
  
  invisible(x)
}

#' Validate PK Parameter Data
#'
#' @param data Data frame containing PK parameters
#' @return Validated and formatted data frame
#' @export
validate_pk_data <- function(data) {
  if (nrow(data) == 0) {
    stop("Data frame is empty")
  }
  
  # Required columns for PK data depend on design
  basic_required <- c("Subject")
  
  # Check for basic required columns
  missing_cols <- setdiff(basic_required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Convert factors to appropriate types
  if ("Subject" %in% names(data)) {
    data$Subject <- as.factor(data$Subject)
  }
  if ("Period" %in% names(data)) {
    data$Period <- as.factor(data$Period)
  }
  if ("Formulation" %in% names(data)) {
    data$Formulation <- as.factor(data$Formulation)
  }
  
  return(data)
}

#' Validate Concentration-Time Data
#'
#' @param data Data frame containing concentration-time data
#' @return Validated and formatted data frame
#' @export
validate_conc_time_data <- function(data) {
  if (nrow(data) == 0) {
    stop("Data frame is empty")
  }
  
  # Required columns for concentration data
  required_cols <- c("Subject", "Time", "Concentration")
  
  # Check for required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check for missing values in key columns
  key_cols <- c("Subject", "Time")
  for (col in key_cols) {
    if (any(is.na(data[[col]]))) {
      stop("Missing values found in column: ", col)
    }
  }
  
  # Validate time values (should be non-negative)
  if (any(data$Time < 0, na.rm = TRUE)) {
    stop("Time values should be non-negative")
  }
  
  # Validate concentration values (should be non-negative, allow NA)
  if (any(data$Concentration < 0, na.rm = TRUE)) {
    stop("Concentration values should be non-negative")
  }
  
  # Convert factors to appropriate types
  data$Subject <- as.factor(data$Subject)
  data$Time <- as.numeric(data$Time)
  data$Concentration <- as.numeric(data$Concentration)
  
  return(data)
}
