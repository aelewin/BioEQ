# Carryover Detection Functions per ICH M13A Guidelines

#' Detect carryover effects in crossover/replicate studies
#' 
#' @description Implements ICH M13A Section 2.2.3.3 carryover detection
#' Compares pre-dose concentrations in Period 2+ to Cmax within the SAME period.
#' If pre-dose concentration exceeds 5% of Cmax (same period), carryover is detected.
#' 
#' @param data Data frame with columns: Subject, Period, Time, Concentration, Treatment
#' @param threshold Percentage of Cmax threshold (default 5% per ICH M13A)
#' @param exclude_subjects Logical, whether to exclude subjects with carryover
#' 
#' @return List containing:
#'   - data: Cleaned data (with or without excluded subjects)
#'   - carryover_summary: Summary of carryover detection
#'   - excluded_subjects: Vector of excluded subject IDs
#'   - carryover_details: Detailed results for each subject/period
#' 
detect_carryover <- function(data, threshold = 5, exclude_subjects = TRUE) {
  
  # Standardize column names if needed (do this FIRST)
  # Handle multiple possible column name variations
  if ("subject" %in% names(data) && !"Subject" %in% names(data)) {
    names(data)[names(data) == "subject"] <- "Subject"
  }
  if ("Subject.ID" %in% names(data) && !"Subject" %in% names(data)) {
    names(data)[names(data) == "Subject.ID"] <- "Subject"
  }
  if ("ID" %in% names(data) && !"Subject" %in% names(data)) {
    names(data)[names(data) == "ID"] <- "Subject"
  }
  
  if ("period" %in% names(data) && !"Period" %in% names(data)) {
    names(data)[names(data) == "period"] <- "Period"
  }
  if ("Period.Number" %in% names(data) && !"Period" %in% names(data)) {
    names(data)[names(data) == "Period.Number"] <- "Period"
  }
  
  if ("time" %in% names(data) && !"Time" %in% names(data)) {
    names(data)[names(data) == "time"] <- "Time"
  }
  if ("Time.Point" %in% names(data) && !"Time" %in% names(data)) {
    names(data)[names(data) == "Time.Point"] <- "Time"
  }
  if ("timepoint" %in% names(data) && !"Time" %in% names(data)) {
    names(data)[names(data) == "timepoint"] <- "Time"
  }
  
  if ("concentration" %in% names(data) && !"Concentration" %in% names(data)) {
    names(data)[names(data) == "concentration"] <- "Concentration"
  }
  if ("Conc" %in% names(data) && !"Concentration" %in% names(data)) {
    names(data)[names(data) == "Conc"] <- "Concentration"
  }
  if ("DV" %in% names(data) && !"Concentration" %in% names(data)) {
    names(data)[names(data) == "DV"] <- "Concentration"
  }
  
  bioeq_log(sprintf("Available columns: %s", paste(names(data), collapse = ", ")), "DEBUG")

  # Ensure required columns exist (after standardization)
  required_cols <- c("Subject", "Period", "Time", "Concentration")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    # Special handling for missing Period column
    if ("Period" %in% missing_cols) {
      bioeq_log("Period column is missing - carryover detection is not applicable for single-period/parallel study data", "DEBUG")

      return(list(
        data = data,
        carryover_summary = "Period column missing - carryover detection not applicable for single-period studies",
        excluded_subjects = character(),
        flagged_subjects = data.frame(),
        carryover_data = data.frame(Message = "Period data required for carryover detection"),
        carryover_details = data.frame(),
        threshold = threshold / 100,
        n_original = nrow(data),
        n_cleaned = nrow(data),
        n_excluded_subjects = 0
      ))
    }
    
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", "), 
               "\nAvailable columns:", paste(names(data), collapse = ", ")))
  }
  
  # Initialize results
  carryover_details <- list()
  subjects_with_carryover <- character()
  
  # Get unique subjects and periods
  subjects <- unique(data$Subject)
  periods <- sort(unique(data$Period))
  
  bioeq_log(sprintf("Analyzing %d subjects across %d periods, threshold %.1f%% of Cmax",
                     length(subjects), length(periods), threshold), "DEBUG")

  # Only check Period 2 and higher (no carryover possible in Period 1)
  check_periods <- periods[periods > 1]

  if (length(check_periods) == 0) {
    bioeq_log("Single period study - no carryover assessment needed", "DEBUG")
    return(list(
      data = data,
      carryover_summary = "Single period study - no carryover possible",
      excluded_subjects = character(),
      carryover_details = NULL
    ))
  }
  
  # Check each subject in each period ≥ 2
  for (subj in subjects) {
    for (per in check_periods) {
      
      # Get subject data for this period
      subj_period_data <- data[data$Subject == subj & data$Period == per, ]
      
      if (nrow(subj_period_data) == 0) next
      
      # Find pre-dose concentration (Time = 0)
      predose_rows <- subj_period_data[subj_period_data$Time == 0, ]
      
      if (nrow(predose_rows) == 0) {
        bioeq_log(sprintf("Subject %s Period %d: no pre-dose sample found - skipping", subj, per), "WARNING")
        next
      }
      
      predose_conc <- predose_rows$Concentration[1]
      
      # Skip if pre-dose is BLQ or zero
      if (is.na(predose_conc) || predose_conc <= 0) {
        carryover_details[[paste(subj, per, sep = "_P")]] <- list(
          subject = subj,
          period = per,
          predose_conc = predose_conc,
          cmax = NA,
          percent_of_cmax = 0,
          carryover_detected = FALSE,
          note = "Pre-dose BLQ or zero"
        )
        next
      }
      
      # Calculate Cmax for this period (excluding pre-dose)
      post_dose_data <- subj_period_data[subj_period_data$Time > 0, ]
      
      if (nrow(post_dose_data) == 0) {
        bioeq_log(sprintf("Subject %s Period %d: no post-dose samples - skipping", subj, per), "WARNING")
        next
      }
      
      cmax <- max(post_dose_data$Concentration, na.rm = TRUE)
      
      # Calculate percentage of Cmax
      percent_of_cmax <- (predose_conc / cmax) * 100
      
      # Determine if carryover is significant
      carryover_detected <- percent_of_cmax > threshold
      
      # Store results
      carryover_details[[paste(subj, per, sep = "_P")]] <- list(
        subject = subj,
        period = per,
        predose_conc = round(predose_conc, 3),
        cmax = round(cmax, 3),
        percent_of_cmax = round(percent_of_cmax, 2),
        carryover_detected = carryover_detected,
        note = ifelse(carryover_detected, 
                     sprintf("CARRYOVER: %.2f%% > %.1f%% threshold", percent_of_cmax, threshold),
                     sprintf("OK: %.2f%% ≤ %.1f%% threshold", percent_of_cmax, threshold))
      )
      
      # Track subjects with significant carryover
      if (carryover_detected) {
        subjects_with_carryover <- unique(c(subjects_with_carryover, subj))
        bioeq_log(sprintf("Subject %s Period %d: CARRYOVER DETECTED (%.2f%% of Cmax)",
                          subj, per, percent_of_cmax), "WARNING")
      }
    }
  }
  
  # Create summary data frame
  if (length(carryover_details) > 0) {
    summary_df <- do.call(rbind, lapply(carryover_details, function(x) {
      data.frame(
        Subject = x$subject,
        Period = x$period,
        `Pre-dose Conc` = x$predose_conc,
        Cmax = x$cmax,
        `% of Cmax` = x$percent_of_cmax,
        Status = ifelse(x$carryover_detected, "EXCLUDE", "OK"),
        Note = x$note,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    summary_df <- data.frame(Message = "No carryover assessment performed")
  }
  
  # Prepare final data
  if (exclude_subjects && length(subjects_with_carryover) > 0) {
    bioeq_log(sprintf("Excluding %d subjects with carryover: %s",
                      length(subjects_with_carryover),
                      paste(subjects_with_carryover, collapse = ", ")), "WARNING")

    cleaned_data <- data[!data$Subject %in% subjects_with_carryover, ]

    bioeq_log(sprintf("Data reduced from %d to %d observations", nrow(data), nrow(cleaned_data)), "DEBUG")
  } else {
    cleaned_data <- data
  }
  
  # Generate overall summary
  carryover_summary <- sprintf(
    "Carryover Assessment Complete:\n- Subjects analyzed: %d\n- Periods checked: %s\n- Threshold: %.1f%% of Cmax\n- Subjects with carryover: %d\n- Subjects excluded: %s",
    length(subjects),
    paste(check_periods, collapse = ", "),
    threshold,
    length(subjects_with_carryover),
    ifelse(exclude_subjects && length(subjects_with_carryover) > 0,
           paste(subjects_with_carryover, collapse = ", "),
           "None")
  )
  
  bioeq_log(carryover_summary, "DEBUG")

  return(list(
    data = cleaned_data,
    carryover_summary = carryover_summary,
    excluded_subjects = subjects_with_carryover,
    flagged_subjects = summary_df[summary_df$Status == "EXCLUDE", ],
    carryover_data = summary_df,
    carryover_details = summary_df,
    threshold = threshold / 100,  # Convert percentage to decimal for dashboard
    n_original = nrow(data),
    n_cleaned = nrow(cleaned_data),
    n_excluded_subjects = length(subjects_with_carryover)
  ))
}

#' Check for carryover in NCA results
#' 
#' @description Quick check for carryover using NCA-calculated parameters
#' Useful when raw concentration-time data is not available
#' 
check_carryover_from_nca <- function(nca_results, threshold = 5) {
  
  # This is a simplified check when only NCA results are available
  # Ideally, carryover should be checked on raw concentration-time data
  
  if (!"predose_conc" %in% names(nca_results)) {
    warning("Pre-dose concentrations not found in NCA results. Carryover detection requires raw concentration-time data.")
    return(NULL)
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required for this function")
  }
  
  carryover_check <- nca_results %>%
    dplyr::filter(Period > 1) %>%
    dplyr::mutate(
      percent_of_cmax = (predose_conc / Cmax) * 100,
      carryover = percent_of_cmax > threshold
    ) %>%
    dplyr::select(Subject, Period, Treatment, predose_conc, Cmax, percent_of_cmax, carryover)
  
  return(carryover_check)
}
