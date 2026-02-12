# BioEQ - Missing Data Handling Functions
# Position-aware missing data strategies for NCA analysis
# Handles: BLQ -> 0, middle point interpolation, terminal point LOCF

#' Handle Missing Data for NCA Analysis (Position-Aware)
#'
#' Applies position-aware missing data handling with separate methods for
#' middle and terminal missing points:
#'   - BLQ values are always set to 0
#'   - Middle missing points: exclude (default), interpolate, or LOCF
#'   - Terminal missing points: exclude (default) or LOCF
#'
#' Returns both the processed data and a detailed imputation log.
#'
#' @param data Data frame with time-concentration data
#' @param middle_method Method for middle missing points ("complete", "interpolate", "locf")
#' @param terminal_method Method for terminal missing points ("complete", "locf")
#' @param group_cols Columns to group by (typically c("Subject", "Treatment"))
#' @param time_col Name of time column
#' @param conc_col Name of concentration column
#' @param period_col Name of period column (optional, for imputation log)
#' @return List with: data (processed data frame), log (data frame of actions taken)
#' @export
handle_missing_data <- function(data, middle_method = "complete", 
                               terminal_method = "complete",
                               group_cols = c("Subject", "Treatment"), 
                               time_col = "Time", conc_col = "Concentration",
                               period_col = "Period") {
  
  cat(sprintf("\U0001f504 Handling missing data — middle: %s, terminal: %s\n", middle_method, terminal_method))
  
  # Validate input
  required_cols <- c(group_cols, time_col, conc_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Imputation log: tracks every action taken
  imputation_log <- data.frame(
    Subject = character(),
    Treatment = character(),
    Period = character(),
    Time = numeric(),
    Position = character(),    # "middle", "terminal", "blq"
    Method = character(),      # "interpolation", "locf", "blq_to_zero", "removed"
    Original = character(),    # original value (NA or BLQ text)
    Imputed = numeric(),       # imputed value
    stringsAsFactors = FALSE
  )
  
  has_period <- period_col %in% names(data)
  
  # ── Step 1: Handle BLQ values (always set to 0) ──
  # Detect BLQ markers: character columns that might contain "BLQ", "<LOQ", etc.
  # Also handle numeric 0 or negative values as potential BLQ
  blq_count <- 0
  if (is.character(data[[conc_col]]) || is.factor(data[[conc_col]])) {
    # Concentration column contains text — look for BLQ markers
    conc_text <- as.character(data[[conc_col]])
    blq_mask <- grepl("^(BLQ|BLOQ|<LOQ|<LLOQ|BQL|NS|ND)$", conc_text, ignore.case = TRUE)
    
    if (any(blq_mask)) {
      for (idx in which(blq_mask)) {
        log_row <- data.frame(
          Subject = as.character(data[[group_cols[1]]][idx]),
          Treatment = as.character(data[[group_cols[2]]][idx]),
          Period = if (has_period) as.character(data[[period_col]][idx]) else "",
          Time = as.numeric(data[[time_col]][idx]),
          Position = "blq",
          Method = "blq_to_zero",
          Original = conc_text[idx],
          Imputed = 0,
          stringsAsFactors = FALSE
        )
        imputation_log <- rbind(imputation_log, log_row)
      }
      blq_count <- sum(blq_mask)
      # Convert column to numeric, setting BLQ to 0
      data[[conc_col]] <- suppressWarnings(as.numeric(conc_text))
      data[[conc_col]][blq_mask] <- 0
    } else {
      # Just convert to numeric
      data[[conc_col]] <- suppressWarnings(as.numeric(conc_text))
    }
  }
  
  if (blq_count > 0) {
    cat(sprintf("\U2705 Set %d BLQ values to 0\n", blq_count))
  }
  
  # ── Step 2: Handle NA concentrations with position-aware methods ──
  na_mask <- is.na(data[[conc_col]])
  
  if (!any(na_mask)) {
    cat("\U2705 No missing concentration values detected\n")
  } else {
    # Split by subject-treatment group to classify positions
    data_grouped <- split(data, do.call(paste, c(data[group_cols], sep = "_")))
    rows_to_remove <- c()  # track row indices to remove (for "complete" method)
    
    processed_list <- lapply(data_grouped, function(group) {
      # Order by time
      group <- group[order(group[[time_col]]), ]
      n <- nrow(group)
      if (n < 2) return(group)
      
      na_idx <- which(is.na(group[[conc_col]]))
      if (length(na_idx) == 0) return(group)
      
      # Classify: terminal = after last valid point
      last_valid <- max(which(!is.na(group[[conc_col]])))
      
      for (i in na_idx) {
        subj <- as.character(group[[group_cols[1]]][i])
        trt  <- as.character(group[[group_cols[2]]][i])
        per  <- if (has_period) as.character(group[[period_col]][i]) else ""
        tm   <- as.numeric(group[[time_col]][i])
        is_terminal <- (i > last_valid)
        position <- if (is_terminal) "terminal" else "middle"
        active_method <- if (is_terminal) terminal_method else middle_method
        
        if (active_method == "complete") {
          # Mark for removal
          group[[conc_col]][i] <- NA  # keep as NA, will be removed
          log_row <- data.frame(
            Subject = subj, Treatment = trt, Period = per, Time = tm,
            Position = position, Method = "removed",
            Original = "NA", Imputed = NA_real_,
            stringsAsFactors = FALSE
          )
          imputation_log <<- rbind(imputation_log, log_row)
          
        } else if (active_method == "interpolate" && !is_terminal) {
          # Linear interpolation between nearest valid neighbors
          prev_valid_idx <- max(which(!is.na(group[[conc_col]][1:i])))
          remaining <- which(!is.na(group[[conc_col]][(i+1):n]))
          next_valid_idx <- if (length(remaining) > 0) remaining[1] + i else NA
          
          if (!is.na(prev_valid_idx) && !is.na(next_valid_idx)) {
            t1 <- group[[time_col]][prev_valid_idx]
            t2 <- group[[time_col]][next_valid_idx]
            c1 <- group[[conc_col]][prev_valid_idx]
            c2 <- group[[conc_col]][next_valid_idx]
            ti <- group[[time_col]][i]
            
            imputed_val <- if (t2 != t1) c1 + (c2 - c1) * (ti - t1) / (t2 - t1) else c1
            group[[conc_col]][i] <- imputed_val
            log_row <- data.frame(
              Subject = subj, Treatment = trt, Period = per, Time = tm,
              Position = "middle", Method = "interpolation",
              Original = "NA", Imputed = round(imputed_val, 6),
              stringsAsFactors = FALSE
            )
          } else if (!is.na(prev_valid_idx)) {
            # Can't interpolate (no next neighbor), fall back to LOCF
            imputed_val <- group[[conc_col]][prev_valid_idx]
            group[[conc_col]][i] <- imputed_val
            log_row <- data.frame(
              Subject = subj, Treatment = trt, Period = per, Time = tm,
              Position = "middle", Method = "locf_fallback",
              Original = "NA", Imputed = imputed_val,
              stringsAsFactors = FALSE
            )
          } else {
            log_row <- data.frame(
              Subject = subj, Treatment = trt, Period = per, Time = tm,
              Position = "middle", Method = "unable",
              Original = "NA", Imputed = NA_real_,
              stringsAsFactors = FALSE
            )
          }
          imputation_log <<- rbind(imputation_log, log_row)
          
        } else if (active_method == "locf") {
          # LOCF: find last valid value before this point
          if (i > 1) {
            prev_valid <- which(!is.na(group[[conc_col]][1:(i-1)]))
            imputed_val <- if (length(prev_valid) > 0) group[[conc_col]][max(prev_valid)] else NA_real_
          } else {
            imputed_val <- NA_real_
          }
          
          if (!is.na(imputed_val)) {
            group[[conc_col]][i] <- imputed_val
            log_row <- data.frame(
              Subject = subj, Treatment = trt, Period = per, Time = tm,
              Position = position, Method = "locf",
              Original = "NA", Imputed = imputed_val,
              stringsAsFactors = FALSE
            )
          } else {
            log_row <- data.frame(
              Subject = subj, Treatment = trt, Period = per, Time = tm,
              Position = position, Method = "unable",
              Original = "NA", Imputed = NA_real_,
              stringsAsFactors = FALSE
            )
          }
          imputation_log <<- rbind(imputation_log, log_row)
        }
      }
      
      return(group)
    })
    
    data <- do.call(rbind, processed_list)
    rownames(data) <- NULL
    
    # Remove rows that were marked for deletion (complete method)
    still_na <- is.na(data[[conc_col]])
    if (any(still_na)) {
      data <- data[!still_na, ]
    }
    
    n_imputed <- sum(imputation_log$Method %in% c("interpolation", "locf", "locf_fallback"), na.rm = TRUE)
    n_removed <- sum(imputation_log$Method == "removed", na.rm = TRUE)
    cat(sprintf("\U2705 Missing data handled: %d imputed, %d excluded across %d profiles\n",
                n_imputed, n_removed, length(data_grouped)))
  }
  
  return(list(
    data = data,
    log = imputation_log
  ))
}

#' Validate Missing Data Handling Results
#'
#' @param original_data Original data before handling
#' @param processed_data Data after missing data handling
#' @param method Method used for handling
#' @param conc_col Name of concentration column
#' @return Summary of changes made
validate_missing_data_handling <- function(original_data, processed_data, method,
                                          conc_col = "Concentration") {
  
  original_missing <- sum(is.na(original_data[[conc_col]]))
  processed_missing <- sum(is.na(processed_data[[conc_col]]))
  
  cat(sprintf("\n\U0001f4cb Missing Data Handling Summary (%s):\n", method))
  cat(sprintf("  Original missing values: %d\n", original_missing))
  cat(sprintf("  Processed missing values: %d\n", processed_missing))
  cat(sprintf("  Original rows: %d\n", nrow(original_data)))
  cat(sprintf("  Processed rows: %d\n", nrow(processed_data)))
  
  if (method == "complete") {
    cat(sprintf("  Rows removed: %d\n", nrow(original_data) - nrow(processed_data)))
  }
  
  return(list(
    method = method,
    original_missing = original_missing,
    processed_missing = processed_missing,
    original_rows = nrow(original_data),
    processed_rows = nrow(processed_data)
  ))
}

#' Check Data Completeness for NCA
#'
#' @param data Concentration-time data
#' @param group_cols Grouping columns
#' @param time_col Time column name
#' @param conc_col Concentration column name
#' @return List with completeness statistics and per-point missing detail
check_data_completeness <- function(data, group_cols = c("Subject", "Treatment"),
                                  time_col = "Time", conc_col = "Concentration",
                                  period_col = "Period") {
  
  has_period <- period_col %in% names(data)
  
  # Identify each missing point with full context
  missing_detail <- NULL
  na_mask <- is.na(data[[conc_col]])
  
  if (any(na_mask)) {
    missing_rows <- data[na_mask, , drop = FALSE]
    missing_detail <- data.frame(
      Subject = as.character(missing_rows[[group_cols[1]]]),
      Treatment = as.character(missing_rows[[group_cols[2]]]),
      Period = if (has_period) as.character(missing_rows[[period_col]]) else "",
      Time = as.numeric(missing_rows[[time_col]]),
      stringsAsFactors = FALSE
    )
  }
  
  # Group-level completeness
  data$group_id <- do.call(paste, c(data[group_cols], sep = "_"))
  unique_groups <- unique(data$group_id)
  
  completeness_by_group <- do.call(rbind, lapply(unique_groups, function(g) {
    grp <- data[data$group_id == g, ]
    data.frame(
      group_id = g,
      total_points = nrow(grp),
      missing_points = sum(is.na(grp[[conc_col]])),
      complete_points = sum(!is.na(grp[[conc_col]])),
      completeness_pct = round(100 * sum(!is.na(grp[[conc_col]])) / nrow(grp), 1),
      stringsAsFactors = FALSE
    )
  }))
  
  overall_stats <- list(
    total_profiles = length(unique_groups),
    overall_completeness = round(100 * sum(!na_mask) / nrow(data), 1),
    profiles_with_missing = sum(completeness_by_group$missing_points > 0),
    total_missing = sum(na_mask),
    min_completeness = min(completeness_by_group$completeness_pct),
    max_completeness = max(completeness_by_group$completeness_pct)
  )
  
  cat(sprintf("\U0001f4ca Data Completeness Summary:\n"))
  cat(sprintf("  Total profiles: %d\n", overall_stats$total_profiles))
  cat(sprintf("  Overall completeness: %.1f%%\n", overall_stats$overall_completeness))
  cat(sprintf("  Profiles with missing data: %d\n", overall_stats$profiles_with_missing))
  cat(sprintf("  Completeness range: %.1f%% - %.1f%%\n", 
              overall_stats$min_completeness, overall_stats$max_completeness))
  
  return(list(
    overall = overall_stats,
    by_profile = completeness_by_group,
    missing_detail = missing_detail
  ))
}
