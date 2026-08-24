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
  
  bioeq_log(sprintf("Handling missing data - middle: %s, terminal: %s", middle_method, terminal_method), "DEBUG")
  
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
    bioeq_log(sprintf("Set %d BLQ values to 0", blq_count), "DEBUG")
  }
  
  # ── Step 2: Handle NA concentrations with position-aware methods ──
  na_mask <- is.na(data[[conc_col]])
  
  if (!any(na_mask)) {
    bioeq_log("No missing concentration values detected", "DEBUG")
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
    bioeq_log(sprintf("Missing data handled: %d imputed, %d excluded across %d profiles",
                      n_imputed, n_removed, length(data_grouped)), "WARNING")
  }
  
  return(list(
    data = data,
    log = imputation_log
  ))
}

