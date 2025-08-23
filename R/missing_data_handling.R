# BioEQ - Missing Data Handling Functions
# Simplified missing data strategies for NCA analysis only

#' Handle Missing Data for NCA Analysis
#'
#' @param data Data frame with time-concentration data
#' @param method Method for handling missing data ("complete", "interpolate", "locf")
#' @param group_cols Columns to group by (typically c("subject", "treatment"))
#' @param time_col Name of time column
#' @param conc_col Name of concentration column
#' @return Data frame with missing data handled
#' @export
handle_missing_data <- function(data, method = "complete", 
                               group_cols = c("subject", "treatment"), 
                               time_col = "time", conc_col = "concentration") {
  
  cat(sprintf("🔄 Handling missing data using method: %s\n", method))
  
  # Validate input
  required_cols <- c(group_cols, time_col, conc_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  switch(method,
    "complete" = {
      # Remove rows with missing concentration values
      complete_data <- data[!is.na(data[[conc_col]]), ]
      cat(sprintf("✅ Complete cases: %d rows retained from %d\n", nrow(complete_data), nrow(data)))
      return(complete_data)
    },
    
    "interpolate" = {
      # Simple linear interpolation for missing concentrations
      data_grouped <- split(data, do.call(paste, c(data[group_cols], sep = "_")))
      
      interpolated_list <- lapply(data_grouped, function(group) {
        # Order by time
        group <- group[order(group[[time_col]]), ]
        
        # Interpolate missing concentrations
        if (any(is.na(group[[conc_col]]))) {
          group[[conc_col]] <- approx(group[[time_col]], group[[conc_col]], 
                                    xout = group[[time_col]], 
                                    rule = 2, method = "linear")$y
        }
        return(group)
      })
      
      result_data <- do.call(rbind, interpolated_list)
      rownames(result_data) <- NULL
      cat(sprintf("✅ Interpolation completed for %d subject-treatment combinations\n", length(data_grouped)))
      return(result_data)
    },
    
    "locf" = {
      # Last Observation Carried Forward
      data_grouped <- split(data, do.call(paste, c(data[group_cols], sep = "_")))
      
      locf_list <- lapply(data_grouped, function(group) {
        # Order by time
        group <- group[order(group[[time_col]]), ]
        
        # Carry forward last observation
        for (i in 2:nrow(group)) {
          if (is.na(group[[conc_col]][i])) {
            group[[conc_col]][i] <- group[[conc_col]][i-1]
          }
        }
        return(group)
      })
      
      result_data <- do.call(rbind, locf_list)
      rownames(result_data) <- NULL
      cat(sprintf("✅ LOCF completed for %d subject-treatment combinations\n", length(data_grouped)))
      return(result_data)
    },
    
    stop("Unknown missing data method: ", method)
  )
}

#' Validate Missing Data Handling Results
#'
#' @param original_data Original data before handling
#' @param processed_data Data after missing data handling
#' @param method Method used for handling
#' @return Summary of changes made
validate_missing_data_handling <- function(original_data, processed_data, method) {
  
  original_missing <- sum(is.na(original_data$concentration))
  processed_missing <- sum(is.na(processed_data$concentration))
  
  cat(sprintf("\n📋 Missing Data Handling Summary (%s):\n", method))
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
#' @return List with completeness statistics
check_data_completeness <- function(data, group_cols = c("subject", "treatment"),
                                  time_col = "time", conc_col = "concentration") {
  
  # Check if dplyr is available
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    # Basic implementation without dplyr
    data$group_id <- do.call(paste, c(data[group_cols], sep = "_"))
    unique_groups <- unique(data$group_id)
    
    overall_stats <- list(
      total_profiles = length(unique_groups),
      overall_completeness = round(100 * sum(!is.na(data[[conc_col]])) / nrow(data), 1),
      profiles_with_missing = length(unique_groups[sapply(unique_groups, function(g) {
        group_data <- data[data$group_id == g, ]
        any(is.na(group_data[[conc_col]]))
      })]),
      min_completeness = 0,
      max_completeness = 100
    )
    
    cat(sprintf("📊 Data Completeness Summary:\n"))
    cat(sprintf("  Total profiles: %d\n", overall_stats$total_profiles))
    cat(sprintf("  Overall completeness: %.1f%%\n", overall_stats$overall_completeness))
    cat(sprintf("  Profiles with missing data: %d\n", overall_stats$profiles_with_missing))
    
    return(list(overall = overall_stats, by_profile = NULL))
  }
  
  # Group data
  data$group_id <- do.call(paste, c(data[group_cols], sep = "_"))
  
  completeness_stats <- data %>%
    dplyr::group_by(group_id) %>%
    dplyr::summarise(
      total_points = dplyr::n(),
      missing_points = sum(is.na(.data[[conc_col]])),
      complete_points = sum(!is.na(.data[[conc_col]])),
      completeness_pct = round(100 * complete_points / total_points, 1),
      .groups = "drop"
    )
  
  overall_stats <- list(
    total_profiles = nrow(completeness_stats),
    overall_completeness = round(100 * sum(completeness_stats$complete_points) / 
                               sum(completeness_stats$total_points), 1),
    profiles_with_missing = sum(completeness_stats$missing_points > 0),
    min_completeness = min(completeness_stats$completeness_pct),
    max_completeness = max(completeness_stats$completeness_pct)
  )
  
  cat(sprintf("📊 Data Completeness Summary:\n"))
  cat(sprintf("  Total profiles: %d\n", overall_stats$total_profiles))
  cat(sprintf("  Overall completeness: %.1f%%\n", overall_stats$overall_completeness))
  cat(sprintf("  Profiles with missing data: %d\n", overall_stats$profiles_with_missing))
  cat(sprintf("  Completeness range: %.1f%% - %.1f%%\n", 
              overall_stats$min_completeness, overall_stats$max_completeness))
  
  return(list(
    overall = overall_stats,
    by_profile = completeness_stats
  ))
}
