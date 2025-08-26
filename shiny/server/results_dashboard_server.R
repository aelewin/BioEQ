# Results Dashboard Server Module
# Comprehensive results processing and rendering

# Source dashboard utilities and interpretation guides
source("utils/dashboard_utils.R", local = TRUE)
source("utils/interpretation_guides.R", local = TRUE)
source("utils/report_generation.R", local = TRUE)

#' Convert log-transformed parameter names to display names
#' 
#' @param log_param_name Log-transformed parameter name (e.g., "lnCmax")
#' @return Display name (e.g., "Cmax")
log_param_to_display_name <- function(log_param_name) {
  # Mapping from log-transformed names to display names
  log_to_display_map <- c(
    "lnCmax" = "Cmax",
    "lnAUC0t" = "AUC0-t", 
    "lnAUC0inf" = "AUC0-∞",
    "lnAUClast" = "AUClast",
    "lnAUC072" = "AUC0-72h",
    "lnpAUC" = "pAUC",
    "lnTmax" = "Tmax",
    "lnT12" = "T½",
    "lnkel" = "kel"
  )
  
  # Return display name if mapping exists, otherwise return original name
  display_name <- log_to_display_map[log_param_name]
  if (is.na(display_name)) {
    # If no mapping found, try to remove "ln" prefix
    if (startsWith(log_param_name, "ln")) {
      return(substring(log_param_name, 3))
    } else {
      return(log_param_name)
    }
  }
  return(display_name)
}

#' Calculate PK Comparison Statistics
#' 
#' @param nca_data Data frame with NCA results containing subject_data
#' @param param_name Name of the parameter to analyze
#' @return List with individual and summary statistics
calculate_pk_comparison <- function(nca_data, param_name) {
  if (is.null(nca_data) || is.null(nca_data$subject_data)) {
    return(NULL)
  }
  
  # Extract subject data
  subject_data <- nca_data$subject_data
  
  # Debug: Check what columns are available
  cat("Available columns in subject_data:", names(subject_data), "\n")
  
  # Check if parameter exists
  if (!param_name %in% names(subject_data)) {
    cat("Parameter", param_name, "not found in columns:", names(subject_data), "\n")
    return(NULL)
  }
  
  cat("Parameter", param_name, "found in data\n")
  
  # Check what columns are available and find the correct column names
  available_cols <- names(subject_data)
  cat("Searching for subject and treatment columns in:", paste(available_cols, collapse = ", "), "\n")
  
  # Find subject column (could be "Subject", "subject", "SUBJECT", etc.)
  subject_col <- NULL
  for (col in c("Subject", "subject", "SUBJECT", "subj", "SUBJ", "ID", "id")) {
    if (col %in% available_cols) {
      subject_col <- col
      break
    }
  }
  
  # Find treatment column (could be "Treatment", "treatment", "TREATMENT", "TRT", etc.)
  treatment_col <- NULL
  for (col in c("Treatment", "treatment", "TREATMENT", "TRT", "trt", "Drug", "drug")) {
    if (col %in% available_cols) {
      treatment_col <- col
      break
    }
  }
  
  # Return NULL if we can't find the required columns
  if (is.null(subject_col) || is.null(treatment_col)) {
    cat("Could not find required columns. subject_col:", subject_col, "treatment_col:", treatment_col, "\n")
    return(list(
      error = paste("Required columns not found. Available columns:", 
                   paste(available_cols, collapse = ", "))
    ))
  }
  
  cat("Found columns - subject_col:", subject_col, "treatment_col:", treatment_col, "\n")
  
  # Filter for the parameter and reshape data using base R
  # Extract relevant columns
  cat("Extracting columns:", subject_col, treatment_col, param_name, "\n")
  
  # Check if all columns exist before extraction
  required_cols <- c(subject_col, treatment_col, param_name)
  missing_cols <- setdiff(required_cols, names(subject_data))
  if (length(missing_cols) > 0) {
    cat("Missing columns:", paste(missing_cols, collapse = ", "), "\n")
    return(list(
      error = paste("Missing columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  param_data <- subject_data[, c(subject_col, treatment_col, param_name)]
  names(param_data) <- c("Subject", "Treatment", "Value")  # Standardize column names
  
  cat("Extracted data dimensions:", nrow(param_data), "rows x", ncol(param_data), "columns\n")
  
  # Remove rows with missing values
  param_data <- param_data[!is.na(param_data$Value), ]
  
  # Check what treatments are available
  unique_treatments <- unique(param_data$Treatment)
  cat("Available treatments:", paste(unique_treatments, collapse = ", "), "\n")
  
  # Find Test and Reference treatments more flexibly
  test_treatment <- NULL
  ref_treatment <- NULL
  
  # Look for Test treatment (could be T, Test, TEST, etc.)
  for (trt in c("T", "Test", "TEST", "test", "1")) {
    if (trt %in% unique_treatments) {
      test_treatment <- trt
      break
    }
  }
  
  # Look for Reference treatment (could be R, Reference, REF, etc.)
  for (trt in c("R", "Reference", "REF", "ref", "REFERENCE", "2")) {
    if (trt %in% unique_treatments) {
      ref_treatment <- trt
      break
    }
  }
  
  if (is.null(test_treatment) || is.null(ref_treatment)) {
    return(list(
      error = paste("Could not identify Test and Reference treatments. Available treatments:", 
                   paste(unique_treatments, collapse = ", "))
    ))
  }
  
  # Split by treatment to get Test and Reference values
  test_data <- param_data[param_data$Treatment == test_treatment, ]
  ref_data <- param_data[param_data$Treatment == ref_treatment, ]
  
  cat("Test data rows:", nrow(test_data), "Reference data rows:", nrow(ref_data), "\n")
  
  # Merge Test and Reference data by Subject
  comparison_data <- merge(test_data[, c("Subject", "Value")], 
                          ref_data[, c("Subject", "Value")], 
                          by = "Subject", 
                          suffixes = c("_Test", "_Reference"),
                          all = TRUE)
  
  # Rename columns and calculate ratio
  names(comparison_data) <- c("Subject", "Test", "Reference")
  comparison_data$Ratio <- comparison_data$Test / comparison_data$Reference
  comparison_data$Subject <- as.character(comparison_data$Subject)
  
  # Remove rows where both Test and Reference are missing
  comparison_data <- comparison_data[!(is.na(comparison_data$Test) & is.na(comparison_data$Reference)), ]
  
  # Calculate summary statistics
  test_values <- comparison_data$Test[!is.na(comparison_data$Test)]
  ref_values <- comparison_data$Reference[!is.na(comparison_data$Reference)]
  ratio_values <- comparison_data$Ratio[!is.na(comparison_data$Ratio)]
  
  # Arithmetic statistics
  # Note: Arithmetic ratio is mean of individual subject ratios (T/R for each subject)
  arithmetic_stats <- data.frame(
    Statistic = c("Arithmetic Mean", "Standard Deviation", "CV%"),
    Test = c(
      mean(test_values, na.rm = TRUE),
      sd(test_values, na.rm = TRUE),
      100 * sd(test_values, na.rm = TRUE) / mean(test_values, na.rm = TRUE)
    ),
    Reference = c(
      mean(ref_values, na.rm = TRUE),
      sd(ref_values, na.rm = TRUE),
      100 * sd(ref_values, na.rm = TRUE) / mean(ref_values, na.rm = TRUE)
    ),
    Ratio = c(
      mean(ratio_values, na.rm = TRUE),  # Mean of individual T/R ratios
      sd(ratio_values, na.rm = TRUE),
      100 * sd(ratio_values, na.rm = TRUE) / mean(ratio_values, na.rm = TRUE)
    )
  )
  
  # Geometric statistics (only for positive values)
  positive_test <- test_values[test_values > 0]
  positive_ref <- ref_values[ref_values > 0]
  positive_ratio <- ratio_values[ratio_values > 0]
  
  # For log-transformed parameters, we don't calculate geometric mean
  is_log_param <- grepl("^(log|ln)", tolower(param_name))
  
  if (!is_log_param && length(positive_test) > 0 && length(positive_ref) > 0) {
    geo_mean_test <- exp(mean(log(positive_test)))
    geo_mean_ref <- exp(mean(log(positive_ref)))
    geo_mean_ratio <- if(length(positive_ratio) > 0) exp(mean(log(positive_ratio))) else geo_mean_test / geo_mean_ref
    
    # Geometric standard deviation calculation
    geo_sd_test <- if(length(positive_test) > 1) exp(sd(log(positive_test))) else NA
    geo_sd_ref <- if(length(positive_ref) > 1) exp(sd(log(positive_ref))) else NA
    geo_sd_ratio <- if(length(positive_ratio) > 1) exp(sd(log(positive_ratio))) else NA
    
    # Geometric CV calculation
    geo_cv_test <- if(length(positive_test) > 1) 100 * sqrt(exp(var(log(positive_test))) - 1) else NA
    geo_cv_ref <- if(length(positive_ref) > 1) 100 * sqrt(exp(var(log(positive_ref))) - 1) else NA
    geo_cv_ratio <- if(length(positive_ratio) > 1) 100 * sqrt(exp(var(log(positive_ratio))) - 1) else NA
    
    geometric_stats <- data.frame(
      Statistic = c("Geometric Mean", "Geometric SD", "Geometric CV%"),
      Test = c(geo_mean_test, geo_sd_test, geo_cv_test),
      Reference = c(geo_mean_ref, geo_sd_ref, geo_cv_ref),
      Ratio = c(geo_mean_ratio, geo_sd_ratio, geo_cv_ratio)
    )
  } else {
    geometric_stats <- NULL
  }
  
  # Determine units based on parameter
  unit <- ""
  if (grepl("AUC", param_name)) {
    unit <- "ng·h/mL"
  } else if (param_name == "Cmax") {
    unit <- "ng/mL"
  } else if (param_name %in% c("Tmax", "t_half", "Tlast")) {
    unit <- "h"
  } else if (param_name %in% c("CL_F", "CLss_F")) {
    unit <- "mL/h"
  } else if (param_name %in% c("Vd_F", "Vss_F")) {
    unit <- "mL"
  } else if (grepl("^(log|ln)", param_name)) {
    unit <- paste0("ln(", sub("^(log|ln)", "", param_name), ")")
  }
  
  # Calculate least squares means with SD and CV%
  # Note: LSM ratio is ratio of treatment means (mean(T) / mean(R))
  # For now, using simple means (should ideally come from ANOVA model)
  lsmeans_test <- mean(test_values, na.rm = TRUE)
  lsmeans_ref <- mean(ref_values, na.rm = TRUE)
  # LSM ratio should be ratio of LSMeans, not mean of individual ratios
  lsmeans_ratio <- lsmeans_test / lsmeans_ref
  
  lsmeans_test_sd <- sd(test_values, na.rm = TRUE)
  lsmeans_ref_sd <- sd(ref_values, na.rm = TRUE)
  # For LSM ratio SD, we need to calculate from the ratio of means approach
  # This is an approximation - ideally would come from ANOVA model
  lsmeans_ratio_sd <- lsmeans_ratio * sqrt((lsmeans_test_sd/lsmeans_test)^2 + (lsmeans_ref_sd/lsmeans_ref)^2)
  
  lsmeans_test_cv <- 100 * lsmeans_test_sd / lsmeans_test
  lsmeans_ref_cv <- 100 * lsmeans_ref_sd / lsmeans_ref
  lsmeans_ratio_cv <- 100 * lsmeans_ratio_sd / lsmeans_ratio
  
  lsmeans <- data.frame(
    Statistic = c("Least Squares Mean", "Standard Deviation", "CV%"),
    Test = c(lsmeans_test, lsmeans_test_sd, lsmeans_test_cv),
    Reference = c(lsmeans_ref, lsmeans_ref_sd, lsmeans_ref_cv),
    Ratio = c(lsmeans_ratio, lsmeans_ratio_sd, lsmeans_ratio_cv)
  )
  
  return(list(
    individual_data = comparison_data,
    lsmeans = lsmeans,
    arithmetic_stats = arithmetic_stats,
    geometric_stats = geometric_stats,
    unit = unit,
    parameter = param_name,
    n_subjects = nrow(comparison_data)
  ))
}

# Format simple ANOVA results for display
format_simple_anova_results <- function(param_result, param_name) {
  
  # Create summary statistics card
  summary_card <- div(class = "card mb-3",
    div(class = "card-header",
      h5(class = "card-title mb-0", 
        icon("calculator"), 
        sprintf(" ANOVA Results for %s", param_name)
      )
    ),
    div(class = "card-body",
      div(class = "row",
        div(class = "col-md-4",
          h6("Model Summary:"),
          tags$ul(
            if (!is.null(param_result$anova_method)) tags$li(
              style = "font-weight: bold; color: #2c3e50;",
              sprintf("ANOVA Method: %s", 
                if (param_result$anova_method == "mixed") "Mixed Effects Model (REML)" else "Fixed Effects Model")
            ),
            tags$li(sprintf("Observations: %d", param_result$n_observations)),
            tags$li(sprintf("R-squared: %.4f", param_result$r_squared)),
            if (!is.null(param_result$adj_r_squared)) tags$li(sprintf("Adj R-squared: %.4f", param_result$adj_r_squared)),
            tags$li(sprintf("Residual MSE: %.6f", param_result$residual_mse)),
            tags$li(sprintf("Residual DF: %d", param_result$residual_df))
          )
        ),
        div(class = "col-md-4",
          h6("Model Diagnostics:"),
          tags$ul(
            if (!is.null(param_result$aic)) tags$li(sprintf("AIC: %.2f", param_result$aic)),
            if (!is.null(param_result$deviance)) tags$li(sprintf("Deviance: %.4f", param_result$deviance)),
            tags$li(sprintf("Formula: %s", param_result$formula))
          )
        ),
        div(class = "col-md-4",
          h6("Treatment Effect:"),
          if (!is.na(param_result$treatment_effect)) {
            tags$ul(
              tags$li(sprintf("Effect: %.6f", param_result$treatment_effect)),
              tags$li(sprintf("Standard Error: %.6f", param_result$treatment_se)),
              tags$li(sprintf("P-value: %.4f", param_result$treatment_pvalue)),
              tags$li(class = if (param_result$treatment_pvalue < 0.05) "text-danger" else "text-success",
                sprintf("Significance: %s", if (param_result$treatment_pvalue < 0.05) "Significant" else "Not Significant"))
            )
          } else {
            p("Treatment effect not available")
          }
        )
      )
    )
  )
  
  # Create ANOVA tables if available
  anova_tables <- div()
  
  # Type I SS (Analysis of Variance Table)
  if (!is.null(param_result$anova)) {
    
    # Format ANOVA table for display
    anova_df <- param_result$anova
    
    # Determine decimal places based on parameter type
    # Log-transformed parameters need decimals, non-log can be whole numbers
    is_log_param <- grepl("^(log|ln)", tolower(param_name), ignore.case = TRUE)
    sum_sq_decimals <- if (is_log_param) 5 else 0
    mean_sq_decimals <- if (is_log_param) 5 else 0
    
    # Debug output
    cat(sprintf("[DEBUG] ANOVA Formatting - Parameter: %s, Is Log: %s, Decimals: %d\n", 
                param_name, is_log_param, sum_sq_decimals))
    
    anova_display <- data.frame(
      Source = rownames(anova_df),
      Df = anova_df$Df,
      `Sum.Sq` = format(round(anova_df$`Sum Sq`, sum_sq_decimals), scientific = FALSE, nsmall = sum_sq_decimals),
      `Mean.Sq` = format(round(anova_df$`Mean Sq`, mean_sq_decimals), scientific = FALSE, nsmall = mean_sq_decimals),
      `F.value` = round(anova_df$`F value`, 4),
      `Pr..F.` = format.pval(anova_df$`Pr(>F)`, digits = 4),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Clean up NA values
    anova_display$`F.value`[is.na(anova_df$`F value`)] <- ""
    anova_display$`Pr..F.`[is.na(anova_df$`Pr(>F)`)] <- ""
    
    type1_table <- div(class = "card mb-3",
      div(class = "card-header",
        h5(class = "card-title mb-0", 
          icon("table"), 
          " Type I SS - Analysis of Variance Table"
        )
      ),
      div(class = "card-body",
        p(style = "font-size: 0.9em; color: #6c757d; margin-bottom: 15px;",
          "Sequential (Type I) sums of squares. Each term is fitted after the previous terms in the model."),
        div(class = "table-responsive",
          tags$table(class = "table table-striped table-hover",
            tags$thead(
              tags$tr(
                lapply(names(anova_display), function(col) {
                  tags$th(col)
                })
              )
            ),
            tags$tbody(
              lapply(1:nrow(anova_display), function(i) {
                tags$tr(
                  lapply(names(anova_display), function(col) {
                    tags$td(as.character(anova_display[i, col]))
                  })
                )
              })
            )
          )
        )
      )
    )
    
    anova_tables <- tagList(anova_tables, type1_table)
  }
  
  # Type III SS (Single term deletions)
  if (!is.null(param_result$type3_ss)) {
    
    type3_df <- param_result$type3_ss
    
    # Format Type III SS table
    type3_display <- data.frame(
      Term = rownames(type3_df),
      Df = type3_df$Df,
      `Sum.of.Sq` = if("Sum of Sq" %in% names(type3_df)) format(round(type3_df$`Sum of Sq`, sum_sq_decimals), scientific = FALSE, nsmall = sum_sq_decimals) else "",
      RSS = format(round(type3_df$RSS, sum_sq_decimals), scientific = FALSE, nsmall = sum_sq_decimals),
      AIC = round(type3_df$AIC, 2),
      `F.value` = round(type3_df$`F value`, 4),
      `Pr..F.` = format.pval(type3_df$`Pr(>F)`, digits = 4),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Clean up NA values
    type3_display$`F.value`[is.na(type3_df$`F value`)] <- ""
    type3_display$`Pr..F.`[is.na(type3_df$`Pr(>F)`)] <- ""
    
    type3_table <- div(class = "card mb-3",
      div(class = "card-header",
        h5(class = "card-title mb-0", 
          icon("table"), 
          " Type III SS - Single Term Deletions"
        )
      ),
      div(class = "card-body",
        p(style = "font-size: 0.9em; color: #6c757d; margin-bottom: 15px;",
          "Marginal (Type III) sums of squares. Each term is tested after fitting all other terms in the model."),
        div(class = "table-responsive",
          tags$table(class = "table table-striped table-hover",
            tags$thead(
              tags$tr(
                lapply(names(type3_display), function(col) {
                  tags$th(col)
                })
              )
            ),
            tags$tbody(
              lapply(1:nrow(type3_display), function(i) {
                tags$tr(
                  lapply(names(type3_display), function(col) {
                    tags$td(as.character(type3_display[i, col]))
                  })
                )
              })
            )
          )
        )
      )
    )
    
    anova_tables <- tagList(anova_tables, type3_table)
  }
  
  # Subject within Sequence Error Term Analysis
  if (!is.null(param_result$subj_seq_analysis) && !is.null(param_result$subj_seq_analysis$error_term)) {
    subj_seq <- param_result$subj_seq_analysis
    
    # Create the hypothesis tests table data matching the reference format
    table_data <- data.frame(
      Source = c("Error: subj", "Error: Within", "prd", "drug", "Residuals"),
      Df = c(
        subj_seq$error_term$df,                    # Subject within sequence DF
        "",                                        # Within header (no values)
        if(!is.null(subj_seq$hypothesis_tests$period)) subj_seq$hypothesis_tests$period$df else 1,
        if(!is.null(subj_seq$hypothesis_tests$drug)) subj_seq$hypothesis_tests$drug$df else 1,
        param_result$residual_df                   # Residual DF
      ),
      `Sum Sq` = c(
        sprintf("%.0f", subj_seq$error_term$ss),   # Subject within sequence SS
        "",                                        # Within header (no values)
        if(!is.null(subj_seq$hypothesis_tests$period)) sprintf("%.0f", subj_seq$hypothesis_tests$period$ss) else "",
        if(!is.null(subj_seq$hypothesis_tests$drug)) sprintf("%.0f", subj_seq$hypothesis_tests$drug$ss) else "",
        sprintf("%.0f", param_result$residual_mse * param_result$residual_df)  # Residual SS
      ),
      `Mean Sq` = c(
        sprintf("%.0f", subj_seq$error_term$ms),   # Subject within sequence MS  
        "",                                        # Within header (no values)
        if(!is.null(subj_seq$hypothesis_tests$period)) sprintf("%.0f", subj_seq$hypothesis_tests$period$ms) else "",
        if(!is.null(subj_seq$hypothesis_tests$drug)) sprintf("%.0f", subj_seq$hypothesis_tests$drug$ms) else "",
        sprintf("%.0f", param_result$residual_mse) # Residual MS
      ),
      `F value` = c(
        "",                                        # Error term doesn't have F-value
        "",                                        # Within header (no values)
        if(!is.null(subj_seq$hypothesis_tests$period)) sprintf("%.4f", subj_seq$hypothesis_tests$period$f_value) else "",
        if(!is.null(subj_seq$hypothesis_tests$drug)) sprintf("%.4f", subj_seq$hypothesis_tests$drug$f_value) else "",
        ""                                         # Residuals don't have F-value
      ),
      `Pr(>F)` = c(
        "",                                        # Error term doesn't have p-value
        "",                                        # Within header (no values)
        if(!is.null(subj_seq$hypothesis_tests$period)) sprintf("%.4f", subj_seq$hypothesis_tests$period$p_value) else "",
        if(!is.null(subj_seq$hypothesis_tests$drug)) sprintf("%.4f", subj_seq$hypothesis_tests$drug$p_value) else "",
        ""                                         # Residuals don't have p-value
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    subj_seq_table <- div(class = "card mb-3",
      div(class = "card-header",
        h5(class = "card-title mb-0", 
          icon("users"), 
          " Tests of Hypothesis for SUBJECT(SEQUENCE) as Error Term"
        )
      ),
      div(class = "card-body",
        # Create the formatted table matching reference format
        div(class = "table-responsive",
          tags$table(class = "table table-sm table-bordered",
            tags$thead(class = "table-light",
              tags$tr(
                tags$th("", style = "text-align: left; width: 120px;"),
                tags$th("Df", style = "text-align: right; width: 60px;"),
                tags$th("Sum Sq", style = "text-align: right; width: 80px;"),
                tags$th("Mean Sq", style = "text-align: right; width: 80px;"),
                tags$th("F value", style = "text-align: right; width: 80px;"),
                tags$th("Pr(>F)", style = "text-align: right; width: 80px;")
              )
            ),
            tags$tbody(
              # Error: subj row
              tags$tr(
                tags$td("Error: subj", style = "text-align: left; font-family: monospace; font-weight: bold;"),
                tags$td(table_data$Df[1], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Sum Sq`[1], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Mean Sq`[1], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`F value`[1], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Pr(>F)`[1], style = "text-align: right; font-family: monospace;")
              ),
              # Error: Within row (header)
              tags$tr(
                tags$td("Error: Within", style = "text-align: left; font-family: monospace; font-weight: bold;"),
                tags$td("", style = "text-align: right; font-family: monospace;"),
                tags$td("", style = "text-align: right; font-family: monospace;"),
                tags$td("", style = "text-align: right; font-family: monospace;"),
                tags$td("", style = "text-align: right; font-family: monospace;"),
                tags$td("", style = "text-align: right; font-family: monospace;")
              ),
              # prd row
              tags$tr(
                tags$td("prd", style = "text-align: left; font-family: monospace; padding-left: 20px;"),
                tags$td(table_data$Df[3], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Sum Sq`[3], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Mean Sq`[3], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`F value`[3], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Pr(>F)`[3], style = "text-align: right; font-family: monospace;")
              ),
              # drug row
              tags$tr(
                tags$td("drug", style = "text-align: left; font-family: monospace; padding-left: 20px;"),
                tags$td(table_data$Df[4], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Sum Sq`[4], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Mean Sq`[4], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`F value`[4], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Pr(>F)`[4], style = "text-align: right; font-family: monospace;")
              ),
              # Residuals row
              tags$tr(
                tags$td("Residuals", style = "text-align: left; font-family: monospace; padding-left: 20px;"),
                tags$td(table_data$Df[5], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Sum Sq`[5], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Mean Sq`[5], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`F value`[5], style = "text-align: right; font-family: monospace;"),
                tags$td(table_data$`Pr(>F)`[5], style = "text-align: right; font-family: monospace;")
              )
            )
          )
        )
      )
    )
    
    anova_tables <- tagList(anova_tables, subj_seq_table)
  }
  
  # Return combined results
  div(
    summary_card,
    anova_tables
  )
}

results_dashboard_server <- function(id, be_results, nca_results, analysis_config, carryover_results = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for tracking state
    values <- reactiveValues(
      export_ready = FALSE,
      report_generating = FALSE
    )
    
    # Create reactive values for column selection - start empty
    column_selection <- reactiveValues(
      subject_info = character(0),
      primary_pk = character(0),
      secondary_pk = character(0),
      lambda_z = character(0),
      log_pk = character(0),
      dynamic = character(0)
    )
    
    # Check if results are available
    results_available <- reactive({
      !is.null(be_results()) && !is.null(nca_results())
    })
    
    # Track available columns from NCA results
    available_columns <- reactive({
      req(nca_results())
      
      # Get the NCA results data
      if (is.data.frame(nca_results())) {
        data <- nca_results()
      } else if (!is.null(nca_results()$parameters)) {
        data <- nca_results()$parameters
      } else if (!is.null(nca_results()$subject_data)) {
        data <- nca_results()$subject_data
      } else {
        return(NULL)
      }
      
      # Return all column names
      names(data)
    })
    
    # Update reactive values when checkboxes change
    observeEvent(input$subject_info_cols, {
      column_selection$subject_info <- input$subject_info_cols
    })
    
    observeEvent(input$primary_pk_cols, {
      column_selection$primary_pk <- input$primary_pk_cols
    })
    
    observeEvent(input$secondary_pk_cols, {
      column_selection$secondary_pk <- input$secondary_pk_cols
    })
    
    observeEvent(input$log_pk_cols, {
      column_selection$log_pk <- input$log_pk_cols
    })
    
    observeEvent(input$lambda_z_cols, {
      column_selection$lambda_z <- input$lambda_z_cols
    })
    
    observeEvent(input$dynamic_cols, {
      column_selection$dynamic <- input$dynamic_cols
    })
    
    # Column selection button handlers
    observeEvent(input$select_all_cols, {
      req(results_available())
      
      tryCatch({
        nca_res <- nca_results()
        
        # Use consistent data source - prioritize subject_data
        if (is.data.frame(nca_res)) {
          data <- nca_res
        } else if (!is.null(nca_res$subject_data)) {
          data <- nca_res$subject_data
        } else if (!is.null(nca_res$parameters)) {
          data <- nca_res$parameters
        } else {
          cat("[DEBUG] Select All - No valid NCA data found\n")
          return()
        }
        
        available_cols <- names(data)
        cat("[DEBUG] Select All - Available columns:", paste(available_cols, collapse=", "), "\n")
        
        # Subject info - use the actual checkbox values
        subject_choices <- c("Subject", "Treatment", "Period", "Sequence", "dose")
        updateCheckboxGroupInput(session, "subject_info_cols", selected = subject_choices)
        
        # Primary PK parameters  
        primary_choices <- c("Cmax", "AUC0t", "AUC0inf")
        if ("pAUC" %in% available_cols) {
          primary_choices <- c(primary_choices, "pAUC")
          cat("[DEBUG] Select All - Adding pAUC to primary choices\n")
        }
        if ("AUC072" %in% available_cols) {
          primary_choices <- c(primary_choices, "AUC072")
          cat("[DEBUG] Select All - Adding AUC072 to primary choices\n")
        }
        updateCheckboxGroupInput(session, "primary_pk_cols", selected = primary_choices)
        
        # Secondary PK parameters (including pAUC if not in primary)
        secondary_choices <- c("Tmax", "t_half", "Tlast", "Clast", "CL_F", "Vd_F", "MRT", "AUC_percent_extrap")
        if ("pAUC" %in% available_cols && !"pAUC" %in% primary_choices) {
          secondary_choices <- c(secondary_choices, "pAUC")
          cat("[DEBUG] Select All - Adding pAUC to secondary choices\n")
        }
        available_secondary <- intersect(secondary_choices, available_cols)
        updateCheckboxGroupInput(session, "secondary_pk_cols", selected = available_secondary)
        
        # Lambda z statistics
        lambda_choices <- c("lambda_z", "lambda_z_r_squared", "lambda_z_p_value")
        available_lambda <- intersect(lambda_choices, available_cols)
        updateCheckboxGroupInput(session, "lambda_z_cols", selected = available_lambda)
        
        # Log-transformed parameters
        log_choices <- c("lnCmax", "lnAUC0t", "lnAUC0inf")
        if ("lnTmax" %in% available_cols) log_choices <- c(log_choices, "lnTmax")
        if ("lnpAUC" %in% available_cols) log_choices <- c(log_choices, "lnpAUC")
        if ("lnAUC072" %in% available_cols) log_choices <- c(log_choices, "lnAUC072")
        updateCheckboxGroupInput(session, "log_pk_cols", selected = log_choices)
        
      }, error = function(e) {
        cat(sprintf("[DEBUG] Error in select_all_cols: %s\n", e$message))
      })
    })
    
    observeEvent(input$deselect_all_cols, {
      updateCheckboxGroupInput(session, "subject_info_cols", selected = character(0))
      updateCheckboxGroupInput(session, "primary_pk_cols", selected = character(0))
      updateCheckboxGroupInput(session, "secondary_pk_cols", selected = character(0))
      updateCheckboxGroupInput(session, "lambda_z_cols", selected = character(0))
      updateCheckboxGroupInput(session, "log_pk_cols", selected = character(0))
    })
    
    observeEvent(input$reset_default_cols, {
      updateCheckboxGroupInput(session, "subject_info_cols", selected = c("Subject", "Treatment"))
      updateCheckboxGroupInput(session, "primary_pk_cols", selected = c("Cmax", "AUC0t", "AUC0inf"))
      updateCheckboxGroupInput(session, "secondary_pk_cols", selected = c("Tmax", "t_half"))
      updateCheckboxGroupInput(session, "lambda_z_cols", selected = c("lambda_z"))
      updateCheckboxGroupInput(session, "log_pk_cols", selected = c("lnCmax", "lnAUC0t", "lnAUC0inf"))
    })
    
    # Dynamic UI for Primary PK Parameters - only show calculated parameters
    output$primary_pk_cols_ui <- renderUI({
      req(results_available())
      
      tryCatch({
        nca_res <- nca_results()
        
        # Use consistent data source - prioritize subject_data
        if (is.data.frame(nca_res)) {
          data <- nca_res
        } else if (!is.null(nca_res$subject_data)) {
          data <- nca_res$subject_data
        } else if (!is.null(nca_res$parameters)) {
          data <- nca_res$parameters
        } else {
          # Fallback if no valid data found
          stop("No valid NCA data found")
        }
        
        cat("[DEBUG] Primary PK UI - Available columns:", paste(names(data), collapse=", "), "\n")
        cat("[DEBUG] Primary PK UI - Checking for pAUC:", "pAUC" %in% names(data), "\n")
        
        # Standard primary parameters always available
        primary_choices <- list(
          "Cmax" = "Cmax",
          "AUC0-t" = "AUC0t",
          "AUC0-inf" = "AUC0inf"
        )
        
        # Optional parameters - only show if calculated
        if ("pAUC" %in% names(data)) {
          primary_choices[["pAUC"]] <- "pAUC"
          cat("[DEBUG] Primary PK UI - Adding pAUC to choices\n")
        }
        if ("AUC072" %in% names(data)) {
          primary_choices[["AUC0-72"]] <- "AUC072"
          cat("[DEBUG] Primary PK UI - Adding AUC072 to choices\n")
        }
        
        cat("[DEBUG] Primary PK UI - Final choices:", paste(names(primary_choices), collapse=", "), "\n")
        
        checkboxGroupInput(
          session$ns("primary_pk_cols"),
          label = NULL,
          choices = primary_choices,
          selected = character(0)
        )
      }, error = function(e) {
        # Fallback to default choices
        checkboxGroupInput(
          session$ns("primary_pk_cols"),
          label = NULL,
          choices = list(
            "Cmax" = "Cmax",
            "AUC0-t" = "AUC0t",
            "AUC0-inf" = "AUC0inf"
          ),
          selected = character(0)
        )
      })
    })
    
    # Dynamic UI for Secondary PK Parameters
    output$secondary_pk_cols_ui <- renderUI({
      req(results_available())
      
      tryCatch({
        nca_res <- nca_results()
        
        # Use consistent data source - prioritize subject_data
        if (is.data.frame(nca_res)) {
          data <- nca_res
        } else if (!is.null(nca_res$subject_data)) {
          data <- nca_res$subject_data
        } else if (!is.null(nca_res$parameters)) {
          data <- nca_res$parameters
        } else {
          # Fallback if no valid data found
          stop("No valid NCA data found")
        }
        
        cat("[DEBUG] Secondary PK UI - Available columns:", paste(names(data), collapse=", "), "\n")
        
        secondary_choices <- list(
          "Tmax" = "Tmax",
          "Half-life" = "t_half",
          "Tlast" = "Tlast",
          "Clast" = "Clast",
          "CL/F" = "CL_F",
          "Vd/F" = "Vd_F",
          "MRT" = "MRT",
          "AUC Extrap %" = "AUC_percent_extrap"
        )
        
        # Add pAUC to secondary parameters if available
        if ("pAUC" %in% names(data)) {
          secondary_choices[["pAUC"]] <- "pAUC"
          cat("[DEBUG] Secondary PK UI - Adding pAUC to choices\n")
        }
        
        cat("[DEBUG] Secondary PK UI - Final choices:", paste(names(secondary_choices), collapse=", "), "\n")
        
        checkboxGroupInput(
          session$ns("secondary_pk_cols"),
          label = NULL,
          choices = secondary_choices,
          selected = character(0)
        )
      }, error = function(e) {
        checkboxGroupInput(
          session$ns("secondary_pk_cols"),
          label = NULL,
          choices = list(),
          selected = NULL
        )
      })
    })
    
    # Dynamic UI for Log-Transformed Parameters - only show calculated parameters
    output$log_pk_cols_ui <- renderUI({
      cat("DEBUG: log_pk_cols_ui called - ENTRY POINT\n")
      
      if (!results_available()) {
        cat("DEBUG: results_available() is FALSE - exiting early\n")
        cat("DEBUG: be_results() is null:", is.null(be_results()), "\n")
        cat("DEBUG: nca_results() is null:", is.null(nca_results()), "\n")
        return(div("No results available yet"))
      }
      
      cat("DEBUG: results_available() is TRUE, proceeding...\n")
      
      tryCatch({
        nca_res <- nca_results()
        
        # Handle different NCA results structures
        if (is.data.frame(nca_res)) {
          data <- nca_res
        } else if (!is.null(nca_res$subject_data)) {
          data <- nca_res$subject_data  # Updated to use subject_data
        } else if (!is.null(nca_res$parameters)) {
          data <- nca_res$parameters
        } else {
          cat("DEBUG: NCA results structure not recognized\n")
          return(div("No log-transformed parameters available"))
        }
        
        # Debug: Print structure of data to console
        cat("DEBUG log_pk_cols_ui: NCA results structure:\n")
        if (is.data.frame(nca_res)) {
          cat("  - nca_res is a data.frame with columns:", paste(names(nca_res), collapse=", "), "\n")
        } else {
          cat("  - nca_res is a list with elements:", paste(names(nca_res), collapse=", "), "\n")
          if (!is.null(nca_res$subject_data)) {
            cat("  - nca_res$subject_data has columns:", paste(names(nca_res$subject_data), collapse=", "), "\n")
          }
          if (!is.null(nca_res$parameters)) {
            cat("  - nca_res$parameters has columns:", paste(names(nca_res$parameters), collapse=", "), "\n")
          }
        }
        cat("  - data has columns:", paste(names(data), collapse=", "), "\n")
        
        log_choices <- list()
        
        # Standard log parameters
        if ("lnCmax" %in% names(data)) {
          log_choices[["ln(Cmax)"]] <- "lnCmax"
        }
        if ("lnAUC0t" %in% names(data)) {
          log_choices[["ln(AUC0-t)"]] <- "lnAUC0t"
        }
        if ("lnAUC0inf" %in% names(data)) {
          log_choices[["ln(AUC0-inf)"]] <- "lnAUC0inf"
        }
        
        # Optional log parameters - only if calculated
        if ("lnTmax" %in% names(data)) {
          log_choices[["ln(Tmax)"]] <- "lnTmax"
        }
        if ("lnpAUC" %in% names(data)) {
          log_choices[["ln(pAUC)"]] <- "lnpAUC"
        }
        if ("lnAUC072" %in% names(data)) {
          log_choices[["ln(AUC0-72)"]] <- "lnAUC072"
        }
        
        cat("DEBUG: log_choices found:", length(log_choices), "items -", paste(names(log_choices), collapse=", "), "\n")
        
        checkboxGroupInput(
          session$ns("log_pk_cols"),
          label = NULL,
          choices = log_choices,
          selected = character(0)
        )
      }, error = function(e) {
        checkboxGroupInput(
          session$ns("log_pk_cols"),
          label = NULL,
          choices = list(),
          selected = NULL
        )
      })
    })
    
    # Dynamic UI for ANOVA Parameter Selection - only show parameters with ANOVA results
    output$anova_parameter_select_ui <- renderUI({
      req(results_available())
      
      tryCatch({
        be_res <- be_results()
        
        # Check if ANOVA results exist (they should be attached to BE results)
        if (is.null(be_res$anova_results) || length(be_res$anova_results) == 0) {
          cat("[DEBUG] ANOVA Parameter UI - No anova_results found in BE results\n")
          return(selectInput(
            session$ns("anova_parameter_select"),
            label = NULL,
            choices = list("No ANOVA results available" = ""),
            selected = ""
          ))
        }
        
        # Get the parameters that were actually analyzed (from Step 2 ANOVA setup)
        anova_data <- be_res$anova_results
        available_params <- if (!is.null(anova_data$parameters)) {
          anova_data$parameters  # These are the parameters selected in Step 2
        } else {
          character(0)
        }
        
        cat("[DEBUG] ANOVA Parameter UI - Parameters from Step 2:", paste(available_params, collapse=", "), "\n")
        
        if (length(available_params) == 0) {
          return(selectInput(
            session$ns("anova_parameter_select"),
            label = NULL,
            choices = list("No parameters selected for ANOVA" = ""),
            selected = ""
          ))
        }
        
        # Create organized choices
        anova_choices <- list()
        
        # Primary parameters
        primary_params <- c("Cmax", "AUC0t", "AUC0inf")
        available_primary <- intersect(primary_params, available_params)
        if (length(available_primary) > 0) {
          anova_choices[["--- Primary Parameters ---"]] <- ""
          for (param in available_primary) {
            anova_choices[[param]] <- param
          }
        }
        
        # Log-transformed parameters
        log_params <- c("lnCmax", "lnAUC0t", "lnAUC0inf", "lnTmax", "lnpAUC", "lnAUC072")
        available_log <- intersect(log_params, available_params)
        if (length(available_log) > 0) {
          anova_choices[["--- Log-Transformed Parameters ---"]] <- ""
          for (param in available_log) {
            display_name <- switch(param,
              "lnCmax" = "ln(Cmax)",
              "lnAUC0t" = "ln(AUC0-t)",
              "lnAUC0inf" = "ln(AUC0-inf)",
              "lnTmax" = "ln(Tmax)",
              "lnpAUC" = "ln(pAUC)",
              "lnAUC072" = "ln(AUC0-72)",
              param
            )
            anova_choices[[display_name]] <- param
          }
        }
        
        # Secondary parameters
        secondary_params <- c("Tmax", "pAUC", "AUC072")
        available_secondary <- intersect(secondary_params, available_params)
        if (length(available_secondary) > 0) {
          anova_choices[["--- Secondary Parameters ---"]] <- ""
          for (param in available_secondary) {
            display_name <- switch(param,
              "AUC072" = "AUC0-72",
              param
            )
            anova_choices[[display_name]] <- param
          }
        }
        
        # Select first available parameter as default
        default_selection <- if (length(available_params) > 0) available_params[1] else ""
        
        selectInput(
          session$ns("anova_parameter_select"),
          label = NULL,
          choices = anova_choices,
          selected = default_selection
        )
        
      }, error = function(e) {
        cat(sprintf("[DEBUG] Error creating ANOVA parameter selection: %s\n", e$message))
        selectInput(
          session$ns("anova_parameter_select"),
          label = NULL,
          choices = list("Error loading parameters" = ""),
          selected = ""
        )
      })
    })
    
    # Individual subject table - moved to PK Parameters tab (previously subject_data_table)
    output$individual_subject_table <- DT::renderDataTable({
      req(results_available())
      
      tryCatch({
        nca_res <- nca_results()
        
        cat(sprintf("[DEBUG] NCA results class: %s\n", class(nca_res)))
        cat(sprintf("[DEBUG] NCA results names: %s\n", paste(names(nca_res), collapse = ", ")))
        
        # Get the actual data
        if (is.data.frame(nca_res)) {
          data <- nca_res
        } else if (!is.null(nca_res$parameters)) {
          data <- nca_res$parameters
        } else if (!is.null(nca_res$subject_data)) {
          data <- nca_res$subject_data
        } else {
          cat("[DEBUG] No valid data found in NCA results\n")
          return(DT::datatable(data.frame(Message = "No data available")))
        }
        
        # Debug: Log available columns
        cat("\n[DEBUG] Available columns in data:", paste(names(data), collapse=", "), "\n")
        
        # Get selected columns from new UI structure - prefer direct input over reactive values
        input_selected_cols <- c(
          input$subject_info_cols,
          input$primary_pk_cols,
          input$secondary_pk_cols,
          input$lambda_z_cols,
          input$log_pk_cols
        )
        
        reactive_selected_cols <- c(
          column_selection$subject_info,
          column_selection$primary_pk,
          column_selection$secondary_pk,
          column_selection$lambda_z,
          column_selection$log_pk
        )
        
        cat("[DEBUG] Selected columns from input:", paste(input_selected_cols, collapse=", "), "\n")
        cat("[DEBUG] Selected columns from reactive values:", paste(reactive_selected_cols, collapse=", "), "\n")
        
        # Prefer input values over reactive values to handle unchecked states properly
        selected_cols <- input_selected_cols
        
        cat("[DEBUG] Final selected columns:", paste(selected_cols, collapse=", "), "\n")
        
        # If nothing selected, create empty table message
        if (length(selected_cols) == 0) {
          cat("[DEBUG] No columns selected, showing empty state message\n")
          return(DT::datatable(data.frame(Message = "No parameters selected for display. Please select columns above."), 
                              options = list(dom = 't', searching = FALSE, paging = FALSE, info = FALSE)))
        }
        
        # Case-insensitive column matching
        available_cols <- names(data)
        matched_cols <- character(0)
        
        for (col in selected_cols) {
          # First try exact match
          if (col %in% available_cols) {
            matched_cols <- c(matched_cols, col)
          } else {
            # Try case-insensitive match
            case_matches <- available_cols[tolower(available_cols) == tolower(col)]
            if (length(case_matches) > 0) {
              matched_cols <- c(matched_cols, case_matches[1])
            }
          }
        }
        
        cat("[DEBUG] Matched columns:", paste(matched_cols, collapse=", "), "\n")
        
        # If no matches found and we have selections, try to find any available columns as fallback
        if (length(matched_cols) == 0 && length(selected_cols) > 0) {
          # Just use minimal columns
          minimal_cols <- c("subject", "treatment")
          minimal_matches <- minimal_cols[minimal_cols %in% available_cols]
          if (length(minimal_matches) > 0) {
            matched_cols <- minimal_matches
            cat("[DEBUG] No column matches found, using minimal display:", paste(matched_cols, collapse=", "), "\n")
          }
        }
        
        # Final fallback: if still no columns, return empty table message
        if (length(matched_cols) == 0) {
          cat("[DEBUG] No valid columns to display\n")
          return(DT::datatable(data.frame(Message = "No columns selected for display")))
        }
        
        # Select the matched columns
        display_data <- data[, matched_cols, drop = FALSE]
        
        cat(sprintf("[DEBUG] Display data dimensions: %d rows x %d columns\n", nrow(display_data), ncol(display_data)))
        cat(sprintf("[DEBUG] Display data column names: %s\n", paste(names(display_data), collapse = ", ")))
        
        # Comprehensive column display names for all 19 potential columns
        col_display_names <- c(
          "Subject" = "Subject",
          "subject" = "Subject",  # Add lowercase version
          "Treatment" = "Treatment", 
          "treatment" = "Treatment",  # Add lowercase version
          "Period" = "Period",
          "period" = "Period",  # Add lowercase version
          "Sequence" = "Sequence",
          "sequence" = "Sequence",  # Add lowercase version
          "Cmax" = "Cₘₐₓ",
          "Tmax" = "Tₘₐₓ (h)",
          "AUC0t" = "AUC₀₋ₜ",
          "AUC0inf" = "AUC₀₋∞",
          "t_half" = "t½ (h)",
          "lambda_z" = "λz (h⁻¹)",
          "lambda_z_r_squared" = "λz R²",
          "lambda_z_p_value" = "λz P-value",
          "Tlast" = "Tₗₐₛₜ (h)",
          "Clast" = "Cₗₐₛₜ",
          "CL_F" = "CL/F",
          "Vd_F" = "Vd/F", 
          "MRT" = "MRT (h)",
          "AUC_percent_extrap" = "AUC Extrap (%)",
          "dose" = "Dose",
          "analysis_time" = "Analysis Time"
        )
        
        # Apply display names
        display_names <- names(display_data)
        cat(sprintf("[DEBUG] Original display names: %s\n", paste(display_names, collapse = ", ")))
        for (i in seq_along(display_names)) {
          if (display_names[i] %in% names(col_display_names)) {
            display_names[i] <- col_display_names[display_names[i]]
          }
        }
        cat(sprintf("[DEBUG] Final display names: %s\n", paste(display_names, collapse = ", ")))
        
        # Enhanced formatting for numeric columns
        for (col in names(display_data)) {
          if (is.numeric(display_data[[col]])) {
            if (grepl("p.value|p-value", col, ignore.case = TRUE)) {
              # Format p-values with scientific notation if very small
              display_data[[col]] <- ifelse(display_data[[col]] < 0.001, 
                                           formatC(display_data[[col]], format = "e", digits = 2),
                                           round(display_data[[col]], 4))
            } else {
              # Round other numeric values to 4 decimal places
              display_data[[col]] <- round(display_data[[col]], 4)
            }
          }
        }
        
        # Create enhanced DataTable with better functionality
        DT::datatable(
          display_data,
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            scrollY = "500px",
            dom = 'Bfrtip',
            buttons = list(
              'copy',
              list(extend = 'csv', filename = 'subject_pk_data'),
              list(extend = 'excel', filename = 'subject_pk_data'),
              'colvis'  # Add column visibility toggle
            ),
            columnDefs = list(
              list(className = 'dt-center', targets = '_all')
            )
          ),
          rownames = FALSE,
          colnames = display_names,
          escape = FALSE,
          caption = paste("Showing", ncol(display_data), "of", ncol(data), "available columns -", 
                         "Use column selection controls above to customize display")
        ) %>%
          formatStyle(
            columns = names(display_data),
            fontSize = '12px'
          ) %>%
          formatStyle(
            columns = names(display_data)[sapply(display_data, is.numeric)],
            textAlign = 'right'
          )
        
        }, error = function(e) {
          cat(sprintf("[ERROR] in individual_subject_table: %s\n", e$message))
          DT::datatable(data.frame(Error = paste("Error rendering table:", e$message)))
        })
    })

    # Individual subject interpretation - moved to PK Parameters tab
    # Carryover summary output
    output$carryover_summary <- renderUI({
      if (is.null(carryover_results) || is.null(carryover_results())) {
        return(div(
          class = "alert alert-info",
          h5("ℹ️ No Carryover Analysis Performed"),
          p("Carryover detection was not enabled for this analysis.")
        ))
      }
      
      carryover_data <- carryover_results()
      n_flagged <- nrow(carryover_data$flagged_subjects)
      n_total <- nrow(carryover_data$carryover_data)
      threshold_percent <- carryover_data$threshold * 100
      
      # Determine which subjects were actually excluded from analysis
      # Check if subjects with carryover are present in the NCA results
      actually_excluded_subjects <- character(0)
      if (n_flagged > 0 && !is.null(nca_results())) {
        flagged_subject_ids <- carryover_data$flagged_subjects$Subject
        
        # Get subjects that are in NCA results
        nca_res <- nca_results()
        if (is.data.frame(nca_res)) {
          analyzed_subjects <- unique(nca_res$subject)
        } else if (!is.null(nca_res$subject_data)) {
          analyzed_subjects <- unique(nca_res$subject_data$subject)
        } else {
          analyzed_subjects <- character(0)
        }
        
        # Subjects with carryover that are NOT in the analysis results were actually excluded
        actually_excluded_subjects <- flagged_subject_ids[!flagged_subject_ids %in% analyzed_subjects]
      }
      
      if (n_flagged == 0) {
        return(div(
          class = "alert alert-success",
          h5("✅ No Carryover Detected"),
          p(paste0("Analysis of ", n_total, " subjects found no carryover exceeding ", 
                   threshold_percent, "% threshold per ICH M13A guidelines."))
        ))
      } else {
        # Get detailed information about flagged subjects
        flagged_details <- carryover_data$flagged_subjects
        
        # Create detailed subject information
        subject_details <- lapply(1:nrow(flagged_details), function(i) {
          row <- flagged_details[i, ]
          excluded_status <- if (row$Subject %in% actually_excluded_subjects) {
            "EXCLUDED from BE analysis"
          } else {
            "INCLUDED in BE analysis"
          }
          
          div(style = "margin: 8px 0; padding: 10px; background-color: #fff3cd; border-radius: 4px; border-left: 3px solid #ffc107;",
            tags$strong(paste0("Subject ", row$Subject, " (Period ", row$Period, "):")),
            br(),
            paste0("Pre-dose: ", format(row$`Pre-dose Conc`, digits = 3, nsmall = 1), 
                   " | Cmax: ", format(row$Cmax, digits = 3, nsmall = 1),
                   " | ", format(row$`% of Cmax`, digits = 3, nsmall = 1), "% of Cmax"),
            br(),
            tags$span(style = if (row$Subject %in% actually_excluded_subjects) "color: #dc3545; font-weight: bold;" else "color: #28a745; font-weight: bold;",
              excluded_status
            )
          )
        })
        
        return(div(
          class = "alert alert-warning",
          h5("⚠️ Carryover Detected"),
          p(paste0("Found carryover in ", n_flagged, " out of ", n_total, 
                   " subjects (", round(n_flagged/n_total*100, 1), "%)."),
            br(),
            paste0("Threshold: ", threshold_percent, "% of Cmax from same period.")),
          br(),
          tags$strong("Affected Subjects:"),
          div(style = "margin-top: 10px;", subject_details),
          br(),
          if (length(actually_excluded_subjects) > 0) {
            div(style = "margin-top: 10px; padding: 8px; background-color: #f8d7da; border-radius: 4px; color: #721c24;",
              tags$strong("Note: "), 
              paste0(length(actually_excluded_subjects), " subject(s) with carryover were excluded from bioequivalence analysis: ",
                     paste(actually_excluded_subjects, collapse = ", "))
            )
          } else {
            div(style = "margin-top: 10px; padding: 8px; background-color: #d4edda; border-radius: 4px; color: #155724;",
              tags$strong("Note: "), 
              "Subjects with carryover were flagged but included in bioequivalence analysis."
            )
          }
        ))
      }
    })
    
    # =======================================================================
    # BIOEQUIVALENCE CONCLUSIONS OUTPUT  
    # =======================================================================
    
    # BE Conclusions for Overview Tab
    output$be_conclusions <- renderUI({
      req(results_available())
      
      tryCatch({
        be_res <- be_results()
        analysis_cfg <- analysis_config()
        
        # Check if BE conclusions exist
        if (is.null(be_res$be_conclusions) || length(be_res$be_conclusions) == 0) {
          return(div(
            class = "alert alert-warning",
            h5("⚠️ No Bioequivalence Results Available"),
            p("Bioequivalence analysis has not been completed or results are not available.")
          ))
        }
        
        # Get BE limits
        be_lower <- analysis_cfg$be_lower %||% 80
        be_upper <- analysis_cfg$be_upper %||% 125
        
        # Get analysis type and method
        analysis_type <- be_res$analysis_type %||% "ABE"
        analysis_method <- be_res$analysis_method %||% "Average Bioequivalence"
        be_method <- be_res$be_method %||% analysis_method
        
        # Get confidence intervals and conclusions
        ci_results <- be_res$confidence_intervals
        be_conclusions <- be_res$be_conclusions
        
        # Analysis type header
        analysis_header <- tagList(
          div(class = "well", style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
            h4(icon("chart-line"), paste("Analysis Type:", be_method), style = "color: #495057; margin-bottom: 10px;"),
            if (analysis_type %in% c("RSABE", "ABEL")) {
              div(class = "alert alert-info", style = "margin-bottom: 0;",
                icon("info-circle"), " ",
                strong("Note: "), paste(analysis_type, "analysis requested but currently using ABE methodology."),
                br(), "Full", analysis_type, "implementation coming soon."
              )
            } else {
              p("Standard bioequivalence analysis with fixed limits", style = "margin-bottom: 0; color: #6c757d;")
            }
          )
        )
        
        # Filter for ANY PK parameters that have BE results (not just primary)
        # Look for all parameters with both confidence intervals and BE conclusions
        
        primary_ci <- list()
        primary_conclusions <- list()
        
        # Get all parameters that have both CI results and BE conclusions
        all_available_params <- intersect(names(ci_results), names(be_conclusions))
        
        cat(sprintf("[DEBUG] All available parameters with BE results: %s\n", 
                   paste(all_available_params, collapse = ", ")))
        
        # Include all available parameters (not just primary ones)
        for (param in all_available_params) {
          if (!is.null(ci_results[[param]]) && !is.null(be_conclusions[[param]])) {
            primary_ci[[param]] <- ci_results[[param]]
            primary_conclusions[[param]] <- be_conclusions[[param]]
            cat(sprintf("[DEBUG] Including parameter: %s\n", param))
          }
        }
        
        cat(sprintf("[DEBUG] Found %d parameters with BE results: %s\n", 
                   length(primary_ci), paste(names(primary_ci), collapse = ", ")))
        
        if (length(primary_ci) == 0) {
          return(tagList(
            analysis_header,
            div(
              class = "alert alert-info",
              h5("ℹ️ No BE Results Available"),
              p("No parameters with BE results are available for display.")
            )
          ))
        }
        
        # Overall bioequivalence status
        valid_conclusions <- primary_conclusions[!is.na(unlist(primary_conclusions))]
        all_be <- length(valid_conclusions) > 0 && all(unlist(valid_conclusions), na.rm = TRUE)
        overall_status <- if (all_be) "🎉 BIOEQUIVALENT" else "❌ NOT BIOEQUIVALENT"
        status_color <- if (all_be) "#28a745" else "#dc3545"
        status_bg <- if (all_be) "#d4edda" else "#f8d7da"
        
        # Create results table with display names
        results_list <- mapply(function(param, is_be) {
          ci <- primary_ci[[param]]
          
          # DEBUG: Check the BE conclusion values
          cat(sprintf("[DEBUG] Processing %s: is_be = %s (class: %s)\n", param, is_be, class(is_be)))
          
          # Handle NA values in BE conclusions
          be_status <- if (is.na(is_be)) {
            cat(sprintf("[DEBUG] %s is NA - setting to UNKNOWN\n", param))
            "❓ UNKNOWN"
          } else if (is_be) {
            cat(sprintf("[DEBUG] %s is TRUE - setting to PASS\n", param))
            "✅ PASS"
          } else {
            cat(sprintf("[DEBUG] %s is FALSE - setting to FAIL\n", param))
            "❌ FAIL"
          }
          
          # Convert log parameter name to display name
          display_param <- log_param_to_display_name(param)
          
          data.frame(
            Parameter = display_param,  # Use display name instead of log name
            `Point Estimate` = sprintf("%.2f%%", ci$point_estimate),
            `90% CI Lower` = sprintf("%.2f%%", ci$ci_lower),
            `90% CI Upper` = sprintf("%.2f%%", ci$ci_upper),
            `BE Criteria` = sprintf("%.1f%% - %.1f%%", be_lower, be_upper),
            `BE Status` = be_status,
            stringsAsFactors = FALSE
          )
        }, names(primary_ci), primary_conclusions, SIMPLIFY = FALSE)
        
        results_rows <- do.call(rbind, results_list)
        
        return(tagList(
          # Analysis type information
          analysis_header,
          
          # Results table
          h5("📊 Bioequivalence Assessment - All Parameters"),
          p(style = "font-size: 0.9em; color: #6c757d; margin-bottom: 15px;", 
            "Bioequivalence assessment for all analyzed parameters. ",
            "Statistical analysis performed on log-transformed data where appropriate; results displayed with original parameter names for clarity."),
          DT::datatable(
            results_rows,
            options = list(
              dom = 't',
              pageLength = 10,
              columnDefs = list(
                list(className = 'dt-center', targets = 1:5),
                list(
                  targets = 5,
                  createdCell = JS("
                    function(td, cellData, rowData, row, col) {
                      if (cellData.includes('PASS')) {
                        $(td).css('color', '#28a745');
                        $(td).css('font-weight', 'bold');
                      } else {
                        $(td).css('color', '#dc3545');
                        $(td).css('font-weight', 'bold');
                      }
                    }
                  ")
                )
              )
            ),
            rownames = FALSE,
            escape = FALSE
          ) %>% 
            DT::formatStyle(columns = 1:6, fontSize = '14px'),
          
          br(),
          div(
            style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 3px solid #6c757d;",
            tags$small(
              tags$strong("Note: "), 
              "Bioequivalence evaluation performed on log-transformed data (regulatory requirement) but results displayed with original parameter names (Cmax, AUC0-t, AUC0-∞) for clarity. ",
              "Complete statistical analysis including all calculated parameters is available in the 'BE Analysis' tab."
            )
          )
        ))
        
      }, error = function(e) {
        cat(sprintf("[ERROR] Error rendering BE conclusions: %s\n", e$message))
        return(div(
          class = "alert alert-danger",
          h5("❌ Error Loading Bioequivalence Results"),
          p("An error occurred while loading the bioequivalence conclusions."),
          tags$small(paste("Error details:", e$message))
        ))
      })
    })
    
    # =======================================================================
    # ANOVA RESULTS OUTPUTS
    # =======================================================================
    
    # Format simple ANOVA results for display (non-reactive)
    format_simple_anova_results <- function(param_result, param_name) {
      
      cat(sprintf("[DEBUG] Formatting ANOVA results for parameter: %s\n", param_name))
      cat(sprintf("[DEBUG] Available fields: %s\n", paste(names(param_result), collapse = ", ")))
      
      # Check if param_result has the expected structure
      if (is.null(param_result) || length(param_result) == 0) {
        return(div(class = "alert alert-warning",
          h5(icon("exclamation-triangle"), " No Parameter Results"),
          p(paste("No ANOVA results found for parameter:", param_name))
        ))
      }
      
      # Check for errors
      if (!is.null(param_result$error)) {
        return(div(class = "alert alert-danger",
          h5(icon("times-circle"), " ANOVA Error"),
          p(paste("Error in ANOVA analysis for", param_name, ":", param_result$error))
        ))
      }
      
      # Extract key statistics (now stored in the anova results)
      method_name <- if (!is.null(param_result$anova_method)) {
        switch(param_result$anova_method,
          "fixed" = "Fixed Effects Model (lm)",
          "nlme" = "Mixed Effects Model (nlme)",
          "satterthwaite" = "Mixed Effects Model (Satterthwaite)",
          "kenward-roger" = "Mixed Effects Model (Kenward-Roger)",
          param_result$anova_method
        )
      } else "Unknown Method"
      
      # Use pre-calculated values from the anova analysis
      root_mse <- param_result$root_mse
      param_mean <- param_result$param_mean
      cv_percent <- param_result$cv_percent
      
      # Enhanced summary card with all key statistics
      summary_info <- div(class = "card mb-3",
        div(class = "card-body",
          # Method and parameter info
          div(class = "row mb-3",
            div(class = "col-12",
              div(class = "alert alert-info mb-2",
                strong("Analysis Method: "), method_name, br(),
                strong("Selected PK Parameter: "), param_name
              )
            )
          ),
          
          # Key statistics in a structured layout
          div(class = "row",
            # Left column - Model fit statistics
            div(class = "col-md-6",
              h6(icon("chart-line"), " Model Fit Statistics:"),
              tags$table(class = "table table-sm table-borderless",
                tags$tbody(
                  if (!is.null(param_result$r_squared) && !is.na(param_result$r_squared)) {
                    tags$tr(
                      tags$td(strong("R-Square:")),
                      tags$td(sprintf("%.6f", param_result$r_squared))
                    )
                  },
                  if (!is.na(cv_percent)) {
                    tags$tr(
                      tags$td(strong("C.V.:")),
                      tags$td(sprintf("%.4f%%", cv_percent))
                    )
                  },
                  if (!is.na(root_mse)) {
                    tags$tr(
                      tags$td(strong("Root MSE:")),
                      tags$td(sprintf("%.4f", root_mse))
                    )
                  },
                  if (!is.na(param_mean)) {
                    tags$tr(
                      tags$td(strong(paste(param_name, "Mean:"))),
                      tags$td(sprintf("%.4f", param_mean))
                    )
                  }
                )
              )
            ),
            
            # Right column - Additional model information
            div(class = "col-md-6",
              h6(icon("info-circle"), " Additional Information:"),
              tags$table(class = "table table-sm table-borderless",
                tags$tbody(
                  if (!is.null(param_result$n_observations)) {
                    tags$tr(
                      tags$td(strong("Observations:")),
                      tags$td(param_result$n_observations)
                    )
                  },
                  if (!is.null(param_result$residual_df)) {
                    tags$tr(
                      tags$td(strong("Residual DF:")),
                      tags$td(param_result$residual_df)
                    )
                  },
                  if (!is.null(param_result$aic)) {
                    tags$tr(
                      tags$td(strong("AIC:")),
                      tags$td(sprintf("%.2f", param_result$aic))
                    )
                  },
                  if (!is.null(param_result$residual_mse)) {
                    tags$tr(
                      tags$td(strong("Residual MSE:")),
                      tags$td(sprintf("%.6f", param_result$residual_mse))
                    )
                  }
                )
              )
            )
          )
        )
      )
      
      # Create all three ANOVA tables
      anova_tables_div <- div(class = "mt-4",
        
        # Table 1: Comprehensive ANOVA (Model/Error/Corrected Total)
        if (!is.null(param_result$anova_comprehensive)) {
          tryCatch({
            comp_df <- param_result$anova_comprehensive
            div(class = "mb-4",
              h6(icon("table"), " Analysis of Variance (Model Summary):"),
              div(class = "table-responsive",
                tags$table(class = "table table-striped table-hover table-sm",
                  tags$thead(class = "table-primary",
                    tags$tr(
                      tags$th("Source"),
                      tags$th("DF"),
                      tags$th("Sum of Squares"),
                      tags$th("Mean Square"),
                      tags$th("F Value"),
                      tags$th("Pr > F")
                    )
                  ),
                  tags$tbody(
                    lapply(1:nrow(comp_df), function(i) {
                      tags$tr(
                        tags$td(style = "font-weight: bold;", comp_df$Source[i]),
                        tags$td(if (!is.na(comp_df$Df[i])) as.character(comp_df$Df[i]) else ""),
                        tags$td(if (!is.na(comp_df$`Sum Sq`[i])) sprintf("%.4f", comp_df$`Sum Sq`[i]) else ""),
                        tags$td(if (!is.na(comp_df$`Mean Sq`[i])) sprintf("%.6f", comp_df$`Mean Sq`[i]) else ""),
                        tags$td(if (!is.na(comp_df$`F value`[i])) sprintf("%.2f", comp_df$`F value`[i]) else ""),
                        tags$td(if (!is.na(comp_df$`Pr(>F)`[i])) format.pval(comp_df$`Pr(>F)`[i], digits = 4) else "")
                      )
                    })
                  )
                )
              )
            )
          }, error = function(e) {
            div(class = "alert alert-warning", p("Could not display comprehensive ANOVA table: ", e$message))
          })
        },
        
        # Table 2: Type I SS (Sequential)
        if (!is.null(param_result$anova)) {
          tryCatch({
            type1_df <- as.data.frame(param_result$anova)
            div(class = "mb-4",
              h6(icon("list-ol"), " Type I Analysis of Variance (Sequential):"),
              div(class = "table-responsive",
                tags$table(class = "table table-striped table-hover table-sm",
                  tags$thead(class = "table-success",
                    tags$tr(
                      tags$th("Source"),
                      tags$th("DF"),
                      tags$th("Type I SS"),
                      tags$th("Mean Square"),
                      tags$th("F Value"),
                      tags$th("Pr > F")
                    )
                  ),
                  tags$tbody(
                    lapply(1:nrow(type1_df), function(i) {
                      row_data <- type1_df[i, , drop = FALSE]
                      tags$tr(
                        tags$td(style = "font-weight: bold;", rownames(type1_df)[i]),
                        lapply(1:ncol(type1_df), function(j) {
                          val <- row_data[[j]]
                          col_name <- names(type1_df)[j]
                          
                          formatted_val <- if (is.numeric(val) && !is.na(val)) {
                            if (col_name %in% c("Pr(>F)", "Pr..F.", "p-value")) {
                              format.pval(val, digits = 4)
                            } else if (col_name %in% c("Df", "NumDF", "DenDF")) {
                              as.character(round(val))
                            } else if (col_name %in% c("Sum Sq", "Sum.Sq", "Mean Sq", "Mean.Sq")) {
                              sprintf("%.4f", val)
                            } else if (col_name %in% c("F value", "F.value", "F-value")) {
                              sprintf("%.2f", val)
                            } else {
                              sprintf("%.4f", val)
                            }
                          } else {
                            as.character(val)
                          }
                          tags$td(formatted_val)
                        })
                      )
                    })
                  )
                )
              )
            )
          }, error = function(e) {
            div(class = "alert alert-warning", p("Could not display Type I ANOVA table: ", e$message))
          })
        },
        
        # Table 3: Type III SS (Marginal)
        if (!is.null(param_result$type3_ss)) {
          tryCatch({
            type3_df <- as.data.frame(param_result$type3_ss)
            div(class = "mb-4",
              h6(icon("calculator"), " Type III Analysis of Variance (Marginal):"),
              div(class = "table-responsive",
                tags$table(class = "table table-striped table-hover table-sm",
                  tags$thead(class = "table-warning",
                    tags$tr(
                      tags$th("Source"),
                      tags$th("DF"),
                      tags$th("Type III SS"),
                      tags$th("Mean Square"),
                      tags$th("F Value"),
                      tags$th("Pr > F")
                    )
                  ),
                  tags$tbody(
                    lapply(1:nrow(type3_df), function(i) {
                      row_data <- type3_df[i, , drop = FALSE]
                      tags$tr(
                        tags$td(style = "font-weight: bold;", rownames(type3_df)[i]),
                        lapply(1:ncol(type3_df), function(j) {
                          val <- row_data[[j]]
                          col_name <- names(type3_df)[j]
                          
                          formatted_val <- if (is.numeric(val) && !is.na(val)) {
                            if (col_name %in% c("Pr(>F)", "Pr..F.", "p-value")) {
                              format.pval(val, digits = 4)
                            } else if (col_name %in% c("Df", "NumDF", "DenDF")) {
                              as.character(round(val))
                            } else if (col_name %in% c("Sum Sq", "Sum.Sq", "Mean Sq", "Mean.Sq")) {
                              sprintf("%.4f", val)
                            } else if (col_name %in% c("F value", "F.value", "F-value")) {
                              sprintf("%.2f", val)
                            } else {
                              sprintf("%.4f", val)
                            }
                          } else {
                            as.character(val)
                          }
                          tags$td(formatted_val)
                        })
                      )
                    })
                  )
                )
              )
            )
          }, error = function(e) {
            div(class = "alert alert-warning", p("Could not display Type III ANOVA table: ", e$message))
          })
        } else {
          div(class = "alert alert-info",
            h6(icon("info-circle"), " Type III Analysis"),
            p("Type III analysis not available for this model configuration.")
          )
        }
      )
      
      # Combine all components
      return(div(
        summary_info,
        anova_tables_div
      ))
    }
    
    # ANOVA parameter selection reactive
    selected_anova_param <- reactive({
      req(input$anova_parameter_select)
      param <- input$anova_parameter_select
      cat("[DEBUG] Selected ANOVA parameter:", param, "\n")
      param
    })
    
    # Main ANOVA display output
    output$anova_display <- renderUI({
      req(results_available())
      
      tryCatch({
        be_res <- be_results()
        
        # Debug: Check the structure of BE results
        cat("[DEBUG] ANOVA Display - BE results structure:\n")
        cat("[DEBUG] BE results names:", paste(names(be_res), collapse=", "), "\n")
        
        # Check if we have BE analysis ANOVA results (from the actual BE analysis)
        if (!is.null(be_res$anova_results_be)) {
          cat("[DEBUG] Found BE analysis ANOVA results\n")
        }
        if (!is.null(be_res$anova_results)) {
          cat("[DEBUG] ANOVA results structure:", paste(names(be_res$anova_results), collapse=", "), "\n")
        }
        
        # Check if ANOVA results exist (they should be attached to BE results)
        if (is.null(be_res$anova_results) || length(be_res$anova_results) == 0) {
          cat("[DEBUG] ANOVA Display - No anova_results found in BE results\n")
          return(div(class = "alert alert-warning",
            h5(icon("exclamation-triangle"), " No ANOVA Results Available"),
            p("ANOVA analysis was not performed or failed to complete. Please check the analysis configuration.")
          ))
        }
        
        # Get the actual simple ANOVA results (from perform_simple_anova function)
        anova_data <- be_res$anova_results$anova_results  # This contains the actual ANOVA results
        
        if (is.null(anova_data) || length(anova_data) == 0) {
          cat("[DEBUG] ANOVA Display - No actual ANOVA results found\n")
          return(div(class = "alert alert-warning",
            h5(icon("exclamation-triangle"), " No ANOVA Results Available"),
            p("ANOVA analysis results are empty. Please check the analysis configuration.")
          ))
        }
        
        param <- selected_anova_param()
        
        # Debug: Check what values we're getting
        cat(sprintf("[DEBUG] UI - Selected param: %s\n", param))
        cat(sprintf("[DEBUG] UI - Available anova parameters: %s\n", paste(names(anova_data), collapse = ", ")))
        
        # Check if parameter exists in ANOVA results
        if (!param %in% names(anova_data)) {
          return(div(class = "alert alert-info",
            h5(icon("info-circle"), " No ANOVA Results"),
            p(paste("ANOVA results not available for parameter:", param))
          ))
        }
        
        # Get the specific parameter results
        param_result <- anova_data[[param]]
        
        # Get ANOVA method from BE analysis results if available
        if (is.null(param_result$anova_method) && !is.null(be_res$anova_method)) {
          param_result$anova_method <- be_res$anova_method
          param_result$method_description <- be_res$anova_method_description
        }
        
        # Check if there's an error
        if ("error" %in% names(param_result)) {
          return(div(class = "alert alert-danger",
            h5(icon("times-circle"), " ANOVA Error"),
            p(paste("Error in ANOVA analysis for", param, ":", param_result$error))
          ))
        }
        
        # Format simple ANOVA results for display
        return(format_simple_anova_results(param_result, param))
        
      }, error = function(e) {
        div(class = "alert alert-danger",
          h5("Error Loading ANOVA Results"),
          p(paste("Error:", e$message))
        )
      })
    })    # =======================================================================
    # COMPLETE BE ANALYSIS OUTPUTS (New Tab)
    # =======================================================================
    
    # Complete BE Analysis output
    output$complete_be_analysis <- renderUI({
      req(results_available())
      
      tryCatch({
        be_res <- be_results()
        
        if (is.null(be_res$confidence_intervals) || length(be_res$confidence_intervals) == 0) {
          return(div(
            class = "alert alert-warning",
            h5("⚠️ Complete BE Analysis Not Available"),
            p("Bioequivalence confidence intervals are not available.")
          ))
        }
        
        ci_results <- be_res$confidence_intervals
        be_conclusions <- be_res$be_conclusions
        
        # Filter out NA or invalid parameters
        valid_ci_params <- names(ci_results)[!is.na(names(ci_results)) & names(ci_results) != ""]
        valid_be_params <- names(be_conclusions)[!is.na(names(be_conclusions)) & names(be_conclusions) != ""]
        
        # Find common parameters that exist in both CI results and BE conclusions
        common_params <- intersect(valid_ci_params, valid_be_params)
        
        # Use ALL parameters that have BE results (not just log-transformed)
        # since the analysis should already be ensuring only appropriate parameters are analyzed
        valid_be_params_final <- common_params
        
        if (length(valid_be_params_final) == 0) {
          return(div(
            class = "alert alert-warning",
            h5("⚠️ No Valid BE Results"),
            p("No valid bioequivalence results available for display.")
          ))
        }
        
        cat(sprintf("[DEBUG] Complete BE analysis showing %d parameters: %s\n", 
                   length(valid_be_params_final), paste(valid_be_params_final, collapse = ", ")))
        
        # Create comprehensive results table for all valid BE parameters
        results_list <- lapply(valid_be_params_final, function(param) {
          ci <- ci_results[[param]]
          is_be <- be_conclusions[[param]]
          
          # Validate CI data exists
          if (is.null(ci) || is.na(ci$point_estimate)) {
            # Convert log parameter name to display name
            display_param <- log_param_to_display_name(param)
            return(data.frame(
              Parameter = display_param,
              `Ratio (%)` = "N/A",
              `90% CI (%)` = "N/A",
              `BE Status` = "⚠️ UNKNOWN",
              stringsAsFactors = FALSE
            ))
          }
          
          # Extract geometric scale values
          point_est_geom <- ci$point_estimate
          ci_lower_geom <- ci$ci_lower
          ci_upper_geom <- ci$ci_upper
          
          # Convert log parameter name to display name
          display_param <- log_param_to_display_name(param)
          
          data.frame(
            Parameter = display_param,  # Use display name instead of log name
            `Ratio (%)` = sprintf("%.2f%%", point_est_geom),
            `90% CI (%)` = sprintf("[%.2f%%, %.2f%%]", ci_lower_geom, ci_upper_geom),
            `BE Status` = ifelse(is.na(is_be), "⚠️ UNKNOWN", 
                                ifelse(is_be, "✅ BIOEQUIVALENT", "❌ NOT BIOEQUIVALENT")),
            stringsAsFactors = FALSE
          )
        })
        
        # Filter out NULL results and combine
        valid_results <- results_list[!sapply(results_list, is.null)]
        
        if (length(valid_results) == 0) {
          return(div(
            class = "alert alert-warning",
            h5("⚠️ No Valid Results to Display"),
            p("No bioequivalence results could be processed for display.")
          ))
        }
        
        all_results <- do.call(rbind, valid_results)
        
        return(div(
          h5("📋 Bioequivalence Assessment Results"),
          p(style = "font-size: 0.9em; color: #6c757d;", 
            "Bioequivalence assessment results with 90% confidence intervals for all evaluated parameters. ",
            "Analysis performed on log-transformed data; parameter names shown without 'ln' prefix for clarity. ",
            "Bioequivalence is determined by whether the confidence interval falls within 80.00%-125.00%."),
          DT::datatable(
            all_results,
            options = list(
              pageLength = 15,
              scrollX = TRUE,
              columnDefs = list(
                list(className = 'dt-center', targets = 1:3),
                list(
                  targets = 3, # BE Status column (now column 3)
                  createdCell = JS("
                    function(td, cellData, rowData, row, col) {
                      if (cellData.includes('BIOEQUIVALENT')) {
                        $(td).css('color', '#28a745');
                        $(td).css('font-weight', 'bold');
                      } else if (cellData.includes('NOT BIOEQUIVALENT')) {
                        $(td).css('color', '#dc3545');
                        $(td).css('font-weight', 'bold');
                      } else {
                        $(td).css('color', '#ffc107');
                        $(td).css('font-weight', 'bold');
                      }
                    }
                  ")
                )
              )
            ),
            rownames = FALSE,
            escape = FALSE
          ) %>% 
            DT::formatStyle(columns = 1:8, fontSize = '13px')
        ))
        
      }, error = function(e) {
        return(div(
          class = "alert alert-danger",
          h5("❌ Error Loading Complete BE Analysis"),
          p("An error occurred while loading the complete bioequivalence analysis."),
          tags$small(paste("Error details:", e$message))
        ))
      })
    })
    
    # BE Statistical Summary output
    output$be_statistical_summary <- renderUI({
      req(results_available())
      
      tryCatch({
        be_res <- be_results()
        analysis_cfg <- analysis_config()
        
        # Get basic statistics
        n_subjects <- be_res$n_subjects %||% "Unknown"
        study_design <- be_res$design %||% analysis_cfg$detected_design %||% "Unknown"
        confidence_level <- analysis_cfg$confidence_level %||% 90
        be_lower <- analysis_cfg$be_lower %||% 80
        be_upper <- analysis_cfg$be_upper %||% 125
        
        # Count all available BE parameters (not just specific log-transformed ones)
        total_params <- length(be_res$confidence_intervals)
        total_be_params <- sum(unlist(be_res$be_conclusions), na.rm = TRUE)
        
        # Identify the parameter types we have
        param_names <- names(be_res$confidence_intervals)
        has_log_params <- any(startsWith(param_names, "ln"))
        
        # Create a list of parameter display names
        param_display_names <- sapply(param_names, log_param_to_display_name)
        unique_display_names <- unique(param_display_names)
        
        return(div(
          tags$ul(
            tags$li(tags$strong("Study Design: "), study_design),
            tags$li(tags$strong("Number of Subjects: "), n_subjects),
            tags$li(tags$strong("Confidence Level: "), paste0(confidence_level, "%")),
            tags$li(tags$strong("BE Limits: "), paste0(be_lower, "% - ", be_upper, "%")),
            tags$li(tags$strong("Parameters Analyzed: "), total_params, " (", paste(unique_display_names, collapse = ", "), ")"),
            tags$li(tags$strong("Bioequivalent Parameters: "), paste0(total_be_params, " of ", total_params)),
            tags$li(tags$strong("Analysis Model: "), analysis_cfg$anova_model %||% "Fixed Effects"),
            tags$li(tags$strong("Analysis Scale: "), if(has_log_params) "Log-transformed (multiplicative model)" else "Original scale"),
            tags$li(tags$strong("Results Presentation: "), "Geometric scale (%) with original parameter names")
          ),
          br(),
          h6("📊 Scale Information:"),
          tags$ul(
            tags$li(tags$strong("Statistical Analysis: "), "Performed on log-transformed data (e.g., ln(Cmax), ln(AUC0-t))"),
            tags$li(tags$strong("Results Display: "), "Shown with original parameter names (e.g., Cmax, AUC0-t) for clarity"),
            tags$li(tags$strong("Geometric Scale: "), "Results expressed as percentage ratios (Test/Reference × 100%)"),
            tags$li(tags$strong("BE Assessment: "), "Based on log-scale analysis per regulatory guidelines")
          )
        ))
        
      }, error = function(e) {
        return(div(
          class = "alert alert-warning",
          p("Statistical summary not available.")
        ))
      })
    })
    
    # BE Regulatory Summary output
    output$be_regulatory_summary <- renderUI({
      req(results_available())
      
      tryCatch({
        be_res <- be_results()
        # Use all available BE conclusions for regulatory assessment
        all_conclusions <- be_res$be_conclusions
        
        # Remove any NULL or NA conclusions
        valid_conclusions <- all_conclusions[!is.na(unlist(all_conclusions))]
        
        # Regulatory status based on all available results
        overall_be <- length(valid_conclusions) > 0 && all(unlist(valid_conclusions))
        overall_status <- if (overall_be) "✅ BIOEQUIVALENT" else "❌ NOT BIOEQUIVALENT"
        status_color <- if (overall_be) "#28a745" else "#dc3545"
        
        # Check if log-transformed parameters were used
        param_names <- names(be_res$confidence_intervals)
        has_log_params <- any(startsWith(param_names, "ln"))
        
        # Regulatory guidelines compliance
        regulatory_notes <- list(
          "✓ 90% Confidence Interval used (regulatory standard)",
          if(has_log_params) "✓ Log-transformed analysis performed" else "✓ Analysis performed on available parameters",
          "✓ BE limits: 80.00% - 125.00% (FDA/EMA standard)",
          if (overall_be) "✓ Meets regulatory bioequivalence criteria" else "✗ Does not meet regulatory bioequivalence criteria"
        )
        
        return(div(
          div(
            style = paste0("padding: 10px; margin-bottom: 15px; border-radius: 5px; background-color: ", 
                          if (overall_be) "#d4edda" else "#f8d7da", ";"),
            h6(style = paste0("color: ", status_color, "; margin: 0;"), overall_status)
          ),
          tags$ul(
            lapply(regulatory_notes, function(note) {
              color <- if (grepl("✓", note)) "#28a745" else "#dc3545"
              tags$li(style = paste0("color: ", color), note)
            })
          ),
          br(),
          div(
            style = "padding: 8px; background-color: #e9ecef; border-radius: 4px;",
            tags$small(
              tags$strong("Regulatory Compliance: "),
              "Analysis follows FDA, EMA, and ICH M13A guidelines for bioequivalence assessment."
            )
          )
        ))
        
      }, error = function(e) {
        return(div(
          class = "alert alert-warning",
          p("Regulatory summary not available.")
        ))
      })
    })
    
    # Refresh button observer  
    observeEvent(input$refresh_anova, {
      # Force refresh of reactive values to re-trigger ANOVA display
      # Just trigger invalidation without arguments
      be_results()
      nca_results()
    })
    
    # PK Comparison placeholder
    output$pk_comparison_content <- renderUI({
      if (!results_available()) {
        return(div(
          class = "alert alert-info",
          "No analysis results available. Please complete the analysis setup and run the analysis first."
        ))
      }
      
      # Placeholder content for now
      div(
        h5("PK Comparison functionality coming soon..."),
        p("This tab will display:"),
        tags$ul(
          tags$li("Individual subject Test and Reference values"),
          tags$li("T/R ratios for each subject"),
          tags$li("Summary statistics (mean, SD, CV%)"),
          tags$li("Geometric means and ratios")
        )
      )
    })
    
    # PK Comparison - Parameter Selection UI
    output$pk_comparison_parameter_select_ui <- renderUI({
      if (!results_available()) {
        return(NULL)
      }
      
      nca_res <- nca_results()
      if (is.null(nca_res) || is.null(nca_res$subject_data)) {
        return(div(class = "alert alert-warning", "No NCA results available"))
      }
      
      # Get available parameters from subject data
      data <- nca_res$subject_data
      
      # Define parameter categories
      primary_params <- c("Cmax", "AUC0t", "AUC0inf", "pAUC")
      secondary_params <- c("Tmax", "t_half", "Tlast", "Clast", "CL_F", "Vd_F", "MRT", "AUC_percent_extrap")
      log_params <- names(data)[grepl("^(log|ln)", names(data))]
      
      # Filter for available parameters
      available_primary <- intersect(primary_params, names(data))
      available_secondary <- intersect(secondary_params, names(data))
      available_log <- log_params
      
      # Create grouped choices
      choices <- list()
      
      if (length(available_primary) > 0) {
        choices[["Primary PK Parameters"]] <- available_primary
      }
      
      if (length(available_log) > 0) {
        choices[["Log-Transformed Parameters"]] <- available_log
      }
      
      if (length(available_secondary) > 0) {
        choices[["Secondary PK Parameters"]] <- available_secondary
      }
      
      # If no parameters available
      if (length(unlist(choices)) == 0) {
        return(div(class = "alert alert-warning", "No PK parameters available for comparison"))
      }
      
      # Create the select input
      selectInput(
        inputId = session$ns("pk_comparison_parameter"),
        label = "Select Parameter:",
        choices = choices,
        selected = unlist(choices)[1],
        width = "100%"
      )
    })
    
    # Reactive for selected comparison parameter
    selected_comparison_param <- reactive({
      input$pk_comparison_parameter
    })
    
    # PK Comparison Display
    output$pk_comparison_display <- renderUI({
      param <- selected_comparison_param()
      
      if (is.null(param) || param == "") {
        return(div(
          class = "alert alert-info",
          "Please select a PK parameter to view comparison results."
        ))
      }
      
      nca_res <- nca_results()
      if (is.null(nca_res)) {
        return(div(class = "alert alert-warning", "No NCA results available"))
      }
      
      # Calculate comparison statistics
      comparison_results <- calculate_pk_comparison(nca_res, param)
      
      if (is.null(comparison_results)) {
        return(div(
          class = "alert alert-warning",
          paste("No data available for parameter:", param)
        ))
      }
      
      # Check if there's an error in the results
      if (!is.null(comparison_results$error)) {
        return(div(
          class = "alert alert-danger",
          h5("Data Structure Error"),
          p(comparison_results$error),
          if (!is.null(nca_res$subject_data)) {
            tagList(
              p("Available columns in NCA subject data:"),
              p(paste(names(nca_res$subject_data), collapse = ", "))
            )
          }
        ))
      }
      
      # Format the display
      tagList(
        h4(icon("chart-line"), paste("PK Comparison Analysis:", param)),
        
        # Individual subject data table
        div(
          class = "pk-comparison-section",
          h5("Individual Subject Data", 
             if (comparison_results$unit != "") paste0(" (", comparison_results$unit, ")") else ""),
          DT::dataTableOutput(session$ns("pk_comparison_individual_table"))
        ),
        
        br(),
        
        # Summary statistics
        div(
          class = "pk-comparison-section",
          h5("Summary Statistics"),
          
          # Least squares means
          div(
            class = "stats-subsection",
            h6("Least Squares Means:"),
            DT::dataTableOutput(session$ns("pk_comparison_lsmeans_table"))
          ),
          
          # Arithmetic statistics
          br(),
          div(
            class = "stats-subsection",
            h6("Arithmetic Statistics:"),
            DT::dataTableOutput(session$ns("pk_comparison_arithmetic_table"))
          ),
          
          # Geometric statistics (if applicable)
          if (!is.null(comparison_results$geometric_stats)) {
            tagList(
              br(),
              div(
                class = "stats-subsection",
                h6("Geometric Statistics:"),
                DT::dataTableOutput(session$ns("pk_comparison_geometric_table"))
              )
            )
          }
        ),
        
        # Sample size info
        br(),
        div(
          class = "alert alert-info",
          paste("Analysis based on", comparison_results$n_subjects, "subjects")
        )
      )
    })
    
    # Render individual subject data table
    output$pk_comparison_individual_table <- DT::renderDataTable({
      param <- selected_comparison_param()
      if (is.null(param)) return(NULL)
      
      nca_res <- nca_results()
      comparison_results <- calculate_pk_comparison(nca_res, param)
      
      if (is.null(comparison_results)) return(NULL)
      
      # Format the individual data
      formatted_data <- comparison_results$individual_data
      formatted_data$Test <- round(formatted_data$Test, 3)
      formatted_data$Reference <- round(formatted_data$Reference, 3)
      formatted_data$Ratio <- round(formatted_data$Ratio, 3)
      
      DT::datatable(
        formatted_data,
        options = list(
          pageLength = 15,
          dom = 'tp',  # 't' = table, 'p' = pagination
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          ),
          scrollX = TRUE,
          lengthMenu = c(15, 25, 50, 100),
          pagingType = "simple_numbers"
        ),
        rownames = FALSE
      )
    })
    
    # Render least squares means table
    output$pk_comparison_lsmeans_table <- DT::renderDataTable({
      param <- selected_comparison_param()
      if (is.null(param)) return(NULL)
      
      nca_res <- nca_results()
      comparison_results <- calculate_pk_comparison(nca_res, param)
      
      if (is.null(comparison_results)) return(NULL)
      
      # Format the LSMeans
      formatted_lsmeans <- comparison_results$lsmeans
      formatted_lsmeans$Test <- round(formatted_lsmeans$Test, 3)
      formatted_lsmeans$Reference <- round(formatted_lsmeans$Reference, 3)
      formatted_lsmeans$Ratio <- round(formatted_lsmeans$Ratio, 3)
      
      DT::datatable(
        formatted_lsmeans,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = 1:3)
          )
        ),
        rownames = FALSE
      )
    })
    
    # Render arithmetic statistics table
    output$pk_comparison_arithmetic_table <- DT::renderDataTable({
      param <- selected_comparison_param()
      if (is.null(param)) return(NULL)
      
      nca_res <- nca_results()
      comparison_results <- calculate_pk_comparison(nca_res, param)
      
      if (is.null(comparison_results)) return(NULL)
      
      # Format the arithmetic statistics
      formatted_stats <- comparison_results$arithmetic_stats
      formatted_stats$Test <- round(formatted_stats$Test, 3)
      formatted_stats$Reference <- round(formatted_stats$Reference, 3)
      formatted_stats$Ratio <- round(formatted_stats$Ratio, 3)
      
      DT::datatable(
        formatted_stats,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = 1:3)
          )
        ),
        rownames = FALSE
      )
    })
    
    # Render geometric statistics table (if applicable)
    output$pk_comparison_geometric_table <- DT::renderDataTable({
      param <- selected_comparison_param()
      if (is.null(param)) return(NULL)
      
      nca_res <- nca_results()
      comparison_results <- calculate_pk_comparison(nca_res, param)
      
      if (is.null(comparison_results) || is.null(comparison_results$geometric_stats)) return(NULL)
      
      # Format the geometric statistics
      formatted_stats <- comparison_results$geometric_stats
      formatted_stats$Test <- round(formatted_stats$Test, 3)
      formatted_stats$Reference <- round(formatted_stats$Reference, 3)
      formatted_stats$Ratio <- round(formatted_stats$Ratio, 3)
      
      DT::datatable(
        formatted_stats,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = 1:3)
          )
        ),
        rownames = FALSE
      )
    })
    
    # Render least squares means table
    # Refresh button handler
    observeEvent(input$refresh_pk_comparison, {
      # Force reactivity
      nca_results()
    })
  })
}
