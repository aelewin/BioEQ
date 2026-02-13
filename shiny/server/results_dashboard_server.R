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
    "lnpAUC" = "pAUC",
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
  
  # Extract subject data - already mapped with standard column names
  subject_data <- nca_data$subject_data
  
  # Check if parameter exists
  if (!param_name %in% names(subject_data)) {
    return(NULL)
  }
  
  # Expected columns from mapped data: Subject, Treatment, Period, Sequence
  required_cols <- c("Subject", "Treatment", param_name)
  if (!all(required_cols %in% names(subject_data))) {
    available <- names(subject_data)
    return(list(
      error = paste0("Required columns not found. Available columns: ", 
                    paste(available, collapse = ", "))
    ))
  }
  
  # Detect if this is a replicate design by checking periods per subject
  periods_per_subject <- subject_data %>%
    group_by(Subject) %>%
    summarise(n_periods = n_distinct(Period), .groups = "drop") %>%
    pull(n_periods) %>%
    max()
  
  is_replicate <- periods_per_subject > 2
  
  # Extract parameter data
  param_data <- subject_data %>%
    select(Subject, Treatment, Period, all_of(param_name)) %>%
    rename(Value = !!param_name) %>%
    filter(!is.na(Value))
  
  if (nrow(param_data) == 0) {
    return(list(error = "No data available for this parameter"))
  }
  
  if (is_replicate) {
    # REPLICATE DESIGN: Calculate T1/R1, T2/R2, and Tavg/Ravg
    
    # Separate Test and Reference data
    test_data <- param_data %>% filter(Treatment == "T")
    ref_data <- param_data %>% filter(Treatment == "R")
    
    # For each subject, get all Test and Reference values by period
    test_by_subject <- test_data %>%
      group_by(Subject) %>%
      arrange(Period) %>%
      summarise(
        T1 = if(n() >= 1) Value[1] else NA_real_,
        T2 = if(n() >= 2) Value[2] else NA_real_,
        T_mean = mean(Value, na.rm = TRUE),
        T_n = n(),
        .groups = "drop"
      )
    
    ref_by_subject <- ref_data %>%
      group_by(Subject) %>%
      arrange(Period) %>%
      summarise(
        R1 = if(n() >= 1) Value[1] else NA_real_,
        R2 = if(n() >= 2) Value[2] else NA_real_,
        R_mean = mean(Value, na.rm = TRUE),
        R_n = n(),
        .groups = "drop"
      )
    
    # Merge Test and Reference data
    comparison_data <- full_join(test_by_subject, ref_by_subject, by = "Subject") %>%
      mutate(
        # Individual period ratios
        Ratio_T1_R1 = T1 / R1,
        Ratio_T2_R2 = T2 / R2,
        # Average ratio
        Ratio_Tavg_Ravg = T_mean / R_mean,
        # Flag missing data
        Missing_Test = replace_na(T_n < periods_per_subject / 2, FALSE),
        Missing_Ref = replace_na(R_n < periods_per_subject / 2, FALSE),
        Subject = as.character(Subject)
      ) %>%
      arrange(as.numeric(Subject))
    
    # Calculate summary statistics for each ratio type
    calc_stats <- function(values, label) {
      valid_values <- values[!is.na(values) & !is.infinite(values)]
      if (length(valid_values) == 0) {
        return(data.frame(
          Ratio_Type = label,
          N = 0,
          Geometric_Mean = NA,
          CV_percent = NA,
          Min = NA,
          Median = NA,
          Max = NA
        ))
      }
      
      data.frame(
        Ratio_Type = label,
        N = length(valid_values),
        Geometric_Mean = exp(mean(log(valid_values))),
        CV_percent = 100 * sqrt(exp(var(log(valid_values))) - 1),
        Min = min(valid_values),
        Median = median(valid_values),
        Max = max(valid_values)
      )
    }
    
    summary_stats <- bind_rows(
      calc_stats(comparison_data$Ratio_T1_R1, "T1/R1"),
      calc_stats(comparison_data$Ratio_T2_R2, "T2/R2"),
      calc_stats(comparison_data$Ratio_Tavg_Ravg, "T_avg/R_avg")
    )
    
  } else {
    # 2x2x2 CROSSOVER DESIGN: Simple T/R ratio per subject
    
    test_data <- param_data %>% filter(Treatment == "T")
    ref_data <- param_data %>% filter(Treatment == "R")
    
    comparison_data <- full_join(
      test_data %>% select(Subject, Value) %>% rename(Test = Value),
      ref_data %>% select(Subject, Value) %>% rename(Reference = Value),
      by = "Subject"
    ) %>%
      mutate(
        Ratio = Test / Reference,
        Subject = as.character(Subject)
      ) %>%
      arrange(as.numeric(Subject))
    
    # Calculate summary statistics
    valid_ratios <- comparison_data$Ratio[!is.na(comparison_data$Ratio) & !is.infinite(comparison_data$Ratio)]
    
    if (length(valid_ratios) > 0) {
      summary_stats <- data.frame(
        Statistic = c("N", "Geometric Mean Ratio", "CV%", "Min", "Median", "Max"),
        Value = c(
          length(valid_ratios),
          exp(mean(log(valid_ratios))),
          100 * sqrt(exp(var(log(valid_ratios))) - 1),
          min(valid_ratios),
          median(valid_ratios),
          max(valid_ratios)
        )
      )
    } else {
      summary_stats <- data.frame(
        Statistic = "No valid ratios",
        Value = NA
      )
    }
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
  
  return(list(
    individual_data = comparison_data,
    summary_stats = summary_stats,
    is_replicate = is_replicate,
    unit = unit,
    parameter = param_name
  ))
}

# Format replicateBE ANOVA results for display
format_replicatebe_anova_results <- function(param_result, param_name, be_res) {
  
  # Extract replicateBE output
  rbe_output <- param_result$replicatebe_output
  
  # Create summary card
  summary_card <- div(class = "card mb-3",
    div(class = "card-header bg-info text-white",
      h5(class = "card-title mb-0", 
        icon("flask"), 
        sprintf(" replicateBE ANOVA Results for %s", param_name)
      )
    ),
    div(class = "card-body",
      div(class = "row",
        div(class = "col-md-4",
          h6("Study Design:"),
          tags$ul(
            tags$li(sprintf("Design: %s", rbe_output$Design)),
            tags$li(sprintf("Method: %s", rbe_output$Method)),
            tags$li(sprintf("Total Subjects: %d", rbe_output$n)),
            tags$li(sprintf("Test Subjects: %d", rbe_output$nTT)),
            tags$li(sprintf("Reference Subjects: %d", rbe_output$nRR)),
            tags$li(sprintf("Degrees of Freedom: %d", rbe_output$DF))
          )
        ),
        div(class = "col-md-4",
          h6("Variability:"),
          tags$ul(
            tags$li(sprintf("CV Within-Subject (Test): %.2f%%", rbe_output$`CVwT(%)`)),
            tags$li(sprintf("CV Within-Subject (Reference): %.2f%%", rbe_output$`CVwR(%)`)),
            tags$li(sprintf("Within-Subject SD (Test): %.4f", rbe_output$swT)),
            tags$li(sprintf("Within-Subject SD (Reference): %.4f", rbe_output$swR)),
            tags$li(sprintf("SD Ratio (T/R): %.4f", rbe_output$sw.ratio))
          )
        ),
        div(class = "col-md-4",
          h6("BE Assessment:"),
          tags$ul(
            tags$li(sprintf("Point Estimate: %.2f%%", rbe_output$`PE(%)`)),
            tags$li(sprintf("CI Lower: %.2f%%", rbe_output$`CL.lo(%)`)),
            tags$li(sprintf("CI Upper: %.2f%%", rbe_output$`CL.hi(%)`)),
            tags$li(sprintf("Scaled Lower Limit: %.2f%%", rbe_output$`L(%)`)),
            tags$li(sprintf("Scaled Upper Limit: %.2f%%", rbe_output$`U(%)`)),
            tags$li(
              class = if(rbe_output$BE == "pass") "text-success font-weight-bold" else "text-danger font-weight-bold",
              sprintf("BE Result: %s", toupper(rbe_output$BE))
            )
          )
        )
      )
    )
  )
  
  # Create informational note
  info_note <- div(class = "alert alert-info",
    h6(icon("info-circle"), " About replicateBE Analysis"),
    p(paste0(
      "This analysis was performed using the replicateBE package (EMA ABEL). ",
      "The package implements ", 
      if(rbe_output$Method == "A") "Method A (Linear Model/ANOVA)" else sprintf("Method B (Mixed Model with %s DF approximation)", be_res$df_approximation %||% "default"),
      ". Results include within-subject variability estimates and scaled bioequivalence limits."
    ))
  )
  
  # Combine all elements
  return(tagList(
    summary_card,
    info_note
  ))
}

# Format RSABE ANOVA results for display
format_rsabe_anova_results <- function(param_result, param_name, be_res) {
  
  rsabe_details <- be_res$rsabe_details[[param_name]]
  isc <- if (!is.null(rsabe_details)) rsabe_details$isc_result else NULL
  rsabe_test <- if (!is.null(rsabe_details)) rsabe_details$rsabe_test else NULL
  is_hv <- if (!is.null(rsabe_details)) rsabe_details$is_hv else FALSE
  rsabe_method <- be_res$rsabe_method %||% "fda_linearized"
  
  # Summary card
  summary_card <- div(class = "card mb-3",
    div(class = "card-header bg-primary text-white",
      h5(class = "card-title mb-0",
        icon("flask"),
        sprintf(" RSABE ANOVA Results for %s", param_name)
      )
    ),
    div(class = "card-body",
      div(class = "row",
        # Model summary
        div(class = "col-md-4",
          h6("Model Summary:"),
          tags$ul(
            tags$li(sprintf("ANOVA Method: %s", 
                    if (param_result$anova_method == "fixed") "Fixed Effects" else "Mixed Effects (nlme)")),
            tags$li(sprintf("Observations: %d", param_result$n_observations)),
            tags$li(sprintf("Residual MSE: %.6f", param_result$residual_mse)),
            tags$li(sprintf("Residual DF: %d", param_result$residual_df)),
            tags$li(sprintf("Treatment Diff (d\u0302): %.6f", param_result$treatment_coef)),
            tags$li(sprintf("SE(d\u0302): %.6f", param_result$treatment_se))
          )
        ),
        # Variance components (ISC)
        div(class = "col-md-4",
          h6("Variance Components (ISC):"),
          tags$ul(
            tags$li(sprintf("s\u00B2_wR: %.6f (df = %d)", param_result$s2_wR, param_result$df_wR)),
            tags$li(sprintf("CV_wR: %.2f%%", param_result$cv_wr_percent)),
            if (!is.na(param_result$s2_wT %||% NA)) {
              tagList(
                tags$li(sprintf("s\u00B2_wT: %.6f (df = %d)", param_result$s2_wT, param_result$df_wT)),
                tags$li(sprintf("CV_wT: %.2f%%", param_result$cv_wt_percent %||% NA))
              )
            } else {
              tags$li("s\u00B2_wT: Not estimable (partial replicate)")
            },
            tags$li(style = if (is_hv) "color: #e65100; font-weight: bold;" else "color: #2e7d32; font-weight: bold;",
              if (is_hv) "HIGH VARIABILITY \u2014 Scaled limits" else "Low variability \u2014 Fixed ABE limits"
            )
          )
        ),
        # RSABE criterion / decision
        div(class = "col-md-4",
          h6("RSABE Assessment:"),
          if (is_hv && !is.null(rsabe_test)) {
            if (rsabe_method == "fda_linearized") {
              tags$ul(
                tags$li(sprintf("\u03B8\u00B2\u209B: %.6f", rsabe_test$theta_sq)),
                tags$li(sprintf("\u03B7\u0302 (point est): %.6f", rsabe_test$eta_hat)),
                tags$li(sprintf("UCB (95%%): %.6f", rsabe_test$ucb)),
                tags$li(sprintf("Scaled limits: [%.2f%%, %.2f%%]", rsabe_test$scaled_lower, rsabe_test$scaled_upper)),
                tags$li(
                  class = if (rsabe_test$rsabe_pass) "text-success font-weight-bold" else "text-danger font-weight-bold",
                  sprintf("Scaling criterion: %s", ifelse(rsabe_test$rsabe_pass, "PASS (UCB \u2264 0)", "FAIL (UCB > 0)"))
                ),
                tags$li(
                  class = if (rsabe_details$pe_constraint_pass) "text-success font-weight-bold" else "text-danger font-weight-bold",
                  sprintf("PE constraint (80-125%%): %s", ifelse(rsabe_details$pe_constraint_pass, "PASS", "FAIL"))
                )
              )
            } else {
              # ncTOST
              tags$ul(
                tags$li(sprintf("t\u2081: %.4f (p = %.4f)", rsabe_test$t1, rsabe_test$p1)),
                tags$li(sprintf("t\u2082: %.4f (p = %.4f)", rsabe_test$t2, rsabe_test$p2)),
                tags$li(sprintf("max(p): %.4f", rsabe_test$overall_p)),
                tags$li(sprintf("Scaled limits: [%.2f%%, %.2f%%]", rsabe_test$scaled_lower_pct, rsabe_test$scaled_upper_pct)),
                tags$li(
                  class = if (rsabe_test$rsabe_pass) "text-success font-weight-bold" else "text-danger font-weight-bold",
                  sprintf("ncTOST: %s", ifelse(rsabe_test$rsabe_pass, "PASS", "FAIL"))
                ),
                tags$li(
                  class = if (rsabe_details$pe_constraint_pass) "text-success font-weight-bold" else "text-danger font-weight-bold",
                  sprintf("PE constraint (80-125%%): %s", ifelse(rsabe_details$pe_constraint_pass, "PASS", "FAIL"))
                )
              )
            }
          } else {
            tags$ul(
              tags$li("Using standard ABE (80-125%)"),
              tags$li(sprintf("CV_wR (%.1f%%) \u2264 switching CV (~25.4%%)", param_result$cv_wr_percent)),
              tags$li("RSABE scaling not required")
            )
          }
        )
      )
    )
  )
  
  # Method note
  method_display <- if (rsabe_method == "nctost") "Non-Central TOST (ncTOST)" else "FDA Linearized Scaled Criterion (Howe UCB)"
  info_note <- div(class = "alert alert-info",
    h6(icon("info-circle"), " About RSABE Analysis"),
    p(sprintf("Analysis performed using %s with Intra-Subject Contrasts (ISC) for variance estimation. ", method_display),
      "ISC avoids convergence issues with mixed models by computing within-subject differences directly. ",
      sprintf("Regulatory constants: \u03B8\u209B = ln(1.25) \u2248 0.2231, switching s\u00B2_w0 = 0.0625 (CV \u2248 25.4%%). "),
      "FDA requires point estimate within 80-125% regardless of scaling decision."
    )
  )
  
  return(tagList(summary_card, info_note))
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

results_dashboard_server <- function(id, be_results, nca_results, analysis_config, carryover_results = NULL, missing_data_log = NULL) {
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
    
    observeEvent(input$pk_cols, {
      column_selection$pk <- input$pk_cols
    })
    
    observeEvent(input$pk_cols_2, {
      column_selection$pk_2 <- input$pk_cols_2
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
          return()
        }
        
        available_cols <- names(data)
        
        # Subject info - use the actual checkbox values
        subject_choices <- c("Subject", "Treatment", "Period", "Sequence", "dose")
        updateCheckboxGroupInput(session, "subject_info_cols", selected = subject_choices)
        
        # PK parameters
        pk_choices <- c("Cmax", "AUC0t", "AUC0inf", "Tmax", "t_half", "Tlast", "Clast", "AUC_percent_extrap")
        if ("pAUC" %in% available_cols) {
          pk_choices <- c(pk_choices, "pAUC")
        }
        n <- length(pk_choices)
        mid <- ceiling(n / 2)
        col1_vals <- pk_choices[1:mid]
        col2_vals <- pk_choices[(mid + 1):n]
        updateCheckboxGroupInput(session, "pk_cols", selected = intersect(col1_vals, available_cols))
        updateCheckboxGroupInput(session, "pk_cols_2", selected = intersect(col2_vals, available_cols))
        
        # Lambda z statistics
        lambda_choices <- c("lambda_z", "lambda_z_r_squared", "lambda_z_p_value", "lambda_z_points", "lambda_z_method")
        available_lambda <- intersect(lambda_choices, available_cols)
        updateCheckboxGroupInput(session, "lambda_z_cols", selected = available_lambda)
        
        # Log-transformed parameters
        log_choices <- c("lnCmax", "lnAUC0t", "lnAUC0inf")
        if ("lnpAUC" %in% available_cols) log_choices <- c(log_choices, "lnpAUC")
        updateCheckboxGroupInput(session, "log_pk_cols", selected = log_choices)
        
      }, error = function(e) {
        # Silent error handling
      })
    })
    
    observeEvent(input$deselect_all_cols, {
      updateCheckboxGroupInput(session, "subject_info_cols", selected = character(0))
      updateCheckboxGroupInput(session, "pk_cols", selected = character(0))
      updateCheckboxGroupInput(session, "pk_cols_2", selected = character(0))
      updateCheckboxGroupInput(session, "lambda_z_cols", selected = character(0))
      updateCheckboxGroupInput(session, "log_pk_cols", selected = character(0))
    })
    
    observeEvent(input$reset_default_cols, {
      updateCheckboxGroupInput(session, "subject_info_cols", selected = c("Subject", "Treatment", "Period", "Sequence"))
      updateCheckboxGroupInput(session, "pk_cols", selected = c("Cmax", "AUC0t"))
      updateCheckboxGroupInput(session, "pk_cols_2", selected = character(0))
      updateCheckboxGroupInput(session, "lambda_z_cols", selected = character(0))
      updateCheckboxGroupInput(session, "log_pk_cols", selected = c("lnCmax", "lnAUC0t", "lnAUC0inf"))
    })
    
    # Dynamic UI for PK Parameters - two-column layout
    output$pk_cols_ui <- renderUI({
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
          stop("No valid NCA data found")
        }
        
        # Build combined PK parameter choices
        pk_choices <- list(
          "Cmax" = "Cmax",
          "AUC0-t" = "AUC0t",
          "AUC0-inf" = "AUC0inf",
          "Tmax" = "Tmax",
          "Half-life" = "t_half",
          "Tlast" = "Tlast",
          "Clast" = "Clast",
          "AUC % Extrapolated" = "AUC_percent_extrap"
        )
        
        # Optional parameters - only show if calculated
        if ("pAUC" %in% names(data)) {
          pk_choices[["pAUC"]] <- "pAUC"
        }
        
        # Split into two columns for compact layout
        n <- length(pk_choices)
        mid <- ceiling(n / 2)
        col1_choices <- pk_choices[1:mid]
        col2_choices <- pk_choices[(mid + 1):n]
        
        fluidRow(
          column(6,
            checkboxGroupInput(
              session$ns("pk_cols"),
              label = NULL,
              choices = col1_choices,
              selected = character(0)
            )
          ),
          column(6,
            checkboxGroupInput(
              session$ns("pk_cols_2"),
              label = NULL,
              choices = col2_choices,
              selected = character(0)
            )
          )
        )
      }, error = function(e) {
        checkboxGroupInput(
          session$ns("pk_cols"),
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
    
    # Dynamic UI for Log-Transformed Parameters - only show calculated parameters
    output$log_pk_cols_ui <- renderUI({
      
      if (!results_available()) {
        return(div("No results available yet"))
      }
      
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
          return(div("No log-transformed parameters available"))
        }
        
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
        if ("lnpAUC" %in% names(data)) {
          log_choices[["ln(pAUC)"]] <- "lnpAUC"
        }
        
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
        log_params <- c("lnCmax", "lnAUC0t", "lnAUC0inf", "lnTmax", "lnpAUC")
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
              param
            )
            anova_choices[[display_name]] <- param
          }
        }
        
        # Secondary parameters
        secondary_params <- c("Tmax", "pAUC")
        available_secondary <- intersect(secondary_params, available_params)
        if (length(available_secondary) > 0) {
          anova_choices[["--- Secondary Parameters ---"]] <- ""
          for (param in available_secondary) {
            anova_choices[[param]] <- param
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
        
        # Get the actual data
        if (is.data.frame(nca_res)) {
          data <- nca_res
        } else if (!is.null(nca_res$parameters)) {
          data <- nca_res$parameters
        } else if (!is.null(nca_res$subject_data)) {
          data <- nca_res$subject_data
        } else {
          return(DT::datatable(data.frame(Message = "No data available")))
        }
        
        # Get selected columns from new UI structure - prefer direct input over reactive values
        input_selected_cols <- c(
          input$subject_info_cols,
          input$pk_cols,
          input$pk_cols_2,
          input$lambda_z_cols,
          input$log_pk_cols
        )
        
        reactive_selected_cols <- c(
          column_selection$subject_info,
          column_selection$pk,
          column_selection$lambda_z,
          column_selection$log_pk
        )
        
        # Prefer input values over reactive values to handle unchecked states properly
        selected_cols <- input_selected_cols
        
        # If nothing selected, create empty table message
        if (length(selected_cols) == 0) {
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
        
        # If no matches found and we have selections, try to find any available columns as fallback
        if (length(matched_cols) == 0 && length(selected_cols) > 0) {
          # Just use minimal columns
          minimal_cols <- c("subject", "treatment")
          minimal_matches <- minimal_cols[minimal_cols %in% available_cols]
          if (length(minimal_matches) > 0) {
            matched_cols <- minimal_matches
          }
        }
        
        # Final fallback: if still no columns, return empty table message
        if (length(matched_cols) == 0) {
          return(DT::datatable(data.frame(Message = "No columns selected for display")))
        }
        
        # Select the matched columns
        display_data <- data[, matched_cols, drop = FALSE]
        
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
          "AUC_percent_extrap" = "AUC % Extrapolated",
          "pAUC" = "pAUC",
          "lnCmax" = "ln(Cₘₐₓ)",
          "lnAUC0t" = "ln(AUC₀₋ₜ)",
          "lnAUC0inf" = "ln(AUC₀₋∞)",
          "lnpAUC" = "ln(pAUC)",
          "lambda_z_points" = "λz Points",
          "lambda_z_method" = "λz Method",
          "dose" = "Dose",
          "analysis_time" = "Analysis Time"
        )
        
        # Apply display names
        display_names <- names(display_data)
        for (i in seq_along(display_names)) {
          if (display_names[i] %in% names(col_display_names)) {
            display_names[i] <- col_display_names[display_names[i]]
          }
        }
        
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
        # Sort by Subject numerically if present
        if ("Subject" %in% names(display_data)) {
          display_data <- display_data[order(as.numeric(as.character(display_data$Subject))), ]
        }
        
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
    
    # ── Missing Data Summary output ──
    output$missing_data_summary <- renderUI({
      log_data <- NULL
      if (!is.null(missing_data_log)) {
        tryCatch({
          log_data <- missing_data_log()
        }, error = function(e) NULL)
      }
      
      # Get the method from analysis config
      config <- NULL
      tryCatch({
        config <- analysis_config()
      }, error = function(e) NULL)
      method_name <- function(m) {
        switch(m, "interpolate" = "Linear interpolation", "locf" = "LOCF", "complete" = "Exclude point", m)
      }
      middle_m <- config$missing_data_middle %||% "complete"
      terminal_m <- config$missing_data_terminal %||% "complete"
      method_label <- sprintf("Middle: %s | Terminal: %s", method_name(middle_m), method_name(terminal_m))
      
      if (is.null(log_data) || nrow(log_data) == 0) {
        return(div(
          class = "alert alert-success",
          style = "margin-bottom: 0;",
          icon("check-circle"),
          tags$strong(" No Missing Data"),
          p("All concentration values were present. No imputation was required.",
            style = "margin-bottom: 0; margin-top: 5px;")
        ))
      }
      
      # Separate BLQ actions from imputation actions
      blq_log <- log_data[log_data$Method == "blq_to_zero", , drop = FALSE]
      impute_log <- log_data[log_data$Method != "blq_to_zero", , drop = FALSE]
      
      # Build the summary UI
      summary_elements <- list()
      
      # Method used
      summary_elements[[length(summary_elements) + 1]] <- div(
        style = "margin-bottom: 12px;",
        tags$strong("Method: ", style = "color: #2d3748;"),
        tags$span(method_label, style = "color: #4a5568;")
      )
      
      # BLQ summary
      if (nrow(blq_log) > 0) {
        summary_elements[[length(summary_elements) + 1]] <- div(
          class = "alert alert-info",
          style = "padding: 10px; margin-bottom: 10px;",
          icon("flask"),
          tags$strong(sprintf(" %d BLQ value(s) set to 0", nrow(blq_log))),
          p(sprintf("Subjects: %s", paste(unique(blq_log$Subject), collapse = ", ")),
            style = "margin-bottom: 0; margin-top: 5px; font-size: 13px;")
        )
      }
      
      # Imputation summary
      if (nrow(impute_log) > 0) {
        n_imputed <- sum(impute_log$Method %in% c("interpolation", "locf", "locf_fallback"))
        n_removed <- sum(impute_log$Method == "removed")
        n_unable <- sum(impute_log$Method == "unable")
        n_middle <- sum(impute_log$Position == "middle")
        n_terminal <- sum(impute_log$Position == "terminal")
        
        # Summary counts
        summary_text <- c()
        if (n_imputed > 0) summary_text <- c(summary_text, sprintf("%d imputed", n_imputed))
        if (n_removed > 0) summary_text <- c(summary_text, sprintf("%d removed", n_removed))
        if (n_unable > 0) summary_text <- c(summary_text, sprintf("%d unable to impute", n_unable))
        
        position_text <- c()
        if (n_middle > 0) position_text <- c(position_text, sprintf("%d middle", n_middle))
        if (n_terminal > 0) position_text <- c(position_text, sprintf("%d terminal", n_terminal))
        
        summary_elements[[length(summary_elements) + 1]] <- div(
          class = "alert alert-warning",
          style = "padding: 10px; margin-bottom: 10px;",
          icon("exclamation-triangle"),
          tags$strong(sprintf(" %d missing concentration(s) handled", nrow(impute_log))),
          p(paste0("Actions: ", paste(summary_text, collapse = ", ")),
            style = "margin-bottom: 2px; margin-top: 5px; font-size: 13px;"),
          if (length(position_text) > 0) {
            p(paste0("Positions: ", paste(position_text, collapse = ", ")),
              style = "margin-bottom: 0; font-size: 13px;")
          }
        )
        
        # Detailed table of all imputed/handled points
        display_log <- impute_log[, c("Subject", "Treatment", "Period", "Time", "Position", "Method", "Imputed"), drop = FALSE]
        display_log$Imputed <- ifelse(is.na(display_log$Imputed), "\u2014", sprintf("%.4f", display_log$Imputed))
        display_log$Position <- ifelse(display_log$Position == "middle", "Middle", 
                                ifelse(display_log$Position == "terminal", "Terminal", display_log$Position))
        display_log$Method <- ifelse(display_log$Method == "interpolation", "Interpolation",
                              ifelse(display_log$Method == "locf", "LOCF",
                              ifelse(display_log$Method == "locf_fallback", "LOCF (fallback)",
                              ifelse(display_log$Method == "removed", "Removed",
                              ifelse(display_log$Method == "unable", "Unable", display_log$Method)))))
        names(display_log) <- c("Subject", "Treatment", "Period", "Time", "Position", "Method", "Imputed Value")
        
        summary_elements[[length(summary_elements) + 1]] <- div(
          style = "margin-top: 10px;",
          tags$details(
            tags$summary(
              style = "cursor: pointer; color: #3498db; font-weight: 600;",
              icon("table"), " View Details"
            ),
            div(
              style = "margin-top: 8px; max-height: 300px; overflow-y: auto;",
              tags$table(
                class = "table table-striped table-sm",
                style = "font-size: 13px;",
                tags$thead(
                  tags$tr(
                    lapply(names(display_log), function(col) tags$th(col, style = "padding: 6px 8px;"))
                  )
                ),
                tags$tbody(
                  lapply(seq_len(nrow(display_log)), function(i) {
                    tags$tr(
                      lapply(display_log[i, ], function(val) tags$td(as.character(val), style = "padding: 4px 8px;"))
                    )
                  })
                )
              )
            )
          )
        )
      }
      
      do.call(div, summary_elements)
    })
    
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
          analyzed_subjects <- unique(nca_res$Subject)
        } else if (!is.null(nca_res$subject_data)) {
          analyzed_subjects <- unique(nca_res$subject_data$Subject)
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
        
        # Get BE limits (default fallback)
        be_lower <- analysis_cfg$be_lower %||% 80
        be_upper <- analysis_cfg$be_upper %||% 125
        
        # Compute dynamic CI label from alpha
        alpha_level <- analysis_cfg$alpha_level %||% 0.05
        ci_pct <- round((1 - alpha_level * 2) * 100)
        ci_label <- paste0(ci_pct, "% CI")
        
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
            if (analysis_type == "RSABE") {
              # RSABE analysis details
              rsabe_method_label <- be_res$rsabe_method %||% "fda_linearized"
              method_display <- if (rsabe_method_label == "nctost") "Non-Central TOST (ncTOST)" else "FDA Linearized Scaled Criterion (Howe UCB)"
              
              div(class = "alert alert-info", style = "margin-bottom: 0;",
                icon("flask"), " ",
                strong("RSABE Method: "), method_display,
                br(),
                tags$small(
                  "Scaling constant \u03B8\u209B = ln(1.25) \u2248 0.2231 | ",
                  "Switching CV: ~25.4% | ",
                  "Point estimate constraint: 80\u2013125% | ",
                  "Variance estimation: Intra-Subject Contrasts (ISC)"
                )
              )
            } else if (analysis_type == "ABEL") {
              # ABEL is implemented - show info about regulator
              abel_reg <- be_res$regulator %||% "EMA"
              reg_label <- if (abel_reg == "HC") "Health Canada" else "EMA"
              div(class = "alert alert-success", style = "margin-bottom: 0;",
                icon("check-circle"), " ",
                strong(sprintf("ABEL Analysis (%s): ", reg_label)),
                "Average Bioequivalence with Expanding Limits using replicateBE package.",
                br(), 
                if (abel_reg == "HC") {
                  "Scaled limits applied to Cmax and AUC0-t; AUC0-∞ uses fixed 80-125% limits."
                } else {
                  "Scaled limits applied to Cmax only; AUC parameters use fixed 80-125% limits."
                }
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
        
        # Include all available parameters (not just primary ones)
        for (param in all_available_params) {
          if (!is.null(ci_results[[param]]) && !is.null(be_conclusions[[param]])) {
            primary_ci[[param]] <- ci_results[[param]]
            primary_conclusions[[param]] <- be_conclusions[[param]]
          }
        }
        
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
        
        # Create results table with display names and per-parameter limits
        results_list <- mapply(function(param, is_be) {
          ci <- primary_ci[[param]]
          
          # Handle NA values in BE conclusions
          be_status <- if (is.na(is_be)) {
            "❓ UNKNOWN"
          } else if (is_be) {
            "✅ PASS"
          } else {
            "❌ FAIL"
          }
          
          # Convert log parameter name to display name
          display_param <- log_param_to_display_name(param)
          
          # Per-parameter limits: use limits_used if available, else fall back to global
          param_lower <- be_lower
          param_upper <- be_upper
          if (!is.null(ci$limits_used)) {
            param_lower <- ci$limits_used$lower %||% be_lower
            param_upper <- ci$limits_used$upper %||% be_upper
          } else if (!is.null(ci$scaled_lower_limit) && !is.na(ci$scaled_lower_limit)) {
            param_lower <- ci$scaled_lower_limit
            param_upper <- ci$scaled_upper_limit
          }
          
          # Determine limit type label
          limit_type <- ""
          if (!is.null(ci$limits_used$type) && ci$limits_used$type == "scaled") {
            limit_type <- " (scaled)"
          }
          
          data.frame(
            Parameter = display_param,
            `Point Estimate` = sprintf("%.2f%%", ci$point_estimate),
            `CI Lower` = sprintf("%.2f%%", ci$ci_lower),
            `CI Upper` = sprintf("%.2f%%", ci$ci_upper),
            `BE Criteria` = sprintf("%.2f%% - %.2f%%%s", param_lower, param_upper, limit_type),
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
            colnames = c("Parameter", "Point Estimate", paste(ci_label, "Lower"), paste(ci_label, "Upper"), "BE Criteria", "BE Status"),
            escape = FALSE
          ) %>% 
            DT::formatStyle(columns = 1:6, fontSize = '14px'),
          
          # AUC0-t / AUC0-inf coverage ratio (Test, Reference, Overall)
          {
            auc_coverage_div <- NULL
            tryCatch({
              nca_res <- nca_results()
              sd <- if (is.data.frame(nca_res)) nca_res else nca_res$subject_data
              if (!is.null(sd) && "AUC0t" %in% names(sd) && "AUC0inf" %in% names(sd)) {
                evaluable <- !is.na(sd$AUC0t) & !is.na(sd$AUC0inf) & sd$AUC0inf > 0
                if (sum(evaluable) > 0) {
                  sd_eval <- sd[evaluable, ]
                  overall_pct <- mean(sd_eval$AUC0t / sd_eval$AUC0inf * 100)
                  
                  # Detect treatment labels
                  trt_col <- sd_eval$Treatment
                  test_mask <- trt_col %in% c("T", "Test")
                  ref_mask <- trt_col %in% c("R", "Reference")
                  
                  test_pct <- if (sum(test_mask) > 0) mean(sd_eval$AUC0t[test_mask] / sd_eval$AUC0inf[test_mask] * 100) else NA
                  ref_pct <- if (sum(ref_mask) > 0) mean(sd_eval$AUC0t[ref_mask] / sd_eval$AUC0inf[ref_mask] * 100) else NA
                  
                  all_pass <- all(c(test_pct, ref_pct, overall_pct) >= 80, na.rm = TRUE)
                  border_color <- if (all_pass) "#28a745" else "#ffc107"
                  
                  # Build value spans
                  fmt_val <- function(label, val) {
                    if (is.na(val)) return(NULL)
                    color <- if (val >= 80) "#28a745" else "#dc3545"
                    tags$span(
                      tags$strong(paste0(label, ": ")),
                      tags$span(style = paste0("color: ", color, "; font-weight: 600;"), sprintf("%.1f%%", val))
                    )
                  }
                  
                  value_spans <- list(fmt_val("Test", test_pct), fmt_val("Reference", ref_pct), fmt_val("Overall", overall_pct))
                  value_spans <- Filter(Negate(is.null), value_spans)
                  # Interleave with separators
                  display_items <- list()
                  for (j in seq_along(value_spans)) {
                    display_items[[length(display_items) + 1]] <- value_spans[[j]]
                    if (j < length(value_spans)) {
                      display_items[[length(display_items) + 1]] <- tags$span(style = "color: #adb5bd; margin: 0 10px;", "|")
                    }
                  }
                  
                  guidance_note <- if (!all_pass) {
                    tags$div(style = "margin-top: 6px;",
                      tags$small(style = "color: #856404;",
                        icon("exclamation-triangle"),
                        " Regulatory guidance recommends AUC0-t should cover \u2265 80% of AUC0-\u221e."
                      )
                    )
                  }
                  
                  auc_coverage_div <- div(
                    style = paste0("padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 3px solid ", border_color, "; margin-top: 5px;"),
                    div(style = "display: flex; align-items: center; flex-wrap: wrap; gap: 5px;",
                      tags$span(style = "font-weight: 700; color: #495057; margin-right: 8px;", "AUC0-t / AUC0-\u221e Coverage:"),
                      display_items
                    ),
                    guidance_note
                  )
                }
              }
            }, error = function(e) {
              cat(sprintf("[DEBUG] AUC coverage calculation skipped: %s\n", e$message))
            })
            if (!is.null(auc_coverage_div)) tagList(br(), auc_coverage_div) else NULL
          },
          
          br(),
          div(
            style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 3px solid #6c757d;",
            tags$small(
              tags$strong("Note: "), 
              "Bioequivalence evaluation performed on log-transformed data (regulatory requirement) but results displayed with original parameter names (Cmax, AUC0-t, AUC0-\u221e) for clarity. ",
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
      param
    })
    
    # Main ANOVA display output
    output$anova_display <- renderUI({
      req(results_available())
      
      tryCatch({
        be_res <- be_results()
        
        # Check if ANOVA results exist (they should be attached to BE results)
        if (is.null(be_res$anova_results) || length(be_res$anova_results) == 0) {
          return(div(class = "alert alert-warning",
            h5(icon("exclamation-triangle"), " No ANOVA Results Available"),
            p("ANOVA analysis was not performed or failed to complete. Please check the analysis configuration.")
          ))
        }
        
        # Get the actual simple ANOVA results (from perform_simple_anova function)
        anova_data <- be_res$anova_results$anova_results  # This contains the actual ANOVA results
        
        if (is.null(anova_data) || length(anova_data) == 0) {
          return(div(class = "alert alert-warning",
            h5(icon("exclamation-triangle"), " No ANOVA Results Available"),
            p("ANOVA analysis results are empty. Please check the analysis configuration.")
          ))
        }
        
        param <- selected_anova_param()
        
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
        
        # Check if this is replicateBE result (has replicatebe_output field)
        if (!is.null(param_result$replicatebe_output)) {
          # Format replicateBE ANOVA results
          return(format_replicatebe_anova_results(param_result, param, be_res))
        } else if (!is.null(param_result$s2_wR)) {
          # RSABE result — has ISC variance components
          return(format_rsabe_anova_results(param_result, param, be_res))
        } else {
          # Format simple ANOVA results for display
          return(format_simple_anova_results(param_result, param))
        }
        
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
              `CI (%)` = "N/A",
              `BE Criteria` = "N/A",
              `BE Status` = "⚠️ UNKNOWN",
              stringsAsFactors = FALSE
            ))
          }
          
          # Extract geometric scale values
          point_est_geom <- ci$point_estimate
          ci_lower_geom <- ci$ci_lower
          ci_upper_geom <- ci$ci_upper
          
          # Per-parameter limits
          param_lower <- 80
          param_upper <- 125
          if (!is.null(ci$limits_used)) {
            param_lower <- ci$limits_used$lower %||% 80
            param_upper <- ci$limits_used$upper %||% 125
          }
          
          # Convert log parameter name to display name
          display_param <- log_param_to_display_name(param)
          
          data.frame(
            Parameter = display_param,
            `Ratio (%)` = sprintf("%.2f%%", point_est_geom),
            `CI (%)` = sprintf("[%.2f%%, %.2f%%]", ci_lower_geom, ci_upper_geom),
            `BE Criteria` = sprintf("%.2f%%-%.2f%%", param_lower, param_upper),
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
        
        # Get dynamic CI label
        analysis_cfg2 <- analysis_config()
        alpha_level2 <- analysis_cfg2$alpha_level %||% 0.05
        ci_pct2 <- round((1 - alpha_level2 * 2) * 100)
        ci_label2 <- paste0(ci_pct2, "% CI")
        
        # Define clean column names for display
        clean_column_names <- c("Parameter", "Ratio (%)", paste0(ci_label2, " (%)"), "BE Criteria", "BE Status")
        
        return(div(
          h5("📋 Bioequivalence Assessment Results"),
          p(style = "font-size: 0.9em; color: #6c757d;", 
            sprintf("Bioequivalence assessment results with %s confidence intervals for all evaluated parameters. ", ci_label2),
            "Analysis performed on log-transformed data; parameter names shown without 'ln' prefix for clarity. ",
            "Bioequivalence is determined by whether the confidence interval falls within the acceptance limits."),
          DT::datatable(
            all_results,
            options = list(
              pageLength = 15,
              scrollX = TRUE,
              columnDefs = list(
                list(className = 'dt-center', targets = 1:4),
                list(
                  targets = 4, # BE Status column (now column 4 with BE Criteria at 3)
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
            colnames = clean_column_names,
            escape = FALSE
          ) %>% 
            DT::formatStyle(columns = 1:4, fontSize = '13px')
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
        
        # Get dynamic labels for regulatory notes
        analysis_cfg3 <- analysis_config()
        alpha_level3 <- analysis_cfg3$alpha_level %||% 0.05
        ci_pct3 <- round((1 - alpha_level3 * 2) * 100)
        be_lower3 <- analysis_cfg3$be_lower %||% 80
        be_upper3 <- analysis_cfg3$be_upper %||% 125
        
        # Regulatory guidelines compliance
        regulatory_notes <- list(
          sprintf("✓ %d%% Confidence Interval used", ci_pct3),
          if(has_log_params) "✓ Log-transformed analysis performed" else "✓ Analysis performed on available parameters",
          sprintf("✓ Default BE limits: %.2f%% - %.2f%%", be_lower3, be_upper3),
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
      
      # Define parameter categories (aligned with Subject Data tab)
      primary_params <- c("Cmax", "AUC0t")
      secondary_params <- c("AUC0inf", "pAUC", "t_half", "AUC_percent_extrap")
      log_params <- c("lnCmax", "lnAUC0t", "lnAUC0inf")
      
      # Filter for available parameters
      available_primary <- intersect(primary_params, names(data))
      available_secondary <- intersect(secondary_params, names(data))
      available_log <- intersect(log_params, names(data))
      
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
        label = NULL,  # No label since we show it inline in UI
        choices = choices,
        selected = unlist(choices)[1],
        width = "100%"
      )
    })
    
    # Reactive for selected comparison parameter
    selected_comparison_param <- reactive({
      input$pk_comparison_parameter
    })
    
    # PK Comparison Display - Left Panel (Individual Subject Table)
    output$pk_comparison_table_display <- renderUI({
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
      
      # Format the left panel display
      tagList(
        div(
          class = "panel panel-default",
          div(class = "panel-heading",
            h4(class = "panel-title", 
               icon("chart-line"), 
               paste("PK Comparison Analysis:", param))
          ),
          div(class = "panel-body",
            # Individual subject data table
            div(
              class = "pk-comparison-section",
              h5("Individual Subject Data", 
                 if (comparison_results$unit != "") paste0(" (", comparison_results$unit, ")") else ""),
              DT::dataTableOutput(session$ns("pk_comparison_individual_table"))
            )
          )
        )
      )
    })
    
    # PK Comparison Display - Right Panel (Summary Statistics)
    output$pk_comparison_stats_display <- renderUI({
      param <- selected_comparison_param()
      
      if (is.null(param) || param == "") {
        return(div(
          class = "alert alert-light",
          style = "text-align: center; color: #6c757d;",
          icon("info-circle"),
          br(), br(),
          "Summary statistics will appear here once a parameter is selected."
        ))
      }
      
      nca_res <- nca_results()
      if (is.null(nca_res)) {
        return(div(class = "alert alert-warning", "No NCA results available"))
      }
      
      # Calculate comparison statistics
      comparison_results <- calculate_pk_comparison(nca_res, param)
      
      if (is.null(comparison_results) || !is.null(comparison_results$error)) {
        return(div(
          class = "alert alert-light",
          style = "text-align: center; color: #6c757d;",
          "Summary statistics not available for this parameter."
        ))
      }
      
      # Format the right panel display
      tagList(
        div(
          class = "panel panel-default",
          div(class = "panel-heading",
            h4(class = "panel-title", 
               icon("calculator"), 
               "Summary Statistics")
          ),
          div(class = "panel-body",
            # Sample size info at the top
            div(
              class = "alert alert-info",
              style = "margin-bottom: 20px;",
              icon("users"),
              paste(" Analysis based on", nrow(comparison_results$individual_data), "subjects",
                   if(comparison_results$is_replicate) " (Replicate Design)" else " (2x2x2 Crossover)")
            ),
            
            # Single consolidated statistics table (removed redundant h6 label)
            div(
              class = "stats-subsection",
              DT::dataTableOutput(session$ns("pk_comparison_consolidated_table"))
            )
          )
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
      
      # Format the individual data based on design type
      formatted_data <- comparison_results$individual_data
      
      if (comparison_results$is_replicate) {
        # REPLICATE DESIGN: Show T1, R1, T2, R2, averages, and all ratios
        display_data <- formatted_data %>%
          select(Subject, T1, R1, T2, R2, T_mean, R_mean, Ratio_T1_R1, Ratio_T2_R2, Ratio_Tavg_Ravg) %>%
          mutate(across(where(is.numeric), ~round(., 3)))
        
        # Add missing data indicators
        display_data <- display_data %>%
          mutate(
            Notes = case_when(
              is.na(T1) & is.na(T2) ~ "Missing all T",
              is.na(R1) & is.na(R2) ~ "Missing all R",
              is.na(T1) | is.na(T2) ~ "Missing T period",
              is.na(R1) | is.na(R2) ~ "Missing R period",
              TRUE ~ ""
            )
          )
        
        col_names <- c("Subject", "T1", "R1", "T2", "R2", "T avg", "R avg", 
                      "T1/R1", "T2/R2", "Tavg/Ravg", "Notes")
        
      } else {
        # 2x2x2 CROSSOVER: Simple T, R, and Ratio
        display_data <- formatted_data %>%
          select(Subject, Test, Reference, Ratio) %>%
          mutate(across(where(is.numeric), ~round(., 3)))
        
        # Add missing data indicators
        display_data <- display_data %>%
          mutate(
            Notes = case_when(
              is.na(Test) ~ "Missing Test",
              is.na(Reference) ~ "Missing Ref",
              TRUE ~ ""
            )
          )
        
        col_names <- c("Subject", "Test", "Reference", "T/R Ratio", "Notes")
      }
      
      DT::datatable(
        display_data,
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
        rownames = FALSE,
        colnames = col_names
      )
    })
    
    # Render consolidated statistics table
    output$pk_comparison_consolidated_table <- DT::renderDataTable({
      param <- selected_comparison_param()
      if (is.null(param)) return(NULL)
      
      nca_res <- nca_results()
      comparison_results <- calculate_pk_comparison(nca_res, param)
      
      if (is.null(comparison_results)) return(NULL)
      
      # Format summary stats based on design type
      summary_stats <- comparison_results$summary_stats
      
      if (comparison_results$is_replicate) {
        # REPLICATE DESIGN: Show stats for each ratio type
        display_data <- summary_stats %>%
          mutate(across(where(is.numeric), ~round(., 4)))
        
        col_names <- c("Ratio Type", "N", "Geometric Mean", "CV%", "Min", "Median", "Max")
        
      } else {
        # 2x2x2 CROSSOVER: Standard summary
        display_data <- summary_stats %>%
          mutate(across(where(is.numeric), ~round(., 4)))
        
        col_names <- c("Statistic", "Value")
      }
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 10,
          dom = 't',  # Just table, no pagination needed for summary
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          ),
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = col_names
      ) %>%
        DT::formatStyle(
          columns = colnames(display_data),
          backgroundColor = '#f9f9f9',
          fontWeight = 'bold'
        )
    })
    
    # Refresh button handler
    observeEvent(input$refresh_pk_comparison, {
      # Force reactivity
      nca_results()
    })
  })
}
