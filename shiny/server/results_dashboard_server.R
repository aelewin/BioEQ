# Results Dashboard Server Module
# Comprehensive results processing and rendering

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
#' @return List with individual_data, means_table, is_replicate, unit, parameter
calculate_pk_comparison <- function(nca_data, param_name) {
  if (is.null(nca_data) || is.null(nca_data$subject_data)) {
    return(NULL)
  }

  subject_data <- nca_data$subject_data

  if (!param_name %in% names(subject_data)) {
    return(NULL)
  }

  required_cols <- c("Subject", "Treatment", param_name)
  if (!all(required_cols %in% names(subject_data))) {
    return(list(
      error = paste0("Required columns not found. Available: ",
                     paste(names(subject_data), collapse = ", "))
    ))
  }

  # ── Helper: SAS MEANS-style descriptive stats (arithmetic) ──────────────────
  calc_sas_means <- function(values, label) {
    v <- values[!is.na(values) & is.finite(values)]
    if (length(v) == 0) {
      return(data.frame(Product = label, N = 0L,
                        Mean = NA_real_, SD = NA_real_,
                        CV_pct = NA_real_, Min = NA_real_,
                        Median = NA_real_, Max = NA_real_,
                        stringsAsFactors = FALSE))
    }
    m  <- mean(v)
    s  <- if (length(v) > 1) sd(v) else NA_real_
    cv <- if (!is.na(s) && m != 0) (s / m) * 100 else NA_real_
    data.frame(Product = label, N = length(v),
               Mean = m, SD = s,
               CV_pct = cv, Min = min(v),
               Median = median(v), Max = max(v),
               stringsAsFactors = FALSE)
  }

  # ── Design detection ────────────────────────────────────────────────────────
  periods_per_subject <- subject_data %>%
    group_by(Subject) %>%
    summarise(n_periods = n_distinct(Period), .groups = "drop") %>%
    pull(n_periods) %>%
    max()

  is_replicate <- periods_per_subject > 2

  param_data <- subject_data %>%
    select(Subject, Sequence, Treatment, Period, all_of(param_name)) %>%
    rename(Value = !!param_name) %>%
    filter(!is.na(Value))

  if (nrow(param_data) == 0) {
    return(list(error = "No data available for this parameter"))
  }

  test_data <- param_data %>% filter(Treatment == "T")
  ref_data  <- param_data %>% filter(Treatment == "R")

  # ── REPLICATE DESIGN ────────────────────────────────────────────────────────
  if (is_replicate) {

    # Build a sequence-level design map: within each (Sequence, Treatment),
    # rank the periods in ascending order. This tells us e.g. in RTRT sequence
    # that period 2 = T1 and period 4 = T2 for ALL subjects in that sequence,
    # regardless of which periods they individually completed.
    design_map <- param_data %>%
      distinct(Sequence, Treatment, Period) %>%
      arrange(Sequence, Treatment, as.numeric(Period)) %>%
      group_by(Sequence, Treatment) %>%
      mutate(rep_num = row_number()) %>%
      ungroup()

    # Label every observation with its design rep number (T1/T2/R1/R2)
    param_labeled <- param_data %>%
      left_join(design_map, by = c("Sequence", "Treatment", "Period"))

    test_labeled <- param_labeled %>% filter(Treatment == "T")
    ref_labeled  <- param_labeled %>% filter(Treatment == "R")

    test_by_subject <- test_labeled %>%
      group_by(Subject) %>%
      summarise(
        T1     = { v <- Value[rep_num == 1]; if (length(v) > 0) v[1] else NA_real_ },
        T2     = { v <- Value[rep_num == 2]; if (length(v) > 0) v[1] else NA_real_ },
        T_mean = mean(Value, na.rm = TRUE),
        T_n    = n(),
        .groups = "drop"
      )

    ref_by_subject <- ref_labeled %>%
      group_by(Subject) %>%
      summarise(
        R1     = { v <- Value[rep_num == 1]; if (length(v) > 0) v[1] else NA_real_ },
        R2     = { v <- Value[rep_num == 2]; if (length(v) > 0) v[1] else NA_real_ },
        R_mean = mean(Value, na.rm = TRUE),
        R_n    = n(),
        .groups = "drop"
      )

    comparison_data <- full_join(test_by_subject, ref_by_subject, by = "Subject") %>%
      mutate(
        Ratio     = T_mean / R_mean,
        Missing_T = replace_na(T_n < max(T_n, na.rm = TRUE), FALSE),
        Missing_R = replace_na(R_n < max(R_n, na.rm = TRUE), FALSE),
        Subject   = as.character(Subject)
      ) %>%
      arrange(as.numeric(Subject))

    n_T_reps <- max(comparison_data$T_n, na.rm = TRUE)
    n_R_reps <- max(comparison_data$R_n, na.rm = TRUE)

    # Build SAS MEANS-style rows: per replicate + combined if >1
    means_rows <- list()

    if (n_T_reps >= 2) {
      means_rows <- c(means_rows,
        list(calc_sas_means(comparison_data$T1, "T1")),
        list(calc_sas_means(comparison_data$T2, "T2")),
        list(calc_sas_means(c(comparison_data$T1, comparison_data$T2), "T (Total)"))
      )
    } else {
      means_rows <- c(means_rows,
        list(calc_sas_means(comparison_data$T1, "T"))
      )
    }

    if (n_R_reps >= 2) {
      means_rows <- c(means_rows,
        list(calc_sas_means(comparison_data$R1, "R1")),
        list(calc_sas_means(comparison_data$R2, "R2")),
        list(calc_sas_means(c(comparison_data$R1, comparison_data$R2), "R (Total)"))
      )
    } else {
      means_rows <- c(means_rows,
        list(calc_sas_means(comparison_data$R1, "R"))
      )
    }

    means_table <- bind_rows(means_rows)

  } else {
    # ── 2×2 CROSSOVER ─────────────────────────────────────────────────────────
    comparison_data <- full_join(
      test_data %>% select(Subject, Value) %>% rename(Test = Value),
      ref_data  %>% select(Subject, Value) %>% rename(Reference = Value),
      by = "Subject"
    ) %>%
      mutate(
        Ratio   = Test / Reference,
        Subject = as.character(Subject)
      ) %>%
      arrange(as.numeric(Subject))

    means_table <- bind_rows(
      calc_sas_means(comparison_data$Test,      "T"),
      calc_sas_means(comparison_data$Reference, "R")
    )
  }

  # ── Units ────────────────────────────────────────────────────────────────────
  unit <- ""
  if      (grepl("AUC", param_name))                  unit <- "ng\u00b7h/mL"
  else if (param_name == "Cmax")                       unit <- "ng/mL"
  else if (param_name %in% c("Tmax","t_half","Tlast")) unit <- "h"
  else if (param_name %in% c("CL_F","CLss_F"))         unit <- "mL/h"
  else if (param_name %in% c("Vd_F","Vss_F"))          unit <- "mL"
  else if (grepl("^(log|ln)", param_name))             unit <- paste0("ln(", sub("^(log|ln)","",param_name), ")")

  return(list(
    individual_data = comparison_data,
    means_table     = means_table,
    is_replicate    = is_replicate,
    unit            = unit,
    parameter       = param_name
  ))
}

# =============================================================================
# Shared SAS PROC GLM-style ANOVA output builders
# =============================================================================
# These render the same set of tables SAS produces for a crossover BE ANOVA
# (Type III SS breakdown, the Subject(Sequence)-as-error-term Sequence test,
# and Least Squares Means) from the fields build_crossover_anova_tables() /
# compute_lsmeans_ci() (R/simple_anova.R) attach to param_result — used
# identically by ABE, ABEL, and RSABE's ANOVA Results display so all three
# analysis types show the same structure for the same underlying model.

# Generic Source/DF/SS/MS/F/Pr(>F) table renderer, keyed by rownames.
.render_anova_source_table <- function(df, header_class = "table-primary") {
  tags$table(class = "table table-striped table-hover table-sm",
    tags$thead(class = header_class,
      tags$tr(tags$th("Source"), lapply(names(df), function(col) tags$th(col)))
    ),
    tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          tags$td(style = "font-weight: bold;", rownames(df)[i]),
          lapply(seq_len(ncol(df)), function(j) {
            val <- df[i, j]
            col_name <- names(df)[j]
            formatted <- if (is.numeric(val) && !is.na(val)) {
              if (col_name %in% c("Pr(>F)", "p-value", "Pr(>|t|)")) format.pval(val, digits = 4)
              else if (col_name %in% c("Df", "NumDF", "DenDF", "numDF", "denDF", "npar")) as.character(round(val))
              else if (col_name %in% c("Sum Sq", "Mean Sq")) sprintf("%.4f", val)
              else if (col_name %in% c("F value", "F-value")) sprintf("%.2f", val)
              else sprintf("%.4f", val)
            } else if (is.na(val)) "" else as.character(val)
            tags$td(formatted)
          })
        )
      })
    )
  )
}

# Card: SAS PROC GLM's own first two output tables for a fixed-effects model —
# the overall Model/Error/Corrected Total F-test, plus the R-Square/Coeff Var/
# Root MSE/Mean line. Computed generically from `param_result$model` (an `lm`
# object) and its `anova()` table, so it works identically for ABE, ABEL, and
# RSABE's fixed-effects fit without needing separately precomputed fields.
# Returns NULL for mixed-effects models (nlme::lme's anova() has no Sum Sq/
# Residual row to build this from — SAS PROC MIXED doesn't print this table
# either; see the Type 3 Tests of Fixed Effects table instead).
render_model_summary_card <- function(param_result, param_name) {
  at <- tryCatch(as.data.frame(param_result$anova), error = function(e) NULL)
  if (is.null(at) || !"Df" %in% names(at) || !"Sum Sq" %in% names(at)) return(NULL)

  n_rows <- nrow(at)
  err_df <- at$Df[n_rows]
  err_ss <- at$`Sum Sq`[n_rows]
  mdl_df <- sum(at$Df[-n_rows], na.rm = TRUE)
  mdl_ss <- sum(at$`Sum Sq`[-n_rows], na.rm = TRUE)
  tot_df <- mdl_df + err_df
  tot_ss <- mdl_ss + err_ss
  mdl_ms <- if (mdl_df > 0) mdl_ss / mdl_df else NA
  err_ms <- if (err_df > 0) err_ss / err_df else NA
  f_val  <- if (!is.na(mdl_ms) && !is.na(err_ms) && err_ms > 0) mdl_ms / err_ms else NA
  p_val  <- if (!is.na(f_val) && mdl_df > 0 && err_df > 0) pf(f_val, mdl_df, err_df, lower.tail = FALSE) else NA

  # R-Square / Coeff Var / Root MSE / Mean — derived from the same model/anova
  # table (mean(fitted(model)) == mean(response) for an OLS fit with intercept).
  r_squared  <- if (!is.na(tot_ss) && tot_ss > 0) mdl_ss / tot_ss else NA
  root_mse   <- if (!is.na(err_ms) && err_ms >= 0) sqrt(err_ms) else NA
  param_mean <- tryCatch(mean(fitted(param_result$model), na.rm = TRUE), error = function(e) NA_real_)
  cv_percent <- if (!is.na(root_mse) && !is.na(param_mean) && param_mean != 0) (root_mse / param_mean) * 100 else NA

  fmt_num <- function(x, d = 4) if (is.na(x)) "" else sprintf(paste0("%.", d, "f"), x)

  div(class = "card mb-3",
    div(class = "card-header",
      h6(class = "card-title mb-0", icon("table"), sprintf(" Dependent Variable: %s", param_name))
    ),
    div(class = "card-body",
      div(class = "table-responsive",
        tags$table(class = "table table-striped table-sm",
          tags$thead(class = "table-primary",
            tags$tr(
              tags$th("Source"), tags$th("DF", style = "text-align: right;"),
              tags$th("Sum of Squares", style = "text-align: right;"),
              tags$th("Mean Square", style = "text-align: right;"),
              tags$th("F Value", style = "text-align: right;"),
              tags$th("Pr > F", style = "text-align: right;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(strong("Model")),
              tags$td(mdl_df, style = "text-align: right;"),
              tags$td(fmt_num(mdl_ss), style = "text-align: right;"),
              tags$td(fmt_num(mdl_ms, 6), style = "text-align: right;"),
              tags$td(fmt_num(f_val, 2), style = "text-align: right;"),
              tags$td(if (is.na(p_val)) "" else format.pval(p_val, digits = 4), style = "text-align: right;")
            ),
            tags$tr(
              tags$td(strong("Error")),
              tags$td(err_df, style = "text-align: right;"),
              tags$td(fmt_num(err_ss), style = "text-align: right;"),
              tags$td(fmt_num(err_ms, 6), style = "text-align: right;"),
              tags$td(""), tags$td("")
            ),
            tags$tr(
              tags$td(strong("Corrected Total")),
              tags$td(tot_df, style = "text-align: right;"),
              tags$td(fmt_num(tot_ss), style = "text-align: right;"),
              tags$td(""), tags$td(""), tags$td("")
            )
          )
        )
      ),
      div(class = "table-responsive mt-2",
        tags$table(class = "table table-sm table-bordered",
          tags$thead(class = "table-light",
            tags$tr(
              tags$th("R-Square", style = "text-align: center;"),
              tags$th("Coeff Var", style = "text-align: center;"),
              tags$th("Root MSE", style = "text-align: center;"),
              tags$th(sprintf("%s Mean", param_name), style = "text-align: center;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(if (!is.na(r_squared)) sprintf("%.6f", r_squared) else "—", style = "text-align: center;"),
              tags$td(if (!is.na(cv_percent)) sprintf("%.4f", cv_percent) else "—", style = "text-align: center;"),
              tags$td(if (!is.na(root_mse)) sprintf("%.6f", root_mse) else "—", style = "text-align: center;"),
              tags$td(if (!is.na(param_mean)) sprintf("%.6f", param_mean) else "—", style = "text-align: center;")
            )
          )
        )
      )
    )
  )
}

# Card: "Analysis of Variance (Type III SS)" — Sequence/Subject(Sequence)/
# Period/Treatment/Residual, all tested against Residual MS (SAS PROC GLM
# default). Requires `param_result$anova_comprehensive` (built by
# build_crossover_anova_tables()).
render_type3_anova_card <- function(param_result) {
  comp_df <- param_result$anova_comprehensive
  if (is.null(comp_df)) return(NULL)
  tryCatch({
    df <- comp_df[, setdiff(names(comp_df), "Source"), drop = FALSE]
    div(class = "card mb-3",
      div(class = "card-header",
        h6(class = "card-title mb-0", icon("table"), " Analysis of Variance (Type III SS)")
      ),
      div(class = "card-body",
        div(class = "table-responsive", .render_anova_source_table(df))
      )
    )
  }, error = function(e) NULL)
}

# Card: "Type 3 Tests of Fixed Effects" — the mixed-model (PROC MIXED)
# equivalent of the Type III SS card above. render_type3_anova_card() only
# fires for the fixed-effects fit (anova_comprehensive is built from an lm()
# model's SS decomposition, which a mixed model doesn't have); this is the
# card that was missing for Method B / mixed-effects results, leaving Method
# B's display short of Method A's. Requires `param_result$type3_ss`, an
# `anova(lme_model, type = "marginal")` object — numDF/denDF/F-value/p-value
# per fixed effect (Intercept, Sequence, Period, Treatment).
render_type3_marginal_card <- function(param_result) {
  t3 <- param_result$type3_ss
  if (is.null(t3)) return(NULL)
  tryCatch({
    df <- as.data.frame(t3)
    div(class = "card mb-3",
      div(class = "card-header",
        h6(class = "card-title mb-0", icon("table"), " Type 3 Tests of Fixed Effects")
      ),
      div(class = "card-body",
        p(style = "font-size: 0.85em; color: #6c757d; margin-bottom: 10px;",
          "Marginal (Type III) F-tests for each fixed effect from the mixed-effects model — the PROC MIXED equivalent of the Type III SS table shown for Fixed Effects (Method A)."),
        div(class = "table-responsive", .render_anova_source_table(df))
      )
    )
  }, error = function(e) NULL)
}

# Card: "Tests of Hypotheses Using the Type III MS for Subject(Sequence) as an
# Error Term" — the Sequence effect (and, when Group is included, the Group
# effect — both are between-subject factors tested against the same
# Subject(Group x Sequence) error term) re-tested against the between-subject MS
# instead of Residual MS (the standard, correct significance test for these
# effects in a crossover design). Loops over every entry in
# `param_result$subj_seq_analysis$hypothesis_tests` (built by
# build_crossover_anova_tables() in R/simple_anova.R) rather than assuming only
# "seq" is present, so the table gains a Group row automatically when Group is
# in the model — the title still says "Subject(Sequence)" for the plain case;
# when Group is present it becomes "Subject(Group x Sequence)" to match.
render_seq_subj_test_card <- function(param_result) {
  ssa <- param_result$subj_seq_analysis
  tests <- tryCatch(ssa$hypothesis_tests, error = function(e) NULL)
  if (is.null(tests) || length(tests) == 0) return(NULL)

  label_for <- function(key) switch(key, seq = "Sequence", grp = "Group", key)
  # Preserve a stable, meaningful order (Group before Sequence, matching the
  # comprehensive Type III table's row order) rather than list-insertion order.
  ordered_keys <- intersect(c("grp", "seq"), names(tests))
  rows <- lapply(ordered_keys, function(key) {
    t <- tests[[key]]
    if (is.null(t) || is.null(t$f_value) || is.na(t$f_value)) return(NULL)
    tags$tr(
      tags$td(style = "font-weight: bold;", label_for(key)),
      tags$td(as.character(t$df), style = "text-align: right;"),
      tags$td(sprintf("%.6f", t$ss), style = "text-align: right;"),
      tags$td(sprintf("%.6f", t$ms), style = "text-align: right;"),
      tags$td(sprintf("%.2f", t$f_value), style = "text-align: right;"),
      tags$td(format.pval(t$p_value, digits = 4), style = "text-align: right;")
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)

  error_term_label <- if ("grp" %in% names(tests)) "Subject(Group x Sequence)" else "Subject(Sequence)"

  div(class = "card mb-3",
    div(class = "card-header",
      h6(class = "card-title mb-0", icon("table"),
         sprintf(" Tests of Hypotheses Using the Type III MS for %s as an Error Term", error_term_label))
    ),
    div(class = "card-body",
      div(class = "table-responsive",
        tags$table(class = "table table-striped table-hover table-sm",
          tags$thead(class = "table-info",
            tags$tr(
              tags$th("Source"), tags$th("DF", style = "text-align: right;"),
              tags$th("Type III SS", style = "text-align: right;"),
              tags$th("Mean Square", style = "text-align: right;"),
              tags$th("F Value", style = "text-align: right;"),
              tags$th("Pr > F", style = "text-align: right;")
            )
          ),
          tags$tbody(rows)
        )
      )
    )
  )
}

# Card: Group x Treatment interaction — supportive/exploratory analysis only.
# Requires `param_result$group_treatment_interaction`
# (compute_group_treatment_interaction() in R/simple_anova.R), populated only
# when the user opted into the "Test Group x Treatment Interaction" checkbox.
# Deliberately styled/worded to make it unmistakable that this test is NOT part
# of the model that determined bioequivalence — per ICH M13A's final guideline,
# Group x Treatment must stay out of the BE-determining model and is reported
# only as a supportive analysis.
render_group_treatment_interaction_card <- function(param_result) {
  gt <- param_result$group_treatment_interaction
  if (is.null(gt) || is.null(gt$f) || is.na(gt$f)) return(NULL)
  div(class = "card mb-3", style = "border-left: 4px solid #ffc107;",
    div(class = "card-header", style = "background-color: #fff3cd;",
      h6(class = "card-title mb-0", icon("triangle-exclamation"),
         " Group × Treatment Interaction — Supportive Analysis")
    ),
    div(class = "card-body",
      p(style = "font-size: 0.85em; color: #856404; margin-bottom: 10px;",
        tags$b("Not used to determine bioequivalence."), " Per ICH M13A's final guideline, the ",
        "Group × Treatment interaction term is excluded from the model that determines BE (it would ",
        "bias the treatment effect); it is evaluated here only to check whether the treatment effect looks ",
        "heterogeneous across groups. Tested against the ", tags$code(gt$error_term), " mean square."),
      div(class = "table-responsive",
        tags$table(class = "table table-striped table-hover table-sm",
          tags$thead(class = "table-warning",
            tags$tr(
              tags$th("Source"), tags$th("DF (num, den)", style = "text-align: right;"),
              tags$th("Type III SS", style = "text-align: right;"),
              tags$th("Mean Square", style = "text-align: right;"),
              tags$th("F Value", style = "text-align: right;"),
              tags$th("Pr > F", style = "text-align: right;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(style = "font-weight: bold;", "Group × Treatment"),
              tags$td(sprintf("%s, %s", gt$df1, gt$df2), style = "text-align: right;"),
              tags$td(sprintf("%.6f", gt$ss), style = "text-align: right;"),
              tags$td(sprintf("%.6f", gt$ms), style = "text-align: right;"),
              tags$td(sprintf("%.2f", gt$f), style = "text-align: right;"),
              tags$td(format.pval(gt$p, digits = 4), style = "text-align: right;")
            )
          )
        )
      )
    )
  )
}

# Cards: the three SAS "Least Squares Means" tables for the Treatment (FORM)
# effect — (1) LSMean per level + H0:LSMean1=LSMean2 p-value, (2) LSMean per
# level + CI, (3) difference between the two LSMeans + CI on the difference.
# Requires `param_result$lsmeans_result` (compute_lsmeans_ci()); also needs
# treatment_coef/treatment_se/residual_df for table 3 (identical to the
# Parameter Estimates card's T-R contrast, shown here in SAS's own layout).
render_lsmeans_cards <- function(param_result, param_name) {
  lsm <- param_result$lsmeans_result
  if (is.null(lsm)) return(NULL)

  tcoef <- param_result$treatment_coef %||% NA
  tse   <- param_result$treatment_se   %||% NA
  tdf   <- param_result$residual_df    %||% lsm$df_ref %||% NA
  has_diff <- !is.na(tcoef) && !is.na(tse) && !is.na(tdf) && tdf > 0
  if (has_diff) {
    t_crit <- qt(1 - (1 - lsm$level) / 2, tdf)
    diff_lo <- tcoef - t_crit * tse
    diff_hi <- tcoef + t_crit * tse
    tval    <- tcoef / tse
    pval    <- 2 * pt(abs(tval), tdf, lower.tail = FALSE)
  }
  ci_pct <- sprintf("%.0f%%", lsm$level * 100)

  div(class = "card mb-3",
    div(class = "card-header",
      h6(class = "card-title mb-0", icon("balance-scale"), " Least Squares Means")
    ),
    div(class = "card-body",
      h6(class = "text-muted", sprintf("%s LSMEAN", param_name)),
      div(class = "table-responsive mb-3",
        tags$table(class = "table table-sm table-bordered",
          tags$thead(class = "table-light",
            tags$tr(tags$th("FORM"), tags$th("LSMEAN", style = "text-align: right;"),
                    tags$th("H0:LSMean1=LSMean2 Pr > |t|", style = "text-align: right;"))
          ),
          tags$tbody(
            tags$tr(tags$td(strong(lsm$test_level)),
                    tags$td(sprintf("%.6f", lsm$lsmean_test_log), style = "text-align: right;"),
                    tags$td(if (has_diff) format.pval(pval, digits = 4) else "", style = "text-align: right;")),
            tags$tr(tags$td(strong(lsm$ref_level)),
                    tags$td(sprintf("%.6f", lsm$lsmean_ref_log), style = "text-align: right;"),
                    tags$td(""))
          )
        )
      ),
      div(class = "table-responsive mb-3",
        tags$table(class = "table table-sm table-bordered",
          tags$thead(class = "table-light",
            tags$tr(tags$th("FORM"), tags$th("LSMEAN", style = "text-align: right;"),
                    tags$th(sprintf("%s Confidence Limits", ci_pct), colspan = "2", style = "text-align: center;"))
          ),
          tags$tbody(
            tags$tr(tags$td(strong(lsm$test_level)),
                    tags$td(sprintf("%.6f", lsm$lsmean_test_log), style = "text-align: right;"),
                    tags$td(sprintf("%.6f", lsm$ci_lower_test_log), style = "text-align: right;"),
                    tags$td(sprintf("%.6f", lsm$ci_upper_test_log), style = "text-align: right;")),
            tags$tr(tags$td(strong(lsm$ref_level)),
                    tags$td(sprintf("%.6f", lsm$lsmean_ref_log), style = "text-align: right;"),
                    tags$td(sprintf("%.6f", lsm$ci_lower_ref_log), style = "text-align: right;"),
                    tags$td(sprintf("%.6f", lsm$ci_upper_ref_log), style = "text-align: right;"))
          )
        )
      ),
      if (has_diff) {
        tagList(
          h6(class = "text-muted", "Least Squares Means for Effect FORM"),
          div(class = "table-responsive",
            tags$table(class = "table table-sm table-bordered",
              tags$thead(class = "table-light",
                tags$tr(
                  tags$th("i"), tags$th("j"),
                  tags$th("Difference Between Means", style = "text-align: right;"),
                  tags$th(sprintf("%s CL for LSMean(i)-LSMean(j)", ci_pct), colspan = "2", style = "text-align: center;")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("1"), tags$td("2"),
                  tags$td(sprintf("%.6f", tcoef), style = "text-align: right;"),
                  tags$td(sprintf("%.6f", diff_lo), style = "text-align: right;"),
                  tags$td(sprintf("%.6f", diff_hi), style = "text-align: right;")
                )
              )
            )
          ),
          tags$p(class = "text-muted mb-0", style = "font-size: 0.8em; margin-top: 6px;",
            sprintf("i = %s, j = %s.", lsm$test_level, lsm$ref_level))
        )
      }
    )
  )
}

# Card: SAS-style "Parameter Estimates" — the Test-Reference contrast alone
# (Estimate/Standard Error/t Value/Pr > |t|), matching SAS PROC GLM's own
# 4-column layout; the difference's CI is shown in the LSMeans card above.
render_parameter_estimates_card <- function(param_result, ref_level = "R", test_level = "T") {
  tcoef <- param_result$treatment_coef %||% NA
  tse   <- param_result$treatment_se   %||% NA
  tdf   <- param_result$residual_df    %||% NA
  if (is.na(tcoef) || is.na(tse)) return(NULL)
  tval <- tcoef / tse
  tp   <- if (!is.na(tdf) && tdf > 0) 2 * pt(abs(tval), tdf, lower.tail = FALSE) else NA
  div(class = "card mb-3",
    div(class = "card-header",
      h6(class = "card-title mb-0", icon("calculator"), " Parameter Estimates")
    ),
    div(class = "card-body",
      div(class = "table-responsive",
        tags$table(class = "table table-sm table-bordered",
          tags$thead(class = "table-light",
            tags$tr(
              tags$th("Parameter"),
              tags$th("Estimate", style = "text-align: right;"),
              tags$th("Standard Error", style = "text-align: right;"),
              tags$th("t Value", style = "text-align: right;"),
              tags$th("Pr > |t|", style = "text-align: right;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(strong(sprintf("%s - %s", test_level, ref_level))),
              tags$td(sprintf("%.6f", tcoef), style = "text-align: right;"),
              tags$td(sprintf("%.6f", tse), style = "text-align: right;"),
              tags$td(sprintf("%.2f", tval), style = "text-align: right;"),
              tags$td(if (!is.na(tp)) format.pval(tp, digits = 4) else "", style = "text-align: right;")
            )
          )
        )
      )
    )
  )
}

# Format replicateBE ANOVA results for display
format_replicatebe_anova_results <- function(param_result, param_name, be_res) {

  # Extract replicateBE output
  rbe_output <- param_result$replicatebe_output

  # NOTE: replicateBE's own `Method` column is unreliable for distinguishing
  # A vs B — the installed package version returns "A" even when method.B()
  # was called (contradicts its documented "B-<option>" format). Use our own
  # anova_method field (set directly by perform_abel_placeholder()) instead.
  is_method_a <- identical(param_result$anova_method, "lm")

  # replicateBE::method.A()/method.B() name the acceptance-limit columns
  # differently depending on whether the drug ended up scaled: `L(%)`/`U(%)`
  # when reference-scaling applied (CVwR > 30%, high-variability), but
  # `BE.lo(%)`/`BE.hi(%)` when it did not (standard 80-125% limits used).
  # Reading only `L(%)`/`U(%)` (as this function previously did) returned
  # NULL for any non-scaled result, which crashed several `if()`/sprintf()
  # calls below ("missing value where TRUE/FALSE needed") — e.g. a full
  # replicate with CVwR just under the 30% switching threshold.
  limit_lo <- rbe_output$`L(%)` %||% rbe_output$`BE.lo(%)`
  limit_hi <- rbe_output$`U(%)` %||% rbe_output$`BE.hi(%)`

  # Determine variability classification. EMA ABEL switches at CV_wR > 30%.
  cv_wr <- rbe_output$`CVwR(%)`
  is_hv <- !is.na(cv_wr) && cv_wr > 30  # EMA ABEL uses 30% switching CV

  # Determine if limits are scaled vs fixed
  limits_are_scaled <- FALSE
  tryCatch({
    if (!is.na(limit_lo) && !is.na(limit_hi) && (limit_lo < 79.9 || limit_hi > 125.1)) limits_are_scaled <- TRUE
  }, error = function(e) NULL)

  # Evaluate individual criteria like RSABE
  pe_pct <- rbe_output$`PE(%)`
  pe_pass <- !is.na(pe_pct) && pe_pct >= 80 & pe_pct <= 125
  ci_lo <- rbe_output$`CL.lo(%)`
  ci_hi <- rbe_output$`CL.hi(%)`
  be_pass <- rbe_output$BE == "pass"
  ci_within <- !is.na(ci_lo) && !is.na(ci_hi) && !is.na(limit_lo) && !is.na(limit_hi) &&
               ci_lo >= limit_lo && ci_hi <= limit_hi
  
  # Detect whether test (swT) is estimable (full vs partial replicate)
  swT_val <- suppressWarnings(as.numeric(rbe_output$swT))
  cvT_val <- suppressWarnings(as.numeric(rbe_output$`CVwT(%)`))
  has_test <- !is.na(swT_val) && !is.na(cvT_val)
  swR_val <- suppressWarnings(as.numeric(rbe_output$swR))
  cvR_val <- suppressWarnings(as.numeric(rbe_output$`CVwR(%)`))

  # 3 columns when full replicate (test estimable), 2 columns for partial replicate
  col_class <- if (has_test) "col-md-4" else "col-md-6"

  # ── Summary card: per-product intra-subject variability (Reference + Test) ──
  summary_card <- div(class = "card mb-3",
    div(class = "card-header bg-primary text-white",
      h5(class = "card-title mb-0", 
        icon("flask"), 
        "ANOVA output from replicateBE"
      )
    ),
    div(class = "card-body",
      div(class = "row",
        # Column 1: Study Design
        div(class = col_class,
          h6(icon("flask"), " Model Summary:"),
          tags$table(class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td(strong("Design:")), tags$td(rbe_output$Design)),
              tags$tr(tags$td(strong("Method:")),
                tags$td(if (is_method_a) "Method A (ANOVA/lm)" else "Method B (Mixed Model/lme4)")),
              tags$tr(tags$td(strong("Total Subjects:")), tags$td(sprintf("%g", rbe_output$n)))
            )
          )
        ),
        # Column 2: Intra-subject Reference (n, s_wR, MSE, CV%)
        div(class = col_class,
          h6(icon("user"), " Intra-subject Reference:"),
          tags$table(class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td(strong("n:")), tags$td(sprintf("%g", rbe_output$nRR))),
              tags$tr(tags$td(strong("s", tags$sub("wR"), ":")),
                tags$td(if (!is.na(swR_val)) sprintf("%.4f", swR_val) else "—")),
              tags$tr(tags$td(strong("MSE (s²", tags$sub("wR"), "):")),
                tags$td(if (!is.na(swR_val)) sprintf("%.6f", swR_val^2) else "—")),
              tags$tr(tags$td(strong("CV%:")),
                tags$td(if (!is.na(cvR_val)) sprintf("%.2f%%", cvR_val) else "—"))
            )
          )
        ),
        # Column 3: Intra-subject Test (only when estimable / full replicate)
        if (has_test) div(class = col_class,
          h6(icon("user"), " Intra-subject Test:"),
          tags$table(class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td(strong("n:")), tags$td(sprintf("%g", rbe_output$nTT))),
              tags$tr(tags$td(strong("s", tags$sub("wT"), ":")), tags$td(sprintf("%.4f", swT_val))),
              tags$tr(tags$td(strong("MSE (s²", tags$sub("wT"), "):")), tags$td(sprintf("%.6f", swT_val^2))),
              tags$tr(tags$td(strong("CV%:")), tags$td(sprintf("%.2f%%", cvT_val)))
            )
          )
        )
      )
    )
  )
  
  # ── SAS PROC GLM-style ANOVA output, from the same independently-fit
  # treatment-effect model used to reproduce replicateBE's PE/CI (fit in
  # R/be_analysis.R via fit_rsabe_model() — the same model replicateBE fits
  # internally: y ~ seq + subj:seq + prd + drug for Method A; y ~ seq + prd +
  # drug, random=~1|subj for Method B). Same structure ABE and RSABE show.
  model_summary_card <- render_model_summary_card(param_result, param_name)
  type3_card     <- render_type3_anova_card(param_result)      # Fixed (Method A) only
  type3_mixed_card <- render_type3_marginal_card(param_result) # Mixed (Method B) only
  seq_subj_card <- render_seq_subj_test_card(param_result)
  lsmeans_cards <- render_lsmeans_cards(param_result, param_name)
  param_est_card <- render_parameter_estimates_card(
    param_result,
    ref_level  = param_result$lsmeans_result$ref_level  %||% "R",
    test_level = param_result$lsmeans_result$test_level %||% "T"
  )

  return(tagList(
    summary_card,
    model_summary_card,
    type3_card,
    type3_mixed_card,
    seq_subj_card,
    lsmeans_cards,
    param_est_card
  ))
}

# Format RSABE ANOVA results for display
format_rsabe_anova_results <- function(param_result, param_name, be_res) {

  rsabe_details <- be_res$rsabe_details[[param_name]]
  rsabe_test <- if (!is.null(rsabe_details)) rsabe_details$rsabe_test else NULL
  is_hv <- if (!is.null(rsabe_details)) rsabe_details$is_hv else FALSE
  rsabe_method <- be_res$rsabe_method %||% "fda_linearized"

  # Detect whether the Test-side variance is estimable (full-replicate only)
  s2wT_val <- suppressWarnings(as.numeric(param_result$s2_wT %||% NA))
  cvwT_val <- suppressWarnings(as.numeric(param_result$cv_wt_percent %||% NA))
  has_test <- !is.na(s2wT_val) && !is.na(cvwT_val)
  s2wR_val <- suppressWarnings(as.numeric(param_result$s2_wR))
  cvwR_val <- suppressWarnings(as.numeric(param_result$cv_wr_percent))
  col_class <- if (has_test) "col-md-4" else "col-md-6"

  method_label <- if (identical(param_result$anova_method, "fixed")) "Fixed Effects (PROC GLM)" else "Mixed Effects (PROC MIXED)"

  # \u2500\u2500 Summary card: per-product intra-subject variability (Reference + Test) \u2500\u2500
  # Same layout as ABEL's summary card (format_replicatebe_anova_results()).
  summary_card <- div(class = "card mb-3",
    div(class = "card-header bg-primary text-white",
      h5(class = "card-title mb-0",
        icon("flask"),
        sprintf(" RSABE Results for %s", param_name)
      )
    ),
    div(class = "card-body",
      div(class = "row",
        # Column 1: Study Design
        div(class = col_class,
          h6(icon("flask"), " Model Summary:"),
          tags$table(class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td(strong("Design:")), tags$td(be_res$design_type %||% "\u2014")),
              tags$tr(tags$td(strong("Method:")), tags$td(method_label)),
              tags$tr(tags$td(strong("Total Subjects:")), tags$td(sprintf("%g", param_result$n_observations)))
            )
          )
        ),
        # Column 2: Intra-subject Reference (n, s_wR, MSE, CV%)
        div(class = col_class,
          h6(icon("user"), " Intra-subject Reference:"),
          tags$table(class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td(strong("n:")), tags$td(sprintf("%s", param_result$n_wR %||% "\u2014"))),
              tags$tr(tags$td(strong("s", tags$sub("wR"), ":")),
                tags$td(if (!is.na(s2wR_val) && s2wR_val >= 0) sprintf("%.4f", sqrt(s2wR_val)) else "\u2014")),
              tags$tr(tags$td(strong("MSE (s\u00b2", tags$sub("wR"), "):")),
                tags$td(if (!is.na(s2wR_val)) sprintf("%.6f", s2wR_val) else "\u2014")),
              tags$tr(tags$td(strong("CV%:")),
                tags$td(if (!is.na(cvwR_val)) sprintf("%.2f%%", cvwR_val) else "\u2014"))
            )
          )
        ),
        # Column 3: Intra-subject Test (only when estimable / full replicate)
        if (has_test) div(class = col_class,
          h6(icon("user"), " Intra-subject Test:"),
          tags$table(class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td(strong("n:")), tags$td(sprintf("%s", param_result$n_wT %||% "\u2014"))),
              tags$tr(tags$td(strong("s", tags$sub("wT"), ":")),
                tags$td(if (s2wT_val >= 0) sprintf("%.4f", sqrt(s2wT_val)) else "\u2014")),
              tags$tr(tags$td(strong("MSE (s\u00b2", tags$sub("wT"), "):")), tags$td(sprintf("%.6f", s2wT_val))),
              tags$tr(tags$td(strong("CV%:")), tags$td(sprintf("%.2f%%", cvwT_val)))
            )
          )
        )
      )
    )
  )

  # \u2500\u2500 SAS-style ANOVA output \u2500\u2500 same card set ABE/ABEL use; the
  # Type III / Subject(Seq) / LSMeans cards simply render nothing when the
  # underlying fields aren't populated (this model has no Period/Treatment/
  # Subject(Sequence) terms), same as ABE/ABEL do for any table they can't build.
  model_summary_card <- render_model_summary_card(param_result, param_name)
  type3_card       <- render_type3_anova_card(param_result)
  type3_mixed_card <- render_type3_marginal_card(param_result)
  seq_subj_card  <- render_seq_subj_test_card(param_result)
  lsmeans_cards  <- render_lsmeans_cards(param_result, param_name)
  param_est_card <- render_parameter_estimates_card(param_result, ref_level = "R", test_level = "T")

  # \u2500\u2500 Collapsible Intermediate Values \u2500\u2500
  intermediate_panel <- NULL
  if (is_hv && !is.null(rsabe_test)) {
    safe_id <- gsub("[^a-zA-Z0-9]", "", param_name)

    if (rsabe_method == "fda_linearized") {
      # Howe UCB intermediate components
      intermediate_panel <- div(class = "card mb-3",
        div(class = "card-header", style = "cursor: pointer;",
            `data-toggle` = "collapse", `data-target` = paste0("#rsabe-detail-", safe_id),
          h6(class = "card-title mb-0",
            icon("cogs"), " Howe UCB Intermediate Components ",
            tags$small(class = "text-muted", "(click to expand)")
          )
        ),
        div(id = paste0("rsabe-detail-", safe_id), class = "collapse",
          div(class = "card-body",
            tags$table(class = "table table-sm",
              tags$tbody(
                if (!is.null(rsabe_test$Em)) tags$tr(tags$td(strong("Em (mean component):")), tags$td(sprintf("%.6f", rsabe_test$Em))),
                if (!is.null(rsabe_test$Es)) tags$tr(tags$td(strong("Es (sigma component):")), tags$td(sprintf("%.6f", rsabe_test$Es))),
                if (!is.null(rsabe_test$Cm)) tags$tr(tags$td(strong("Cm:")), tags$td(sprintf("%.6f", rsabe_test$Cm))),
                if (!is.null(rsabe_test$Cs)) tags$tr(tags$td(strong("Cs:")), tags$td(sprintf("%.6f", rsabe_test$Cs))),
                if (!is.null(rsabe_test$Lm)) tags$tr(tags$td(strong("Lm:")), tags$td(sprintf("%.6f", rsabe_test$Lm))),
                if (!is.null(rsabe_test$Ls)) tags$tr(tags$td(strong("Ls:")), tags$td(sprintf("%.6f", rsabe_test$Ls))),
                tags$tr(tags$td(strong("UCB = (Em\u2212Es) + \u221a(Lm+Ls):")), tags$td(sprintf("%.6f", rsabe_test$ucb)))
              )
            )
          )
        )
      )
    } else {
      # ncTOST details
      intermediate_panel <- div(class = "card mb-3",
        div(class = "card-header", style = "cursor: pointer;",
            `data-toggle` = "collapse", `data-target` = paste0("#rsabe-detail-", safe_id),
          h6(class = "card-title mb-0",
            icon("cogs"), " ncTOST Intermediate Values ",
            tags$small(class = "text-muted", "(click to expand)")
          )
        ),
        div(id = paste0("rsabe-detail-", safe_id), class = "collapse",
          div(class = "card-body",
            tags$table(class = "table table-sm",
              tags$tbody(
                if (!is.null(rsabe_test$cr)) tags$tr(tags$td(strong("Hedges\u2019 correction c\u1d63(df):")), tags$td(sprintf("%.6f", rsabe_test$cr))),
                if (!is.null(rsabe_test$ncp_lower)) tags$tr(tags$td(strong("NCP lower (\u2212\u03B8/K):")), tags$td(sprintf("%.6f", rsabe_test$ncp_lower))),
                if (!is.null(rsabe_test$ncp_upper)) tags$tr(tags$td(strong("NCP upper (\u03B8/K):")), tags$td(sprintf("%.6f", rsabe_test$ncp_upper))),
                if (!is.null(rsabe_test$df_nct %||% NULL)) tags$tr(tags$td(strong("DF (noncentral t):")), tags$td(sprintf("%.0f", rsabe_test$df_nct))),
                if (!is.null(rsabe_test$p_lower_cdf)) tags$tr(tags$td(strong("p\u2081 (lower CDF):")), tags$td(sprintf("%.8f", rsabe_test$p_lower_cdf))),
                if (!is.null(rsabe_test$p_upper_cdf)) tags$tr(tags$td(strong("p\u2082 (upper CDF):")), tags$td(sprintf("%.8f", rsabe_test$p_upper_cdf)))
              )
            )
          )
        )
      )
    }
  }

  return(tagList(summary_card, model_summary_card, type3_card, type3_mixed_card, seq_subj_card, lsmeans_cards, param_est_card, intermediate_panel))
}

# Format parallel group statistical results for display
format_parallel_results <- function(param_name, be_res) {

  # Resolve the results key: the ANOVA-tab dropdown supplies log names (e.g.
  # "lnCmax"), but the parallel path keys statistical_results/confidence_intervals
  # by the base parameter name (e.g. "Cmax"). Try as-is, then the base name,
  # then a case-insensitive base-name match.
  resolve_key <- function(nm, lst) {
    if (is.null(lst)) return(nm)
    if (!is.null(lst[[nm]])) return(nm)
    base <- sub("^(ln|log)", "", nm, ignore.case = TRUE)
    if (!is.null(lst[[base]])) return(base)
    hit <- names(lst)[tolower(names(lst)) == tolower(base)]
    if (length(hit) > 0) return(hit[1])
    nm
  }
  ci_key <- resolve_key(param_name, be_res$confidence_intervals)
  stats_key <- resolve_key(param_name, be_res$statistical_results)

  # Extract statistical results for this parameter
  stats <- be_res$statistical_results[[stats_key]]
  ci <- be_res$confidence_intervals[[ci_key]]
  # Display the resolved (base) parameter name
  param_name <- log_param_to_display_name(param_name)

  if (is.null(stats) && is.null(ci)) {
    return(div(class = "alert alert-warning",
      h5(icon("exclamation-triangle"), " No Statistical Results"),
      p(paste("No statistical results found for parameter:", param_name))
    ))
  }
  
  # Determine t-test method
  method_name <- stats$method %||% "Two-sample t-test"
  n_test <- stats$n_test %||% NA
  n_ref <- stats$n_ref %||% NA
  df_val <- stats$degrees_freedom %||% ci$df %||% NA
  t_stat <- ci$t_statistic %||% NA
  p_val <- stats$p_value %||% ci$p_value %||% NA
  
  # Summary card (same card style as other format functions)
  summary_card <- div(class = "card mb-3",
    div(class = "card-header bg-primary text-white",
      h5(class = "card-title mb-0",
        icon("chart-bar"),
        sprintf(" Statistical Results for %s", param_name)
      )
    ),
    div(class = "card-body",
      div(class = "row",
        # Model Summary
        div(class = "col-md-6",
          h6(icon("flask"), " Test Method:"),
          tags$table(class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(
                tags$td(strong("Method:")),
                tags$td(method_name)
              ),
              tags$tr(
                tags$td(strong("Design:")),
                tags$td("Parallel Group")
              ),
              if (!is.na(n_test)) tags$tr(
                tags$td(strong("N (Test):")),
                tags$td(n_test)
              ),
              if (!is.na(n_ref)) tags$tr(
                tags$td(strong("N (Reference):")),
                tags$td(n_ref)
              ),
              if (!is.na(n_test) && !is.na(n_ref)) tags$tr(
                tags$td(strong("N (Total):")),
                tags$td(n_test + n_ref)
              )
            )
          )
        ),
        # Test Statistics
        div(class = "col-md-6",
          h6(icon("calculator"), " Test Statistics:"),
          tags$table(class = "table table-sm table-borderless",
            tags$tbody(
              if (!is.na(t_stat)) tags$tr(
                tags$td(strong("t-statistic:")),
                tags$td(sprintf("%.4f", t_stat))
              ),
              if (!is.na(df_val)) tags$tr(
                tags$td(strong("Degrees of Freedom:")),
                tags$td(sprintf("%.1f", df_val))
              ),
              if (!is.na(p_val)) tags$tr(
                tags$td(strong("P-value:")),
                tags$td(format.pval(p_val, digits = 4))
              ),
              tags$tr(
                tags$td(strong("Analysis:")),
                tags$td("Log-transformed data")
              )
            )
          )
        )
      )
    )
  )
  
  # Log-scale results (collapsed by default)
  log_scale_panel <- NULL
  if (!is.null(ci$point_estimate) && !is.null(ci$ci_lower)) {
    log_pe <- log(ci$point_estimate / 100)
    log_lower <- log(ci$ci_lower / 100)
    log_upper <- log(ci$ci_upper / 100)
    
    log_scale_panel <- div(class = "card mb-3",
      div(class = "card-header", style = "cursor: pointer;",
           `data-toggle` = "collapse", `data-target` = paste0("#logscale-parallel-", gsub("[^a-zA-Z0-9]", "", param_name)),
        h5(class = "card-title mb-0",
          icon("compress-arrows-alt"),
          " Log-Scale Results ",
          tags$small(class = "text-muted", "(click to expand)")
        )
      ),
      div(id = paste0("logscale-parallel-", gsub("[^a-zA-Z0-9]", "", param_name)),
          class = "collapse",
        div(class = "card-body",
          tags$table(class = "table table-sm",
            tags$tbody(
              tags$tr(tags$td(strong("Log Difference (T\u2212R):")), tags$td(sprintf("%.6f", log_pe))),
              tags$tr(tags$td(strong("Log CI Lower:")), tags$td(sprintf("%.6f", log_lower))),
              tags$tr(tags$td(strong("Log CI Upper:")), tags$td(sprintf("%.6f", log_upper)))
            )
          )
        )
      )
    )
  }
  
  # Info note
  info_note <- div(class = "alert alert-info",
    h6(icon("info-circle"), " About Parallel Group Analysis"),
    p("Parallel group bioequivalence was assessed using a two-sample t-test on log-transformed data. ",
      if (grepl("Welch", method_name, ignore.case = TRUE)) {
        "Welch's approximation was used for unequal variances (recommended). "
      } else {
        "Equal variances were assumed (Student's t-test). "
      },
      "No ANOVA table is produced for parallel designs; the t-test provides the treatment comparison directly."
    )
  )
  
  return(tagList(summary_card, log_scale_panel, info_note))
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
        
        # Create organized choices — only log-transformed parameters are
        # appropriate for BE ANOVA, so non-log primary/secondary params are
        # intentionally excluded from this dropdown.
        anova_choices <- list()

        # Log-transformed parameters
        log_params <- c("lnCmax", "lnAUC0t", "lnAUC0inf", "lnTmax", "lnpAUC")
        available_log <- intersect(log_params, available_params)
        if (length(available_log) > 0) {
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

        if (length(anova_choices) == 0) {
          return(selectInput(
            session$ns("anova_parameter_select"),
            label = NULL,
            choices = list("No log-transformed parameters available" = ""),
            selected = ""
          ))
        }

        # Select first available log parameter as default
        default_selection <- available_log[1]
        
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
            } else if (grepl("^ln", col, ignore.case = TRUE)) {
              # ln-transformed columns (lnCmax, lnAUC0t, ...) get 4 decimals —
              # normal-scale NCA values stay at 2 (matches the SAS reference
              # output's display convention).
              display_data[[col]] <- round(display_data[[col]], 4)
            } else {
              # Round NCA parameter values to 2 decimal places (matches the SAS
              # reference output's display convention, and standardizes this
              # table with the report's per-subject listing).
              display_data[[col]] <- round(display_data[[col]], 2)
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
        # All analysis types share the same blue "alert-info" box style (left
        # border matches other elements on this page) with a bold "<Type>
        # Method:" line plus a small subtitle line of key design/criteria
        # facts — RSABE was the original pattern; ABE/ABEL/Parallel now match.
        analysis_header <- if (analysis_type == "RSABE") {
          rsabe_method_label <- be_res$rsabe_method %||% "fda_linearized"
          method_display <- if (rsabe_method_label == "nctost") "Non-Central TOST (ncTOST)" else "FDA Linearized Scaled Criterion (Howe UCB)"

          div(class = "alert alert-info",
            icon("flask"), " ",
            strong("RSABE Method: "), method_display,
            br(),
            tags$small(
              "Scaling constant θₛ = ln(1.25)/σ₀ ≈ 0.8924 | ",
              "Switching: s", tags$sub("wR"), " ≥ 0.294 (CV", tags$sub("wR"), " ≈ 30%) | ",
              "Point estimate constraint: 80–125%"
            )
          )
        } else if (analysis_type == "ABEL") {
          design_label <- be_res$design %||% "replicate"

          div(class = "alert alert-info",
            icon("flask"), " ",
            strong("ABEL Method: "), be_method,
            br(),
            tags$small(
              "Design: ", design_label, " | ",
              "Switching: CV", tags$sub("wR"), " > 30% → reference-scaled expanding limits | ",
              "Non-scaled limits: ", sprintf("%.0f–%.0f%%", be_lower, be_upper)
            )
          )
        } else {
          is_parallel <- identical(be_res$design %||% "", "parallel")
          model_label <- if (is_parallel) {
            if (isTRUE(analysis_cfg$welch_correction)) "Welch's Two-Sample t-test" else "Student's Two-Sample t-test"
          } else if (identical(analysis_cfg$anova_model, "mixed")) {
            "Mixed Effects (nlme)"
          } else {
            "Fixed Effects (lm)"
          }

          div(class = "alert alert-info",
            icon("flask"), " ",
            strong("ABE Method: "), model_label,
            br(),
            tags$small(
              "Design: ", be_res$design %||% "2x2x2 crossover", " | ",
              "BE Limits: ", sprintf("%.0f–%.0f%%", be_lower, be_upper)
            )
          )
        }
        
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
        
        is_rsabe <- identical(analysis_type, "RSABE")

        # Create results table with display names and per-parameter limits
        results_list <- mapply(function(param, is_be) {
          ci <- primary_ci[[param]]

          # Handle NA values in BE conclusions
          be_status <- if (is.na(is_be)) {
            "\u2753 UNKNOWN"
          } else if (is_be) {
            "\u2705 PASS"
          } else {
            "\u274c FAIL"
          }

          # Convert log parameter name to display name
          display_param <- log_param_to_display_name(param)
          n_val <- ci$n_subjects %||% be_res$n_subjects %||% NA

          if (is_rsabe) {
            # RSABE's conclusion is NOT a 90% CI check \u2014 per FDA guidance Step 3
            # it's the Howe UCB (or ncTOST) scaling criterion plus the point-
            # estimate constraint. A standard 90% CI applies only for parameters
            # where s_wR < 0.294 (not high variability) \u2014 FDA's own Step 1a
            # specifies the two-one-sided-tests/CI procedure there instead, so
            # those rows fall back to the CI display.
            rsabe_det  <- be_res$rsabe_details[[param]]
            is_hv      <- if (!is.null(rsabe_det)) isTRUE(rsabe_det$is_hv) else FALSE
            rsabe_test <- if (!is.null(rsabe_det)) rsabe_det$rsabe_test else NULL

            if (is_hv && !is.null(rsabe_test)) {
              ucb_val <- rsabe_test$ucb %||% NA_real_
              criterion_str <- if (!is.na(ucb_val)) {
                sprintf("UCB = %.4f (\u2264 0? %s)", ucb_val, ifelse(ucb_val <= 0, "Yes", "No"))
              } else {
                sprintf("Overall p = %.4f", rsabe_test$overall_p %||% NA_real_)
              }
              pe_pass <- if (!is.null(rsabe_det)) rsabe_det$pe_constraint_pass else NA
              constraint_str <- if (!is.na(pe_pass)) {
                sprintf("PE in [80,125]%%? %s", ifelse(pe_pass, "Yes", "No"))
              } else {
                "\u2014"
              }
            } else {
              criterion_str  <- sprintf("90%% CI: [%.2f%%, %.2f%%]", ci$ci_lower, ci$ci_upper)
              constraint_str <- "80\u2013125% (standard ABE, s_wR < 0.294)"
            }

            return(data.frame(
              Parameter = display_param,
              N = if (!is.na(n_val)) as.character(round(n_val)) else "\u2014",
              `Point Estimate` = sprintf("%.2f%%", ci$point_estimate),
              Criterion = criterion_str,
              `PE Constraint` = constraint_str,
              `BE Status` = be_status,
              stringsAsFactors = FALSE
            ))
          }

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
          if (!is.null(ci$limits_used$type) && ci$limits_used$type == "expanded") {
            limit_type <- " (expanded ABEL)"
          } else if (!is.null(ci$limits_used$type) && ci$limits_used$type == "scaled") {
            limit_type <- " (scaled RSABE)"
          } else {
            limit_type <- " (fixed)"
          }

          data.frame(
            Parameter = display_param,
            N = if (!is.na(n_val)) as.character(round(n_val)) else "\u2014",
            `Point Estimate` = sprintf("%.2f%%", ci$point_estimate),
            `CI Lower` = sprintf("%.2f%%", ci$ci_lower),
            `CI Upper` = sprintf("%.2f%%", ci$ci_upper),
            `BE Criteria` = sprintf("%.2f%% \u2013 %.2f%%%s", param_lower, param_upper, limit_type),
            `BE Status` = be_status,
            stringsAsFactors = FALSE
          )
        }, names(primary_ci), primary_conclusions, SIMPLIFY = FALSE)
        
        results_rows <- do.call(rbind, results_list)

        # ── Intra-subject Reference summary (CV%_wR and variance s²_wR) ──
        # Only meaningful for replicate designs (RSABE/ABEL), where the reference
        # is replicated so a reference-specific within-subject variance is
        # estimable and drives the scaling decision. A 2×2×2 (or parallel) design
        # has a single reference observation per subject, so CVwR/swR is NOT
        # estimable there (the residual is a pooled T+R variance) — omit it.
        intra_ref_block <- if (!(analysis_type %in% c("RSABE", "ABEL"))) NULL else tryCatch({
          anova_data2 <- be_res$anova_results$anova_results
          rows <- list()
          for (p in names(primary_ci)) {
            ci_p <- primary_ci[[p]]
            pr <- NULL
            if (!is.null(anova_data2)) {
              pr <- anova_data2[[p]] %||% anova_data2[[paste0("ln", p)]] %||%
                    anova_data2[[sub("^ln", "", p)]]
            }
            cvR <- NA_real_; s2R <- NA_real_; src <- ""

            # Priority 1: ABEL — ci has cv_wr + sw_reference (log scale)
            if (!is.null(ci_p$cv_wr) && !is.na(ci_p$cv_wr)) {
              cvR <- suppressWarnings(as.numeric(ci_p$cv_wr))
              if (!is.null(ci_p$sw_reference)) {
                swR <- suppressWarnings(as.numeric(ci_p$sw_reference))
                if (!is.na(swR)) s2R <- swR^2
              }
              src <- "ABEL (replicateBE)"
            }
            # Priority 2: replicateBE output on the anova_data entry
            else if (!is.null(pr) && !is.null(pr$replicatebe_output)) {
              rb <- pr$replicatebe_output
              swR <- suppressWarnings(as.numeric(rb$swR))
              cvR <- suppressWarnings(as.numeric(rb$`CVwR(%)`))
              if (!is.na(swR)) s2R <- swR^2
              src <- "ABEL (replicateBE)"
            }
            # Priority 3: RSABE — Reference-only ANOVA
            else if (!is.null(pr) && !is.null(pr$s2_wR)) {
              s2R <- suppressWarnings(as.numeric(pr$s2_wR))
              cvR <- suppressWarnings(as.numeric(pr$cv_wr_percent))
              src <- "RSABE (Reference-only ANOVA)"
            }
            # Priority 4: simple ABE — log-scale residual MSE
            # Only valid if the analysis was on log scale.
            else {
              # ci_p$mse doesn't exist on ABEL/RSABE confidence-interval objects
              # (only plain ABE's does) — as.numeric(NULL) is a zero-length
              # vector, and is.na() on that breaks the if() below with "missing
              # value where TRUE/FALSE needed"; coalesce to a proper length-1 NA.
              mse_val <- suppressWarnings(as.numeric(ci_p$mse %||% NA_real_))
              if (is.na(mse_val) && !is.null(pr)) {
                if (isTRUE(pr$data_was_logged) || startsWith(pr$parameter %||% "", "ln") ||
                    startsWith(p, "ln")) {
                  mse_val <- suppressWarnings(as.numeric(pr$residual_mse %||% NA_real_))
                }
              }
              if (!is.na(mse_val) && mse_val >= 0 && mse_val < 5) {
                # Plausible log-scale MSE (CV up to ~1200%); guard against raw-scale values.
                s2R <- mse_val
                cvR <- 100 * sqrt(exp(mse_val) - 1)
                src <- "ABE (log-scale residual MSE)"
              }
            }

            if (is.na(cvR) && is.na(s2R)) next
            disp_param <- log_param_to_display_name(p)
            swR_val <- if (!is.na(s2R) && s2R >= 0) sqrt(s2R) else NA_real_
            # Classification: s_wR >= 0.294 is the actual FDA RSABE / EMA ABEL
            # switching criterion (not a CV%-based proxy, which is only an
            # approximate conversion and can disagree with s_wR at the margin).
            is_hv_p <- !is.na(swR_val) && swR_val >= 0.294
            rows[[length(rows) + 1]] <- tags$li(
              tags$strong(disp_param), ": ",
              "s", tags$sub("wR"), " = ",
              if (!is.na(swR_val)) sprintf("%.4f", swR_val) else "\u2014",
              " (CV% = ",
              if (!is.na(cvR)) sprintf("%.2f%%", cvR) else "\u2014",
              ")",
              " \u2014 ",
              tags$span(style = if (is_hv_p) "color: #e65100; font-weight: bold;" else "color: #2e7d32; font-weight: bold;",
                if (is_hv_p) "HIGH VARIABILITY" else "Low variability")
            )
          }
          if (length(rows) == 0) NULL else div(
            style = "padding: 8px 12px; background-color: #f8f9fa; border-left: 3px solid #17a2b8; border-radius: 4px; margin-bottom: 8px;",
            tags$div(style = "font-weight: 600; margin-bottom: 4px;",
              "Intra-subject variability of the Reference product:"),
            tags$ul(style = "margin-bottom: 0; padding-left: 20px;", rows)
          )
        }, error = function(e) NULL)

        return(tagList(
          # Analysis type information
          analysis_header,

          # Intra-subject Reference summary (above BE results table)
          intra_ref_block,

          # Results table — RSABE has one fewer column (Criterion + PE
          # Constraint instead of CI Lower/CI Upper); the "BE Status" column
          # is always last, so its 0-indexed target is ncol - 1 either way.
          {
            status_target <- ncol(results_rows) - 1
            table_colnames <- if (is_rsabe) {
              c("Parameter", "N", "Point Estimate", "Criterion", "PE Constraint", "BE Status")
            } else {
              c("Parameter", "N", "Point Estimate", paste(ci_label, "Lower"), paste(ci_label, "Upper"), "BE Criteria", "BE Status")
            }

            DT::datatable(
              results_rows,
              options = list(
                dom = 't',
                pageLength = 10,
                columnDefs = list(
                  list(className = 'dt-center', targets = 1:status_target),
                  list(
                    targets = status_target,
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
              colnames = table_colnames,
              escape = FALSE
            ) %>%
              DT::formatStyle(columns = seq_len(ncol(results_rows)), fontSize = '14px')
          },
          
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
          }
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
      
      # Extract key statistics
      method_name <- if (!is.null(param_result$anova_method)) {
        switch(param_result$anova_method,
          "fixed" = "Fixed Effects Model (lm)",
          "nlme" = "Mixed Effects Model (nlme)",
          "satterthwaite" = "Mixed Effects Model (Satterthwaite)",
          "kenward-roger" = "Mixed Effects Model (Kenward-Roger)",
          param_result$anova_method
        )
      } else "Unknown Method"
      
      root_mse <- param_result$root_mse
      param_mean <- param_result$param_mean
      cv_percent <- param_result$cv_percent

      # Mixed-effects models (lme/lmerTest) do not yield a SAS PROC GLM-style
      # sum-of-squares decomposition or a comparable model frame. For those we
      # suppress the GLM-only "Class Level Information" and "Model/Error/
      # Corrected Total" cards (which otherwise render empty/degenerate) and rely
      # on the variance-component summary, the fixed-effects test table, and the
      # treatment contrast — the appropriate output for a mixed model.
      is_mixed_model <- !is.null(param_result$anova_method) &&
        param_result$anova_method %in% c("nlme", "satterthwaite", "kenward-roger")

      # ── Summary card: critical variability and BE data up front ──
      # ── Header: model + parameter ─────────────────────────────────────────────
      model_header <- div(class = "mb-3",
        strong(param_name), " \u2014 ", method_name,
        if (!is.null(param_result$n_observations))
          tags$span(class = "text-muted ms-2",
            sprintf("  (N = %g observations)", param_result$n_observations))
      )

      # ── Class Level Information (mirrors SAS PROC GLM) ────────────────────────
      class_info_card <- if (is_mixed_model) NULL else {
        mdl <- param_result$model
        cli <- NULL
        if (!is.null(mdl) && !is.null(mdl$model)) {
          mdf <- mdl$model
          want <- intersect(c("subj", "drug", "prd", "seq", "grp"), names(mdf))
          rows <- lapply(want, function(v) {
            lvls <- unique(as.character(mdf[[v]]))
            lvls <- lvls[!is.na(lvls)]
            label <- switch(v,
              "subj" = "Subject",
              "drug" = "Treatment",
              "prd"  = "Period",
              "seq"  = "Sequence",
              "grp"  = "Group",
              v
            )
            lvls_show <- if (length(lvls) > 30)
              paste0(paste(head(lvls, 30), collapse = " "), " \u2026")
            else paste(lvls, collapse = " ")
            tags$tr(
              tags$td(strong(label)),
              tags$td(length(lvls), style = "text-align: center;"),
              tags$td(lvls_show, style = "font-family: monospace; font-size: 0.85em;")
            )
          })
          cli <- tags$table(class = "table table-sm table-bordered",
            tags$thead(class = "table-light",
              tags$tr(tags$th("Class"), tags$th("Levels", style = "text-align: center;"),
                       tags$th("Values"))
            ),
            tags$tbody(rows)
          )
        }
        if (is.null(cli)) NULL else
          div(class = "card mb-3",
            div(class = "card-header",
              h6(class = "card-title mb-0", icon("layer-group"),
                 " Class Level Information")
            ),
            div(class = "card-body",
              div(class = "table-responsive", cli),
              p(class = "text-muted mb-0", style = "font-size: 0.85em;",
                sprintf("Number of Observations Used: %s",
                        param_result$n_observations %||% "\u2014"))
            )
          )
      }

      # ── Top-of-output Model / Error / Corrected Total summary (SAS style) ─────
      model_summary_card <- if (is_mixed_model) NULL else render_model_summary_card(param_result, param_name)


      # ── Detect replicate design and compute per-product intra-subject variability
      # For replicate studies (any subject has >1 replicate of the same drug), we
      # replace the single Intra/Inter columns with Intra-subject Reference and,
      # when estimable (full replicate), Intra-subject Test boxes.
      #
      # compute_replicate_intra_subject_variability() (R/simple_anova.R) does the
      # actual per-arm SAS-style ANOVA (y ~ Seq + Subject(Seq) + Period on each
      # arm separately) - shared with the SAS-style HTML report's Intra-subject
      # Variability Summary section (sas_style_report.R) so the two stay in sync.
      data_logged <- isTRUE(param_result$data_was_logged) ||
                     grepl("^(ln|log)", param_name, ignore.case = TRUE)
      replicate_info <- compute_replicate_intra_subject_variability(
        param_result$model, is_log_scale = data_logged)

      # ── Summary card: 3 focused columns ───────────────────────────────────────
      summary_card <- {
        # Intra-subject stats (within-subject residual error)
        intra_cv  <- param_result$cv_intra_pct %||% NA
        mse_intra <- param_result$residual_mse  %||% NA
        df_intra  <- param_result$residual_df   %||% NA

        # Inter-subject stats (Subject(Sequence) MS / variance component)
        inter_cv  <- param_result$cv_inter_pct %||% NA
        mse_intr  <- param_result$mse_inter    %||% NA
        df_intr   <- param_result$df_inter     %||% NA
        f_intr    <- param_result$f_inter      %||% NA
        p_intr    <- param_result$p_inter      %||% NA

        # BE assessment — derive from model coefficients directly (authoritative)
        tcoef <- param_result$treatment_coef %||% NA
        tse   <- param_result$treatment_se   %||% NA
        tdf   <- param_result$residual_df    %||% NA
        if (!is.na(tcoef) && !is.na(tse) && !is.na(tdf) && tdf > 0) {
          t_crit_be  <- qt(0.95, tdf)
          gmr_val    <- 100 * exp(tcoef)
          ci_lo_be   <- 100 * exp(tcoef - t_crit_be * tse)
          ci_hi_be   <- 100 * exp(tcoef + t_crit_be * tse)
          be_pass    <- ci_lo_be >= 80.0 && ci_hi_be <= 125.0
        } else {
          gmr_val <- ci_lo_be <- ci_hi_be <- NA
          be_pass <- FALSE
        }

        # ── Replicate design: build per-product intra-subject columns ────────────
        if (!is.null(replicate_info) && isTRUE(replicate_info$is_replicate)) {
          has_test_intra <- !is.na(replicate_info$s2_wT) && replicate_info$df_wT > 0
          rep_col_class  <- if (has_test_intra) "col-md-6" else "col-md-12"

          ref_label_disp <- sprintf("Intra-subject Reference (%s):", replicate_info$ref_label)
          ref_col <- div(class = rep_col_class,
            h6(icon("user"), " ", ref_label_disp),
            tags$table(class = "table table-sm table-borderless",
              tags$tbody(
                tags$tr(tags$td(strong("s", tags$sub("wR"), ":")),
                  tags$td(if (!is.na(replicate_info$s2_wR) && replicate_info$s2_wR >= 0)
                            sprintf("%.4f", sqrt(replicate_info$s2_wR)) else "\u2014")),
                tags$tr(tags$td(strong("MSE (s\u00b2", tags$sub("wR"), "):")),
                  tags$td(if (!is.na(replicate_info$s2_wR))
                            sprintf("%.6f", replicate_info$s2_wR) else "\u2014")),
                tags$tr(tags$td(strong("CV%:")),
                  tags$td(if (!is.na(replicate_info$cv_wR))
                            sprintf("%.2f%%", replicate_info$cv_wR) else "\u2014"))
              )
            )
          )

          test_col <- if (has_test_intra) {
            test_label_disp <- sprintf("Intra-subject Test (%s):", replicate_info$test_label)
            div(class = rep_col_class,
              h6(icon("user"), " ", test_label_disp),
              tags$table(class = "table table-sm table-borderless",
                tags$tbody(
                  tags$tr(tags$td(strong("s", tags$sub("wT"), ":")),
                    tags$td(sprintf("%.4f", sqrt(replicate_info$s2_wT)))),
                  tags$tr(tags$td(strong("MSE (s\u00b2", tags$sub("wT"), "):")),
                    tags$td(sprintf("%.6f", replicate_info$s2_wT))),
                  tags$tr(tags$td(strong("CV%:")),
                    tags$td(sprintf("%.2f%%", replicate_info$cv_wT)))
                )
              )
            )
          } else NULL

          return_card <- div(class = "card mb-3",
            div(class = "card-body",
              div(class = "row", ref_col, test_col)
            )
          )
          return_card
        } else {
          # Non-replicate 2x2 crossover: no separate summary card here — the
          # between-subject (Subject(Sequence)) DF/SS/MS/F/p is already shown,
          # unduplicated, as its own row in the Type III SS table below (matching
          # SAS PROC GLM's own output, which has no equivalent standalone card).
          NULL
        }
      }

      # ── ANOVA Table ──────────────────────────────────────────────────────────
      # Helper to render the SAS-style ANOVA data.frame as an HTML table
      render_anova_table <- function(df, header_class = "table-primary") {
        tags$table(class = "table table-striped table-hover table-sm",
          tags$thead(class = header_class,
            tags$tr(
              tags$th("Source"),
              lapply(names(df), function(col) tags$th(col))
            )
          ),
          tags$tbody(
            lapply(1:nrow(df), function(i) {
              tags$tr(
                tags$td(style = "font-weight: bold;", rownames(df)[i]),
                lapply(1:ncol(df), function(j) {
                  val <- df[i, j]
                  col_name <- names(df)[j]
                  formatted <- if (is.numeric(val) && !is.na(val)) {
                    if (col_name %in% c("Pr(>F)", "Pr..F.", "p-value")) format.pval(val, digits = 4)
                    else if (col_name %in% c("Df", "NumDF", "DenDF", "numDF", "denDF")) as.character(round(val))
                    else if (col_name %in% c("Sum Sq", "Sum.Sq", "Mean Sq", "Mean.Sq", "Sum of Sq", "RSS")) sprintf("%.4f", val)
                    else if (col_name %in% c("F value", "F.value", "F-value")) sprintf("%.2f", val)
                    else sprintf("%.4f", val)
                  } else as.character(val)
                  tags$td(formatted)
                })
              )
            })
          )
        )
      }

      # ── GLM-style Model/Error/Corrected Total summary (fixed-effects lm only) ──
      # Not a standard summary for REML mixed models (there is no single-df
      # "Model" term to test against a "Corrected Total"); mixed models instead
      # get the Type 3 Tests of Fixed Effects table below (type3_tests_div).
      anova_tables_div <- if (is_mixed_model) NULL else render_type3_anova_card(param_result)

      # ── Type 3 Tests of Fixed Effects (mixed models only) ──────────────────
      # The per-term significance table (numDF/denDF/F-value/p-value for each
      # fixed effect: Sequence, Period, Treatment). This is the standard
      # SAS PROC MIXED "Type 3 Tests of Fixed Effects" output and is the
      # necessary complement to the treatment-contrast/variance-component cards
      # for a mixed-effects model, which gets no table from anova_tables_div
      # above. NOT shown for fixed-effects lm: there, `anova_comprehensive`
      # (anova_tables_div) already IS the full per-term Type III table
      # (Sequence/Subject(Sequence)/Period/Treatment/Residual) — adding this
      # too would duplicate it.
      type3_tests_div <- if (!is_mixed_model || is.null(param_result$type3_ss)) NULL else {
        tryCatch({
          type3_df <- as.data.frame(param_result$type3_ss)
          div(class = "card mb-3",
            div(class = "card-header",
              h6(class = "card-title mb-0", icon("table"), " Type 3 Tests of Fixed Effects"),
              tags$small(class = "text-muted",
                "Marginal (Type III) F-tests for each fixed effect from the mixed-effects model.")
            ),
            div(class = "card-body",
              div(class = "table-responsive", render_anova_table(type3_df, "table-primary"))
            )
          )
        }, error = function(e) NULL)
      }
      
      # ── Second table: "Tests of Hypotheses Using Type III MS for Subject(Sequence)" ──
      # (or "Subject(Group x Sequence)", gaining a Group row, when Group is in the model)
      seq_subj_test_div <- render_seq_subj_test_card(param_result)

      # ── Group x Treatment interaction (supportive analysis only, opt-in) ──
      # Populated only when the user checked "Test Group x Treatment
      # Interaction"; never affects pe_estimate/ci_lower/ci_upper above.
      group_treatment_div <- render_group_treatment_interaction_card(param_result)

      # ── Least Squares Means (SAS-style 3-table layout) ──
      lsmeans_cards <- render_lsmeans_cards(param_result, param_name)

      # ── Parameter Estimates (T − R): Estimate/SE/t/Pr>|t| ──
      # CI on the difference is shown in the Least Squares Means card above
      # (matching how SAS PROC GLM splits this across its own two tables).
      contrast_card <- render_parameter_estimates_card(
        param_result,
        ref_level  = param_result$lsmeans_result$ref_level  %||% "R",
        test_level = param_result$lsmeans_result$test_level %||% "T"
      )

      # Combine all components
      return(div(
        model_header,
        summary_card,
        class_info_card,
        model_summary_card,
        anova_tables_div,
        type3_tests_div,
        seq_subj_test_div,
        group_treatment_div,
        lsmeans_cards,
        contrast_card
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

        # ── RSABE / ABEL: show the purpose-built scaled formatter ──
        # The dropdown supplies a log name (e.g. "lnCmax"); the BE engine's
        # scaled output (`scaled_anova`) is keyed by base name ("Cmax"). Resolve
        # the key and route to the RSABE or replicateBE (ABEL) formatter so
        # the scaled variance components / limits / decision are shown instead of
        # a fixed-limit ABE ANOVA table.
        atype <- be_res$analysis_type %||% "ABE"
        if (atype %in% c("RSABE", "ABEL") && !is.null(be_res$scaled_anova$anova_results)) {
          eng <- be_res$scaled_anova$anova_results
          sel <- selected_anova_param()
          base <- sub("^(ln|log)", "", sel, ignore.case = TRUE)
          key <- if (!is.null(eng[[sel]])) sel else if (!is.null(eng[[base]])) base else names(eng)[1]
          pr <- eng[[key]]
          if (!is.null(pr)) {
            if (!is.null(pr$replicatebe_output)) return(format_replicatebe_anova_results(pr, base, be_res))
            if (!is.null(pr$s2_wR))             return(format_rsabe_anova_results(pr, base, be_res))
          }
        }

        # ── Parallel group design: no ANOVA, use t-test format ──
        if (identical(be_res$design, "parallel") || !is.null(be_res$statistical_results)) {
          param <- selected_anova_param()
          return(format_parallel_results(param, be_res))
        }
        
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
          # RSABE result — has intra-subject reference variance components
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
    })
    # Refresh button observer  
    observeEvent(input$refresh_anova, {
      # Force refresh of reactive values to re-trigger ANOVA display
      # Just trigger invalidation without arguments
      be_results()
      nca_results()
    })
    
    # =======================================================================
    # Descriptive Statistics Tab — one descriptive table per product
    # (split by period for replicate designs)
    # =======================================================================

    # Build a per-group descriptive-stats data frame for a single product.
    # `df` is the subset of subject_data for the group; `param_specs` is a
    # list of c(source_column, display_label) pairs.
    .build_group_stats <- function(df, param_specs) {
      rows <- lapply(param_specs, function(spec) {
        col   <- spec[1]
        label <- spec[2]
        if (!col %in% names(df)) return(NULL)
        x <- suppressWarnings(as.numeric(df[[col]]))
        x <- x[is.finite(x)]
        if (length(x) == 0) return(NULL)
        m  <- mean(x)
        s  <- stats::sd(x)
        cv <- if (is.finite(s) && is.finite(m) && m != 0) (s / m) * 100 else NA_real_
        data.frame(
          Variable  = label,
          N         = length(x),
          Mean      = round(m, 2),
          `Std Dev` = round(s, 2),
          Minimum   = round(min(x), 2),
          Median    = round(stats::median(x), 2),
          Maximum   = round(max(x), 2),
          `CV (%)`  = round(cv, 2),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    }

    # Grouping dropdown for the Descriptive Statistics tab. "Each Exposure"
    # (T1/T2/R1/R2) only makes sense for replicate designs — a standard 2x2
    # or parallel study has exactly one Test and one Reference exposure per
    # subject, so it's identical to "Each Treatment" and is left out entirely
    # rather than shown as a redundant option.
    output$summary_stats_group_by_ui <- renderUI({
      req(results_available())
      nca_res <- nca_results()
      is_rep <- FALSE
      if (!is.null(nca_res) && !is.null(nca_res$subject_data)) {
        sd_data <- nca_res$subject_data
        if (all(c("Subject", "Treatment") %in% names(sd_data))) {
          counts_by_subj_tr <- table(sd_data$Subject, sd_data$Treatment)
          is_rep <- any(counts_by_subj_tr > 1)
        }
      }

      choices <- list(
        "Each Sequence"  = "sequence",
        "Each Period"    = "period",
        "Each Treatment" = "treatment"
      )
      if (is_rep) {
        choices <- c(list("Each Exposure" = "formulation"), choices)
      }

      selectInput(session$ns("summary_stats_group_by"), label = NULL,
        choices = choices,
        selected = "treatment",
        width = "100%"
      )
    })

    output$summary_stats_tables <- renderUI({
      req(results_available())
      nca_res <- nca_results()
      if (is.null(nca_res) || is.null(nca_res$subject_data)) {
        return(div(class = "text-muted",
                   "Run the analysis to view descriptive statistics."))
      }

      sd_data <- nca_res$subject_data

      # Derive any columns that aren't directly in the NCA output but are
      # required for the summary table.
      if (all(c("AUC0inf", "AUC0t") %in% names(sd_data))) {
        sd_data$Residual_Area <- suppressWarnings(
          as.numeric(sd_data$AUC0inf) - as.numeric(sd_data$AUC0t)
        )
      }
      if (all(c("lambda_z", "lambda_z_se") %in% names(sd_data))) {
        lz <- suppressWarnings(as.numeric(sd_data$lambda_z))
        se <- suppressWarnings(as.numeric(sd_data$lambda_z_se))
        sd_data$Kel_Lower <- lz - 1.96 * se
        sd_data$Kel_Upper <- lz + 1.96 * se
      }

      # Display order and human-readable labels (per user spec).
      param_specs <- list(
        c("Tmax",                "Tmax"),
        c("Cmax",                "Cmax"),
        c("AUC0t",               "AUC0t"),
        c("AUC0inf",             "AUC0inf"),
        c("AUC_percent_extrap",  "AUC %extrap"),
        c("lambda_z",            "Lambda z"),
        c("lambda_z",            "Kel"),
        c("lambda_z_points",     "N (Lambda z / Kel)"),
        c("Kel_Lower",           "Kel lower"),
        c("Kel_Upper",           "Kel upper"),
        c("t_half",              "t \u00bd"),
        c("Residual_Area",       "Residual area")
      )
      # Keep only those whose source column actually exists in sd_data
      param_specs <- param_specs[vapply(param_specs,
                                         function(s) s[1] %in% names(sd_data),
                                         logical(1))]
      if (length(param_specs) == 0) {
        return(div(class = "text-muted", "No PK parameters available."))
      }

      treatments <- unique(as.character(sd_data$Treatment))
      treatments <- treatments[!is.na(treatments) & nzchar(treatments)]

      # Replicate detection: ANY subject has > 1 row for the same treatment.
      # In that case the design provides repeated T and/or R exposures per
      # subject (e.g. partial / full replicate).
      counts_by_subj_tr <- table(sd_data$Subject, sd_data$Treatment)
      is_replicate <- any(counts_by_subj_tr > 1)

      has_period <- "Period" %in% names(sd_data)
      has_seq    <- "Sequence" %in% names(sd_data)

      # Design-position exposure index (T1/T2/R1/R2) — rank each Period within
      # (Sequence, Treatment) using every period observed anywhere in that
      # sequence, not just what one subject happened to complete. A per-subject
      # chronological ordering would mislabel a subject with a missing early
      # exposure: e.g. an RTRT subject who only completed periods 1 (R) and 4
      # (T) belongs in R1 (period 1 is the sequence's first designed R slot)
      # and T2 (period 4 is the second designed T slot) — not R1/T1, which is
      # what counting "this subject's first observed R/T" in isolation would
      # produce.
      if (has_period) {
        key_cols <- if (has_seq) c("Sequence", "Treatment", "Period") else c("Treatment", "Period")
        design_map <- unique(sd_data[, key_cols, drop = FALSE])
        ord <- if (has_seq) {
          order(design_map$Sequence, design_map$Treatment, suppressWarnings(as.numeric(design_map$Period)))
        } else {
          order(design_map$Treatment, suppressWarnings(as.numeric(design_map$Period)))
        }
        design_map <- design_map[ord, , drop = FALSE]
        group_key  <- if (has_seq) interaction(design_map$Sequence, design_map$Treatment, drop = TRUE) else design_map$Treatment
        design_map$.expo <- ave(seq_len(nrow(design_map)), group_key, FUN = seq_along)
        sd_data <- merge(sd_data, design_map, by = key_cols, all.x = TRUE, sort = FALSE)
      } else {
        sd_data$.expo <- NA_integer_
      }

      group_by <- input$summary_stats_group_by %||% "treatment"

      groups <- list()
      if (group_by == "formulation" && is_replicate) {
        for (tr in treatments) {
          tr_rows <- sd_data[sd_data$Treatment == tr, , drop = FALSE]
          if (nrow(tr_rows) == 0) next
          if (all(is.na(tr_rows$.expo))) {
            # No Period column to rank by — fall back to original row order.
            tr_rows$.expo <- seq_len(nrow(tr_rows))
          }
          max_expo <- max(tr_rows$.expo, na.rm = TRUE)
          tr_label <- substr(tr, 1, 1)  # "T" or "R"
          for (k in seq_len(max_expo)) {
            sub <- tr_rows[!is.na(tr_rows$.expo) & tr_rows$.expo == k, , drop = FALSE]
            if (nrow(sub) == 0) next
            groups[[paste0(tr_label, k, " — ", tr,
                            " exposure ", k)]] <- sub
          }
        }
      } else if (group_by == "sequence" && has_seq) {
        # One table per Sequence, pooling every Treatment/Period assigned to it.
        seqs <- sort(unique(as.character(sd_data$Sequence)))
        seqs <- seqs[!is.na(seqs) & nzchar(seqs)]
        for (sq in seqs) {
          sub <- sd_data[as.character(sd_data$Sequence) == sq, , drop = FALSE]
          if (nrow(sub) == 0) next
          groups[[paste0("Sequence ", sq)]] <- sub
        }
      } else if (group_by == "period" && has_period) {
        # One table per Period, pooling every Sequence/Treatment observed then.
        pds <- suppressWarnings(as.numeric(unique(sd_data$Period)))
        pds <- sort(pds[!is.na(pds)])
        for (pd in pds) {
          sub <- sd_data[suppressWarnings(as.numeric(sd_data$Period)) == pd, , drop = FALSE]
          if (nrow(sub) == 0) next
          groups[[paste0("Period ", pd)]] <- sub
        }
      } else {
        # "Each Treatment": T1+T2 and R1+R2 pooled into a single T / R table
        # (also the fallback for parallel / standard 2x2 designs, and for
        # "formulation"/"sequence"/"period" modes when the underlying column
        # needed for that breakdown isn't available).
        for (tr in treatments) {
          sub <- sd_data[sd_data$Treatment == tr, , drop = FALSE]
          if (nrow(sub) == 0) next
          groups[[tr]] <- sub
        }
      }

      if (length(groups) == 0) {
        return(div(class = "text-muted", "No data available."))
      }

      tag_list <- lapply(seq_along(groups), function(i) {
        gname <- names(groups)[i]
        gdf   <- groups[[gname]]
        n_obs <- length(unique(gdf$Subject))
        stats_df <- .build_group_stats(gdf, param_specs)
        if (is.null(stats_df) || nrow(stats_df) == 0) return(NULL)

        div(class = "summary-card",
            style = "margin-bottom: 18px; padding: 14px;",
            div(style = "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 8px;",
                tags$h4(gname,
                        style = "margin: 0; color: #1f3b73;"),
                tags$span(paste0("N = ", n_obs, " subject",
                                  if (n_obs == 1) "" else "s"),
                          style = "color: #6c757d; font-size: 13px;")
            ),
            DT::datatable(
              stats_df,
              options = list(
                pageLength = 25, dom = 't', ordering = FALSE,
                scrollX = TRUE,
                columnDefs = list(list(className = 'dt-center',
                                        targets = '_all'))
              ),
              rownames = FALSE,
              class = 'compact stripe hover'
            ) %>%
              DT::formatRound(
                columns = intersect(c("Mean", "Std Dev", "Minimum", "Median", "Maximum", "CV (%)"),
                                     names(stats_df)),
                digits = 2
              )
        )
      })

      do.call(tagList, tag_list)
    })

    # =======================================================================
    # BE Comparison Tab (individual + overall T/R ratios, normal & log scale)
    # =======================================================================

    output$be_comp_parameter_ui <- renderUI({
      req(results_available())
      nca_res <- nca_results()
      if (is.null(nca_res) || is.null(nca_res$subject_data)) return(NULL)
      data <- nca_res$subject_data

      # Limit to the PK parameters selected for analysis + always include AUC0inf.
      # Exclude log variants (the Scale toggle handles ln internally).
      cfg <- analysis_config()
      selected <- cfg$selected_pk_params %||% c("Cmax", "AUC0t")
      # Strip any ln* entries from selected; add AUC0inf always
      selected_base <- selected[!startsWith(selected, "ln")]
      candidates <- unique(c(selected_base, "AUC0inf"))
      available <- intersect(candidates, names(data))
      if (length(available) == 0) {
        return(div(class = "alert alert-warning", "No PK parameters available."))
      }
      selectInput(
        inputId  = session$ns("be_comp_parameter"),
        label    = NULL,
        choices  = available,
        selected = available[1],
        width    = "100%"
      )
    })

    # Build the (subject × replicate) table for the active param + scale.
    # Returns list(table, overall, scale, param, is_replicate)
    be_comp_data <- reactive({
      req(input$be_comp_parameter)
      nca_res <- nca_results()
      req(nca_res, nca_res$subject_data)

      param <- input$be_comp_parameter
      scale <- input$be_comp_scale %||% "normal"

      base <- calculate_pk_comparison(nca_res, param)
      if (is.null(base) || !is.null(base$error)) return(NULL)

      ind <- base$individual_data

      if (scale == "log") {
        # Log-transform per-subject T and R values; ratio becomes the
        # difference on log scale, overall geometric ratio = exp(mean diff).
        if (base$is_replicate) {
          ind$T1     <- suppressWarnings(log(ind$T1))
          ind$T2     <- suppressWarnings(log(ind$T2))
          ind$T_mean <- suppressWarnings(log(ind$T_mean))
          ind$R1     <- suppressWarnings(log(ind$R1))
          ind$R2     <- suppressWarnings(log(ind$R2))
          ind$R_mean <- suppressWarnings(log(ind$R_mean))
          ind$Ratio  <- ind$T_mean - ind$R_mean
        } else {
          ind$Test      <- suppressWarnings(log(ind$Test))
          ind$Reference <- suppressWarnings(log(ind$Reference))
          ind$Ratio     <- ind$Test - ind$Reference
        }
      }

      # Overall summary
      ratios <- ind$Ratio[is.finite(ind$Ratio)]
      n      <- length(ratios)
      if (scale == "normal") {
        # Arithmetic mean of T/R ratios + geometric mean ratio
        arith_ratio <- if (n > 0) mean(ratios) else NA_real_
        # GMR via log of ratios
        log_r <- suppressWarnings(log(ratios))
        log_r <- log_r[is.finite(log_r)]
        gmr <- if (length(log_r) > 0) exp(mean(log_r)) else NA_real_
        cv  <- if (n > 1) (sd(ratios) / mean(ratios)) * 100 else NA_real_
        overall <- list(
          n = n,
          mean_label = "Arithmetic Mean Ratio",
          mean_val   = arith_ratio,
          gmr        = gmr,
          cv_pct     = cv
        )
      } else {
        # Log scale: ratio is a difference; mean(diff) -> geometric ratio
        mean_diff <- if (n > 0) mean(ratios) else NA_real_
        gmr <- if (is.finite(mean_diff)) exp(mean_diff) else NA_real_
        cv  <- if (n > 1) (sd(ratios) / abs(mean(ratios))) * 100 else NA_real_
        overall <- list(
          n = n,
          mean_label = "Mean ln(T) − ln(R)",
          mean_val   = mean_diff,
          gmr        = gmr,
          cv_pct     = cv
        )
      }

      list(individual = ind, overall = overall, scale = scale,
           param = param, is_replicate = base$is_replicate)
    })

    output$be_comp_individual_table <- DT::renderDataTable({
      d <- be_comp_data()
      validate(need(!is.null(d), "Select a parameter to view T/R ratios."))

      ind <- d$individual

      # ln-transformed (log scale) values get 4 decimals; normal-scale values
      # stay capped at 2 — the log values are typically small (e.g. ~0.05-0.5)
      # where 2 decimals loses meaningful precision.
      value_digits <- if (d$scale == "log") 4 else 2

      if (d$is_replicate) {
        display <- ind[, c("Subject", "T1", "T2", "T_mean",
                            "R1", "R2", "R_mean", "Ratio"),
                        drop = FALSE]
        display[, -1] <- lapply(display[, -1], function(x) round(x, value_digits))
        col_names <- c("Subject", "T1", "T2", "T mean",
                        "R1", "R2", "R mean",
                        if (d$scale == "log") "ln(T) − ln(R)" else "T/R Ratio")
      } else {
        display <- ind[, c("Subject", "Test", "Reference", "Ratio"),
                        drop = FALSE]
        display[, -1] <- lapply(display[, -1], function(x) round(x, value_digits))
        col_names <- c("Subject",
                        if (d$scale == "log") "ln(Test)"      else "Test",
                        if (d$scale == "log") "ln(Reference)" else "Reference",
                        if (d$scale == "log") "ln(T) − ln(R)" else "T/R Ratio")
      }

      DT::datatable(
        display,
        options = list(
          pageLength = 25, dom = 'tp', ordering = FALSE, scrollX = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = '_all'))
        ),
        rownames = FALSE,
        colnames = col_names
      ) %>%
        DT::formatRound(columns = setdiff(names(display), "Subject"), digits = value_digits)
    })

    output$be_comp_overall_summary <- renderUI({
      req(results_available())
      param <- req(input$be_comp_parameter)
      scale <- input$be_comp_scale %||% "normal"

      nca_res <- nca_results()
      if (is.null(nca_res)) return(NULL)
      sd <- if (is.data.frame(nca_res)) nca_res else nca_res$subject_data
      if (is.null(sd)) return(NULL)

      # Use one row per subject-treatment (NCA params are duplicated across time
      # points in concentration data; take unique Subject+Treatment combos).
      if ("Time" %in% names(sd)) {
        sd <- sd[!duplicated(sd[, c("Subject", "Treatment")]), ]
      }

      # ── 1. Regular (arithmetic) mean from raw data ──────────────────────
      test_vals <- sd[[param]][sd$Treatment %in% c("T", "Test") & !is.na(sd[[param]]) & sd[[param]] > 0]
      ref_vals  <- sd[[param]][sd$Treatment %in% c("R", "Reference") & !is.na(sd[[param]]) & sd[[param]] > 0]
      n_T <- length(test_vals)
      n_R <- length(ref_vals)

      arith_T <- if (n_T > 0) mean(test_vals) else NA_real_
      arith_R <- if (n_R > 0) mean(ref_vals)  else NA_real_
      arith_ratio <- if (!is.na(arith_T) && !is.na(arith_R) && arith_R > 0) arith_T / arith_R else NA_real_

      # ── 2. Geometric LS means from ANOVA (via be_results) ───────────────
      # Map base param → ln param name — standard ABE's confidence_intervals
      # is keyed by the ln-prefixed name (e.g. "lnCmax"), but RSABE/ABEL key
      # theirs by the base name (e.g. "Cmax") — try both so this works for
      # every analysis type, not just standard ABE.
      ln_param_map <- c(
        Cmax   = "lnCmax",
        AUC0t  = "lnAUC0t",
        AUC0inf = "lnAUC0inf",
        pAUC   = "lnpAUC"
      )
      ln_param <- ln_param_map[param]

      lsmean_T     <- NA_real_
      lsmean_R     <- NA_real_
      lsmean_ratio <- NA_real_
      lsmean_ln_T  <- NA_real_
      lsmean_ln_R  <- NA_real_
      lsmean_diff  <- NA_real_

      be_res_val <- tryCatch(be_results(), error = function(e) NULL)
      ci_all <- be_res_val$confidence_intervals
      ci_ln <- NULL
      if (!is.null(ci_all)) {
        if (!is.null(ln_param) && !is.na(ln_param) && !is.null(ci_all[[ln_param]])) {
          ci_ln <- ci_all[[ln_param]]
        } else if (!is.null(ci_all[[param]])) {
          ci_ln <- ci_all[[param]]
        }
      }
      if (!is.null(ci_ln)) {
        pe <- ci_ln$point_estimate  # e.g. 106.23 means 106.23%
        if (!is.null(pe) && is.finite(pe) && pe > 0 && n_T > 0 && n_R > 0) {
          # Derive individual LS means directly on the log scale:
          # grand_ln = (mean(ln T) + mean(ln R)) / 2
          # lsmean_ln_T = grand_ln + log(PE/100)/2
          # lsmean_ln_R = grand_ln - log(PE/100)/2
          grand_ln    <- (mean(log(test_vals)) + mean(log(ref_vals))) / 2
          half_coef   <- log(pe / 100) / 2
          lsmean_ln_T <- grand_ln + half_coef
          lsmean_ln_R <- grand_ln - half_coef
          lsmean_diff <- lsmean_ln_T - lsmean_ln_R   # == log(pe/100), the ANOVA treatment coefficient
          lsmean_T    <- exp(lsmean_ln_T)
          lsmean_R    <- exp(lsmean_ln_R)
          lsmean_ratio <- pe / 100
        }
      }

      # ── 3. Render ────────────────────────────────────────────────────────
      # Normal scale shows two rows: the plain arithmetic mean of the raw
      # data, and the geometric mean derived from the ANOVA LS means
      # (back-transformed). ln scale shows only the LS Mean, in the log
      # domain directly (not log() applied cosmetically to a back-transformed
      # number) — an arithmetic "mean of logs" row would just duplicate the
      # LS mean under a balanced design and diverge from it under an
      # unbalanced one, which is more confusing than useful here.
      is_log <- identical(scale, "log")

      fmt_n <- function(x, digits = 2) {
        if (is.null(x) || is.na(x) || !is.finite(x)) return("—")
        formatC(x, format = "f", digits = digits, big.mark = ",")
      }
      fmt_ratio <- function(x) {
        if (is.null(x) || is.na(x) || !is.finite(x)) return("—")
        sprintf("%.2f%%", x * 100)
      }
      fmt_ln <- function(x, digits = 4) {
        if (is.null(x) || is.na(x) || !is.finite(x)) return("—")
        formatC(x, format = "f", digits = digits, big.mark = ",")
      }

      has_ls <- !is.na(lsmean_T) && !is.na(lsmean_R)

      test_hdr  <- if (is_log) "ln(Test)"      else "Test"
      ref_hdr   <- if (is_log) "ln(Reference)" else "Reference"
      ratio_hdr <- if (is_log) "ln(T) − ln(R)" else "Ratio (T/R)"

      mean_row_sub <- sprintf("(n = %d / %d)", n_T, n_R)

      ls_row_sub <- if (is_log) "(ANOVA, ln scale)" else "(ANOVA LS Means, back-transformed)"
      ls_test_val  <- if (is_log) fmt_ln(lsmean_ln_T) else fmt_n(lsmean_T)
      ls_ref_val   <- if (is_log) fmt_ln(lsmean_ln_R) else fmt_n(lsmean_R)
      ls_ratio_val <- if (is_log) fmt_ln(lsmean_diff) else fmt_ratio(lsmean_ratio)

      tagList(
        tags$table(
          class = "table table-bordered table-sm",
          style = "margin-bottom: 0; font-size: 14px;",
          tags$thead(
            style = "background-color: #f0f4f8;",
            tags$tr(
              tags$th(style = "width: 34%;", ""),
              tags$th(style = "width: 22%; text-align: center;", test_hdr),
              tags$th(style = "width: 22%; text-align: center;", ref_hdr),
              tags$th(style = "width: 22%; text-align: center;", ratio_hdr)
            )
          ),
          tags$tbody(
            if (!is_log) {
              tags$tr(
                tags$td(tags$strong("Mean"),
                        tags$div(style = "font-size: 11px; color: #6c757d;", mean_row_sub)),
                tags$td(style = "text-align: center;", fmt_n(arith_T)),
                tags$td(style = "text-align: center;", fmt_n(arith_R)),
                tags$td(style = "text-align: center; font-weight: 600;", fmt_ratio(arith_ratio))
              )
            },
            if (has_ls) {
              tags$tr(
                tags$td(tags$strong(if (is_log) "LS Mean" else "Geometric Mean"),
                        tags$div(style = "font-size: 11px; color: #6c757d;", ls_row_sub)),
                tags$td(style = "text-align: center;", ls_test_val),
                tags$td(style = "text-align: center;", ls_ref_val),
                tags$td(style = "text-align: center; font-weight: 600;", ls_ratio_val)
              )
            } else if (is_log) {
              tags$tr(
                tags$td(colspan = 4, class = "text-muted",
                        "LS Mean not available for this parameter (no ANOVA result found).")
              )
            }
          )
        )
      )
    })

    observeEvent(input$refresh_be_comp, {
      nca_results()
    })

  })
}
