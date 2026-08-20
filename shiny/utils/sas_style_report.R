# SAS-style Bioequivalence Analysis Report
#
# Self-contained HTML report mirroring the structure of a typical SAS PROC GLM
# bioequivalence annexure. Sections (in order):
#
#   1. Subject-level listings (untransformed and log-transformed)
#   2. Descriptive statistics (Treatment x Period, Sequence, Period, pooled
#      by Treatment)
#   3. Per-product ANOVA on log scale (Reference, and Test if estimable)
#      with Type I and Type III sums of squares
#   4. Full ANOVA on log-transformed PK with Treatment / Sequence / Period /
#      Subject(Sequence): Type I, Type III, LS Means, treatment difference,
#      parameter estimate
#   5. Intra-subject variability summary
#   6. Final BE conclusion (LSM Test/Ref, GeoLSMeans, ratio, 90% CI, power,
#      Intra CV%-Ref, acceptance limits, BE Yes/No)
#
# All ANOVA, LS Means and CI work is performed on log-transformed values;
# descriptive statistics are presented on untransformed values. Where possible
# the report reuses results already attached to be_results / nca_results;
# additional per-product ANOVA tables and LS Means are computed on the fly
# from subject-level data via lm() refits.

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------
.fmt_num <- function(x, digits = 4) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) || !is.finite(x)) return("\u2014")
  formatC(x, format = "f", digits = digits, big.mark = "")
}
.fmt_pct <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("\u2014")
  paste0(formatC(x, format = "f", digits = digits), "%")
}
.fmt_int <- function(x) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("\u2014")
  formatC(round(x), format = "d", big.mark = "")
}
.fmt_p <- function(x) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) || !is.finite(x)) return("\u2014")
  if (x < 0.0001) return("&lt;.0001")
  formatC(x, format = "f", digits = 4)
}
.html_escape <- function(s) {
  if (is.null(s)) return("")
  s <- as.character(s)
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s
}

.df_to_html <- function(df, caption = NULL, escape = TRUE) {
  if (is.null(df) || nrow(df) == 0) return("")
  cols <- colnames(df)
  thead <- paste0("<thead><tr>",
                  paste0("<th>", vapply(cols, .html_escape, ""), "</th>",
                         collapse = ""),
                  "</tr></thead>")
  rows <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    cells <- vapply(cols, function(cn) {
      v <- df[i, cn]
      if (escape) .html_escape(v) else as.character(v)
    }, character(1))
    rows[i] <- paste0("<tr>",
                      paste0("<td>", cells, "</td>", collapse = ""),
                      "</tr>")
  }
  tbody <- paste0("<tbody>", paste(rows, collapse = ""), "</tbody>")
  cap <- if (!is.null(caption)) paste0("<caption>", .html_escape(caption),
                                       "</caption>") else ""
  paste0("<table class='sas-table'>", cap, thead, tbody, "</table>")
}

# Display label for a parameter: "lnCmax" -> "Cmax", etc.
.display_param <- function(p) {
  if (exists("log_param_to_display_name", mode = "function")) {
    return(log_param_to_display_name(p))
  }
  if (startsWith(p, "ln")) substring(p, 3) else p
}

# Pull the subject-level NCA data frame in a tolerant way and standardise
# common column casing.
.get_subject_data <- function(nca_results) {
  sd <- if (is.data.frame(nca_results)) nca_results
        else nca_results$subject_data %||% nca_results$parameters
  if (is.null(sd) || !is.data.frame(sd) || nrow(sd) == 0) return(NULL)
  # Normalise common alias columns
  rename_map <- c(subj = "Subject", subject = "Subject",
                  tmt = "Treatment", treat = "Treatment",
                  treatment = "Treatment", FORM = "Treatment",
                  period = "Period", seq = "Sequence",
                  sequence = "Sequence")
  for (old in names(rename_map)) {
    new <- rename_map[[old]]
    if (old %in% names(sd) && !(new %in% names(sd))) {
      names(sd)[names(sd) == old] <- new
    }
  }
  sd
}

# Identify available numeric PK parameter columns.
.pk_columns <- function(sd) {
  if (is.null(sd)) return(character(0))
  id_cols <- c("Subject", "Treatment", "Period", "Sequence", "Time",
               "Concentration", "Group", "dose", "Dose", "Formulation")
  cand <- setdiff(names(sd), id_cols)
  cand <- cand[vapply(cand, function(cn) is.numeric(sd[[cn]]), logical(1))]
  preferred <- c("Cmax", "AUC0t", "AUClast", "AUC0inf", "AUCinf",
                 "Tmax", "T12", "t_half", "THALF", "Kel", "KEL",
                 "Lambda_z", "lambda_z")
  ordered <- intersect(preferred, cand)
  c(ordered, setdiff(cand, ordered))
}

# Pretty header columns for subject listings
.listing_id_cols <- function(sd) {
  out <- intersect(c("Subject", "Sequence", "Period", "Treatment"), names(sd))
  if (length(out) == 0) names(sd)[1:min(4, ncol(sd))] else out
}

# ---------------------------------------------------------------------------
# Descriptive statistics helpers
# ---------------------------------------------------------------------------
.describe_one_group <- function(sub, pk_cols) {
  rows <- lapply(pk_cols, function(p) {
    v <- suppressWarnings(as.numeric(sub[[p]]))
    v <- v[is.finite(v)]
    if (length(v) == 0) {
      return(data.frame(Variable = p, N = 0L,
                        Mean = NA_real_, SD = NA_real_,
                        Min = NA_real_, Median = NA_real_, Max = NA_real_,
                        CV_pct = NA_real_, stringsAsFactors = FALSE))
    }
    m <- mean(v); sdv <- if (length(v) > 1) sd(v) else NA_real_
    cv <- if (!is.na(sdv) && m != 0) sdv / m * 100 else NA_real_
    data.frame(Variable = p, N = length(v), Mean = m, SD = sdv,
               Min = min(v), Median = median(v), Max = max(v),
               CV_pct = cv, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.format_descr <- function(df) {
  # NCA parameter values shown at 2 decimal places throughout (matches the SAS
  # reference output's display convention, and the per-subject listing below —
  # these were previously inconsistent, showing the same raw values at 4
  # decimals here but 2 decimals in .section_subject_listing).
  data.frame(
    Variable = df$Variable,
    N = vapply(df$N, .fmt_int, ""),
    Mean = vapply(df$Mean, .fmt_num, "", digits = 2),
    `Std Dev` = vapply(df$SD, .fmt_num, "", digits = 2),
    Minimum = vapply(df$Min, .fmt_num, "", digits = 2),
    Median = vapply(df$Median, .fmt_num, "", digits = 2),
    Maximum = vapply(df$Max, .fmt_num, "", digits = 2),
    `Coeff of Variation (%)` = vapply(df$CV_pct, .fmt_num, "", digits = 2),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

.section_descriptives_grouped <- function(sd, pk_cols, group_cols, group_label) {
  if (is.null(sd) || length(pk_cols) == 0) return("")
  group_cols <- intersect(group_cols, names(sd))
  if (length(group_cols) == 0) return("")
  if (any(vapply(group_cols, function(g) length(unique(stats::na.omit(sd[[g]]))) < 1,
                 logical(1)))) return("")
  # Build group key
  key <- do.call(paste, c(lapply(group_cols, function(g) sd[[g]]), sep = "|"))
  uniq <- sort(unique(key))
  if (length(uniq) == 0) return("")
  blocks <- character(length(uniq))
  for (i in seq_along(uniq)) {
    sub <- sd[key == uniq[i], , drop = FALSE]
    label_parts <- strsplit(uniq[i], "|", fixed = TRUE)[[1]]
    label <- paste(paste0(group_cols, " = ", label_parts), collapse = ", ")
    tab <- .describe_one_group(sub, pk_cols)
    blocks[i] <- paste0(
      "<h4 class='sas-subhead'>", .html_escape(label),
      " &mdash; N<sub>obs</sub> = ", .fmt_int(nrow(sub)), "</h4>",
      .df_to_html(.format_descr(tab))
    )
  }
  paste(blocks, collapse = "\n")
}

# "Each Exposure" (T1/T2/R1/R2) grouping — replicate designs only. Ranks
# each Period within (Sequence, Treatment) using every period observed
# anywhere in that sequence (not just what one subject completed), so a
# subject missing an early exposure is still bucketed by its PLANNED design
# position — same design-map logic as the interactive Descriptive Statistics
# tab's "Each Exposure" option (results_dashboard_server.R), so the report
# matches it exactly.
.section_descriptives_exposure <- function(sd, pk_cols) {
  if (is.null(sd) || length(pk_cols) == 0) return("")
  if (!all(c("Treatment", "Period") %in% names(sd))) return("")
  counts_by_subj_tr <- table(sd$Subject, sd$Treatment)
  if (!any(counts_by_subj_tr > 1)) return("")  # not a replicate design

  has_seq <- "Sequence" %in% names(sd)
  key_cols <- if (has_seq) c("Sequence", "Treatment", "Period") else c("Treatment", "Period")
  design_map <- unique(sd[, key_cols, drop = FALSE])
  ord <- if (has_seq) {
    order(design_map$Sequence, design_map$Treatment, suppressWarnings(as.numeric(design_map$Period)))
  } else {
    order(design_map$Treatment, suppressWarnings(as.numeric(design_map$Period)))
  }
  design_map <- design_map[ord, , drop = FALSE]
  group_key <- if (has_seq) interaction(design_map$Sequence, design_map$Treatment, drop = TRUE) else design_map$Treatment
  design_map$.expo <- ave(seq_len(nrow(design_map)), group_key, FUN = seq_along)
  sd2 <- merge(sd, design_map, by = key_cols, all.x = TRUE, sort = FALSE)

  treatments <- sort(unique(as.character(sd2$Treatment)))
  blocks <- character(0)
  for (tr in treatments) {
    tr_rows <- sd2[sd2$Treatment == tr, , drop = FALSE]
    if (nrow(tr_rows) == 0) next
    max_expo <- max(tr_rows$.expo, na.rm = TRUE)
    tr_label <- substr(tr, 1, 1)
    for (k in seq_len(max_expo)) {
      sub <- tr_rows[!is.na(tr_rows$.expo) & tr_rows$.expo == k, , drop = FALSE]
      if (nrow(sub) == 0) next
      tab <- .describe_one_group(sub, pk_cols)
      blocks <- c(blocks, paste0(
        "<h4 class='sas-subhead'>", tr_label, k, " — ", .html_escape(tr),
        " exposure ", k, " &mdash; N<sub>obs</sub> = ", .fmt_int(nrow(sub)), "</h4>",
        .df_to_html(.format_descr(tab))
      ))
    }
  }
  paste(blocks, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Subject-level listings
# ---------------------------------------------------------------------------
.section_subject_listing <- function(sd, pk_cols, log_only = FALSE) {
  if (is.null(sd) || nrow(sd) == 0) return("")
  id_cols <- .listing_id_cols(sd)
  if (log_only) {
    log_pk <- intersect(c("Cmax", "AUC0t", "AUClast", "AUC0inf", "AUCinf"), pk_cols)
    if (length(log_pk) == 0) return("")
    show <- sd[, c(id_cols, log_pk), drop = FALSE]
    for (p in log_pk) {
      v <- suppressWarnings(as.numeric(show[[p]]))
      v[!is.finite(v) | v <= 0] <- NA
      show[[p]] <- log(v)
    }
    names(show)[(length(id_cols) + 1):ncol(show)] <-
      paste0("ln", names(show)[(length(id_cols) + 1):ncol(show)])
  } else {
    show <- sd[, c(id_cols, pk_cols), drop = FALSE]
  }
  # Pretty-format numerics
  pretty <- show
  for (cn in names(pretty)) {
    if (is.numeric(pretty[[cn]])) {
      pretty[[cn]] <- vapply(pretty[[cn]], .fmt_num, "",
                             digits = if (log_only) 4 else 2)
    } else {
      pretty[[cn]] <- vapply(pretty[[cn]], function(x) .html_escape(x),
                             character(1))
    }
  }
  pretty <- cbind(Obs = seq_len(nrow(pretty)), pretty)
  pretty$Obs <- as.character(pretty$Obs)
  .df_to_html(pretty)
}

# Render a Source/DF/SS/MS/F/Pr>F table (accepts a data.frame with either a
# "Source" column or rownames, and any of the common Df/Sum Sq/Mean Sq/F
# value/Pr(>F) column-name variants — used for both Type III SS and the
# Subject(Sequence) error-term test tables).
.render_ss_table <- function(ss_df, n_obs = NULL) {
  if (is.null(ss_df) || nrow(ss_df) == 0) return("")
  cols <- names(ss_df)
  src_col <- if ("Source" %in% cols) "Source" else NULL
  if (is.null(src_col)) {
    src <- rownames(ss_df)
  } else { src <- ss_df[[src_col]] }
  df_v   <- ss_df[[intersect(c("Df", "DF", "numDF"), cols)[1]]] %||% rep(NA, nrow(ss_df))
  ss_v   <- ss_df[["Sum Sq"]] %||% rep(NA, nrow(ss_df))
  ms_v   <- ss_df[["Mean Sq"]] %||%
            (suppressWarnings(as.numeric(ss_v) / as.numeric(df_v)))
  fv_v   <- ss_df[[intersect(c("F value", "F-value"), cols)[1]]] %||% rep(NA, nrow(ss_df))
  pv_v   <- ss_df[[intersect(c("Pr(>F)", "p-value"), cols)[1]]] %||% rep(NA, nrow(ss_df))
  out <- data.frame(
    Source = vapply(src, .html_escape, ""),
    DF     = vapply(df_v, .fmt_int, ""),
    `Sum of Squares` = vapply(ss_v, .fmt_num, "", digits = 8),
    `Mean Square`    = vapply(ms_v, .fmt_num, "", digits = 8),
    `F Value` = vapply(fv_v, .fmt_num, "", digits = 2),
    `Pr > F`  = vapply(pv_v, .fmt_p, ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  .df_to_html(out)
}

# ---------------------------------------------------------------------------
# Real-engine ANOVA output (Sections 3+) — sourced from the SAME per-parameter
# results the interactive ANOVA Results tab uses (results_dashboard_server.R:
# render_type3_anova_card / render_seq_subj_test_card / render_lsmeans_cards /
# render_parameter_estimates_card / render_model_summary_card), so the report
# always shows the exact same numbers as what's on screen. This REPLACES the
# report's previous approach of independently refitting lm()/drop1() models
# from raw subject data, which duplicated (and could silently diverge from,
# e.g. the Sequence Type III SS bug fixed in build_crossover_anova_tables())
# the app's own validated computation.
# ---------------------------------------------------------------------------

# Resolve the real per-parameter ANOVA engine result for a PK parameter.
# Mirrors output$anova_display's own resolution order (results_dashboard_
# server.R): RSABE/ABEL's scaled-analysis engine first (be_results$scaled_
# anova$anova_results), falling back to the plain ABE engine (be_results$
# anova_results$anova_results); tries both the ln-prefixed and base name,
# since the two engines key their results differently.
.resolve_param_result <- function(be_results, param) {
  base <- sub("^(ln|log)", "", param, ignore.case = TRUE)
  atype <- be_results$analysis_type %||% "ABE"

  try_engine <- function(eng) {
    if (is.null(eng)) return(NULL)
    for (key in c(param, base)) {
      cand <- eng[[key]]
      if (!is.null(cand) && is.null(cand$error)) return(cand)
    }
    NULL
  }

  if (atype %in% c("RSABE", "ABEL")) {
    hit <- try_engine(be_results$scaled_anova$anova_results)
    # Mirrors the interactive ANOVA Results tab's dispatch exactly (see
    # output$anova_display, results_dashboard_server.R): a scaled_anova entry
    # only represents a genuine RSABE/ABEL-scaled result when it carries
    # replicatebe_output (ABEL) or s2_wR (RSABE). Parameters not selected for
    # scaling (e.g. a secondary PK parameter under ABEL) get a fixed-ABE
    # fallback entry there with model = NULL — the interactive tab falls
    # through to the plain ABE engine's copy (which has the real fitted
    # model) for those, so the report must too, or it silently drops their
    # entire ANOVA section.
    if (!is.null(hit) && (!is.null(hit$replicatebe_output) || !is.null(hit$s2_wR))) {
      return(hit)
    }
  }
  fallback <- try_engine(be_results$anova_results$anova_results)
  if (!is.null(fallback)) return(fallback)
  if (atype %in% c("RSABE", "ABEL")) try_engine(be_results$scaled_anova$anova_results) else NULL
}

# "Class Level Information" — from the fitted model's own model frame
# (subj/drug/prd/seq columns), identical to the interactive tab's card.
.render_class_info_real <- function(param_result) {
  mdl <- param_result$model
  if (is.null(mdl) || is.null(mdl$model)) return("")
  mdf <- mdl$model
  want <- intersect(c("subj", "drug", "prd", "seq", "grp"), names(mdf))
  if (length(want) == 0) return("")
  rows <- lapply(want, function(v) {
    lvls <- unique(as.character(mdf[[v]]))
    lvls <- lvls[!is.na(lvls)]
    label <- switch(v, "subj" = "Subject", "drug" = "Treatment",
                     "prd" = "Period", "seq" = "Sequence", "grp" = "Group", v)
    data.frame(Class = label, Levels = length(lvls),
               Values = paste(lvls, collapse = " "), stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  paste0(
    .df_to_html(tab),
    "<table class='sas-table sas-meta-tbl'>",
    "<tr><th>Number of Observations Used</th><td>",
    .fmt_int(param_result$n_observations), "</td></tr></table>"
  )
}

# SAS PROC GLM's own first two output tables — overall Model/Error/Corrected
# Total F-test, plus R-Square/Coeff Var/Root MSE/Mean — computed generically
# from param_result$model/anova() (same as render_model_summary_card()).
.render_model_summary_real <- function(param_result, dep_label) {
  at <- tryCatch(as.data.frame(param_result$anova), error = function(e) NULL)
  if (is.null(at) || !"Df" %in% names(at) || !"Sum Sq" %in% names(at)) return("")

  n_rows <- nrow(at)
  err_df <- at$Df[n_rows]; err_ss <- at$`Sum Sq`[n_rows]
  mdl_df <- sum(at$Df[-n_rows], na.rm = TRUE)
  mdl_ss <- sum(at$`Sum Sq`[-n_rows], na.rm = TRUE)
  tot_df <- mdl_df + err_df; tot_ss <- mdl_ss + err_ss
  mdl_ms <- if (mdl_df > 0) mdl_ss / mdl_df else NA
  err_ms <- if (err_df > 0) err_ss / err_df else NA
  f_val  <- if (!is.na(mdl_ms) && !is.na(err_ms) && err_ms > 0) mdl_ms / err_ms else NA
  p_val  <- if (!is.na(f_val) && mdl_df > 0 && err_df > 0) pf(f_val, mdl_df, err_df, lower.tail = FALSE) else NA

  r_squared  <- if (!is.na(tot_ss) && tot_ss > 0) mdl_ss / tot_ss else NA
  root_mse   <- if (!is.na(err_ms) && err_ms >= 0) sqrt(err_ms) else NA
  param_mean <- tryCatch(mean(fitted(param_result$model), na.rm = TRUE), error = function(e) NA_real_)
  cv_pct     <- if (!is.na(root_mse) && !is.na(param_mean) && param_mean != 0) root_mse / param_mean * 100 else NA

  top <- data.frame(
    Source = c("Model", "Error", "Corrected Total"),
    DF = c(.fmt_int(mdl_df), .fmt_int(err_df), .fmt_int(tot_df)),
    `Sum of Squares` = c(.fmt_num(mdl_ss, 8), .fmt_num(err_ss, 8), .fmt_num(tot_ss, 8)),
    `Mean Square` = c(.fmt_num(mdl_ms, 8), .fmt_num(err_ms, 8), ""),
    `F Value` = c(.fmt_num(f_val, 2), "", ""),
    `Pr > F` = c(.fmt_p(p_val), "", ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  bottom <- data.frame(
    `R-Square` = .fmt_num(r_squared, 6),
    `Coeff Var` = .fmt_num(cv_pct, 6),
    `Root MSE` = .fmt_num(root_mse, 6),
    Mean = .fmt_num(param_mean, 6),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(bottom)[4] <- paste0(dep_label, " Mean")
  paste0(.df_to_html(top), .df_to_html(bottom))
}

# Type III SS table — straight from param_result$anova_comprehensive (built
# by build_crossover_anova_tables() via emmeans::joint_tests(), verified to
# exactly reproduce SAS PROC GLM's Type III SS including the Sequence row).
.render_type3_real <- function(param_result) {
  comp_df <- param_result$anova_comprehensive
  if (is.null(comp_df)) return("")
  .render_ss_table(comp_df)
}

# "Tests of Hypotheses Using the Type III MS for Subject(Sequence) as an
# Error Term" — the Sequence effect re-tested against the between-subject MS.
.render_seq_subj_test_real <- function(param_result) {
  seq_test <- tryCatch(param_result$subj_seq_analysis$hypothesis_tests$seq,
                        error = function(e) NULL)
  if (is.null(seq_test) || is.null(seq_test$f_value) || is.na(seq_test$f_value)) return("")
  tab <- data.frame(
    Source = "Sequence",
    DF = .fmt_int(seq_test$df),
    `Type III SS` = .fmt_num(seq_test$ss, 6),
    `Mean Square` = .fmt_num(seq_test$ms, 6),
    `F Value` = .fmt_num(seq_test$f_value, 2),
    `Pr > F` = .fmt_p(seq_test$p_value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  .df_to_html(tab)
}

# Least Squares Means — the same 3-table SAS layout as the interactive tab's
# render_lsmeans_cards(): per-FORM LSMean + H0 p-value, per-FORM LSMean + CI,
# and the FORM difference + CI. Sourced from param_result$lsmeans_result
# (compute_lsmeans_ci(), via emmeans — includes proper per-arm CIs, not a
# crude predict()-average).
.render_lsmeans_real <- function(param_result, dep_label) {
  lsm <- param_result$lsmeans_result
  if (is.null(lsm)) return("")

  tcoef <- param_result$treatment_coef %||% NA
  tse   <- param_result$treatment_se   %||% NA
  tdf   <- param_result$residual_df    %||% lsm$df_ref %||% NA
  level <- lsm$level %||% 0.90
  ci_pct <- sprintf("%.0f%%", level * 100)
  has_diff <- !is.na(tcoef) && !is.na(tse) && !is.na(tdf) && tdf > 0

  p_diff <- if (has_diff) 2 * pt(abs(tcoef / tse), tdf, lower.tail = FALSE) else NA

  tab1 <- data.frame(
    FORM = c(lsm$test_level, lsm$ref_level),
    LSMEAN = c(.fmt_num(lsm$lsmean_test_log, 6), .fmt_num(lsm$lsmean_ref_log, 6)),
    x = c(if (has_diff) .fmt_p(p_diff) else "", ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(tab1)[3] <- "H0:LSMean1=LSMean2 Pr > |t|"

  tab2 <- data.frame(
    FORM = c(lsm$test_level, lsm$ref_level),
    LSMEAN = c(.fmt_num(lsm$lsmean_test_log, 6), .fmt_num(lsm$lsmean_ref_log, 6)),
    Lo = c(.fmt_num(lsm$ci_lower_test_log, 6), .fmt_num(lsm$ci_lower_ref_log, 6)),
    Hi = c(.fmt_num(lsm$ci_upper_test_log, 6), .fmt_num(lsm$ci_upper_ref_log, 6)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(tab2)[3:4] <- paste0(ci_pct, c(" CL Lower", " CL Upper"))

  diff_html <- if (has_diff) {
    t_crit <- qt(1 - (1 - level) / 2, tdf)
    diff_lo <- tcoef - t_crit * tse
    diff_hi <- tcoef + t_crit * tse
    tab3 <- data.frame(
      i = "1", j = "2",
      Diff = .fmt_num(tcoef, 6),
      Lo = .fmt_num(diff_lo, 6), Hi = .fmt_num(diff_hi, 6),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    names(tab3)[3:5] <- c("Difference Between Means",
                           paste0(ci_pct, " CL Lower"), paste0(ci_pct, " CL Upper"))
    paste0(
      "<h4 class='sas-subhead'>Least Squares Means for Effect FORM</h4>",
      .df_to_html(tab3),
      "<p class='sas-note'>i = ", .html_escape(lsm$test_level),
      ", j = ", .html_escape(lsm$ref_level), ".</p>"
    )
  } else ""

  paste0(
    "<h4 class='sas-subhead'>", .html_escape(dep_label), " LSMEAN</h4>",
    .df_to_html(tab1),
    .df_to_html(tab2),
    diff_html
  )
}

# "Parameter Estimates" — SAS PROC GLM's own 4-column layout (Estimate/SE/t/
# Pr>|t|); the difference's CI is shown in the LSMeans card above (matching
# how SAS itself splits this across two tables).
.render_parameter_estimates_real <- function(param_result, ref_lvl, test_lvl) {
  tcoef <- param_result$treatment_coef %||% NA
  tse   <- param_result$treatment_se   %||% NA
  tdf   <- param_result$residual_df    %||% NA
  if (is.na(tcoef) || is.na(tse)) return("")
  tval <- tcoef / tse
  tp <- if (!is.na(tdf) && tdf > 0) 2 * pt(abs(tval), tdf, lower.tail = FALSE) else NA
  tab <- data.frame(
    Parameter = paste0(test_lvl, " - ", ref_lvl),
    Estimate = .fmt_num(tcoef, 6),
    `Standard Error` = .fmt_num(tse, 6),
    `t Value` = .fmt_num(tval, 2),
    `Pr > |t|` = .fmt_p(tp),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  .df_to_html(tab)
}

# ---------------------------------------------------------------------------
# Section 3: Full ANOVA on log-transformed data (Class Level Info, Model/
# Error/Corrected Total, Type III SS, Subject(Sequence) test, LS Means,
# Parameter Estimates) — one block per PK parameter, built entirely from the
# real analysis engine's own per-parameter results (see .resolve_param_result
# above), not an independent refit.
# ---------------------------------------------------------------------------
.section_full_anova <- function(sd, pk_cols, be_results) {
  primary_pk <- intersect(c("Cmax", "AUC0t", "AUClast", "AUC0inf"), pk_cols)
  if (length(primary_pk) == 0) primary_pk <- pk_cols[1]

  blocks <- character(0)
  for (p in primary_pk) {
    ln_param <- paste0("ln", p)
    pr <- .resolve_param_result(be_results, ln_param)
    if (is.null(pr)) pr <- .resolve_param_result(be_results, p)
    if (is.null(pr) || is.null(pr$model)) next

    log_label <- ln_param
    ref_lvl <- pr$lsmeans_result$ref_level %||% "R"
    test_lvl <- pr$lsmeans_result$test_level %||% "T"

    class_html   <- .render_class_info_real(pr)
    summary_html <- .render_model_summary_real(pr, log_label)
    type3_html   <- .render_type3_real(pr)
    seqsub_html  <- .render_seq_subj_test_real(pr)
    lsmeans_html <- .render_lsmeans_real(pr, log_label)
    param_html   <- .render_parameter_estimates_real(pr, ref_lvl, test_lvl)

    if (!nzchar(class_html) && !nzchar(summary_html) && !nzchar(type3_html)) next

    blocks <- c(blocks, paste0(
      "<h3 class='sas-subhead'>ANOVA for log-transformed ", .html_escape(p), "</h3>",
      if (nzchar(class_html)) paste0(
        "<h4 class='sas-subhead'>Class Level Information</h4>", class_html) else "",
      if (nzchar(summary_html)) paste0(
        "<h4 class='sas-subhead'>Model Summary &mdash; Dependent Variable: ",
        .html_escape(log_label), "</h4>", summary_html) else "",
      if (nzchar(type3_html)) paste0(
        "<h4 class='sas-subhead'>Analysis of Variance (Type III SS)</h4>", type3_html) else "",
      if (nzchar(seqsub_html)) paste0(
        "<h4 class='sas-subhead'>Tests of Hypotheses Using the Type III MS ",
        "for Subject(Sequence) as an Error Term</h4>", seqsub_html) else "",
      lsmeans_html,
      if (nzchar(param_html)) paste0(
        "<h4 class='sas-subhead'>Parameter Estimates</h4>", param_html) else "",
      "<hr class='sas-rule'>"
    ))
  }
  paste(blocks, collapse = "\n")
}


# ---------------------------------------------------------------------------
# Section 5: Intra-subject variability summary
# ---------------------------------------------------------------------------
.section_intra_cv <- function(anova_data) {
  if (is.null(anova_data) || length(anova_data) == 0) return("")
  rows <- list()
  for (p in names(anova_data)) {
    pr <- anova_data[[p]]
    if (is.null(pr) || "error" %in% names(pr)) next
    disp <- .display_param(p)
    cvR <- NA_real_; s2R <- NA_real_; cvT <- NA_real_; s2T <- NA_real_
    src <- ""
    if (!is.null(pr$replicatebe_output)) {
      rb <- pr$replicatebe_output
      cvR <- suppressWarnings(as.numeric(rb$`CVwR(%)`))
      cvT <- suppressWarnings(as.numeric(rb$`CVwT(%)`))
      sR <- suppressWarnings(as.numeric(rb$swR))
      sT <- suppressWarnings(as.numeric(rb$swT))
      if (!is.na(sR)) s2R <- sR^2
      if (!is.na(sT)) s2T <- sT^2
      src <- "ABEL (replicateBE)"
    } else if (!is.null(pr$s2_wR)) {
      s2R <- as.numeric(pr$s2_wR)
      s2T <- as.numeric(pr$s2_wT %||% NA)
      cvR <- as.numeric(pr$cv_wr_percent %||% NA)
      cvT <- as.numeric(pr$cv_wt_percent %||% NA)
      src <- "RSABE (Reference-only ANOVA)"
    } else if (!is.null(pr$residual_mse)) {
      mse <- as.numeric(pr$residual_mse)
      if (!is.na(mse) && mse >= 0 && mse < 5) {
        s2R <- mse; cvR <- 100 * sqrt(exp(mse) - 1)
        src <- "ABE (pooled residual MSE)"
      }
    }
    if (is.na(cvR) && is.na(s2R)) next
    n_wR <- pr$n_wR %||% NA
    n_wT <- pr$n_wT %||% NA
    rows[[length(rows) + 1]] <- data.frame(
      Parameter = disp,
      n_R       = .fmt_int(n_wR),
      CVwR_pct  = .fmt_pct(cvR),
      swR_      = if (!is.na(s2R) && s2R >= 0) .fmt_num(sqrt(s2R), 4) else "\u2014",
      s2wR      = .fmt_num(s2R, 6),
      n_T       = .fmt_int(n_wT),
      CVwT_pct  = .fmt_pct(cvT),
      swT_      = if (!is.na(s2T) && s2T >= 0) .fmt_num(sqrt(s2T), 4) else "\u2014",
      s2wT      = .fmt_num(s2T, 6),
      Source    = src,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return("")
  tab <- do.call(rbind, rows)
  colnames(tab) <- c("Parameter", "n (Ref)", "CVwR (%)", "swR", "s\u00b2wR",
                     "n (Test)", "CVwT (%)", "swT", "s\u00b2wT", "Source")
  .df_to_html(tab)
}

# ---------------------------------------------------------------------------
# Analysis Configuration summary — the parameters selected for this run
# ---------------------------------------------------------------------------
.section_analysis_config <- function(analysis_config, be_results) {
  if (is.null(analysis_config)) return("")
  ac <- analysis_config
  analysis_type <- be_results$analysis_type %||% ac$be_analysis_type %||% "ABE"
  is_parallel <- identical(ac$study_design, "parallel") ||
                 identical(be_results$design, "parallel") ||
                 !is.null(be_results$statistical_results)

  rows <- list()
  add <- function(label, value) {
    if (!is.null(value) && !(is.character(value) && !nzchar(value)) && !is.na(value))
      rows[[length(rows) + 1]] <<- c(label, as.character(value))
  }

  add("Study Design", be_results$design %||% ac$study_design)
  add("Bioequivalence Analysis Type", analysis_type)
  if (identical(analysis_type, "RSABE")) {
    add("RSABE Method", switch(ac$rsabe_method %||% "fda_linearized",
                                "fda_linearized" = "FDA Linearized Scaled Criterion (Howe UCB)",
                                "nctost" = "Non-Central TOST (ncTOST)",
                                ac$rsabe_method))
  } else if (identical(analysis_type, "ABEL")) {
    add("ABEL-eligible Parameters",
        paste(ac$abel_eligible_params %||% character(0), collapse = ", "))
  }
  add("ANOVA Model", switch(ac$anova_model %||% "fixed",
                             "fixed" = "Fixed Effects (lm)",
                             "nlme" = "Mixed Effects — nlme (REML)",
                             "satterthwaite" = "Mixed Effects — Satterthwaite DF",
                             "kenward-roger" = "Mixed Effects — Kenward-Roger DF",
                             ac$anova_model))
  if (!is_parallel && !identical(ac$anova_model, "fixed"))
    add("Random Effects Structure", ac$random_effects)
  if (is_parallel)
    add("Parallel Variance Assumption",
        if (isTRUE(ac$welch_correction)) "Welch correction (unequal variances)"
        else "Equal variances assumption")
  add("PK Parameters Analyzed",
      paste(ac$selected_pk_params %||% ac$pk_parameters %||% character(0), collapse = ", "))
  add("AUC Calculation Method", ac$auc_method)
  add("Lambda_z Estimation Method", ac$lambda_z_method)
  add("Missing Data Handling (middle points)", ac$missing_data_middle)
  add("Missing Data Handling (terminal points)", ac$missing_data_terminal)
  add("Carryover Assessment (ICH M13A)",
      if (isTRUE(ac$test_carryover))
        sprintf("Performed (exclusion threshold %s%%)", ac$carryover_threshold)
      else "Not performed")
  alpha_cfg <- ac$alpha_level %||% 0.05
  add("Alpha / Confidence Level",
      sprintf("%.3f (%.0f%% CI)", alpha_cfg, (1 - 2 * alpha_cfg) * 100))
  add("Acceptance Limits (default)",
      sprintf("%.2f%% – %.2f%%", ac$be_lower %||% 80, ac$be_upper %||% 125))

  if (length(rows) == 0) return("")
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(df) <- c("Setting", "Value")
  .df_to_html(df)
}

# ---------------------------------------------------------------------------
# Statistical Method — parallel-group two-sample t-test
# (crossover designs get their ANOVA from sections 3/4 below; a parallel
# design has no Sequence/Period/Subject(Sequence) structure, so those
# sections render empty — this is the appropriate substitute.)
# ---------------------------------------------------------------------------
.section_parallel_ttest <- function(be_results) {
  ci_results <- be_results$confidence_intervals
  stats_results <- be_results$statistical_results
  if (is.null(ci_results) || is.null(stats_results) || length(stats_results) == 0) return("")

  rows <- list()
  for (p in names(stats_results)) {
    st <- stats_results[[p]]
    ci <- ci_results[[p]]
    if (is.null(st) || is.null(ci)) next
    rows[[length(rows) + 1]] <- data.frame(
      Parameter   = .display_param(p),
      Method      = st$method %||% "Two-sample t-test",
      N_Test      = .fmt_int(st$n_test),
      N_Reference = .fmt_int(st$n_ref),
      t_Statistic = .fmt_num(ci$t_statistic %||% NA, 4),
      DF          = .fmt_num(st$degrees_freedom %||% ci$df %||% NA, 2),
      P_Value     = .fmt_p(st$p_value %||% ci$p_value %||% NA),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return("")
  tab <- do.call(rbind, rows)
  colnames(tab) <- c("Parameter", "Method", "N (Test)", "N (Reference)",
                     "t Statistic", "DF", "P-value")
  paste0(
    .df_to_html(tab),
    "<p class='sas-note'>Bioequivalence for parallel-group designs is assessed via a ",
    "two-sample t-test on log-transformed data (Welch approximation used for unequal ",
    "variances where selected). Point estimate and confidence interval are shown in the ",
    "Bioequivalence Conclusion below.</p>"
  )
}

# ---------------------------------------------------------------------------
# Reproducible Analysis Summary — the function calls and parameters used
# ---------------------------------------------------------------------------
.section_reproducible_code <- function(analysis_config, be_results, ran_nca) {
  ac <- analysis_config %||% list()
  analysis_type <- be_results$analysis_type %||% ac$be_analysis_type %||% "ABE"
  design <- be_results$design %||% ac$study_design %||% "auto"
  is_parallel <- identical(design, "parallel") || !is.null(be_results$statistical_results)
  params <- ac$selected_pk_params %||% ac$pk_parameters %||% character(0)
  params_str <- paste0("c(", paste(sprintf("\"%s\"", params), collapse = ", "), ")")

  nca_block <- if (isTRUE(ran_nca)) paste0(
    "# Non-Compartmental Analysis (per subject)\n",
    "nca_results <- perform_enhanced_nca_analysis(\n",
    "  data,\n",
    "  lambda_points = ", ac$lambda_z_points %||% 3, "\n",
    ")\n",
    "# AUC method: ", ac$auc_method %||% "mixed", "\n",
    "# Lambda_z method: ", ac$lambda_z_method %||% "aic", "\n\n"
  ) else ""

  extra_params <- character(0)
  if (identical(analysis_type, "RSABE")) {
    extra_params <- c(extra_params, sprintf("    rsabe_method  = \"%s\"",
                                             ac$rsabe_method %||% "fda_linearized"))
  }
  if (identical(analysis_type, "ABEL")) {
    extra_params <- c(extra_params, sprintf(
      "    abel_eligible_params = c(%s)",
      paste(sprintf("\"%s\"", ac$abel_eligible_params %||% "Cmax"), collapse = ", ")))
  }
  if (is_parallel) {
    extra_params <- c(extra_params,
                       sprintf("    welch_correction = %s", isTRUE(ac$welch_correction)))
  } else {
    extra_params <- c(extra_params, sprintf("    anova_model   = \"%s\"",
                                             ac$anova_model %||% "fixed"))
  }

  be_block <- paste0(
    "# Bioequivalence Analysis\n",
    "be_results <- perform_be_analysis_by_type(\n",
    "  data          = ", if (isTRUE(ran_nca)) "nca_results" else "data", ",\n",
    "  analysis_type = \"", analysis_type, "\",\n",
    "  design        = \"", design, "\",\n",
    "  params = list(\n",
    "    alpha_level   = ", ac$alpha_level %||% 0.05, ",\n",
    "    be_limits     = list(lower = ", ac$be_lower %||% 80,
    ", upper = ", ac$be_upper %||% 125, "),\n",
    "    pk_parameters = ", params_str,
    if (length(extra_params) > 0) paste0(",\n", paste(extra_params, collapse = ",\n")) else "",
    "\n  )\n",
    ")\n"
  )

  paste0(
    "<pre class='sas-code'>", .html_escape(paste0(nca_block, be_block)), "</pre>",
    "<p class='sas-note'>Reflects the actual function calls and parameter values used to ",
    "produce this report; provided for reproducibility, not a verbatim execution transcript.</p>"
  )
}

# ---------------------------------------------------------------------------
# Section 6: Final BE summary (SAS-style normal-scale results table)
# ---------------------------------------------------------------------------
.section_be_summary <- function(be_results, sd, alpha) {
  ci_results <- be_results$confidence_intervals
  be_conclusions <- be_results$be_conclusions
  if (is.null(ci_results) || length(ci_results) == 0) return("")
  params <- intersect(names(ci_results), names(be_conclusions))
  if (length(params) == 0) params <- names(ci_results)
  anova_data <- be_results$anova_results$anova_results

  has_powertost <- requireNamespace("PowerTOST", quietly = TRUE)
  design_code <- be_results$design %||% ""

  rows <- list()
  for (p in params) {
    ci <- ci_results[[p]]
    if (is.null(ci)) next
    disp <- .display_param(p)
    n_val <- ci$n_subjects %||% be_results$n_subjects %||% NA
    pe <- ci$point_estimate
    lo <- ci$ci_lower; hi <- ci$ci_upper
    log_diff <- ci$log_difference %||% (if (!is.null(pe)) log(pe / 100) else NA)
    se_diff  <- ci$treatment_se %||% NA
    df_val   <- ci$degrees_freedom %||% NA
    cv_wr    <- ci$cv_wr %||% NA
    pr_intra <- if (!is.null(anova_data)) anova_data[[p]] else NULL
    if (is.na(cv_wr) && !is.null(pr_intra)) {
      cv_wr <- pr_intra$cv_wr_percent %||%
               (if (!is.null(pr_intra$replicatebe_output))
                  as.numeric(pr_intra$replicatebe_output$`CVwR(%)`) else NA)
    }
    is_be <- be_conclusions[[p]]
    be_lo <- ci$limits_used$lower %||% 80
    be_hi <- ci$limits_used$upper %||% 125

    # LS Means: from the real analysis engine (param_result$lsmeans_result,
    # via emmeans — same numbers shown in the interactive ANOVA Results tab
    # and in Section 3 above), not an independent refit.
    ls_test <- NA_real_; ls_ref <- NA_real_
    geo_test <- NA_real_; geo_ref <- NA_real_
    pr_eng <- .resolve_param_result(be_results, p)
    lsm <- pr_eng$lsmeans_result
    if (!is.null(lsm)) {
      ls_test <- lsm$lsmean_test_log; ls_ref <- lsm$lsmean_ref_log
      geo_test <- lsm$lsmean_test_geo; geo_ref <- lsm$lsmean_ref_geo
      if (is.na(df_val)) df_val <- pr_eng$residual_df %||% lsm$df_ref %||% NA
    }

    # Power calc via PowerTOST (post-hoc, given observed CV and ratio)
    power_pct <- NA_real_
    if (has_powertost && !is.na(cv_wr) && !is.na(pe) && !is.na(n_val)) {
      pt_design <- if (grepl("2x2x4", design_code)) "2x2x4"
                   else if (grepl("2x2x3", design_code)) "2x2x3"
                   else if (grepl("parallel", design_code, ignore.case = TRUE)) "parallel"
                   else "2x2x2"
      power_pct <- tryCatch({
        100 * PowerTOST::power.TOST(
          alpha = alpha, theta1 = be_lo / 100, theta2 = be_hi / 100,
          theta0 = pe / 100, CV = cv_wr / 100,
          n = as.integer(n_val), design = pt_design,
          robust = TRUE
        )
      }, error = function(e) NA_real_)
    }

    rows[[length(rows) + 1]] <- data.frame(
      PK_Parameter   = disp,
      LSM_Test       = .fmt_num(ls_test, 4),
      LSM_Ref        = .fmt_num(ls_ref, 4),
      SE_Diff        = .fmt_num(se_diff, 4),
      GeoLSMean_Test = .fmt_num(geo_test, 2),
      GeoLSMean_Ref  = .fmt_num(geo_ref, 2),
      DF_            = .fmt_int(df_val),
      Ratio_pct      = .fmt_num(pe, 2),
      CI_Lower       = .fmt_num(lo, 2),
      CI_Upper       = .fmt_num(hi, 2),
      Power_pct      = .fmt_num(power_pct, 2),
      IntraCV_Ref    = .fmt_pct(cv_wr),
      Lower_Limit    = .fmt_num(be_lo, 2),
      Upper_Limit    = .fmt_num(be_hi, 2),
      Bioequivalence = if (is.na(is_be)) "\u2014"
                          else if (isTRUE(is_be)) "Yes" else "No",
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return("")
  tab <- do.call(rbind, rows)
  colnames(tab) <- c("PK Parameter", "LSM Test", "LSM Ref", "S.E.(Diff)",
                     "GeoLSMean Test", "GeoLSMean Ref", "DF",
                     "Ratio (%)", "90% CI Lower", "90% CI Upper",
                     "Power (%)", "Intra CV% \u2013 Ref",
                     "Lower Limit", "Upper Limit", "Bioequivalence")
  .df_to_html(tab)
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------
generate_sas_style_html_report <- function(be_results,
                                           nca_results,
                                           analysis_config = NULL,
                                           output_file,
                                           data_type = NULL) {
  alpha <- analysis_config$alpha_level %||% 0.05
  ci_pct <- round((1 - alpha * 2) * 100)
  ci_label <- paste0(ci_pct, "% CI")
  design <- be_results$design %||% analysis_config$detected_design %||% "Unknown"
  method <- be_results$be_method %||% be_results$analysis_method %||%
            "Average Bioequivalence"
  analysis_type <- be_results$analysis_type %||% "ABE"
  n_subj <- be_results$n_subjects %||% NA
  is_parallel_run <- identical(design, "parallel") || !is.null(be_results$statistical_results)
  ran_nca <- identical(data_type, "concentration")

  sd <- .get_subject_data(nca_results)
  pk_cols <- .pk_columns(sd)

  # ── Analysis Configuration — selected parameters for this run ─────────────
  config_block <- .section_analysis_config(analysis_config, be_results)

  # ── Section 1: Subject-level listings ─────────────────────────────────────
  listing_untrans <- .section_subject_listing(sd, pk_cols, log_only = FALSE)
  listing_log     <- .section_subject_listing(sd, pk_cols, log_only = TRUE)

  # ── Section 2: Descriptive statistics — same four groupings as the
  # interactive Descriptive Statistics tab (results_dashboard_server.R):
  # Each Exposure (T1/T2/R1/R2, replicate designs only), Each Sequence,
  # Each Period, Each Treatment — plus a bonus Treatment x Period cross not
  # offered interactively. ──────────────────────────────────────────────────
  desc_expo <- .section_descriptives_exposure(sd, pk_cols)
  desc_seq <- if (!is.null(sd) && "Sequence" %in% names(sd) &&
                  length(unique(stats::na.omit(sd$Sequence))) >= 2)
    .section_descriptives_grouped(sd, pk_cols, "Sequence", "Sequence") else ""
  desc_per <- if (!is.null(sd) && "Period" %in% names(sd) &&
                  length(unique(stats::na.omit(sd$Period))) >= 2)
    .section_descriptives_grouped(sd, pk_cols, "Period", "Period") else ""
  desc_treat <- if (!is.null(sd) && "Treatment" %in% names(sd))
    .section_descriptives_grouped(sd, pk_cols, "Treatment",
                                  "Treatment (pooled)") else ""
  desc_treat_per <- if (!is.null(sd) && all(c("Treatment", "Period") %in% names(sd)))
    .section_descriptives_grouped(sd, pk_cols,
                                  c("Treatment", "Period"),
                                  "Treatment x Period") else ""

  # ── Section 3 (parallel only): Statistical Method — two-sample t-test ─────
  parallel_block <- if (is_parallel_run) .section_parallel_ttest(be_results) else ""

  # ── Section 3 (crossover/replicate only): Full ANOVA ───────────────────────
  # Sourced entirely from the real analysis engine's own per-parameter
  # results (same as the interactive ANOVA Results tab) — see
  # .resolve_param_result() / .section_full_anova() above.
  full_anova_block <- .section_full_anova(sd, pk_cols, be_results)

  # ── Section 4: Intra-subject variability ──────────────────────────────────
  # Not applicable to parallel designs: there is no within-subject repeated
  # dosing at all (each subject receives a single treatment), so no
  # intra-subject variance is estimable, unlike a crossover/replicate design.
  intra_block <- if (is_parallel_run) "" else
    .section_intra_cv(be_results$scaled_anova$anova_results %||% be_results$anova_results$anova_results)

  # ── Section 5: Final BE summary ───────────────────────────────────────────
  be_block <- .section_be_summary(be_results, sd, alpha)

  # ── Reproducible Analysis Summary ─────────────────────────────────────────
  code_block <- .section_reproducible_code(analysis_config, be_results, ran_nca)

  # ── Header ────────────────────────────────────────────────────────────────
  header_html <- paste0(
    "<div class='sas-header'>",
    "<h1>BioEQ &mdash; Bioequivalence Analysis Report</h1>",
    "<p class='sas-meta'>Generated ",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>",
    "<table class='sas-meta-tbl'>",
    "<tr><th>Study Design</th><td>", .html_escape(design), "</td></tr>",
    "<tr><th>Analysis Type</th><td>", .html_escape(analysis_type),
    " &mdash; ", .html_escape(method), "</td></tr>",
    "<tr><th>N Subjects</th><td>", .fmt_int(n_subj), "</td></tr>",
    "<tr><th>Alpha</th><td>", .fmt_num(alpha, 4),
    " (", ci_label, ")</td></tr>",
    "</table></div>"
  )

  # ── Body assembly ─────────────────────────────────────────────────────────
  body <- paste0(
    header_html,

    if (nzchar(config_block)) paste0(
      "<h2 class='sas-section'>Analysis Configuration</h2>",
      config_block) else "",

    if (nzchar(listing_untrans) || nzchar(listing_log))
      "<h2 class='sas-section'>1. Subject-Level Pharmacokinetic Listings</h2>"
    else "",
    if (nzchar(listing_untrans)) paste0(
      "<h3 class='sas-subhead'>1.1 Untransformed Pharmacokinetic Parameters</h3>",
      listing_untrans) else "",
    if (nzchar(listing_log)) paste0(
      "<h3 class='sas-subhead'>1.2 Log-transformed Pharmacokinetic Parameters (lnCmax, lnAUC0-t)</h3>",
      listing_log) else "",

    if (nzchar(desc_expo) || nzchar(desc_seq) ||
        nzchar(desc_per) || nzchar(desc_treat) || nzchar(desc_treat_per))
      "<h2 class='sas-section'>2. Descriptive Statistics &mdash; Untransformed Pharmacokinetic Parameters</h2>"
    else "",
    if (nzchar(desc_expo)) paste0(
      "<h3 class='sas-subhead'>2.1 Summary statistics &mdash; for each Exposure (T1/T2/R1/R2)</h3>",
      desc_expo) else "",
    if (nzchar(desc_seq)) paste0(
      "<h3 class='sas-subhead'>2.2 Summary statistics &mdash; for each Sequence</h3>",
      desc_seq) else "",
    if (nzchar(desc_per)) paste0(
      "<h3 class='sas-subhead'>2.3 Summary statistics &mdash; for each Period</h3>",
      desc_per) else "",
    if (nzchar(desc_treat)) paste0(
      "<h3 class='sas-subhead'>2.4 Summary statistics &mdash; for each Treatment</h3>",
      desc_treat) else "",
    if (nzchar(desc_treat_per)) paste0(
      "<h3 class='sas-subhead'>2.5 Summary statistics &mdash; for each Treatment x Period</h3>",
      desc_treat_per) else "",

    if (nzchar(parallel_block)) paste0(
      "<h2 class='sas-section'>3. Statistical Method &mdash; Two-Sample t-test</h2>",
      parallel_block) else "",

    if (nzchar(full_anova_block)) paste0(
      "<h2 class='sas-section'>3. Full ANOVA &mdash; Log-transformed Data</h2>",
      "<p class='sas-note'>Model: ln(PK) = Sequence + Subject(Sequence) + ",
      "Period + Treatment. Same Type III SS engine as the interactive ",
      "ANOVA Results tab (emmeans-based; matches SAS PROC GLM exactly, ",
      "including the Sequence row under Subject(Sequence) nesting).</p>",
      full_anova_block) else "",

    if (nzchar(intra_block)) paste0(
      "<h2 class='sas-section'>4. Intra-subject Variability Summary</h2>",
      intra_block) else "",

    if (nzchar(be_block)) paste0(
      "<h2 class='sas-section'>5. Bioequivalence Conclusion &mdash; Normal-scale Results of Log-transformed Data</h2>",
      be_block) else "",

    if (nzchar(code_block)) paste0(
      "<h2 class='sas-section'>6. Reproducible Analysis Summary</h2>",
      code_block) else "",

    "<hr class='sas-rule'>",
    "<p class='sas-footer'>Generated by BioEQ. ",
    "Descriptive statistics are computed on untransformed PK values; ",
    "ANOVA, intra-subject variability and BE confidence intervals are ",
    "computed on log-transformed values per regulatory convention.</p>"
  )

  css <- "
    body { font-family: 'Helvetica Neue', Arial, sans-serif; color:#212529;
           margin:32px; line-height:1.45; max-width:1200px; }
    .sas-header { border-bottom:2px solid #1e3a5f; padding-bottom:12px;
                   margin-bottom:24px; }
    .sas-header h1 { color:#1e3a5f; margin:0 0 4px 0; font-size:22px; }
    .sas-meta { color:#6c757d; font-size:12px; margin:0 0 10px 0; }
    table.sas-meta-tbl { border-collapse:collapse; font-size:13px; }
    table.sas-meta-tbl th { text-align:left; padding:2px 14px 2px 0;
                       color:#495057; font-weight:600; }
    table.sas-meta-tbl td { padding:2px 0; }
    h2.sas-section { color:#1e3a5f; border-bottom:1px solid #dee2e6;
                     margin-top:32px; padding-bottom:4px; font-size:17px; }
    h3.sas-subhead { color:#2c5282; margin-top:18px; font-size:14px;
                     font-weight:600; }
    h4.sas-subhead { color:#495057; margin:12px 0 6px 0; font-size:13px;
                     font-weight:600; }
    table.sas-table { border-collapse:collapse; margin:6px 0 16px 0;
                      font-size:12px; min-width:50%; }
    table.sas-table caption { caption-side:top; text-align:left;
                              font-weight:600; color:#495057;
                              padding-bottom:4px; }
    table.sas-table th, table.sas-table td {
      border:1px solid #ced4da; padding:4px 10px; text-align:right; }
    table.sas-table thead th { background:#f1f3f5; color:#212529;
                                font-weight:600; text-align:center; }
    table.sas-table tbody tr:nth-child(even) { background:#fafbfc; }
    table.sas-table td:first-child, table.sas-table th:first-child {
      text-align:left; }
    p.sas-note { color:#6c757d; font-size:12px; font-style:italic;
                 margin:4px 0 12px 0; }
    p.sas-footer { color:#6c757d; font-size:11px; margin-top:24px; }
    hr.sas-rule { border:none; border-top:1px dashed #ced4da; margin:18px 0; }
    pre.sas-code { background:#f8f9fa; border:1px solid #dee2e6; border-radius:4px;
                   padding:14px 16px; font-size:12px; line-height:1.5;
                   overflow-x:auto; white-space:pre; font-family:'SF Mono',
                   Consolas,'Courier New',monospace; color:#2d3748; }

    @media print {
      body { margin:12px; max-width:none; }
      h2.sas-section { break-after:avoid; page-break-after:avoid; }
      h3.sas-subhead, h4.sas-subhead { break-after:avoid; page-break-after:avoid; }
      table.sas-table, pre.sas-code { break-inside:avoid; page-break-inside:avoid; }
      table.sas-table tbody tr { break-inside:avoid; page-break-inside:avoid; }
    }
  "

  html <- paste0(
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'>",
    "<title>BioEQ \u2013 SAS-style BE Report</title>",
    "<style>", css, "</style></head><body>", body, "</body></html>"
  )

  writeLines(html, output_file, useBytes = TRUE)
  invisible(output_file)
}
