# BioEQ - Analysis Summary / Console Report
#
# One user-facing report replaces the ~250 lines of debug/progress chatter
# that used to print during "Run Bioequivalence Analysis". Three pieces,
# printed in this order as the run progresses (not all at once at the end):
#   1. print_analysis_header()      - once, right after design detection
#   2. bioeq_phase()                - once per phase, as each completes
#   3. print_be_analysis_summary()  - once, at the very end (success path only)
# All three use plain cat() (not bioeq_log()) - see the note on
# print_be_analysis_summary() for why.

#' Format a phase line for the console, mirroring the withProgress() UI bar
#' in shiny/server/analysis_setup_server.R step-for-step so the console and
#' the on-screen progress bar never diverge.
#' @param step Current phase number (1-based)
#' @param total Total number of phases for this run
#' @param label Short phase name (e.g. "NCA")
#' @param detail Optional one-line result to show after the label
#' @export
bioeq_phase <- function(step, total, label, detail = NULL) {
  cat(sprintf(
    "  [%d/%d] %-20s %s\n",
    step, total, label,
    if (is.null(detail) || !nzchar(detail)) "" else detail
  ))
  invisible(NULL)
}

#' Format a single numeric value for the summary table/header.
#' Deliberately NOT %||% - that operator (R/utils.R:10) silently swallows NA
#' and doesn't reliably guard non-finite/length>1 values, which would print
#' a blank or throw inside sprintf. This never does either.
#' @keywords internal
.bioeq_fmt_num <- function(x, digits = 2) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !is.finite(x)) return("--")
  sprintf(paste0("%.", digits, "f"), x)
}

#' Format an integer-ish value (N, df) for the summary table.
#' @keywords internal
.bioeq_fmt_int <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x)) return("--")
  as.character(round(as.numeric(x)))
}

#' @keywords internal
.bioeq_anova_model_label <- function(anova_model) {
  switch(anova_model %||% "fixed",
    "fixed"         = "Fixed Effects (lm)",
    "nlme"          = "Mixed Effects (nlme)",
    "satterthwaite" = "Mixed Effects (Satterthwaite)",
    "kenward-roger" = "Mixed Effects (Kenward-Roger)",
    anova_model %||% "unknown model"
  )
}

#' Print the analysis configuration header - design, subjects, sequences,
#' NCA settings, method. Called once, right after study design detection
#' (before the NCA/ANOVA/BE phases run), so the reader knows what's about to
#' happen before the phase lines start ticking by.
#' @param config The analysis_config list built in analysis_setup_server.R
#' @param data The uploaded data (for Subjects/Observations counts)
#' @param design_info Optional return value of detect_replicate_design() -
#'   supplies the full design name, period count, and sequence list when
#'   available; falls back to config$detected_design/study_design otherwise
#' @export
print_analysis_header <- function(config, data, design_info = NULL) {
  tryCatch({
    .print_analysis_header_impl(config, data, design_info)
  }, error = function(e) {
    cat(sprintf("[WARNING] Could not render the analysis header: %s\n", e$message))
  })
  invisible(NULL)
}

#' @keywords internal
.print_analysis_header_impl <- function(config, data, design_info) {
  width <- 88
  rule <- strrep("=", width)
  thin_rule <- strrep("-", width)

  title <- "BioEQ - Bioequivalence Analysis"
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M")
  pad <- max(1, width - 2 - nchar(title) - nchar(timestamp))
  cat("\n", rule, "\n", sep = "")
  cat("  ", title, strrep(" ", pad), timestamp, "\n", sep = "")
  cat(rule, "\n", sep = "")

  # design_type (e.g. "2x2x4 Full Replicate") rather than design_name (e.g.
  # "2x2x4 (Full Replicate)") - the latter already has its own parens, which
  # would double up with the "(N periods)" suffix added below.
  design_label <- if (!is.null(design_info) && !is.null(design_info$design_type)) {
    np <- design_info$n_periods
    if (!is.null(np) && !is.na(np)) {
      sprintf("%s (%d period%s)", design_info$design_type, np, if (np != 1) "s" else "")
    } else {
      design_info$design_type
    }
  } else {
    config$detected_design %||% config$study_design %||% "unknown"
  }
  cat(sprintf("  Design      %s\n", design_label))

  n_subjects <- length(unique(data$Subject))
  n_obs <- nrow(data)
  cat(sprintf("  Subjects    %-8s Observations  %s\n", n_subjects, n_obs))

  if (!is.null(design_info) && !is.null(design_info$sequences) && length(design_info$sequences) > 0) {
    cat(sprintf("  Sequences   %s\n", paste(design_info$sequences, collapse = ", ")))
  }

  auc_label <- switch(config$auc_method %||% "mixed",
    "mixed"  = "lin-up/log-down",
    "linear" = "linear trapezoidal",
    config$auc_method %||% "?"
  )
  lambda_label <- switch(config$lambda_z_method %||% "ttt",
    "ttt"    = "TTT",
    "manual" = "manual",
    "ars"    = "ARS",
    toupper(config$lambda_z_method %||% "?")
  )
  cat(sprintf("  NCA         AUC %s . lambda_z %s\n", auc_label, lambda_label))

  cat(sprintf("  Method      %s . %s\n",
              config$be_analysis_type %||% "ABE", .bioeq_anova_model_label(config$anova_model)))
  cat(thin_rule, "\n", sep = "")

  invisible(NULL)
}

#' Print a compact, BE-type-aware RESULTS block for a completed
#' bioequivalence analysis. Called once, at the very end of the run (success
#' path only - never on an error path, see the call site in
#' analysis_setup_server.R for why that's structurally guaranteed).
#'
#' Handles all three routes returned by perform_be_analysis_by_type()
#' (R/be_analysis.R:69) - ABE, ABEL, RSABE - by reading only the fields each
#' route actually guarantees, rather than assuming one shape. Confirmed
#' guaranteed fields across all three (verified against the construction
#' sites, not assumed): point_estimate/ci_lower/ci_upper/confidence_level on
#' every confidence_intervals[[param]] entry, and be_conclusions/n_subjects
#' on the top-level result. Everything else (cv_wr/cv_wt, limits_used,
#' rsabe_scaling, ...) is read defensively.
#'
#' Uses plain cat(), not bioeq_log(): this is the analysis *result*, not a
#' log record, so it must never be hidden by a raised log_level, and
#' bioeq_log()'s per-line emoji prefix (bioeq_main.R:98-103) would break the
#' table's fixed-width column alignment.
#'
#' @param result The list returned by perform_be_analysis_by_type()
#' @param elapsed_sec Wall-clock seconds for the run (numeric or NULL)
#' @param config Unused (kept for call-site symmetry with print_analysis_header())
#' @param ref_test_cv Optional list(ref=, test=) - the first primary
#'   parameter's Reference/Test intra-subject CV%, from
#'   compute_reference_anova_variance() (R/simple_anova.R). That function
#'   requires Treatment coded as literal "R"/"T", which confidence_intervals
#'   entries are not guaranteed to be, so the caller computes this (with the
#'   necessary recoding) rather than this function reaching for it itself.
#'   Falls back to a pooled-MSE approximation from the CI's own `mse` field
#'   (plain ABE only) when omitted or NULL.
#' @export
print_be_analysis_summary <- function(result, elapsed_sec = NULL, config = NULL, ref_test_cv = NULL) {
  tryCatch({
    .print_be_analysis_summary_impl(result, elapsed_sec, ref_test_cv)
  }, error = function(e) {
    # The summary is a reporting nicety layered on top of a completed
    # analysis - a formatting bug in it must never look like the analysis
    # itself failed.
    cat(sprintf("[WARNING] Could not render the analysis summary: %s\n", e$message))
  })
  invisible(NULL)
}

#' @keywords internal
.print_be_analysis_summary_impl <- function(result, elapsed_sec, ref_test_cv = NULL) {
  width <- 88
  rule <- strrep("=", width)
  thin_rule <- strrep("-", width)

  cat(thin_rule, "\n", sep = "")

  if (is.null(result) || is.null(result$confidence_intervals) ||
      length(result$confidence_intervals) == 0) {
    cat("  RESULTS\n")
    cat("  No confidence intervals available to summarize.\n")
    cat(rule, "\n\n", sep = "")
    return(invisible(NULL))
  }

  ci_list <- result$confidence_intervals
  be_conclusions <- result$be_conclusions
  n_subjects <- result$n_subjects

  cat("  RESULTS\n")
  cat(sprintf("  %-12s %4s %8s %19s %19s  %s\n",
              "Parameter", "N", "PE%", "90% CI", "Limits", "Verdict"))

  footnotes <- character(0)
  be_count <- 0L
  total_count <- 0L
  # Reference/Test intra-subject CV% for the summary line below the table.
  # Priority: caller-supplied ref_test_cv (the real per-arm split, plain ABE
  # included - see the ref_test_cv param doc) > the first parameter's own
  # cv_wr/cv_wt (ABEL/RSABE CI entries carry these directly) > a pooled-MSE
  # approximation as a last resort (does not distinguish Reference from Test).
  first_cv <- if (!is.null(ref_test_cv) && !is.null(ref_test_cv$ref) && !is.na(ref_test_cv$ref)) {
    list(ref = ref_test_cv$ref, test = ref_test_cv$test)
  } else {
    NULL
  }

  for (param in names(ci_list)) {
    ci <- ci_list[[param]]
    if (is.null(ci)) next
    total_count <- total_count + 1L

    n_val <- ci$n_subjects %||% n_subjects
    pe <- .bioeq_fmt_num(ci$point_estimate)
    ci_str <- sprintf("%8s - %8s", .bioeq_fmt_num(ci$ci_lower), .bioeq_fmt_num(ci$ci_upper))

    lim <- ci$limits_used
    lim_type <- if (!is.null(lim) && !is.null(lim$type)) lim$type else NA_character_
    lim_str <- if (!is.null(lim)) {
      sprintf("%8s - %8s", .bioeq_fmt_num(lim$lower), .bioeq_fmt_num(lim$upper))
    } else {
      sprintf("%8s - %8s", "80.00", "125.00")
    }
    # Only annotate when limits are NOT the plain fixed 80-125% case - that's
    # the common row and annotating every one would be noise. An ABEL/RSABE
    # run mixing fixed + expanded/scaled rows needs the reader to know which
    # is which, so the annotation earns its keep exactly when it's not "fixed".
    if (!is.na(lim_type) && lim_type != "fixed") {
      lim_str <- paste0(lim_str, " (", lim_type, ")")
    }

    if (is.null(first_cv)) {
      if (!is.null(ci$cv_wr) && !is.na(ci$cv_wr)) {
        first_cv <- list(ref = ci$cv_wr, test = ci$cv_wt)
      } else if (!is.null(ci$mse) && is.numeric(ci$mse)) {
        # Plain ABE's confidence_intervals entries don't carry a separate
        # Reference/Test split (one pooled residual MSE from the crossover
        # model) - show that one pooled figure rather than fabricate a split.
        pooled <- tryCatch(sqrt(exp(ci$mse) - 1) * 100, error = function(e) NA)
        if (!is.na(pooled)) first_cv <- list(ref = pooled, test = NULL, pooled = TRUE)
      }
    }

    # Verdict resolution order: the authoritative per-parameter conclusion
    # first, the confidence-interval's own flag as fallback, "?" only if
    # genuinely neither is available.
    verdict <- NA
    if (!is.null(be_conclusions) && param %in% names(be_conclusions)) {
      verdict <- isTRUE(be_conclusions[[param]])
    } else if (!is.null(ci$within_limits)) {
      verdict <- isTRUE(ci$within_limits)
    }
    if (isTRUE(verdict)) {
      be_count <- be_count + 1L
      verdict_str <- "PASS"
    } else if (isFALSE(verdict)) {
      verdict_str <- "FAIL"
    } else {
      verdict_str <- "?"
    }

    star <- ""
    if (isTRUE(ci$rsabe_scaling)) {
      star <- " *"
      rsabe_test <- tryCatch(result$rsabe_details[[param]]$rsabe_test, error = function(e) NULL)
      crit_str <- if (isTRUE(ci$rsabe_criterion_pass)) "PASS" else if (isFALSE(ci$rsabe_criterion_pass)) "FAIL" else "?"
      pe_str <- if (isTRUE(ci$pe_constraint_pass)) "PASS" else if (isFALSE(ci$pe_constraint_pass)) "FAIL" else "?"
      bound_line <- if (!is.null(rsabe_test) && !is.null(rsabe_test$ucb)) {
        sprintf("UCB = %s (<= 0 required): %s", .bioeq_fmt_num(rsabe_test$ucb, 6), crit_str)
      } else if (!is.null(rsabe_test) && !is.null(rsabe_test$overall_p)) {
        sprintf("overall p = %s (ncTOST): %s", .bioeq_fmt_num(rsabe_test$overall_p, 6), crit_str)
      } else {
        sprintf("scaling criterion: %s", crit_str)
      }
      footnotes <- c(footnotes, sprintf(
        "  * %s decided by the RSABE scaled criterion, not CI containment:\n      %s   |   PE constraint 80-125%%: %s",
        param, bound_line, pe_str
      ))
    }

    cat(sprintf("  %-12s %4s %8s %19s %19s  %s%s\n",
                param, .bioeq_fmt_int(n_val), pe, ci_str, lim_str, verdict_str, star))
  }

  if (length(footnotes) > 0) {
    cat("\n")
    for (fn in footnotes) cat(fn, "\n", sep = "")
  }

  cat("\n")
  if (!is.null(first_cv)) {
    if (!is.null(first_cv$test) && !is.na(first_cv$test)) {
      cat(sprintf("  Intra-subject CV%%   Reference %s%% . Test %s%%\n",
                  .bioeq_fmt_num(first_cv$ref), .bioeq_fmt_num(first_cv$test)))
    } else {
      cat(sprintf("  Intra-subject CV%%   %s%% (pooled)\n", .bioeq_fmt_num(first_cv$ref)))
    }
  }
  cat(sprintf("  Conclusion          %d of %d parameters bioequivalent\n", be_count, total_count))
  if (!is.null(elapsed_sec)) {
    cat(sprintf("  Elapsed             %ss\n", .bioeq_fmt_num(elapsed_sec, 1)))
  }
  cat(rule, "\n\n", sep = "")

  invisible(NULL)
}
