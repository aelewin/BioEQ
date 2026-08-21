# BioEQ - PKNCA-Backed NCA Engine
#
# Replaces the hand-written AUC/lambda_z math in nca_functions.R with the
# PKNCA CRAN package (validated, published, actively maintained). This file
# provides calculate_pk_parameters_pknca(), a drop-in replacement for
# calculate_pk_parameters() with an IDENTICAL signature and an output
# contract that preserves every column name downstream consumers rely on
# (see bioeq-dead-code-audit-2026-08 / the NCA audit for the full consumer
# map). perform_nca_analysis() in nca_functions.R calls this function; all
# grouping/ID/rbind logic there is unchanged.
#
# Design decisions (2026-08 NCA migration):
#   - AUC methods: only "linear" and "mixed" (-> PKNCA "lin up/log down")
#     are supported. The old "log" and "linear_log" methods are removed.
#   - lambda_z methods: only "ttt", "ars", "manual" are supported ("aic" is
#     removed). PKNCA has no native TTT — the TTT/manual point-selection
#     RULE is computed here and handed to PKNCA via the logical
#     include_half.life column (verified against the installed package's
#     own v06-half-life-calculation vignette; must be logical TRUE/FALSE,
#     NOT a character reason string — an earlier attempt using character
#     values was silently ignored by PKNCA). "ars" is PKNCA's own default
#     best-fit adjusted-R^2 curve-stripping (include_half.life left NA).
#   - BLQ concentrations are 0 (conc.blq = 0); unavailable/missing points
#     are dropped (conc.na = "drop") - not imputed.
#   - MRT, CL_F, Vd_F are dropped entirely (MRT was 1/lambda_z, not
#     AUMC/AUC; CL_F/Vd_F were computed against a hardcoded dose=1).
#   - lambda_z_slope/_intercept/_se/_t_value/_p_value and the serialized
#     lambda_z_terminal_times/_concs strings have no PKNCA equivalent (PKNCA
#     reports only the window boundaries lambda.z.time.first/.time.last).
#     They are reconstructed here by re-fitting lm(log(conc) ~ time) on
#     exactly PKNCA's selected window - required by the Lambda Z Regression
#     plot (R/plotting.R:create_lambda_z_regression_plots()), which draws
#     the line as exp(intercept + slope * time) and therefore needs the
#     raw (negative) ln-scale slope, not lambda_z itself.

#' Calculate PK parameters for a single concentration-time curve using PKNCA
#'
#' Drop-in replacement for calculate_pk_parameters() (R/nca_functions.R).
#' Same signature, same conceptual output (a named list convertible via
#' as.data.frame()), computed via the PKNCA package instead of hand-written
#' trapezoidal/regression code.
#'
#' @param time Numeric vector of sample times
#' @param conc Numeric vector of concentrations (BLQ should be coded as 0)
#' @param dose Administered dose (default 1; CL_F/Vd_F are no longer
#'   computed, so this only affects nothing downstream today, but is kept
#'   for signature compatibility and possible future use)
#' @param lambda_z_method One of "ttt", "ars", "manual"
#' @param auc_method One of "linear", "mixed"
#' @param lambda_z_points Number of terminal points to use when
#'   lambda_z_method = "manual"
#' @param calculate_pAUC Logical, whether to also compute a partial AUC
#' @param pAUC_start,pAUC_end Partial AUC interval bounds
#' @return A named list of PK parameters (see module header for the field
#'   list), or list(error = ...) on failure - matching the original
#'   function's error-handling contract.
#' @export
calculate_pk_parameters_pknca <- function(time, conc, dose = 1, lambda_z_method = "manual",
                                           auc_method = "mixed", lambda_z_points = 3,
                                           calculate_pAUC = FALSE, pAUC_start = 0, pAUC_end = 2) {

  # ---- Clean & sort inputs (mirrors the original function exactly) -------
  valid_idx <- !is.na(time) & !is.na(conc)
  time_clean <- time[valid_idx]
  conc_clean <- conc[valid_idx]

  if (length(time_clean) < 2) {
    return(list(error = "Insufficient data points"))
  }

  ord <- order(time_clean)
  time_clean <- time_clean[ord]
  conc_clean <- conc_clean[ord]
  n <- length(time_clean)

  if (!requireNamespace("PKNCA", quietly = TRUE)) {
    return(list(error = "PKNCA package is required but not installed"))
  }

  # ---- Map BioEQ's auc_method to PKNCA's auc.method option ---------------
  pknca_auc_method <- switch(auc_method,
    "linear" = "linear",
    "mixed"  = "lin up/log down",
    {
      warning("Unsupported auc_method '", auc_method, "' for PKNCA engine; using 'lin up/log down'")
      "lin up/log down"
    }
  )

  # ---- Build the single-profile working data frame ------------------------
  d <- data.frame(.subj = "1", .time = time_clean, .conc = conc_clean, stringsAsFactors = FALSE)

  tmax_val <- time_clean[which.max(conc_clean)]

  # include_half.life: logical column. NA (the default) means "no override -
  # let PKNCA's own curve-stripping best-fit (adjusted R^2) run", which is
  # exactly the "ars" method. TTT/manual set specific points to TRUE/FALSE.
  d$.include_hl <- NA

  if (identical(lambda_z_method, "ttt")) {
    two_tmax <- 2 * tmax_val
    eligible <- time_clean >= two_tmax & conc_clean > 0
    if (sum(eligible) < 3) {
      # Fall back to "any point after Tmax" if 2x-Tmax is too strict for
      # this profile's sampling schedule - mirrors the original TTT logic.
      eligible <- time_clean > tmax_val & conc_clean > 0
    }
    if (sum(eligible) >= 3) {
      d$.include_hl <- eligible
    }
    # else: leave as NA (all-NA => "treated as-is", per PKNCA's own
    # documented behavior) so TTT gracefully falls back to PKNCA's default
    # best-fit rather than silently returning all-NA results.

  } else if (identical(lambda_z_method, "manual")) {
    nz_idx <- which(conc_clean > 0)
    n_use <- min(lambda_z_points, length(nz_idx))
    if (n_use >= 3) {
      chosen <- utils::tail(nz_idx, n_use)
      incl <- rep(FALSE, n)
      incl[chosen] <- TRUE
      d$.include_hl <- incl
    }
  }
  # "ars" (or anything else): d$.include_hl stays all-NA -> PKNCA's own
  # default adjusted-R^2 best-fit curve-stripping, which already excludes
  # Tmax by default (allow.tmax.in.half.life = FALSE) - this is the fix for
  # the original engine's Cmax-inclusion bug.

  conc_obj <- tryCatch(
    PKNCA::PKNCAconc(d, .conc ~ .time | .subj, include_half.life = ".include_hl"),
    error = function(e) NULL
  )
  if (is.null(conc_obj)) {
    return(list(error = "PKNCA concentration object construction failed"))
  }

  dose_df <- data.frame(.subj = "1", .time = 0, .dose = dose, stringsAsFactors = FALSE)
  dose_obj <- tryCatch(
    PKNCA::PKNCAdose(dose_df, .dose ~ .time | .subj),
    error = function(e) NULL
  )
  if (is.null(dose_obj)) {
    return(list(error = "PKNCA dose object construction failed"))
  }

  # ---- Analysis interval(s): main (0,Inf) plus an optional partial-AUC ----
  main_interval <- data.frame(
    start = 0, end = Inf,
    cmax = TRUE, tmax = TRUE, tlast = TRUE, clast.obs = TRUE,
    auclast = TRUE, aucinf.obs = TRUE, aucpext.obs = TRUE,
    lambda.z = TRUE, lambda.z.n.points = TRUE,
    lambda.z.time.first = TRUE, lambda.z.time.last = TRUE,
    r.squared = TRUE, adj.r.squared = TRUE, span.ratio = TRUE,
    half.life = TRUE
  )

  intervals <- main_interval
  if (isTRUE(calculate_pAUC)) {
    pAUC_interval <- main_interval
    pAUC_interval[1, ] <- FALSE
    pAUC_interval$start <- pAUC_start
    pAUC_interval$end <- pAUC_end
    pAUC_interval$auclast <- TRUE
    intervals <- rbind(main_interval, pAUC_interval)
  }

  data_obj <- tryCatch(
    PKNCA::PKNCAdata(
      conc_obj, dose_obj, intervals = intervals,
      options = list(auc.method = pknca_auc_method, conc.blq = 0, conc.na = "drop")
    ),
    error = function(e) NULL
  )
  if (is.null(data_obj)) {
    return(list(error = "PKNCA data object construction failed"))
  }

  res <- tryCatch(
    suppressWarnings(suppressMessages(PKNCA::pk.nca(data_obj))),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(list(error = "PKNCA calculation failed"))
  }

  out <- as.data.frame(res)

  main_rows <- out[out$start == 0 & is.infinite(out$end), , drop = FALSE]
  get_val <- function(code) {
    v <- main_rows$PPORRES[main_rows$PPTESTCD == code]
    if (length(v) == 0) NA_real_ else v[1]
  }

  pk_params <- list()
  pk_params$Cmax   <- get_val("cmax")
  pk_params$Tmax   <- get_val("tmax")
  pk_params$AUC0t  <- get_val("auclast")
  pk_params$Tlast  <- get_val("tlast")
  pk_params$Clast  <- get_val("clast.obs")
  pk_params$AUC0inf <- get_val("aucinf.obs")
  pk_params$AUC_percent_extrap <- get_val("aucpext.obs")

  pk_params$lambda_z <- get_val("lambda.z")
  pk_params$lambda_z_r_squared <- get_val("r.squared")
  pk_params$lambda_z_adj_r_squared <- get_val("adj.r.squared")
  pk_params$lambda_z_span_ratio <- get_val("span.ratio")
  pk_params$lambda_z_points <- get_val("lambda.z.n.points")
  pk_params$lambda_z_method <- lambda_z_method
  pk_params$t_half <- get_val("half.life")

  # ---- Partial AUC (secondary interval) -----------------------------------
  # Matches the original function's contract exactly: pAUC/pAUC_start/
  # pAUC_end are only present as columns at all when calculate_pAUC=TRUE
  # (not present-with-NA otherwise) - several UI conditionals key off
  # column presence ("pAUC" %in% names(...)) to decide whether to show a
  # pAUC selector at all.
  if (isTRUE(calculate_pAUC)) {
    pauc_rows <- out[out$start == pAUC_start & out$end == pAUC_end, , drop = FALSE]
    pauc_v <- pauc_rows$PPORRES[pauc_rows$PPTESTCD == "auclast"]
    pk_params$pAUC <- if (length(pauc_v)) pauc_v[1] else NA_real_
    pk_params$pAUC_start <- pAUC_start
    pk_params$pAUC_end <- pAUC_end
  }

  # ---- Reconstruct the terminal window for slope/intercept/SE/serialized -
  # points. PKNCA's curve-stripping (default or via include_half.life) only
  # ever selects a contiguous run of positive-concentration points, so
  # [time.first, time.last] intersected with conc > 0 reconstructs exactly
  # the window PKNCA used. The point-count guard below catches the rare
  # case where that assumption doesn't hold, falling back to NA rather than
  # silently reporting a mismatched fit.
  t_first <- get_val("lambda.z.time.first")
  t_last  <- get_val("lambda.z.time.last")
  n_expected <- pk_params$lambda_z_points

  fit_fields <- c("lambda_z_slope", "lambda_z_intercept", "lambda_z_se",
                   "lambda_z_t_value", "lambda_z_p_value",
                   "lambda_z_terminal_times", "lambda_z_terminal_concs")
  for (fld in fit_fields) pk_params[[fld]] <- NA

  if (!is.na(t_first) && !is.na(t_last) && !is.na(n_expected)) {
    win_idx <- which(time_clean >= t_first & time_clean <= t_last & conc_clean > 0)
    if (length(win_idx) == n_expected && n_expected >= 2) {
      wt <- time_clean[win_idx]
      wc <- conc_clean[win_idx]
      fit <- tryCatch(stats::lm(log(wc) ~ wt), error = function(e) NULL)
      if (!is.null(fit)) {
        s <- summary(fit)
        cf <- s$coefficients
        pk_params$lambda_z_slope <- unname(stats::coef(fit)[2])
        pk_params$lambda_z_intercept <- unname(stats::coef(fit)[1])
        if (nrow(cf) >= 2) {
          pk_params$lambda_z_se <- cf[2, "Std. Error"]
          pk_params$lambda_z_t_value <- cf[2, "t value"]
          pk_params$lambda_z_p_value <- cf[2, "Pr(>|t|)"]
        }
        pk_params$lambda_z_terminal_times <- paste(wt, collapse = ",")
        pk_params$lambda_z_terminal_concs <- paste(wc, collapse = ",")
      }
    }
  }

  # ---- Metadata ------------------------------------------------------------
  pk_params$dose <- dose

  # ---- Log-transformed parameters (required by BE/ANOVA) -------------------
  ln_or_na <- function(x) {
    if (!is.null(x) && !is.na(x) && x > 0) log(x) else NA_real_
  }
  pk_params$lnCmax    <- ln_or_na(pk_params$Cmax)
  pk_params$lnAUC0t   <- ln_or_na(pk_params$AUC0t)
  pk_params$lnAUC0inf <- ln_or_na(pk_params$AUC0inf)
  pk_params$lnTmax    <- ln_or_na(pk_params$Tmax)
  # lnpAUC is always present (NA when pAUC wasn't calculated), matching the
  # original function's contract exactly.
  pk_params$lnpAUC    <- if (!is.null(pk_params$pAUC)) ln_or_na(pk_params$pAUC) else NA_real_

  return(pk_params)
}
