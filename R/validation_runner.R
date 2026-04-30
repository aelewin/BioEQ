# BioEQ Validation Runner
#
# Black-box validation engine. All reference datasets and expected results are
# EMBEDDED in R/validation_embedded.RData and loaded once at startup.
# The runner never writes files and users never need filesystem access to
# validation/datasets/ or validation/expected_results/.
#
# CRITICAL DESIGN PRINCIPLE
# -------------------------
# This runner NEVER caches BioEQ-computed results. Every call to
# `run_validation_for_dataset()` reads the input data from the embedded store
# and invokes the BioEQ analysis pipeline from scratch. The only stored
# artifacts consulted during a run are the embedded datasets and reference
# values baked into validation_embedded.RData at application build time.
#
# DO NOT add memoization, caching, or result persistence to the functions in
# this file. Doing so would defeat the purpose of black-box validation.

# ---------------------------------------------------------------------------
# Embedded data store
# ---------------------------------------------------------------------------

# Locate and load validation_embedded.RData once, storing in a private env.
.validation_env <- new.env(parent = emptyenv())
.validation_env$loaded <- FALSE

#' Load the embedded validation store (idempotent)
#' @keywords internal
.load_embedded <- function() {
  if (isTRUE(.validation_env$loaded)) return(invisible(NULL))
  candidates <- c(
    file.path("R", "validation_embedded.RData"),
    file.path("..", "R", "validation_embedded.RData"),
    file.path(getwd(), "R", "validation_embedded.RData")
  )
  # Walk up from cwd up to 3 levels
  cur <- getwd()
  for (i in 1:3) {
    candidates <- c(candidates, file.path(cur, "R", "validation_embedded.RData"))
    cur <- dirname(cur)
  }
  for (p in candidates) {
    if (file.exists(p)) {
      local_env <- new.env(parent = emptyenv())
      load(p, envir = local_env)
      if (exists(".bioeq_validation", envir = local_env)) {
        vd <- get(".bioeq_validation", envir = local_env)
        .validation_env$manifest <- vd$manifest
        .validation_env$groups   <- vd$groups
        .validation_env$datasets <- vd$datasets
        .validation_env$expected <- vd$expected
        .validation_env$loaded   <- TRUE
        return(invisible(NULL))
      }
    }
  }
  stop("validation_embedded.RData not found. ",
       "Run validation/scripts/embed_validation_data.R from the project root.")
}

# ---------------------------------------------------------------------------
# Path resolution (kept for backwards compatibility; not used at runtime)
# ---------------------------------------------------------------------------

#' Locate the validation directory (only needed by developer build scripts)
#' @export
get_validation_dir <- function() {
  candidates <- c(
    "validation",
    "../validation",
    file.path(getwd(), "validation"),
    normalizePath("validation", mustWork = FALSE)
  )
  for (p in candidates) {
    if (dir.exists(p) && file.exists(file.path(p, "manifest.csv"))) {
      return(normalizePath(p))
    }
  }
  cur <- getwd()
  for (i in 1:3) {
    cand <- file.path(cur, "validation")
    if (dir.exists(cand) && file.exists(file.path(cand, "manifest.csv"))) {
      return(normalizePath(cand))
    }
    cur <- dirname(cur)
  }
  # Return a non-existent path gracefully — embedded store is used instead
  file.path(getwd(), "validation")
}

# ---------------------------------------------------------------------------
# Manifest + dataset loading  (all backed by embedded store)
# ---------------------------------------------------------------------------

#' Load the validation manifest from the embedded store
#' @export
load_validation_manifest <- function(validation_dir = NULL) {
  .load_embedded()
  m <- .validation_env$manifest
  if (is.null(m)) stop("Validation manifest not found in embedded store.")
  if (!"group_id" %in% names(m)) m$group_id <- NA_character_
  m
}

#' Load the validation groups registry from the embedded store
#' @export
load_validation_groups <- function(validation_dir = NULL) {
  .load_embedded()
  g <- .validation_env$groups
  if (is.null(g)) return(NULL)
  for (col in c("pmid", "doi")) {
    if (!col %in% names(g)) g[[col]] <- NA_character_
  }
  g$is_bioeq_generated <- as.logical(g$is_bioeq_generated)
  g
}

#' @keywords internal
.resolve_validation_path <- function(rel, validation_dir) {
  if (is.na(rel) || rel == "") return(NA_character_)
  if (!is.null(validation_dir)) file.path(validation_dir, rel) else NA_character_
}

#' Check whether a dataset is available in the embedded store
#' @export
validation_dataset_available <- function(dataset_id, manifest = NULL,
                                         validation_dir = NULL) {
  .load_embedded()
  dataset_id %in% names(.validation_env$datasets)
}

#' Load an input dataset from the embedded store (always fresh copy)
#' @export
load_validation_dataset <- function(dataset_id, manifest = NULL,
                                    validation_dir = NULL) {
  .load_embedded()
  ds <- .validation_env$datasets[[dataset_id]]
  if (is.null(ds)) {
    stop(sprintf("Dataset '%s' not found in embedded store. ", dataset_id),
         "Re-run validation/scripts/embed_validation_data.R to rebuild the store.")
  }
  ds  # returns a copy each time
}

#' Load expected/reference results from the embedded store
#' @export
load_expected_results <- function(dataset_id, manifest = NULL,
                                   validation_dir = NULL) {
  .load_embedded()
  df <- .validation_env$expected[[dataset_id]]
  if (is.null(df) || nrow(df) == 0) return(NULL)
  required <- c("parameter", "scope", "subject", "treatment", "expected_value",
                "tolerance_type", "tolerance_value")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    warning(sprintf("Expected results for '%s' missing columns: %s",
                    dataset_id, paste(missing, collapse = ", ")))
    return(NULL)
  }
  df
}

# ---------------------------------------------------------------------------
# Tolerance comparison
# ---------------------------------------------------------------------------

#' Compare a single computed value against an expected value with tolerance
#' @export
compare_value <- function(computed, expected, tolerance_type = "relative",
                          tolerance_value = 0.001) {
  if (identical(tolerance_type, "exact")) {
    pass <- !is.na(computed) && !is.na(expected) &&
            identical(as.character(computed), as.character(expected))
    return(list(pass = pass, deviation = NA_real_))
  }
  comp_num <- suppressWarnings(as.numeric(computed))
  exp_num  <- suppressWarnings(as.numeric(expected))
  if (is.na(comp_num) || is.na(exp_num)) {
    return(list(pass = FALSE, deviation = NA_real_))
  }
  diff <- abs(comp_num - exp_num)
  if (identical(tolerance_type, "relative")) {
    if (exp_num == 0) {
      dev <- diff
      pass <- diff <= tolerance_value
    } else {
      dev <- diff / abs(exp_num)
      pass <- dev <= tolerance_value
    }
  } else { # absolute (default fallback)
    dev <- diff
    pass <- diff <= tolerance_value
  }
  list(pass = pass, deviation = dev)
}

# ---------------------------------------------------------------------------
# Computed-value extraction
# ---------------------------------------------------------------------------

#' Look up a computed value in the BioEQ result object given parameter/scope/subject/treatment
#' @keywords internal
.lookup_computed_value <- function(parameter, scope, subject, treatment,
                                   computed_subject_df, computed_study_list) {
  if (identical(scope, "subject")) {
    if (is.null(computed_subject_df) || nrow(computed_subject_df) == 0) return(NA)
    df <- computed_subject_df
    # Match subject (string-compare to avoid type issues)
    df <- df[as.character(df$subject) == as.character(subject), , drop = FALSE]
    if (!is.null(treatment) && !is.na(treatment) && nzchar(treatment) &&
        "treatment" %in% names(df)) {
      df <- df[as.character(df$treatment) == as.character(treatment), , drop = FALSE]
    }
    if (nrow(df) == 0 || !(parameter %in% names(df))) return(NA)
    return(df[[parameter]][1])
  }
  if (identical(scope, "study")) {
    if (is.null(computed_study_list)) return(NA)
    if (!is.null(treatment) && !is.na(treatment) && nzchar(treatment)) {
      key <- paste0(parameter, "_", treatment)
      if (key %in% names(computed_study_list)) return(computed_study_list[[key]])
    }
    if (parameter %in% names(computed_study_list)) return(computed_study_list[[parameter]])
    return(NA)
  }
  NA
}

# ---------------------------------------------------------------------------
# NCA layer execution (fresh per call)
# ---------------------------------------------------------------------------

#' Normalize raw input data to the (subj, tmt, time, conc) schema used by NCA
#' @keywords internal
.normalize_nca_input <- function(data) {
  cols <- tolower(names(data))
  pick <- function(...) {
    candidates <- tolower(c(...))
    idx <- which(cols %in% candidates)
    if (length(idx) == 0) return(NA_integer_)
    idx[1]
  }
  subj_i <- pick("subject", "subj", "id", "subjid")
  trt_i  <- pick("treatment", "tmt", "trt", "formulation", "drug", "product")
  time_i <- pick("time", "timepoint", "hour", "hr")
  conc_i <- pick("conc", "concentration", "value", "result", "dv", "plasma_conc")

  if (is.na(subj_i) || is.na(time_i) || is.na(conc_i)) {
    stop("NCA input missing required columns. Need subject, time, and concentration.")
  }
  out <- data.frame(
    subj = data[[subj_i]],
    tmt  = if (is.na(trt_i)) "R" else data[[trt_i]],
    time = suppressWarnings(as.numeric(data[[time_i]])),
    conc = suppressWarnings(as.numeric(data[[conc_i]])),
    stringsAsFactors = FALSE
  )
  out
}

#' Run BioEQ NCA layer on a validation dataset, return computed values in
#' a normalized form for comparison.
#' @keywords internal
.run_nca_layer <- function(data, lambda_points = 3) {
  nca_in <- .normalize_nca_input(data)
  res <- perform_enhanced_nca_analysis(nca_in, lambda_points = lambda_points)
  if (is.null(res) || nrow(res) == 0) {
    stop("NCA produced no results for this dataset.")
  }
  # Standardize column names to match expected-results schema
  std <- data.frame(
    subject   = as.character(res$subj),
    treatment = as.character(res$tmt),
    Cmax      = res$Cmax,
    Tmax      = res$Tmax,
    AUC0t     = res$AUC0t,
    AUC0inf   = res$AUC0inf,
    t_half    = res$t_half,
    lambda_z  = res$Lambda_z,
    lambda_z_r2 = res$Lambda_z_r2,
    lambda_z_n_points = res$Lambda_z_points,
    stringsAsFactors = FALSE
  )
  list(subject_df = std, study = list())
}

# ---------------------------------------------------------------------------
# ANOVA + BE layer execution (fresh per call)
#
# Self-contained, minimal BE math used by the validation runner. Bypasses the
# main-app perform_be_analysis() wrapper to keep the validation harness
# decoupled from analysis-config / regulatory-evaluation code paths and to
# return exactly the parameter names referenced by expected_results CSVs:
#   parallel  : PE, CI_lower_welch, CI_upper_welch, CI_lower_classical, CI_upper_classical
#   2x2x2     : PE, CI_lower, CI_upper
#   replicate : PE, CI_lower, CI_upper, CVwR (Method A, fixed-effects)
# ---------------------------------------------------------------------------

#' Pick the PK column to evaluate from a BE dataset
#' @keywords internal
.pick_pk_column <- function(data) {
  prefs <- c("PK", "Cmax", "AUC0t", "AUC0inf", "AUC", "Var")
  hit <- intersect(prefs, names(data))
  if (length(hit) == 0) {
    # Last-ditch: any single numeric column that isn't a design factor
    factor_cols <- c("Subject", "Treatment", "Period", "Sequence",
                     "subject", "treatment", "period", "sequence")
    cand <- setdiff(names(data), factor_cols)
    num <- cand[vapply(cand, function(n) is.numeric(data[[n]]), logical(1))]
    if (length(num) == 0) stop("No PK column found in BE dataset.")
    return(num[1])
  }
  hit[1]
}

#' Standardize column names (Subject, Treatment, Period, Sequence)
#' @keywords internal
.std_be_cols <- function(data) {
  cols <- names(data)
  rename_map <- list(
    Subject   = c("subject", "subj", "id", "subjid"),
    Treatment = c("treatment", "tmt", "trt", "formulation", "drug"),
    Period    = c("period", "per", "phase"),
    Sequence  = c("sequence", "seq", "grp", "group")
  )
  for (target in names(rename_map)) {
    if (target %in% cols) next
    cands <- rename_map[[target]]
    hit <- which(tolower(cols) %in% tolower(cands))
    if (length(hit) > 0) names(data)[hit[1]] <- target
  }
  data
}

#' Parallel BE: two-sample t on log(PK), Welch and classical CIs
#' @keywords internal
.be_parallel <- function(data, alpha = 0.10) {
  pk_col <- .pick_pk_column(data)
  data <- data[!is.na(data[[pk_col]]) & data[[pk_col]] > 0, , drop = FALSE]
  trt <- as.character(data$Treatment)
  trt <- toupper(trt)
  # Map common labels to T/R
  trt[trt %in% c("T", "TEST")]      <- "T"
  trt[trt %in% c("R", "REF", "REFERENCE")] <- "R"
  if (!all(trt %in% c("T", "R"))) {
    stop("Parallel BE expects exactly two treatment levels (T and R). Found: ",
         paste(unique(trt), collapse = ", "))
  }
  y <- log(data[[pk_col]])
  yT <- y[trt == "T"]; yR <- y[trt == "R"]
  if (length(yT) < 2 || length(yR) < 2) stop("Need >=2 subjects per arm.")
  diff <- mean(yT) - mean(yR)
  vT <- var(yT); vR <- var(yR); nT <- length(yT); nR <- length(yR)
  # Welch
  se_w  <- sqrt(vT / nT + vR / nR)
  df_w  <- (vT / nT + vR / nR)^2 / ((vT / nT)^2 / (nT - 1) + (vR / nR)^2 / (nR - 1))
  tcw   <- qt(1 - alpha / 2, df_w)
  ci_lw <- 100 * exp(diff - tcw * se_w)
  ci_uw <- 100 * exp(diff + tcw * se_w)
  # Classical (equal variance, pooled)
  sp2  <- ((nT - 1) * vT + (nR - 1) * vR) / (nT + nR - 2)
  se_c <- sqrt(sp2 * (1 / nT + 1 / nR))
  tcc  <- qt(1 - alpha / 2, nT + nR - 2)
  ci_lc <- 100 * exp(diff - tcc * se_c)
  ci_uc <- 100 * exp(diff + tcc * se_c)
  list(
    PE                  = 100 * exp(diff),
    CI_lower_welch      = ci_lw,
    CI_upper_welch      = ci_uw,
    CI_lower_classical  = ci_lc,
    CI_upper_classical  = ci_uc,
    n_T = nT, n_R = nR, pk_column = pk_col
  )
}

#' 2x2x2 crossover BE: fixed-effects ANOVA on log(PK)
#' Treatment coefficient -> PE, 90% CI per Chow & Liu / Schutz 2014
#' @keywords internal
.be_2x2x2 <- function(data, alpha = 0.10) {
  pk_col <- .pick_pk_column(data)
  data <- data[!is.na(data[[pk_col]]) & data[[pk_col]] > 0, , drop = FALSE]
  d <- data.frame(
    Subject   = factor(data$Subject),
    Sequence  = factor(data$Sequence),
    Period    = factor(data$Period),
    Treatment = factor(data$Treatment),
    y         = log(data[[pk_col]])
  )
  fit <- lm(y ~ Sequence + Sequence:Subject + Period + Treatment, data = d)
  co  <- summary(fit)$coefficients
  trt_rows <- grep("^Treatment", rownames(co))
  if (length(trt_rows) != 1) stop("Could not locate Treatment coefficient.")
  est <- co[trt_rows, "Estimate"]
  se  <- co[trt_rows, "Std. Error"]
  df  <- fit$df.residual
  tc  <- qt(1 - alpha / 2, df)
  # Coef sign: Treatment factor levels are alphabetical (R then T) by default,
  # so the coefficient is T - R. PE = 100 * exp(T - R).
  list(
    PE       = 100 * exp(est),
    CI_lower = 100 * exp(est - tc * se),
    CI_upper = 100 * exp(est + tc * se),
    df       = df, n_subj = length(unique(d$Subject)), pk_column = pk_col
  )
}

#' Replicate BE (Method A, fixed-effects ANOVA) on log(PK)
#' @keywords internal
.be_replicate <- function(data, alpha = 0.10) {
  pk_col <- .pick_pk_column(data)
  data <- data[!is.na(data[[pk_col]]) & data[[pk_col]] > 0, , drop = FALSE]
  d <- data.frame(
    Subject   = factor(data$Subject),
    Sequence  = factor(data$Sequence),
    Period    = factor(data$Period),
    Treatment = factor(data$Treatment),
    y         = log(data[[pk_col]])
  )
  # Method A: subject(sequence) as fixed effect
  fit <- lm(y ~ Sequence + Sequence:Subject + Period + Treatment, data = d)
  co  <- summary(fit)$coefficients
  trt_rows <- grep("^Treatment", rownames(co))
  if (length(trt_rows) != 1) stop("Could not locate Treatment coefficient.")
  est <- co[trt_rows, "Estimate"]; se <- co[trt_rows, "Std. Error"]
  df  <- fit$df.residual
  tc  <- qt(1 - alpha / 2, df)
  # CVwR per EMA Q&A: ANOVA on Reference-only data with sequence + subject(sequence) + period
  ref <- d[d$Treatment == levels(d$Treatment)[1], , drop = FALSE]   # alphabetic: R first
  cv_wr <- NA_real_
  if (nrow(ref) > 0 && length(unique(ref$Subject)) > 1) {
    ref$Subject  <- droplevels(ref$Subject)
    ref$Sequence <- droplevels(ref$Sequence)
    ref$Period   <- droplevels(ref$Period)
    fit_r <- tryCatch(
      lm(y ~ Sequence + Sequence:Subject + Period, data = ref),
      error = function(e) NULL
    )
    if (!is.null(fit_r)) {
      msewR <- sum(residuals(fit_r)^2) / fit_r$df.residual
      cv_wr <- 100 * sqrt(exp(msewR) - 1)
    }
  }
  # ---------------- Method B: mixed model with Kenward-Roger DF -----------
  # Per EMA/Schutz: lmerTest::lmer(log(PK) ~ Sequence + Period + Treatment
  #                               + (1|Subject)) with Kenward-Roger ddf.
  mb_PE <- mb_CI_l <- mb_CI_u <- mb_CVwR <- NA_real_
  if (requireNamespace("lmerTest", quietly = TRUE) &&
      requireNamespace("pbkrtest", quietly = TRUE)) {
    fitB <- tryCatch(
      lmerTest::lmer(y ~ Sequence + Period + Treatment + (1 | Subject),
                     data = d, REML = TRUE),
      error = function(e) NULL
    )
    if (!is.null(fitB)) {
      coB    <- summary(fitB, ddf = "Kenward-Roger")$coefficients
      trtB   <- grep("^Treatment", rownames(coB))
      if (length(trtB) == 1) {
        estB  <- coB[trtB, "Estimate"]
        seB   <- coB[trtB, "Std. Error"]
        dfB   <- coB[trtB, "df"]
        tcB   <- qt(1 - alpha / 2, dfB)
        mb_PE   <- 100 * exp(estB)
        mb_CI_l <- 100 * exp(estB - tcB * seB)
        mb_CI_u <- 100 * exp(estB + tcB * seB)
      }
    }
    # Method B CVwR: replicateBE uses the SAME fixed-effects ANOVA on R-only
    # data as Method A (see replicateBE::CV.calc). Reuse cv_wr.
    mb_CVwR <- cv_wr
  }

  list(
    PE       = 100 * exp(est),
    CI_lower = 100 * exp(est - tc * se),
    CI_upper = 100 * exp(est + tc * se),
    CVwR     = cv_wr,
    # Method-A-prefixed aliases so expected_results files using the
    # replicateBE schema (MethodA_PE, MethodA_CI_lower, ...) match.
    MethodA_PE       = 100 * exp(est),
    MethodA_CI_lower = 100 * exp(est - tc * se),
    MethodA_CI_upper = 100 * exp(est + tc * se),
    MethodA_CVwR     = cv_wr,
    # Method B (mixed-effects, Kenward-Roger DF) aliases.
    MethodB_PE       = mb_PE,
    MethodB_CI_lower = mb_CI_l,
    MethodB_CI_upper = mb_CI_u,
    MethodB_CVwR     = mb_CVwR,
    df       = df, n_subj = length(unique(d$Subject)), pk_column = pk_col
  )
}

#' Run BioEQ BE layer on pre-calculated PK parameter data
#' @keywords internal
.run_be_layer <- function(data, design = "auto") {
  data <- .std_be_cols(data)

  # Auto-detect when needed
  if (is.null(design) || is.na(design) || design == "" || design == "auto") {
    if (!"Period" %in% names(data) || length(unique(data$Period)) < 2) {
      design <- "parallel"
    } else if (length(unique(data$Period)) == 2) {
      design <- "2x2x2"
    } else {
      design <- "replicate"
    }
  }
  design <- tolower(design)

  res <- switch(design,
    "parallel"  = .be_parallel(data),
    "2x2x2"     = .be_2x2x2(data),
    "crossover" = .be_2x2x2(data),
    "replicate" = .be_replicate(data),
    stop("Unsupported BE design: ", design)
  )
  list(subject_df = NULL, study = res, raw = res)
}

# ---------------------------------------------------------------------------
# Top-level dispatcher
# ---------------------------------------------------------------------------

#' Run validation for a single dataset.
#'
#' Loads the input from disk, runs the appropriate BioEQ analysis FRESH,
#' compares the result to the stored reference values, and returns a list
#' describing the run. Never caches results.
#'
#' @param dataset_id Manifest dataset_id
#' @param manifest Optional pre-loaded manifest (read once for batch runs)
#' @param validation_dir Path to validation/ directory
#' @return list with fields: dataset_id, status, layer, started_at, finished_at,
#'   computed (list with subject_df and study), expected (data.frame or NULL),
#'   comparison (data.frame), summary (named integer vector), error (character or NULL)
#' @export
run_validation_for_dataset <- function(dataset_id, manifest = NULL,
                                       lambda_points = 3) {
  started <- Sys.time()
  if (is.null(manifest)) manifest <- load_validation_manifest()
  row <- manifest[manifest$dataset_id == dataset_id, , drop = FALSE]
  if (nrow(row) == 0) stop("Unknown dataset_id: ", dataset_id)

  result <- list(
    dataset_id   = dataset_id,
    name         = row$name[1],
    group        = row$group[1],
    layer        = row$layer[1],
    design       = row$design[1],
    status       = row$status[1],
    started_at   = started,
    finished_at  = NA,
    computed     = NULL,
    expected     = NULL,
    comparison   = NULL,
    summary      = c(total = 0L, pass = 0L, fail = 0L, no_ref = 0L),
    error        = NULL,
    overall      = "NOT_RUN"
  )

  # Hard guard: if dataset is not in embedded store, return DATA_MISSING
  if (!validation_dataset_available(dataset_id)) {
    result$error <- "Dataset not found in embedded store."
    result$overall <- "DATA_MISSING"
    result$finished_at <- Sys.time()
    return(result)
  }

  # Always read fresh from embedded store
  data <- tryCatch(
    load_validation_dataset(dataset_id),
    error = function(e) { result$error <<- conditionMessage(e); NULL }
  )
  if (is.null(data)) {
    result$overall <- "ERROR"
    result$finished_at <- Sys.time()
    return(result)
  }

  expected <- load_expected_results(dataset_id)
  result$expected <- expected

  # Run the appropriate analysis fresh
  computed <- tryCatch({
    layer <- toupper(row$layer[1])
    if (layer == "NCA") {
      .run_nca_layer(data, lambda_points = lambda_points)
    } else if (layer %in% c("ANOVA+BE", "ANOVA_BE", "BE")) {
      .run_be_layer(data, design = row$design[1])
    } else if (layer == "BOTH") {
      nca <- .run_nca_layer(data, lambda_points = lambda_points)
      # For "Both" layer, BE analysis would consume the NCA output - treat as future extension.
      list(subject_df = nca$subject_df, study = list())
    } else {
      stop("Unknown layer: ", row$layer[1])
    }
  }, error = function(e) {
    result$error <<- conditionMessage(e)
    NULL
  })
  result$computed <- computed

  if (is.null(computed)) {
    result$overall <- "ERROR"
    result$finished_at <- Sys.time()
    return(result)
  }

  # Build comparison table
  if (is.null(expected) || nrow(expected) == 0) {
    result$overall <- "NO_REFERENCE"
    result$finished_at <- Sys.time()
    return(result)
  }

  cmp <- expected
  cmp$computed_value <- NA
  cmp$deviation      <- NA_real_
  cmp$pass           <- NA
  for (i in seq_len(nrow(cmp))) {
    cv <- .lookup_computed_value(
      parameter = cmp$parameter[i],
      scope     = cmp$scope[i],
      subject   = cmp$subject[i],
      treatment = if ("treatment" %in% names(cmp)) cmp$treatment[i] else NA,
      computed_subject_df = computed$subject_df,
      computed_study_list = computed$study
    )
    cmp$computed_value[i] <- if (is.na(cv)) NA else as.character(cv)
    if (is.na(cv)) {
      cmp$pass[i] <- NA
      next
    }
    chk <- compare_value(
      computed = cv,
      expected = cmp$expected_value[i],
      tolerance_type = cmp$tolerance_type[i],
      tolerance_value = suppressWarnings(as.numeric(cmp$tolerance_value[i]))
    )
    cmp$pass[i] <- chk$pass
    cmp$deviation[i] <- chk$deviation
  }

  result$comparison <- cmp
  result$summary <- c(
    total  = nrow(cmp),
    pass   = sum(isTRUE_vec(cmp$pass)),
    fail   = sum(cmp$pass %in% FALSE),
    no_ref = sum(is.na(cmp$pass))
  )
  result$overall <- if (result$summary[["fail"]] == 0 && result$summary[["pass"]] > 0) {
    "PASS"
  } else if (result$summary[["fail"]] > 0) {
    "FAIL"
  } else {
    "NO_REFERENCE"
  }
  result$finished_at <- Sys.time()
  result
}

#' @keywords internal
isTRUE_vec <- function(x) vapply(x, isTRUE, logical(1))

#' Run validation across multiple datasets.
#' @export
run_validation_suite <- function(dataset_ids, validation_dir = NULL,
                                 lambda_points = 3, progress_fn = NULL) {
  manifest <- load_validation_manifest()
  out <- list()
  total <- length(dataset_ids)
  for (i in seq_along(dataset_ids)) {
    id <- dataset_ids[i]
    if (is.function(progress_fn)) progress_fn(i, total, id)
    out[[id]] <- tryCatch(
      run_validation_for_dataset(id, manifest, lambda_points),
      error = function(e) {
        list(dataset_id = id, overall = "ERROR", error = conditionMessage(e),
             started_at = Sys.time(), finished_at = Sys.time(),
             summary = c(total = 0L, pass = 0L, fail = 0L, no_ref = 0L))
      }
    )
  }
  attr(out, "started_at") <- if (length(out) > 0) out[[1]]$started_at else Sys.time()
  attr(out, "finished_at") <- Sys.time()
  out
}

# ---------------------------------------------------------------------------
# Reporting helpers
# ---------------------------------------------------------------------------

#' Flatten a suite result into a single data.frame (one row per parameter checked)
#' @export
validation_suite_to_dataframe <- function(suite) {
  rows <- list()
  for (id in names(suite)) {
    r <- suite[[id]]
    if (!is.null(r$comparison) && nrow(r$comparison) > 0) {
      df <- r$comparison
      df$dataset_id <- id
      df$dataset_name <- r$name %||% id
      df$dataset_overall <- r$overall
      rows[[length(rows) + 1]] <- df
    } else {
      rows[[length(rows) + 1]] <- data.frame(
        dataset_id = id,
        dataset_name = r$name %||% id,
        dataset_overall = r$overall,
        parameter = NA, scope = NA, subject = NA, treatment = NA,
        expected_value = NA, tolerance_type = NA, tolerance_value = NA,
        unit = NA, reference_source = NA, notes = r$error %||% "",
        computed_value = NA, deviation = NA, pass = NA,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(data.frame())
  # rbind with column tolerance
  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(d) {
    miss <- setdiff(all_cols, names(d))
    for (m in miss) d[[m]] <- NA
    d[all_cols]
  })
  do.call(rbind, rows)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
