# BioEQ - Anomaly / Fraud Detection Analysis Functions
#
# Pure-R analysis functions for the Anomaly Detection Shiny module.
# Detection methods are organised by category and operate on concentration-
# time profile matrices (rows = subject-period, cols = nominal time).
#
# References:
#   - Fuglsang A. (SaToWIB linear regression method for BE fraud detection)
#   - Lin LI (1989). A concordance correlation coefficient. Biometrics.
#   - FDA. f2 similarity factor (dissolution guidance, applied here to PK).
#   - 2024 ISPR paper (validated fraud-detection methods)

# =============================================================================
# Profile preparation
# =============================================================================

#' Build a (subject-period) x time concentration matrix
#'
#' @param data Long-format data frame with Subject, Time, Concentration, and
#'   optionally Treatment / Period columns.
#' @param treatment Optional treatment filter (e.g. "T" or "R"). NULL keeps all.
#' @return list(matrix = numeric matrix, ids = character, treatments = character,
#'   periods = character or NULL, times = numeric)
prepare_profile_matrix <- function(data,
                                    treatment = NULL,
                                    subject_col = "Subject",
                                    time_col = "Time",
                                    conc_col = "Concentration",
                                    treatment_col = "Treatment",
                                    period_col = "Period") {
  if (!subject_col %in% names(data) || !time_col %in% names(data) ||
      !conc_col %in% names(data)) {
    stop("Data must contain Subject, Time, and Concentration columns")
  }

  d <- data
  d[[subject_col]] <- as.character(d[[subject_col]])

  if (!is.null(treatment) && treatment_col %in% names(d)) {
    d <- d[d[[treatment_col]] %in% treatment, , drop = FALSE]
  }

  has_period <- period_col %in% names(d)
  has_trt    <- treatment_col %in% names(d)

  d$.id <- if (has_period && has_trt) {
    paste(d[[subject_col]], d[[period_col]], d[[treatment_col]], sep = "_")
  } else if (has_trt) {
    paste(d[[subject_col]], d[[treatment_col]], sep = "_")
  } else {
    d[[subject_col]]
  }

  times <- sort(unique(as.numeric(d[[time_col]])))
  ids   <- unique(d$.id)

  M <- matrix(NA_real_, nrow = length(ids), ncol = length(times),
              dimnames = list(ids, as.character(times)))

  for (i in seq_along(ids)) {
    sub <- d[d$.id == ids[i], , drop = FALSE]
    idx <- match(as.numeric(sub[[time_col]]), times)
    M[i, idx] <- as.numeric(sub[[conc_col]])
  }

  # Carry metadata for display
  meta <- unique(d[, c(".id", subject_col,
                       intersect(c(treatment_col, period_col), names(d))),
                  drop = FALSE])
  meta <- meta[match(ids, meta$.id), , drop = FALSE]

  list(matrix = M, ids = ids, times = times, meta = meta)
}

# =============================================================================
# Internal helpers
# =============================================================================

# Iterate every unordered pair (i < j); returns indices as two integer vectors
.pair_indices <- function(n) {
  if (n < 2) return(list(i = integer(0), j = integer(0)))
  ij <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  list(i = ij[, 1], j = ij[, 2])
}

# Build a long-format pair data frame skeleton from a matrix
.pair_skeleton <- function(M) {
  ids <- rownames(M)
  pi  <- .pair_indices(nrow(M))
  data.frame(
    id1 = ids[pi$i],
    id2 = ids[pi$j],
    stringsAsFactors = FALSE
  )
}

# Pairwise complete observations between two vectors
.complete <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  list(a = a[ok], b = b[ok], n = sum(ok))
}

# =============================================================================
# Section 2a: Time-aligned pairwise metrics
# =============================================================================

#' SaToWIB linear regression (Fuglsang)
#'
#' For each pair (A, B) fits B ~ A (and A ~ B), reports slope, r-squared,
#' and the mismatch (1 - r^2). Catches duplication and proportional-scaling
#' patterns simultaneously.
pairwise_satowib <- function(M) {
  out <- .pair_skeleton(M)
  pi  <- .pair_indices(nrow(M))
  slope <- r2 <- numeric(length(pi$i))
  for (k in seq_along(pi$i)) {
    cc <- .complete(M[pi$i[k], ], M[pi$j[k], ])
    if (cc$n < 3 || sd(cc$a) == 0 || sd(cc$b) == 0) {
      slope[k] <- NA_real_; r2[k] <- NA_real_; next
    }
    fit <- stats::lm(cc$b ~ cc$a)
    slope[k] <- unname(stats::coef(fit)[2])
    r2[k]    <- summary(fit)$r.squared
  }
  out$slope    <- slope
  out$r2       <- r2
  out$residual <- 1 - r2
  out
}

#' Lin's Concordance Correlation Coefficient
pairwise_ccc <- function(M) {
  out <- .pair_skeleton(M)
  pi  <- .pair_indices(nrow(M))
  ccc <- numeric(length(pi$i))
  for (k in seq_along(pi$i)) {
    cc <- .complete(M[pi$i[k], ], M[pi$j[k], ])
    if (cc$n < 3) { ccc[k] <- NA_real_; next }
    mx <- mean(cc$a); my <- mean(cc$b)
    sx <- stats::var(cc$a); sy <- stats::var(cc$b)
    sxy <- stats::cov(cc$a, cc$b)
    denom <- sx + sy + (mx - my)^2
    ccc[k] <- if (denom > 0) 2 * sxy / denom else NA_real_
  }
  out$ccc <- ccc
  out
}

#' f2 similarity factor
pairwise_f2 <- function(M) {
  out <- .pair_skeleton(M)
  pi  <- .pair_indices(nrow(M))
  f2  <- numeric(length(pi$i))
  for (k in seq_along(pi$i)) {
    cc <- .complete(M[pi$i[k], ], M[pi$j[k], ])
    if (cc$n < 1) { f2[k] <- NA_real_; next }
    msd <- mean((cc$a - cc$b)^2)
    f2[k] <- 50 * log10(100 / sqrt(1 + msd))
  }
  out$f2 <- f2
  out
}

# =============================================================================
# Section 2c: Shape-only metrics
# =============================================================================

.normalize_rows <- function(M, by = c("cmax", "auc")) {
  by <- match.arg(by)
  apply(M, 1, function(r) {
    if (by == "cmax") {
      mx <- suppressWarnings(max(r, na.rm = TRUE))
      if (is.finite(mx) && mx > 0) r / mx else r
    } else { # AUC by linear trapezoid using positional spacing
      r / max(sum(abs(r), na.rm = TRUE), .Machine$double.eps)
    }
  }) |> t()
}

#' Pearson correlation on Cmax-normalised profiles (shape-only)
pairwise_pearson_shape <- function(M, normalize_by = c("cmax", "auc")) {
  Mn <- .normalize_rows(M, by = match.arg(normalize_by))
  .pair_corr(Mn, method = "pearson", colname = "shape_pearson")
}

.pair_corr <- function(M, method, colname) {
  out <- .pair_skeleton(M)
  pi  <- .pair_indices(nrow(M))
  rho <- numeric(length(pi$i))
  for (k in seq_along(pi$i)) {
    cc <- .complete(M[pi$i[k], ], M[pi$j[k], ])
    if (cc$n < 3) { rho[k] <- NA_real_; next }
    rho[k] <- suppressWarnings(stats::cor(cc$a, cc$b, method = method))
  }
  out[[colname]] <- rho
  out
}

#' Pearson correlation on first differences (local slopes)
pairwise_derivative <- function(M) {
  D <- t(apply(M, 1, diff))
  if (is.null(dim(D))) D <- matrix(D, nrow = nrow(M))
  rownames(D) <- rownames(M)
  .pair_corr(D, method = "pearson", colname = "derivative_pearson")
}

# =============================================================================
# Section 2b: Time-flexible methods (DTW + cross-correlation)
# =============================================================================

#' Dynamic Time Warping distance with optimal lag detection
#'
#' Uses the `dtw` package if available; falls back to NA otherwise. Optimal lag
#' is recovered from cross-correlation alignment so callers can flag pattern 1c.
pairwise_dtw <- function(M) {
  out <- .pair_skeleton(M)
  pi  <- .pair_indices(nrow(M))
  dist_aligned <- lag_opt <- numeric(length(pi$i))
  has_dtw <- requireNamespace("dtw", quietly = TRUE)
  for (k in seq_along(pi$i)) {
    a <- M[pi$i[k], ]; b <- M[pi$j[k], ]
    cc <- .complete(a, b)
    if (cc$n < 4) { dist_aligned[k] <- NA_real_; lag_opt[k] <- NA_real_; next }
    if (has_dtw) {
      d <- tryCatch(
        dtw::dtw(cc$a, cc$b, distance.only = TRUE)$normalizedDistance,
        error = function(e) NA_real_
      )
      dist_aligned[k] <- d
    } else {
      dist_aligned[k] <- sqrt(mean((cc$a - cc$b)^2))
    }
    if (stats::sd(cc$a) == 0 || stats::sd(cc$b) == 0) {
      lag_opt[k] <- NA_real_
    } else {
      lag_opt[k] <- tryCatch({
        cf <- suppressWarnings(stats::ccf(cc$a, cc$b, plot = FALSE,
                                           lag.max = max(1, floor(cc$n / 2))))
        as.numeric(cf$lag[which.max(abs(cf$acf))])
      }, error = function(e) NA_real_)
    }
  }
  out$dtw_distance <- dist_aligned
  out$dtw_lag      <- lag_opt
  out
}

#' Peak cross-correlation, corresponding lag, and post-shift agreement.
#'
#' For each pair we find the lag that maximises |corr|, then we *apply* that
#' lag and measure how well the two aligned sub-vectors agree (Pearson r^2).
#' Cross-correlation alone only measures shape similarity at a lag — many
#' normal PK profiles peak together at lag ±1. The post-shift r^2 is the
#' direct test for a time-shifted COPY: a true shifted duplicate gives r^2 ~ 1
#' on the aligned overlap; a coincidental shape match gives r^2 << 1.
pairwise_crosscorr <- function(M) {
  out <- .pair_skeleton(M)
  pi  <- .pair_indices(nrow(M))
  peak <- lag <- shift_r2 <- numeric(length(pi$i))
  for (k in seq_along(pi$i)) {
    cc <- .complete(M[pi$i[k], ], M[pi$j[k], ])
    if (cc$n < 4 || stats::sd(cc$a) == 0 || stats::sd(cc$b) == 0) {
      peak[k] <- NA_real_; lag[k] <- NA_real_; shift_r2[k] <- NA_real_; next
    }
    res <- tryCatch({
      cf  <- suppressWarnings(stats::ccf(cc$a, cc$b, plot = FALSE,
                                          lag.max = max(1, floor(cc$n / 2))))
      idx <- which.max(abs(cf$acf))
      list(peak = as.numeric(cf$acf[idx]), lag = as.numeric(cf$lag[idx]))
    }, error = function(e) list(peak = NA_real_, lag = NA_real_))
    peak[k] <- res$peak
    lag[k]  <- res$lag

    # Post-shift agreement: align b to a using the optimal lag, then compute
    # r^2 on the overlapping segment. ccf convention is corr(a[t], b[t+lag]):
    # at lag L we compare a[1..n-|L|] with b[1+|L|..n] for L<0 and
    # a[|L|+1..n] with b[1..n-|L|] for L>0 — i.e. drop |L| values from
    # whichever end aligns the two series.
    L <- res$lag
    if (is.finite(L) && abs(L) >= 1 && (cc$n - abs(L)) >= 4) {
      if (L < 0) { x <- cc$a[seq_len(cc$n - abs(L))]; y <- cc$b[(abs(L) + 1):cc$n] }
      else       { x <- cc$a[(L + 1):cc$n];           y <- cc$b[seq_len(cc$n - L)] }
      shift_r2[k] <- tryCatch({
        if (stats::sd(x) == 0 || stats::sd(y) == 0) NA_real_
        else suppressWarnings(stats::cor(x, y))^2
      }, error = function(e) NA_real_)
    } else {
      shift_r2[k] <- NA_real_
    }
  }
  out$xcorr_peak <- peak
  out$xcorr_lag  <- lag
  out$shift_r2   <- shift_r2
  out
}


`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# Section 9b: Focused pair batteries (one per fraud pattern)
# =============================================================================
#
# Each battery runs only the metrics that detect a specific kind of profile
# re-use, computes a single suspicion score in [0,1] (1 = most suspicious),
# and returns the ranked pair table. Keep these separate from
# aggregate_pair_ranks so each analysis has cutoffs tuned to its pattern.


#' Overlapping-duplicate battery
#'
#' Detects literal / near-literal re-use. Sorted by Lin's CCC — the single
#' most direct measure of profile-level agreement (shape + scale). f2, slope,
#' and r² are included as supporting columns.
run_overlap_battery <- function(M) {
  base <- .pair_skeleton(M)
  base <- merge(base, pairwise_ccc(M),     by = c("id1", "id2"))
  base <- merge(base, pairwise_f2(M),      by = c("id1", "id2"))
  base <- merge(base, pairwise_satowib(M), by = c("id1", "id2"))
  base[order(-base$ccc, na.last = TRUE), , drop = FALSE]
}

#' Scaled-duplicate battery (dilution / concentration)
#'
#' Profiles whose values are a proportional rescaling of each other. Sorted by
#' r² of the SaToWIB linear regression — the cleanest line fit wins. No gating
#' on slope, so exact duplicates (slope = 1) appear alongside scaled copies.
#' Inspect the slope column to distinguish: slope ≈ 1 = exact dup, slope ≠ 1
#' = scaled copy.
run_scaled_battery <- function(M) {
  base <- .pair_skeleton(M)
  base <- merge(base, pairwise_satowib(M), by = c("id1", "id2"))
  base$scale_score <- base$r2
  base[order(-base$scale_score, na.last = TRUE), , drop = FALSE]
}

#' Time-shifted duplicate battery
#'
#' Detects profiles that are an actual COPY of another, time-shifted. The key
#' test is post-shift agreement: at the optimal lag, the two aligned vectors
#' must match almost exactly (r^2 -> 1). High xcorr_peak alone is not enough,
#' because many normal PK profiles share the same rise/fall shape and produce
#' high correlations at lag ±1 by coincidence.
#'
#' Gates (on the cross-correlation columns, unchanged):
#'   - |xcorr_lag|  >= 1   (best alignment is NOT at zero shift)
#'   - xcorr_peak   > 0    (positive correlation at that lag)
#'   - shift_r2     >= 0.90 (post-alignment values truly agree)
#' Sorted by shift_r2 descending — the strongest aligned-copy match wins.
#'
#' dtw_distance/dtw_lag (pairwise_dtw()) are merged in as supporting evidence
#' only — Dynamic Time Warping finds the best nonlinear alignment rather than
#' a single integer lag, so a small dtw_distance corroborating a qualifying
#' xcorr-based match is a stronger signal than either metric alone. They do
#' not affect the gate above and are NA for the same 0 rows the gate excludes,
#' since a table row only survives when its (gated) shift_r2 is non-NA.
run_lag_battery <- function(M) {
  base <- .pair_skeleton(M)
  base <- merge(base, pairwise_crosscorr(M), by = c("id1", "id2"))
  base <- merge(base, pairwise_dtw(M),       by = c("id1", "id2"))

  valid <- !is.na(base$xcorr_lag) & abs(base$xcorr_lag) >= 1 &
           !is.na(base$xcorr_peak) & base$xcorr_peak > 0 &
           !is.na(base$shift_r2)  & base$shift_r2 >= 0.90
  base$xcorr_peak[!valid] <- NA_real_
  base$xcorr_lag[!valid]  <- NA_real_
  base$shift_r2[!valid]   <- NA_real_
  base[order(-base$shift_r2, na.last = TRUE), , drop = FALSE]
}

#' Dynamic-pattern battery
#'
#' Detects reuse of rise/fall dynamics independent of magnitude. Sorted by
#' derivative_pearson — the Pearson correlation of first differences, which
#' directly measures whether two profiles share the same direction and magnitude
#' of change between timepoints. shape_pearson and ccc are included as
#' supporting columns.
run_dynamics_battery <- function(M) {
  base <- .pair_skeleton(M)
  base <- merge(base, pairwise_derivative(M),    by = c("id1", "id2"))
  base <- merge(base, pairwise_pearson_shape(M), by = c("id1", "id2"))
  base <- merge(base, pairwise_ccc(M),           by = c("id1", "id2"))
  base[order(-base$derivative_pearson, na.last = TRUE), , drop = FALSE]
}

# =============================================================================
# Section 7: Distributional / structural sanity checks
# =============================================================================

#' Within-subject CV of a chosen PK metric across periods
#'
#' Operates on either NCA results (per-subject AUC/Cmax) or raw conc-time data
#' (will compute Cmax/AUC summaries per subject-period first).
compute_within_subject_cv <- function(data,
                                       subject_col = "Subject",
                                       treatment_col = "Treatment",
                                       metric = "Cmax") {
  if (metric %in% names(data)) {
    df <- data
  } else {
    # Compute per-subject-treatment Cmax / AUC0t from raw long data
    if (!all(c("Time", "Concentration") %in% names(data))) {
      stop("Data must contain Time and Concentration if metric is not present")
    }
    grp <- split(data, list(data[[subject_col]], data[[treatment_col]]),
                 drop = TRUE)
    df <- do.call(rbind, lapply(grp, function(g) {
      g <- g[order(g$Time), ]
      data.frame(
        Subject   = unique(g[[subject_col]]),
        Treatment = unique(g[[treatment_col]]),
        Cmax  = max(g$Concentration, na.rm = TRUE),
        AUC0t = sum(diff(g$Time) * (head(g$Concentration, -1) +
                                       tail(g$Concentration, -1)) / 2,
                    na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  }
  res <- aggregate(df[[metric]],
                   by = list(Subject = df[[subject_col]]),
                   FUN = function(x) {
                     if (length(x) < 2) return(NA_real_)
                     sqrt(exp(stats::var(log(x[x > 0]))) - 1) * 100
                   })
  names(res)[2] <- "CV_within_pct"
  res
}

#' Tmax distribution per subject-period
compute_tmax_distribution <- function(data,
                                       subject_col = "Subject",
                                       treatment_col = "Treatment",
                                       period_col = "Period",
                                       time_col = "Time",
                                       conc_col = "Concentration") {
  if (!all(c(subject_col, time_col, conc_col) %in% names(data))) {
    stop("Data must contain Subject, Time, and Concentration columns")
  }
  by_cols <- intersect(c(subject_col, treatment_col, period_col), names(data))
  grp <- split(data, lapply(by_cols, function(c) data[[c]]), drop = TRUE)
  do.call(rbind, lapply(grp, function(g) {
    g <- g[order(g[[time_col]]), ]
    tmax <- g[[time_col]][which.max(g[[conc_col]])]
    cmax <- max(g[[conc_col]], na.rm = TRUE)
    data.frame(
      setNames(lapply(by_cols, function(c) unique(g[[c]])[1]), by_cols),
      Tmax = tmax, Cmax = cmax,
      stringsAsFactors = FALSE
    )
  }))
}
