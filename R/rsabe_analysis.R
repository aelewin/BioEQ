# BioEQ - Reference-Scaled Average Bioequivalence (RSABE) Analysis
# 
# Implements two RSABE methods:
#   1. FDA Linearized Scaled Criterion (Howe UCB) — FDA guidance default
#   2. Non-Central TOST (ncTOST) — Exact method per Tóthfalusi & Endrényi
#
# Key references:
#   - FDA Guidance for HVD/HVDPs (2021)
#   - Tóthfalusi L, Endrényi L (2016). An Exact Procedure for the Evaluation
#     of Reference-Scaled Average Bioequivalence. The AAPS Journal, 18(2),
#     476-489. DOI: 10.1208/s12248-016-9873-6
#   - Howe WG (1974). Approximate Confidence Limits on the Mean of X + Y
#     Where X and Y Are Two Tabled Independent Random Variables. Journal of
#     the American Statistical Association, 69, 789-794.
#
# Regulatory constants:
#   - θ_s = ln(1.25)/σ_w0 = 0.2231/0.25 ≈ 0.8924 (FDA scaling proportionality constant)
#   - σ_w0 = 0.25 — the criterion constant used INSIDE the scaled-limits
#     formula only (s²_w0 = 0.0625, CV ≈ 25.4%). This is NOT the switching
#     threshold that decides whether RSABE scaling applies to a drug — that
#     decision uses a separate, higher threshold: s_wR ≥ 0.294 (CV_wR ≈ 30%,
#     s²_wR ≈ 0.0864). See s_wR_switch / s2_wR_switch below, where that
#     threshold is actually applied.
#   - Point estimate constraint: 80-125% (FDA requirement)
#
# The linearized criterion: η = d² − θ²_s · σ²_wR
#   Evaluated at σ_wR = σ_w0 = 0.25, the formula's scaled limits happen to
#   collapse exactly to standard ABE limits — a mathematical property of the
#   constant, not the switching decision itself:
#     θ_s · σ_w0 = 0.8924 × 0.25 = 0.2231 = ln(1.25)
#     → exp(±0.2231) = [80%, 125%]
#   When σ_wR > σ_w0, θ_s · σ_wR > ln(1.25) → the formula's limits mathematically
#   expand beyond 80-125% — but RSABE scaling is only actually applied once
#   σ_wR reaches the real switching threshold, σ_wR ≥ 0.294.
#
# =============================================================================

# Helper: null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x


# =============================================================================
# SECTION 0: HELPER FUNCTIONS
# =============================================================================

#' Hedges' Bias Correction Factor
#'
#' The sample effect size d = (Y_T - Y_R) / s_WR is a biased estimator of
#' the population effect size delta. Hedges' correction adjusts for this.
#'
#' Eq. 8 of Tóthfalusi & Endrényi (2016):
#'   cr(df) = 1 - 3 / (4*df - 1)
#'
#' The exact form is cr(df) = Gamma(df/2) / [sqrt(df/2) * Gamma((df-1)/2)]
#' but the approximation is standard and accurate for df >= 3.
#'
#' @param df Degrees of freedom (associated with s_WR)
#' @return Bias correction factor cr (< 1)
#' @export
hedges_correction <- function(df) {
  1 - 3 / (4 * df - 1)
}


#' Compute Design-Dependent Constant K
#'
#' K encodes the relationship Var(d_hat) = K^2 * sigma^2_WR, where
#' d_hat = Y_T - Y_R (treatment difference on log scale) and sigma^2_WR
#' is the within-subject reference variance.
#'
#' The formulas below are from Tóthfalusi & Endrényi (2016):
#'   Eq. 21: 2x2x4 full replicate (TRTR/RTRT)
#'   Eq. 24: 2x3x3 partial replicate (TRR/RTR/RRT)
#'   Eq. 25: 2x2x3 full replicate (TRT/RTR)
#'   Eq. 22: General formula for arbitrary designs
#'
#' Under homoscedasticity (z = s_WT/s_WR = 1):
#'   2x2x4: K = sqrt((z^2 + 1) / (2*n))  -> 1/sqrt(n)
#'   2x3x3 partial: K = sqrt((z^2 + 0.5) / n)  -> sqrt(1.5/n)
#'   2x2x3: K = sqrt(0.75*(z^2 + 1) / n)  -> sqrt(1.5/n)
#'
#' Under heteroscedasticity, z is estimated from the data and the
#' formulas retain the z terms.
#'
#' @param design_info Design info from detect_replicate_design()
#' @param refvar_result Reference/Test intra-subject variance results, from
#'   compute_reference_anova_variance() (provides s2_wR, s2_wT, n_R, n_T)
#' @return List with K, z, df_nct (degrees of freedom for noncentral t)
#' @export
compute_K_constant <- function(design_info, refvar_result) {
  
  s2_wR <- refvar_result$s2_wR
  s2_wT <- refvar_result$s2_wT
  n_R <- refvar_result$n_R  # Number of subjects with replicated R
  n_T <- refvar_result$n_T  # Number of subjects with replicated T
  
  design_type <- design_info$design_type
  n_total <- design_info$n_subjects_total
  n_seq <- length(design_info$sequences)
  seq_dist <- design_info$sequence_distribution
  
  # Estimate z = s_WT / s_WR (heteroscedasticity ratio)
  # For partial replicate designs, s2_wT may not be estimable;
  # assume homoscedasticity (z = 1) in that case.
  if (!is.na(s2_wT) && s2_wR > 0 && s2_wT > 0) {
    z <- sqrt(s2_wT / s2_wR)
    bioeq_log(sprintf("Heteroscedasticity ratio z = s_WT/s_WR = %.4f", z), "DEBUG")
  } else {
    z <- 1.0
    bioeq_log("Assuming homoscedasticity (z=1) - s2_wT not available", "DEBUG")
  }
  
  z2 <- z^2
  K <- NA_real_
  df_nct <- NA_real_
  design_label <- ""
  
  if (grepl("2x2x4|Full Replicate", design_type, ignore.case = TRUE) &&
      design_info$n_periods == 4) {
    # -----------------------------------------------------------------------
    # 2x2x4 Full Replicate (TRTR/RTRT) — Eq. 21
    # K = sqrt((z^2 + 1) / (2*n)) for balanced (n per sequence)
    # For unbalanced: use per-sequence sample sizes
    # -----------------------------------------------------------------------
    design_label <- "2x2x4 Full Replicate (Eq. 21)"
    
    if (length(seq_dist) == 2) {
      n1 <- as.numeric(seq_dist[1])
      n2 <- as.numeric(seq_dist[2])
      # General unbalanced: K^2 = (z^2 + 1)/2 * (1/n1 + 1/n2) / 2
      # Per Eq. 22 with contrast coefficients C_T = C_R = 1/2 for each period:
      # Var(d_hat) = sigma^2_WR * (z^2 + 1) / (2) * (1/n1 + 1/n2) / 2
      # Actually for TRTR/RTRT, each sequence contributes d_hat_j = Y_T - Y_R
      # with Var = (z^2+1)*sigma^2_WR/2 per subject. Combined:
      # Var(d_hat) = (z^2+1)*sigma^2_WR/2 * (1/n1 + 1/n2)/2  -- wrong
      # 
      # For TRTR/RTRT balanced (n per seq), the standard result is:
      #   Var(d_hat) = (z^2 + 1) * sigma^2_WR / (2*n)
      # For unbalanced, the inverse-variance weighted estimator gives:
      #   1/Var(d_hat) = n1/(sigma^2*(z^2+1)/2) + n2/(sigma^2*(z^2+1)/2)
      #                = (n1+n2)*2 / (sigma^2*(z^2+1))
      #   Var(d_hat) = sigma^2_WR*(z^2+1) / (2*(n1+n2))
      K <- sqrt((z2 + 1) / (2 * (n1 + n2)))
      df_nct <- n1 + n2 - 2  # Eq. from paper: sum(nj) - s
    } else {
      # Fallback: use total subjects
      n_per_seq <- n_total / n_seq
      K <- sqrt((z2 + 1) / (2 * n_per_seq))
      df_nct <- n_total - n_seq
    }
    
  } else if (grepl("Partial Replicate|2x3x3", design_type, ignore.case = TRUE) ||
             (design_info$is_partial_replicate && design_info$n_periods == 3)) {
    # -----------------------------------------------------------------------
    # 2x3x3 Partial Replicate (TRR/RTR/RRT) — Eq. 24
    # K = sqrt((z^2 + 0.5) / n) for balanced (n per sequence)
    # -----------------------------------------------------------------------
    design_label <- "2x3x3 Partial Replicate (Eq. 24)"
    
    if (length(seq_dist) >= 2) {
      # For unbalanced partial replicate, the harmonic-mean approach:
      # Eq. 22 with the appropriate contrast coefficients.
      # For balanced: n_per_seq = n_total / 3
      n_per_seq <- n_total / n_seq  # approximate for balanced
      K <- sqrt((z2 + 0.5) / n_per_seq)
      # df for s_WR: only subjects in sequences with replicated R
      # In TRR/RTR/RRT, 2 out of 3 sequences have replicated R
      # df = n_R (already computed correctly by compute_reference_anova_variance())
      df_nct <- n_R
    } else {
      n_per_seq <- n_total / 3
      K <- sqrt((z2 + 0.5) / n_per_seq)
      df_nct <- n_R
    }
    
  } else if (grepl("2x2x3", design_type, ignore.case = TRUE) ||
             (!design_info$is_partial_replicate && design_info$n_periods == 3 &&
              design_info$is_replicate)) {
    # -----------------------------------------------------------------------
    # 2x2x3 Full Replicate (TRT/RTR) — Eq. 25
    # K = sqrt(0.75 * (z^2 + 1) / n) for balanced
    # df = n/2 - 1 (s_WR estimated only from RTR sequence)
    # -----------------------------------------------------------------------
    design_label <- "2x2x3 Full Replicate (Eq. 25)"
    
    if (length(seq_dist) == 2) {
      n1 <- as.numeric(seq_dist[1])
      n2 <- as.numeric(seq_dist[2])
      n_per_seq <- (n1 + n2) / 2
      K <- sqrt(0.75 * (z2 + 1) / n_per_seq)
      # For TRT/RTR, s_WR comes from RTR sequence only
      # df = number of subjects in the sequence with replicated R minus 1
      # compute_reference_anova_variance() already handles this — use its n_R
      df_nct <- n_R
    } else {
      n_per_seq <- n_total / 2
      K <- sqrt(0.75 * (z2 + 1) / n_per_seq)
      df_nct <- n_R
    }
    
  } else {
    # -----------------------------------------------------------------------
    # Fallback: estimate K from SE_d and s_WR
    # Since Var(d_hat) = K^2 * sigma^2_WR, we can estimate:
    #   K = SE_d / s_WR
    # This works for any design but requires the ANOVA SE to be trustworthy.
    # -----------------------------------------------------------------------
    design_label <- "Generic (K estimated from SE_d / s_WR)"
    bioeq_log("Unknown design type for K constant - estimating from SE_d/s_WR", "WARNING")
    K <- NA_real_  # Will be filled in by the caller from SE_d / s_WR
    df_nct <- n_R  # Best available DF for s_WR
  }

  bioeq_log(sprintf("Design: %s%s", design_label,
                     if (!is.na(K)) sprintf(" | K=%.6f, df_nct=%d", K, df_nct) else ""), "DEBUG")
  
  return(list(
    K = K,
    z = z,
    df_nct = as.integer(df_nct),
    design_label = design_label
  ))
}




# =============================================================================
# SECTION 1B: FDA GUIDANCE — PER-SUBJECT REFERENCE-SCALED CONTRASTS
# =============================================================================
#
# The FDA's own worked example (Progesterone Capsules product-specific
# guidance, "Method for Statistical Analysis Using the Reference-Scaled
# Average Bioequivalence Approach," Recommended Apr 2010 / Revised Feb 2011)
# does NOT fit a multi-term ANOVA on the raw per-period observations for
# either the treatment-effect estimate or s²_wR. It first collapses each
# subject's data to two per-subject derived quantities:
#
#   D_ij = R_ij1 - R_ij2          (a subject's two Reference replicates,
#                                   differenced; chronological order)
#   I_ij = T_ij - (R_ij1+R_ij2)/2  (partial replicate, one Test period), or
#   I_ij = (T_ij1+T_ij2)/2 - (R_ij1+R_ij2)/2 (full replicate, two Test periods)
#
# and then fits a SINGLE-TERM model, `~ Sequence` only, on each of I and D
# (PROC GLM for the partial 3-way example; PROC MIXED with DDFM=SATTERTH but
# no RANDOM/REPEATED statement for the full 4-way example — with no random
# effect declared, PROC MIXED's REML residual variance is numerically
# identical to PROC GLM's OLS residual MS here, so both collapse to the same
# `lm(~ Sequence)` computation in R). Only subjects with >=2 valid Reference
# observations contribute (this is also how the FDA guidance's 2x2x3
# TRT/RTR design note — "s_wR comes from RTR sequence only" — falls out
# automatically, with no separate design-specific branch needed).
#
# This function builds those per-subject I/D pairs generically for any
# replicate design (2x3x3 partial, 2x2x4 full, 2x2x3, or an arbitrary
# multi-sequence variant), and — where a subject also has 2 Test periods —
# the analogous Test-side contrast D_T = T_ij1 - T_ij2, used only to obtain
# s²_wT/df_wT for the heteroscedasticity ratio (compute_K_constant()); it
# plays no role in the FDA guidance's own worked example, which is silent on
# s²_wT entirely.
# =============================================================================

#' Build Per-Subject Reference-Scaled Contrasts (FDA Guidance Method)
#'
#' @param data Data frame with Subject, Sequence, Period, Treatment, and the
#'   log-transformed parameter column
#' @param param_col Name of the log-transformed parameter column
#' @return Data frame with one row per qualifying subject: Subject, Sequence,
#'   I (treatment contrast), D (Reference replicate difference), D_T
#'   (Test replicate difference, NA unless the subject has 2 Test periods)
#' @export
build_rsabe_subject_contrasts <- function(data, param_col,
                                           subject_col = "Subject",
                                           sequence_col = "Sequence",
                                           period_col = "Period",
                                           treatment_col = "Treatment") {

  data <- data[!is.na(data[[param_col]]), ]
  trt_upper <- toupper(as.character(data[[treatment_col]]))
  is_ref  <- trt_upper %in% c("R", "REFERENCE", "REF")
  is_test <- trt_upper %in% c("T", "TEST")

  subjects <- unique(as.character(data[[subject_col]]))
  out <- vector("list", length(subjects))
  k <- 0L

  for (subj in subjects) {
    sd_mask <- as.character(data[[subject_col]]) == subj
    r_rows <- data[sd_mask & is_ref, , drop = FALSE]
    t_rows <- data[sd_mask & is_test, , drop = FALSE]

    if (nrow(r_rows) < 2) next  # needs a replicated Reference to contribute

    r_rows <- r_rows[order(r_rows[[period_col]]), ]
    R1 <- r_rows[[param_col]][1]
    R2 <- r_rows[[param_col]][2]

    if (nrow(t_rows) >= 2) {
      t_rows <- t_rows[order(t_rows[[period_col]]), ]
      T1 <- t_rows[[param_col]][1]
      T2 <- t_rows[[param_col]][2]
      T_mean <- (T1 + T2) / 2
      D_T <- T1 - T2
    } else if (nrow(t_rows) == 1) {
      T_mean <- t_rows[[param_col]][1]
      D_T <- NA_real_
    } else {
      next  # no Test observation at all — can't form I_ij
    }

    seq_val <- as.character(r_rows[[sequence_col]][1])
    k <- k + 1L
    out[[k]] <- data.frame(
      Subject = subj, Sequence = seq_val,
      I = T_mean - (R1 + R2) / 2,
      D = R1 - R2,
      D_T = D_T,
      stringsAsFactors = FALSE
    )
  }

  out <- out[seq_len(k)]
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}


#' Fit the FDA Guidance's Sequence-Only Model for the I_ij Contrast
#'
#' Reproduces SAS's `proc glm; class seq; model ilat=seq; estimate 'average'
#' intercept 1 seq (1/m ... 1/m);` (partial replicate) / the equivalent
#' `proc mixed ... ddfm=satterth` call with no RANDOM/REPEATED statement
#' (full replicate) — an EQUALLY-WEIGHTED marginal mean of I across Sequence
#' levels (not the raw sample mean, which would over-weight larger
#' sequences), with its SE and CI obtained directly from the linear model's
#' variance-covariance matrix via the averaging contrast.
#'
#' @param values Numeric vector of per-subject I_ij values
#' @param sequence Character/factor vector of each subject's Sequence
#' @param alpha One-sided alpha (default 0.05 -> 90% two-sided CI, matching
#'   the FDA guidance's `alpha=0.1` CLPARM call)
#' @return List with estimate, se, df, ci_lower, ci_upper, model
#' @export
fit_rsabe_seq_mean <- function(values, sequence, alpha = 0.05) {
  seqf <- factor(sequence)
  df_fit <- data.frame(y = values, seq = seqf)
  model <- lm(y ~ seq, data = df_fit)
  df_resid <- df.residual(model)

  levs <- levels(seqf)
  newdata <- data.frame(seq = factor(levs, levels = levs))
  X <- model.matrix(~seq, data = newdata)
  Xavg <- colMeans(X)  # equal 1/m weight per Sequence level

  estimate <- as.numeric(Xavg %*% coef(model))
  se <- as.numeric(sqrt(t(Xavg) %*% vcov(model) %*% Xavg))
  t_crit <- qt(1 - alpha, df_resid)

  list(
    estimate = estimate, se = se, df = as.integer(df_resid),
    ci_lower = estimate - t_crit * se, ci_upper = estimate + t_crit * se,
    model = model
  )
}


#' Fit the FDA Guidance's Sequence-Only Model for the D_ij Contrast
#'
#' Reproduces SAS's `proc glm; class seq; model dlat=seq;` (partial
#' replicate) / `proc mixed ... ddfm=satterth` with no RANDOM/REPEATED
#' statement (full replicate) — both reduce to an ordinary `lm(D ~ Sequence)`
#' since there is no random effect to estimate (each subject contributes
#' exactly one D value). Var(D_ij) = 2*sigma^2_wR (difference of two iid
#' replicate measurements), so s²_wR = Residual Mean Sq / 2.
#'
#' @param values Numeric vector of per-subject D_ij (or D_T) values
#' @param sequence Character/factor vector of each subject's Sequence
#' @return List with s2 (=s²_wR or s²_wT), df, model
#' @export
fit_rsabe_seq_variance <- function(values, sequence) {
  seqf <- factor(sequence)
  model <- lm(values ~ seqf)
  at <- anova(model)
  s2 <- at["Residuals", "Mean Sq"] / 2
  dfw <- at["Residuals", "Df"]
  list(s2 = s2, df = as.integer(dfw), model = model)
}


# =============================================================================
# SECTION 2: ANOVA MODEL FOR TREATMENT DIFFERENCE
# =============================================================================

#' Fit ANOVA/Mixed Model for RSABE Treatment Effect
#' 
#' Fits the standard model: ln(PK) ~ Sequence + Period + Treatment + Subject(Sequence)
#' Extracts the treatment difference (d = μ_T - μ_R on log scale) and its SE.
#'
#' @param data Data frame with Subject, Treatment, Period, Sequence, and PK parameter
#' @param param_col Name of the log-transformed parameter column
#' @param anova_model ANOVA method: "fixed" or "nlme"
#' @return List with d_hat (treatment diff), se_d, df_d, model details
#' @export
fit_rsabe_model <- function(data, param_col, anova_model = "fixed") {
  
  # Prepare data
  model_data <- data[!is.na(data[[param_col]]), ]
  model_data$subj <- as.factor(model_data$Subject)
  model_data$seq <- as.factor(model_data$Sequence)
  model_data$prd <- as.factor(model_data$Period)
  
  # Set Treatment factor with R as reference
  unique_treatments <- unique(model_data$Treatment)
  if (all(c("R", "T") %in% unique_treatments)) {
    model_data$drug <- factor(model_data$Treatment, levels = c("R", "T"))
    drug_coef_name <- "drugT"
  } else if (all(c("Reference", "Test") %in% unique_treatments)) {
    model_data$drug <- factor(model_data$Treatment, levels = c("Reference", "Test"))
    drug_coef_name <- "drugTest"
  } else {
    model_data$drug <- as.factor(model_data$Treatment)
    drug_coef_name <- paste0("drug", levels(model_data$drug)[2])
  }
  
  # Response variable
  model_data$y <- model_data[[param_col]]
  
  # Comprehensive SAS-style ANOVA breakdown (Sequence/Subject(Sequence)/Period/
  # Treatment/Residual, Type III SS, plus the Subject(Seq)-as-error-term test for
  # Sequence) — a parallel/accompanying table alongside RSABE's own Howe UCB /
  # ncTOST / ISC-based scaling decision (which is computed separately, unaffected
  # by this). Reuses the exact same builder ABE's perform_simple_anova() uses, so
  # the underlying model always determines it — never independently recomputed.
  anova_comprehensive <- NULL
  subj_seq_analysis <- NULL
  type3_ss <- NULL

  if (anova_model == "fixed") {
    # Fixed effects model: y ~ seq + subj:seq + prd + drug
    model <- lm(y ~ seq + subj:seq + prd + drug, data = model_data, na.action = na.omit)
    model_summary <- summary(model)
    anova_table <- anova(model)

    coeffs <- coef(model)
    if (!(drug_coef_name %in% names(coeffs))) {
      stop("Treatment coefficient '", drug_coef_name, "' not found in model")
    }

    d_hat <- coeffs[[drug_coef_name]]
    se_d <- model_summary$coefficients[drug_coef_name, "Std. Error"]
    df_d <- anova_table["Residuals", "Df"]
    residual_mse <- anova_table["Residuals", "Mean Sq"]

    bioeq_log(sprintf("Fixed model: d_hat=%.6f, SE=%.6f, df=%d", d_hat, se_d, df_d), "DEBUG")

    built <- tryCatch(build_crossover_anova_tables(model), error = function(e) NULL)
    if (!is.null(built)) {
      anova_comprehensive <- built$comprehensive_anova
      subj_seq_analysis <- built$subj_seq_analysis
    }

  } else {
    # Mixed effects model using nlme
    # Note: RSABE only supports fixed and nlme. If user selected satterthwaite or
    # kenward-roger, map to nlme with a warning (RSABE's intra-subject reference
    # variance is estimated via a separate Reference-only ANOVA — see
    # compute_reference_anova_variance() — not this treatment-effect model, so
    # the DF approximation method chosen here is less critical).
    if (anova_model %in% c("satterthwaite", "kenward-roger")) {
      bioeq_log(sprintf(
        "RSABE does not support '%s' DF approximation - using nlme (REML) instead (s2_wR/CVwR come from a separate Reference-only ANOVA, not this model)",
        anova_model), "WARNING")
    }
    
    if (!requireNamespace("nlme", quietly = TRUE)) {
      stop("nlme package required for mixed effects RSABE model")
    }
    
    model <- nlme::lme(y ~ seq + prd + drug,
                       random = ~1 | subj,
                       data = model_data,
                       method = "REML",
                       na.action = na.omit)
    
    model_summary <- summary(model)
    tTable <- model_summary$tTable
    
    if (!(drug_coef_name %in% rownames(tTable))) {
      stop("Treatment coefficient '", drug_coef_name, "' not found in mixed model")
    }
    
    d_hat <- tTable[drug_coef_name, "Value"]
    se_d <- tTable[drug_coef_name, "Std.Error"]
    df_d <- tTable[drug_coef_name, "DF"]
    residual_mse <- model_summary$sigma^2
    
    anova_table <- anova(model)

    # Type III (marginal) tests of fixed effects — the mixed-model equivalent of
    # the fixed branch's comprehensive table above (SAS PROC MIXED "Type 3 Tests
    # of Fixed Effects"): Sequence, Period, Treatment each with numDF/denDF/F/p.
    type3_ss <- tryCatch(anova(model, type = "marginal"), error = function(e) anova_table)

    bioeq_log(sprintf("Mixed model: d_hat=%.6f, SE=%.6f, df=%d", d_hat, se_d, df_d), "DEBUG")
  }

  # Point estimate (geometric mean ratio)
  pe_ratio <- exp(d_hat) * 100  # as percentage

  # Least-squares means for Reference and Test — the population-average log-scale
  # mean for each treatment from this SAME model, marginalized over seq/period
  # (and, for the fixed model, the subj:seq nesting) via emmeans. By construction
  # LSMean(Test) - LSMean(Reference) equals d_hat exactly; this just makes the
  # two individual means visible (and their geometric-mean back-transforms),
  # matching how SAS PROC GLM/MIXED reports FORM LSMEANs alongside the estimate.
  drug_levels <- levels(model_data$drug)
  lsmeans_result <- compute_lsmeans_ci(model, drug_levels[1], drug_levels[2], level = 0.90)
  if (is.null(lsmeans_result)) {
    bioeq_log("Could not compute LSMeans via emmeans", "WARNING")
  }

  return(list(
    d_hat = d_hat,
    se_d = se_d,
    df_d = df_d,
    pe_ratio = pe_ratio,
    residual_mse = residual_mse,
    model = model,
    anova_table = anova_table,
    anova_comprehensive = anova_comprehensive,
    subj_seq_analysis = subj_seq_analysis,
    type3_ss = type3_ss,
    lsmeans_result = lsmeans_result,
    anova_model = anova_model,
    drug_coef_name = drug_coef_name,
    n_observations = nrow(model_data),
    n_subjects = length(unique(model_data$subj))
  ))
}


# =============================================================================
# SECTION 3: FDA LINEARIZED SCALED CRITERION (Howe UCB)
# =============================================================================

#' FDA Linearized RSABE Test (Howe UCB)
#'
#' Reproduces the exact worked example in FDA's Progesterone Capsules
#' product-specific guidance ("Method for Statistical Analysis Using the
#' Reference-Scaled Average Bioequivalence Approach," Recommended Apr 2010 /
#' Revised Feb 2011), verified line-by-line against that guidance's SAS code
#' for both the partial-replicate (3-way, PROC GLM) and full-replicate
#' (4-way, PROC MIXED) examples:
#'
#'   x       = d̂² − SE_d²                         (bias-corrected point
#'                                                   estimate of d²; d̂² alone
#'                                                   is a biased estimator
#'                                                   since E[d̂²]=d²+Var(d̂))
#'   boundx  = max(|d̂ − t·SE_d|, |d̂ + t·SE_d|)²   (90% two-sided CI on d̂,
#'                                                   i.e. t = t_{1-α,df_d},
#'                                                   squared at whichever
#'                                                   bound is farther from 0)
#'   y       = −θ² · s²_wR
#'   boundy  = y · df_wR / χ²_{1-α,df_wR}           (UPPER chi-square
#'                                                   percentile in the
#'                                                   denominator — SAS
#'                                                   `cinv(0.95, df)`)
#'   critbound (UCB) = (x+y) + sqrt((boundx−x)² + (boundy−y)²)
#'
#' RSABE is demonstrated if critbound ≤ 0.
#'
#' d_hat/se_d/df_d and s2_wR/df_wR must come from the FDA guidance's own
#' per-subject contrast models (see build_rsabe_subject_contrasts(),
#' fit_rsabe_seq_mean(), fit_rsabe_seq_variance()) — NOT from a general
#' multi-term ANOVA on the raw observations, which the guidance's worked
#' example does not use for this calculation.
#'
#' @param d_hat Treatment difference on log scale (μ_T - μ_R estimate),
#'   from fit_rsabe_seq_mean() on the I_ij contrasts
#' @param se_d Standard error of d_hat
#' @param df_d Degrees of freedom for d_hat
#' @param s2_wR Within-subject variance for reference, from
#'   fit_rsabe_seq_variance() on the D_ij contrasts
#' @param df_wR Degrees of freedom for s2_wR
#' @param alpha Significance level (default 0.05 for 95% UCB)
#' @param theta_s Regulatory scaling constant (default ln(1.25)/sigma_w0 = 0.8924)
#' @return List with criterion value, UCB, conclusion
#' @export
rsabe_linearized_test <- function(d_hat, se_d, df_d, s2_wR, df_wR,
                                   alpha = 0.05, theta_s = log(1.25) / 0.25) {

  # Per FDA Progesterone Capsules guidance worked example (Howe 1974). The
  # step-by-step derivation (x, Es, boundx, boundy, Lm, Ls...) that used to be
  # printed here is DEBUG-only below - the UCB value and pass/fail verdict
  # are what a normal run needs, and both are already surfaced in the
  # analysis summary's RSABE footnote (R/analysis_summary.R) since they're
  # returned in this function's result list.

  # Regulatory constants
  theta2 <- theta_s^2
  sw_R <- sqrt(s2_wR)

  # Step 1: Bias-corrected point estimate of d² (guidance: x = estimate² − stderr²)
  #   Es = θ² · s²_wR ; η̂ = x - Es
  Em <- d_hat^2 - se_d^2
  Es <- theta2 * s2_wR
  eta_hat <- Em - Es

  # Step 2: Individual upper confidence bounds
  #   boundx = max(|d̂-t·SE|, |d̂+t·SE|)²  — the 90% two-sided CI on d̂,
  #     whichever endpoint is farther from zero, squared
  #   boundy = y · df_wR / χ²_{1-α,df_wR}   — SAS cinv(0.95, df); note this
  #     uses the UPPER chi-square percentile, not the lower one — it is
  #     applied directly to the (negative) y term, not decomposed into a
  #     separate "upper bound on σ²" the way boundx is for d̂².
  t_alpha <- qt(1 - alpha, df_d)
  ci_lo <- d_hat - t_alpha * se_d
  ci_hi <- d_hat + t_alpha * se_d
  Cm <- max(abs(ci_lo), abs(ci_hi))^2

  chi2_upper <- qchisq(1 - alpha, df_wR)  # SAS cinv(1-alpha, df) — upper tail
  boundy <- (-Es) * df_wR / chi2_upper
  Cs <- -boundy  # kept on the same (positive) scale as Es for display/legacy fields

  # Step 3: Squared confidence interval half-lengths
  #   Lm = (boundx - x)² ; Ls = (boundy - y)² = (Cs - Es)²  [since y=-Es, boundy=-Cs]
  Lm <- (Cm - Em)^2
  Ls <- (Cs - Es)^2

  # Step 4: Howe's combined UCB (critbound)
  #   UCB = (x + y) + sqrt(Lm + Ls) = (Em - Es) + sqrt(Lm + Ls)
  # RSABE is demonstrated if UCB ≤ 0.
  ucb <- (Em - Es) + sqrt(Lm + Ls)

  # Decision
  rsabe_pass <- (ucb <= 0)
  bioeq_log(sprintf(
    "FDA Linearized RSABE (Howe UCB): d_hat=%.6f SE_d=%.6f Em=%.6f Es=%.6f Cm=%.6f Cs=%.6f Lm=%.6f Ls=%.6f -> UCB=%.6f (<=0? %s -> %s)",
    d_hat, se_d, Em, Es, Cm, Cs, Lm, Ls, ucb, ucb <= 0,
    ifelse(rsabe_pass, "RSABE DEMONSTRATED", "RSABE NOT DEMONSTRATED")), "DEBUG")

  # Scaled BE limits for informational purposes
  scaled_lower <- exp(-theta_s * sw_R) * 100
  scaled_upper <- exp(theta_s * sw_R) * 100
  
  return(list(
    method = "FDA Linearized Scaled Criterion (Howe UCB)",
    eta_hat = eta_hat,
    ucb = ucb,
    rsabe_pass = rsabe_pass,
    d_hat = d_hat,
    d_hat_sq = Em,
    se_d = se_d,
    df_d = df_d,
    s2_wR = s2_wR,
    df_wR = df_wR,
    theta_s = theta_s,
    theta_sq = theta2,
    t_alpha = t_alpha,
    chi2_alpha = chi2_upper,
    Em = Em,
    Es = Es,
    Cm = Cm,
    Cs = Cs,
    Lm = Lm,
    Ls = Ls,
    alpha = alpha,
    scaled_lower = scaled_lower,
    scaled_upper = scaled_upper,
    scaled_sw_R = sw_R
  ))
}


# =============================================================================
# SECTION 4: NON-CENTRAL TOST (ncTOST) - EXACT METHOD
# =============================================================================

#' Non-Central TOST (ncTOST) for RSABE — Exact Method
#'
#' Implements the exact procedure of Tóthfalusi & Endrényi (2016),
#' "An Exact Procedure for the Evaluation of Reference-Scaled Average
#' Bioequivalence," The AAPS Journal, 18(2), 476-489.
#'
#' The method uses the noncentral t distribution directly. The key insight
#' is that the pivotal index d/K follows a noncentral t distribution:
#'
#'   d = d̂ / s_wR         (Glass's effect size estimator, Eq. 11)
#'   d/K ~ t(df, ncp = δ / (K · σ_wR))
#'
#' where K is the design-dependent constant (Eqs. 21-25) and df is the
#' degrees of freedom associated with s_wR.
#'
#' Hedges' bias correction cr(df) = 1 - 3/(4·df - 1) (Eq. 8) is applied
#' to obtain an unbiased estimate of the noncentrality parameter.
#'
#' RSABE is demonstrated if BOTH (Eqs. 17a, 17b):
#'   T_nc(0.95; -θ/K; df)  <  d / (K · cr(df))    [lower test]
#'   T_nc(0.05;  θ/K; df)  >  d / (K · cr(df))    [upper test]
#'
#' where T_nc(p; ncp; df) is the quantile function of the noncentral t.
#'
#' Equivalently, as p-values:
#'   p_lower = P[t(df, ncp = -θ/K) ≤ d/(K·cr)]    [must be > 1-α]
#'   p_upper = P[t(df, ncp =  θ/K) ≥ d/(K·cr)]    [must be > 1-α]
#'
#' @param d_hat Treatment difference estimate (log-scale: μ_T - μ_R)
#' @param se_d Standard error of d_hat (from ANOVA)
#' @param df_d Degrees of freedom for d_hat (from ANOVA, used for CI only)
#' @param s2_wR Within-subject reference variance (from a Reference-only ANOVA;
#'   see compute_reference_anova_variance())
#' @param df_wR Degrees of freedom for s2_wR (from the same Reference-only ANOVA)
#' @param K Design-dependent constant from compute_K_constant()
#' @param df_nct Degrees of freedom for the noncentral t (from compute_K_constant())
#' @param alpha Significance level (default 0.05)
#' @param theta_s Regulatory scaling constant (default ln(1.25)/0.25 ≈ 0.8924)
#' @return List with test results
#' @export
rsabe_nctost_test <- function(d_hat, se_d, df_d, s2_wR, df_wR,
                               K, df_nct,
                               alpha = 0.05, theta_s = log(1.25) / 0.25) {
  
  # Per Eqs. 11, 17a, 17b (Tóthfalusi & Endrényi 2016) with Hedges' correction
  # (Eq. 8). The step-by-step derivation that used to be printed here is
  # DEBUG-only below - the overall p-value and pass/fail verdict are what a
  # normal run needs, surfaced in the analysis summary's RSABE footnote
  # (R/analysis_summary.R) since they're returned in this function's result.

  sw_R <- sqrt(s2_wR)

  # Step 1: Compute pivotal index d (Eq. 11 — Glass's estimator): d = d̂ / s_wR
  d_index <- d_hat / sw_R

  # Step 2: Hedges' bias correction (Eq. 8)
  #   cr(df) = 1 - 3 / (4·df - 1)
  #   The unbiased noncentrality parameter estimate is d / (K · cr)
  cr <- hedges_correction(df_nct)
  stat <- d_index / (K * cr)  # bias-corrected test statistic

  # =========================================================================
  # Step 3: Noncentral t TOST (Eqs. 17a, 17b)
  #
  # Lower test (Eq. 17a): reject H01: δ ≤ -θ
  #   Pass if: T_nc(1-α; -θ/K; df) < d/(K·cr)
  #   Equivalently: p_lower = pt(stat, df, ncp = -θ/K) > 1 - α
  #
  # Upper test (Eq. 17b): reject H02: δ ≥ θ  
  #   Pass if: T_nc(α; θ/K; df) > d/(K·cr)
  #   Equivalently: p_upper = pt(stat, df, ncp = θ/K, lower.tail=FALSE) < α
  #   Or: 1 - pt(stat, df, ncp = θ/K) < α
  #
  # RSABE demonstrated if both tests reject at level α.
  # =========================================================================
  ncp_lower <- -theta_s / K   # ncp for lower test
  ncp_upper <-  theta_s / K   # ncp for upper test
  
  # p_lower = P[t(df, ncp=-θ/K) ≤ stat] — want this > 1-α (i.e., 0.95)
  p_lower <- pt(stat, df_nct, ncp = ncp_lower)
  
  # p_upper = P[t(df, ncp=θ/K) ≥ stat] — want this > 1-α (i.e., 0.95)  
  p_upper <- pt(stat, df_nct, ncp = ncp_upper, lower.tail = FALSE)
  
  # For reporting, convert to one-sided p-values (reject if < α):
  p1 <- 1 - p_lower  # lower test p-value
  p2 <- 1 - p_upper  # upper test p-value
  
  # Decision: both must exceed 1-α
  lower_pass <- (p_lower > 1 - alpha)
  upper_pass <- (p_upper > 1 - alpha)
  rsabe_pass <- lower_pass && upper_pass

  # Overall p-value = max of the two one-sided p-values
  overall_p <- max(p1, p2)

  bioeq_log(sprintf(
    "ncTOST: d=%.6f K=%.6f cr=%.6f stat=%.6f | lower=%s (p=%.6f) upper=%s (p=%.6f) | overall_p=%.6f -> %s",
    d_index, K, cr, stat,
    ifelse(lower_pass, "PASS", "FAIL"), p_lower,
    ifelse(upper_pass, "PASS", "FAIL"), p_upper,
    overall_p, ifelse(rsabe_pass, "RSABE DEMONSTRATED", "RSABE NOT DEMONSTRATED")), "DEBUG")
  
  # =========================================================================
  # Compute display values: CI and scaled limits on ratio scale
  # =========================================================================
  
  # Standard (1-2α)% CI for the treatment difference (from ANOVA, for display)
  t_crit <- qt(1 - alpha, df_d)
  ci_lower_log <- d_hat - t_crit * se_d
  ci_upper_log <- d_hat + t_crit * se_d
  ci_lower_pct <- exp(ci_lower_log) * 100
  ci_upper_pct <- exp(ci_upper_log) * 100
  
  # Scaled limits on ratio scale
  limit_upper <- theta_s * sw_R
  limit_lower <- -theta_s * sw_R
  scaled_lower_pct <- exp(limit_lower) * 100
  scaled_upper_pct <- exp(limit_upper) * 100
  
  return(list(
    method = "Non-Central TOST (ncTOST) — Exact",
    # ncTOST-specific results
    d_index = d_index,
    K = K,
    cr = cr,
    stat = stat,
    ncp_lower = ncp_lower,
    ncp_upper = ncp_upper,
    p_lower_cdf = p_lower,
    p_upper_cdf = p_upper,
    # Standard reporting format
    p1 = p1,
    p2 = p2,
    overall_p = overall_p,
    rsabe_pass = rsabe_pass,
    # Inputs
    d_hat = d_hat,
    se_d = se_d,
    df_d = df_d,
    df_nct = df_nct,
    s2_wR = s2_wR,
    sw_R = sw_R,
    df_wR = df_wR,
    theta_s = theta_s,
    alpha = alpha,
    # Display values
    limit_lower_log = limit_lower,
    limit_upper_log = limit_upper,
    ci_lower_log = ci_lower_log,
    ci_upper_log = ci_upper_log,
    ci_lower_pct = ci_lower_pct,
    ci_upper_pct = ci_upper_pct,
    scaled_lower_pct = scaled_lower_pct,
    scaled_upper_pct = scaled_upper_pct
  ))
}


# =============================================================================
# SECTION 5: MAIN RSABE ANALYSIS FUNCTION
# =============================================================================

#' Perform Reference-Scaled Average Bioequivalence (RSABE) Analysis
#'
#' Main entry point for RSABE analysis. Routes to FDA Linearized or ncTOST method.
#'
#' Workflow:
#'   1. Detect replicate design structure
#'   2. For each log-transformed PK parameter:
#'     a. Fit ANOVA model (complete dataset, all subjects) → get d̂, SE_d, df_d
#'     b. Fit a Reference-only ANOVA (subjects with >=2 Reference periods
#'        only) → get s²_wR, df_wR — see compute_reference_anova_variance()
#'     c. Check switching condition: CV_wR ≈ 30% (s_wR ≥ 0.294)
#'     d. If HV: apply RSABE (scaled limits)
#'        If not HV: fall back to ABE (fixed 80-125%)
#'     e. Check point estimate constraint (FDA): PE must be within 80-125%
#'   3. Compile results
#'
#' @param data Data frame with Subject, Treatment, Period, Sequence, PK parameters
#' @param design Study design ("2x2x3", "2x2x4", "replicate", "auto")
#' @param params List of analysis parameters
#' @return RSABE analysis results
#' @export
perform_rsabe <- function(data, design = "auto", params = list()) {
  
  # Extract configuration
  alpha <- params$alpha_level %||% 0.05
  parameters <- params$pk_parameters %||% c("lnCmax", "lnAUC0t", "lnAUC0inf")
  # NOTE: anova_model is NOT read from params here — see the auto-selection
  # right after design detection below. FDA's own guidance dictates the
  # model by replicate design, not a free user choice.
  rsabe_method <- params$rsabe_method %||% "fda_linearized"
  anova_results_input <- params$anova_results %||% NULL
  
  # FDA regulatory constants
  # θ_s = ln(1.25) / σ_w0, where σ_w0 = 0.25 is the regulatory cutoff SD
  # At the switching boundary (σ_wR = σ_w0), the scaled limits equal standard ABE:
  #   exp(±θ_s · σ_w0) = exp(±ln(1.25)) = [80%, 125%]
  # For HV drugs (σ_wR > σ_w0), limits expand proportionally.
  theta_s <- log(1.25) / 0.25  # ≈ 0.8924 (criterion scaling constant; σ_w0 = 0.25)
  s2_w0 <- 0.25^2              # regulatory σ²_w0 = 0.0625 used in the scaled criterion
  # FDA switching (decision) cutoff: apply reference scaling only when the
  # within-subject SD of the reference s_wR ≥ 0.294 (i.e. CV_wR ≈ 30%). NOTE this
  # is DISTINCT from σ_w0 = 0.25 (CV 25.4%), which is only the criterion constant.
  # Ref: FDA progesterone guidance; Davit et al. AAPS J 2012.
  s_wR_switch <- 0.294
  s2_wR_switch <- s_wR_switch^2  # ≈ 0.0864
  pe_constraint_lower <- 80.0   # Point estimate constraint
  pe_constraint_upper <- 125.0
  
  method_label <- if (rsabe_method == "nctost") {
    "Non-Central TOST (ncTOST)"
  } else {
    "FDA Linearized Scaled Criterion"
  }
  
  bioeq_log(sprintf(
    "RSABE analysis (%s, method=%s): alpha=%.3f (%.0f%% CI), theta_s=%.4f, switching cutoff s_wR>=%.3f (CV_wR~%.1f%%)",
    method_label, rsabe_method, alpha, (1 - 2 * alpha) * 100, theta_s,
    s_wR_switch, sqrt(exp(s_wR_switch^2) - 1) * 100), "DEBUG")

  # Detect replicate design
  design_info <- detect_replicate_design(data)

  if (!design_info$is_replicate) {
    stop("RSABE analysis requires a replicate design (2x2x3 or 2x2x4). Detected: ", design_info$design_type)
  }

  bioeq_log(sprintf("Design: %s (%s)", design_info$design_type, design_info$design_name), "DEBUG")

  # Per FDA guidance, the statistical model is dictated by the replicate
  # design, not a free user choice: PROC GLM (Fixed Effects) for partial-
  # replicate (3-way) designs, PROC MIXED with DDFM=Satterthwaite for
  # full-replicate (4-way) designs. Both reduce to the identical
  # `lm(~ Sequence)` computation for this specific FDA calculation (see
  # build_rsabe_subject_contrasts()/fit_rsabe_seq_mean()/
  # fit_rsabe_seq_variance()) — the guidance's own PROC MIXED call declares
  # no RANDOM/REPEATED statement, so with no random effect and no
  # heterogeneous-variance term to estimate, its REML residual variance and
  # Satterthwaite denominator DF are numerically identical to PROC GLM's OLS
  # residual MS/DF. This is a regulatory labeling distinction, not a
  # computational one. Any `params$anova_model` the caller supplies is
  # ignored for RSABE.
  anova_model <- if (isTRUE(design_info$is_partial_replicate)) "fixed" else "mixed"
  anova_model_label <- if (anova_model == "fixed") {
    "Fixed Effects (PROC GLM) — auto-selected: partial-replicate design"
  } else {
    "Mixed Effects (PROC MIXED, DDFM=Satterthwaite) — auto-selected: full-replicate design"
  }
  bioeq_log(sprintf("ANOVA Model: %s", anova_model_label), "DEBUG")

  # The ANOVA Results display for RSABE uses the SAME calculation ABEL uses
  # (replicateBE::method.A()/method.B(), auto-selected Fixed/Mixed by design)
  # — this is deliberately a separate computation from the FDA Howe UCB/
  # ncTOST decision above, per the user's explicit direction: the ANOVA
  # table should look identical to ABEL's, and RSABE vs. ABEL should differ
  # only in the BE Conclusion section, not the ANOVA display. Falls back to
  # NULL (per-parameter fallback further below) if replicateBE errors.
  abel_anova_model <- if (isTRUE(design_info$is_partial_replicate)) "fixed" else "nlme"
  abel_style_anova <- tryCatch({
    perform_abel_placeholder(data, design = "auto", params = list(
      alpha_level = alpha,
      pk_parameters = parameters,
      anova_model = abel_anova_model,
      abel_eligible_params = unique(sub("^(ln|log)", "", parameters, ignore.case = TRUE)),
      abel_upper_cap = "50"
    ))
  }, error = function(e) {
    bioeq_log(sprintf("Could not compute replicateBE-based ANOVA display: %s", e$message), "WARNING")
    NULL
  })

  # Verify required columns exist
  required_cols <- c("Subject", "Period", "Sequence", "Treatment")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Track processed base parameters to avoid duplicates
  processed_params <- c()
  
  # Result storage
  ci_list <- list()
  conclusion_list <- list()
  all_results <- list()
  rsabe_details <- list()
  
  for (param in parameters) {
    
    # Determine log vs raw parameter handling
    is_log_param <- grepl("^(ln|log)", param, ignore.case = TRUE)
    
    if (is_log_param) {
      base_param_name <- sub("^(ln|log)", "", param, ignore.case = TRUE)
    } else {
      base_param_name <- param
    }
    
    # Skip duplicates
    if (base_param_name %in% processed_params) {
      bioeq_log(sprintf("Skipping '%s' - already processed as '%s'", param, base_param_name), "DEBUG")
      next
    }

    # Resolve parameter: prefer raw data (we'll log-transform ourselves)
    if (is_log_param) {
      raw_param <- base_param_name
      log_param <- param

      if (raw_param %in% names(data)) {
        param_to_use <- raw_param
        need_log_transform <- TRUE
        bioeq_log(sprintf("Using raw '%s' (will log-transform)", raw_param), "DEBUG")
      } else if (log_param %in% names(data)) {
        param_to_use <- log_param
        need_log_transform <- FALSE
        bioeq_log(sprintf("Using pre-logged '%s'", log_param), "DEBUG")
      } else {
        bioeq_log(sprintf("Parameter '%s' not found - skipping", param), "WARNING")
        next
      }
    } else {
      if (param %in% names(data)) {
        param_to_use <- param
        need_log_transform <- TRUE
      } else {
        log_param <- paste0("ln", param)
        if (log_param %in% names(data)) {
          param_to_use <- log_param
          need_log_transform <- FALSE
        } else {
          bioeq_log(sprintf("Parameter '%s' not found - skipping", param), "WARNING")
          next
        }
      }
    }

    processed_params <- c(processed_params, base_param_name)

    bioeq_log(sprintf("RSABE Analysis for %s", base_param_name), "DEBUG")

    tryCatch({

      # Prepare data: ensure log-transformed column exists
      analysis_data <- data
      log_col_name <- paste0("ln_", base_param_name, "_rsabe")

      if (need_log_transform) {
        raw_vals <- as.numeric(analysis_data[[param_to_use]])
        valid_mask <- !is.na(raw_vals) & raw_vals > 0
        analysis_data[[log_col_name]] <- NA_real_
        analysis_data[[log_col_name]][valid_mask] <- log(raw_vals[valid_mask])
        bioeq_log(sprintf("Log-transformed %s -> %s (%d valid values)",
                          param_to_use, log_col_name, sum(valid_mask)), "DEBUG")
      } else {
        analysis_data[[log_col_name]] <- as.numeric(analysis_data[[param_to_use]])
      }

      # Remove rows with NA in the analysis column
      analysis_data <- analysis_data[!is.na(analysis_data[[log_col_name]]), ]

      if (nrow(analysis_data) < 6) {
        bioeq_log(sprintf("Insufficient data for %s (%d rows) - skipping", base_param_name, nrow(analysis_data)), "WARNING")
        next
      }
      
      # Step 1: Fit the FDA guidance's own per-subject contrast models. See
      # SECTION 1B above — the FDA worked example (Progesterone Capsules
      # product-specific guidance) collapses each subject's data to I_ij
      # (treatment contrast) and D_ij (Reference replicate difference)
      # BEFORE fitting anything; it does not run a multi-term ANOVA on the
      # raw per-period data for either the treatment effect or s²_wR. These
      # contrast models — not a general ANOVA — are what actually drive the
      # RSABE Howe UCB / ncTOST decision below.
      contrasts_df <- build_rsabe_subject_contrasts(analysis_data, log_col_name)
      if (is.null(contrasts_df) || nrow(contrasts_df) < 2) {
        stop("Could not form per-subject Reference-scaled contrasts for ",
             base_param_name, " (need >=2 subjects with a replicated Reference)")
      }

      i_fit <- fit_rsabe_seq_mean(contrasts_df$I, contrasts_df$Sequence, alpha = alpha)
      d_fit <- fit_rsabe_seq_variance(contrasts_df$D, contrasts_df$Sequence)

      d_hat_c    <- i_fit$estimate
      se_d_c     <- i_fit$se
      df_d_c     <- i_fit$df
      s2_wR_c    <- d_fit$s2
      df_wR_c    <- d_fit$df
      pe_ratio_c <- exp(d_hat_c) * 100
      cv_wR_c    <- sqrt(exp(s2_wR_c) - 1) * 100
      n_R_c      <- nrow(contrasts_df)

      # Test-side replicate difference (only where a subject has 2 Test
      # periods, i.e. a full-replicate design) — used only for the
      # heteroscedasticity ratio z = s_wT/s_wR in compute_K_constant() (the
      # ncTOST path); the FDA guidance's own worked example never uses
      # s²_wT at all.
      dt_rows <- contrasts_df[!is.na(contrasts_df$D_T), , drop = FALSE]
      if (nrow(dt_rows) >= 2) {
        t_fit   <- fit_rsabe_seq_variance(dt_rows$D_T, dt_rows$Sequence)
        s2_wT_c <- t_fit$s2
        df_wT_c <- t_fit$df
        cv_wT_c <- sqrt(exp(s2_wT_c) - 1) * 100
        n_T_c   <- nrow(dt_rows)
      } else {
        s2_wT_c <- NA_real_; df_wT_c <- NA_real_; cv_wT_c <- NA_real_; n_T_c <- NA_integer_
      }

      bioeq_log(sprintf(
        "[%s] FDA contrast model: d_hat=%.6f SE=%.6f df=%d (n=%d subjects w/ replicated Reference) | s2_wR=%.6f (df=%d, CVwR=%.2f%%)",
        base_param_name, d_hat_c, se_d_c, df_d_c, n_R_c, s2_wR_c, df_wR_c, cv_wR_c), "DEBUG")

      refvar_result <- list(
        s2_wR = s2_wR_c, cv_wR = cv_wR_c, df_wR = df_wR_c, n_R = n_R_c,
        anova_wR = anova(d_fit$model), model_wR = d_fit$model, ref_subjects = contrasts_df$Subject,
        s2_wT = s2_wT_c, cv_wT = cv_wT_c, df_wT = df_wT_c, n_T = n_T_c,
        anova_wT = if (nrow(dt_rows) >= 2) anova(t_fit$model) else NULL,
        model_wT = if (nrow(dt_rows) >= 2) t_fit$model else NULL,
        test_subjects = dt_rows$Subject
      )

      # Single source of truth for both the RSABE decision AND the ANOVA
      # Results display — no separate/parallel model is fit any more. The
      # "ANOVA" here is the I_ij ~ Sequence model (the FDA guidance's own
      # treatment-effect model); the D_ij ~ Sequence model behind s²_wR is
      # carried separately (refvar_result$anova_wR above) and rendered as
      # its own card in the Results dashboard.
      t_value_c <- d_hat_c / se_d_c
      p_value_c <- if (df_d_c > 0) 2 * pt(abs(t_value_c), df_d_c, lower.tail = FALSE) else NA_real_

      model_result <- list(
        d_hat = d_hat_c, se_d = se_d_c, df_d = df_d_c, pe_ratio = pe_ratio_c,
        t_value = t_value_c, p_value = p_value_c,
        residual_mse = NA_real_,  # not meaningful for this model; s2_wR is the variance of interest
        model = i_fit$model,
        anova_table = anova(i_fit$model),
        anova_comprehensive = NULL,  # no Subject(Sequence)/Period/Treatment terms in this model
        subj_seq_analysis = NULL,
        type3_ss = NULL,
        lsmeans_result = NULL,  # I_ij is a contrast, not separate T/R means — no LSMeans analog
        anova_model = anova_model,
        anova_model_label = anova_model_label,
        drug_coef_name = "I (T - mean(R1,R2))",
        n_observations = nrow(contrasts_df),
        n_subjects = n_R_c
      )

      # Step 2: Check switching condition — FDA applies reference scaling only
      # when s_wR ≥ 0.294 (CV_wR ≈ 30%), NOT at σ_w0 = 0.25 (CV 25.4%). ONLY
      # Reference variability determines this — Test variability (s2_wT) is
      # not part of the switching criterion; it factors into the scaled
      # criterion/bound computation for heteroscedastic designs (see
      # compute_K_constant()), which is a separate step below.
      is_hv <- (refvar_result$s2_wR >= s2_wR_switch)

      bioeq_log(sprintf("[%s] Switching: s_wR (%.4f) %s cutoff (%.3f) -> %s",
                        base_param_name, sqrt(refvar_result$s2_wR), ifelse(is_hv, ">=", "<"), s_wR_switch,
                        ifelse(is_hv, "HIGH VARIABILITY -> Use RSABE", "LOW VARIABILITY -> Use ABE")), "DEBUG")

      # The replicateBE-based fit for this parameter (same ANOVA as ABE/ABEL,
      # already computed above for the ANOVA Results display) — needed here
      # too, since the non-HV branch below reuses its PE/CI directly.
      abel_param_result <- if (!is.null(abel_style_anova)) {
        abel_style_anova$anova_results$anova_results[[base_param_name]]
      } else NULL

      if (is_hv) {
        # =====================================================================
        # HIGH VARIABILITY: Apply RSABE (scaled limits)
        # =====================================================================
        
        if (rsabe_method == "nctost") {
          # ncTOST exact method — per Tóthfalusi & Endrényi (2016)
          # Compute design-dependent constant K and degrees of freedom
          K_result <- compute_K_constant(design_info, refvar_result)
          K_val <- K_result$K
          df_nct_val <- K_result$df_nct
          
          # If K could not be computed from design structure, estimate from SE_d / s_wR
          if (is.na(K_val)) {
            K_val <- model_result$se_d / sqrt(refvar_result$s2_wR)
            bioeq_log(sprintf("K estimated from SE_d/s_wR: %.6f / %.6f = %.6f",
                              model_result$se_d, sqrt(refvar_result$s2_wR), K_val), "DEBUG")
          }
          
          rsabe_test <- rsabe_nctost_test(
            d_hat = model_result$d_hat,
            se_d = model_result$se_d,
            df_d = model_result$df_d,
            s2_wR = refvar_result$s2_wR,
            df_wR = refvar_result$df_wR,
            K = K_val,
            df_nct = df_nct_val,
            alpha = alpha,
            theta_s = theta_s
          )
          
          scaled_lower <- rsabe_test$scaled_lower_pct
          scaled_upper <- rsabe_test$scaled_upper_pct
          ci_lower_val <- rsabe_test$ci_lower_pct
          ci_upper_val <- rsabe_test$ci_upper_pct
          
        } else {
          # FDA Linearized (Howe UCB) — default
          rsabe_test <- rsabe_linearized_test(
            d_hat = model_result$d_hat,
            se_d = model_result$se_d,
            df_d = model_result$df_d,
            s2_wR = refvar_result$s2_wR,
            df_wR = refvar_result$df_wR,
            alpha = alpha,
            theta_s = theta_s
          )
          
          scaled_lower <- rsabe_test$scaled_lower
          scaled_upper <- rsabe_test$scaled_upper
          
          # For linearized method, compute CI for display
          t_crit <- qt(1 - alpha, model_result$df_d)
          ci_lower_val <- exp(model_result$d_hat - t_crit * model_result$se_d) * 100
          ci_upper_val <- exp(model_result$d_hat + t_crit * model_result$se_d) * 100
        }
        
        # FDA Point Estimate Constraint: PE must be within 80-125%
        pe_within_constraint <- (model_result$pe_ratio >= pe_constraint_lower) && 
                                 (model_result$pe_ratio <= pe_constraint_upper)
        
        # Overall RSABE pass: scaling criterion + PE constraint
        be_pass <- rsabe_test$rsabe_pass && pe_within_constraint
        
        bioeq_log(sprintf(
          "[%s] PE constraint: %.2f%% within [%.0f%%, %.0f%%]? %s | Overall RSABE: %s (scaling: %s, PE: %s)",
          base_param_name, model_result$pe_ratio, pe_constraint_lower, pe_constraint_upper,
          ifelse(pe_within_constraint, "YES", "NO"),
          ifelse(be_pass, "PASS", "FAIL"),
          ifelse(rsabe_test$rsabe_pass, "PASS", "FAIL"),
          ifelse(pe_within_constraint, "PASS", "FAIL")), "DEBUG")
        
        # Store results
        ci_list[[base_param_name]] <- list(
          parameter = base_param_name,
          point_estimate = model_result$pe_ratio,
          ci_lower = ci_lower_val,
          ci_upper = ci_upper_val,
          confidence_level = (1 - 2 * alpha) * 100,
          geometric_mean_ratio = model_result$pe_ratio / 100,
          within_limits = be_pass,
          # RSABE-specific
          cv_wr = refvar_result$cv_wR,
          cv_wt = refvar_result$cv_wT %||% NA,
          scaled_lower_limit = scaled_lower,
          scaled_upper_limit = scaled_upper,
          limits_used = list(lower = scaled_lower, upper = scaled_upper, type = "scaled"),
          method = method_label,
          regulator = "FDA",
          degrees_freedom = model_result$df_d,
          n_subjects = model_result$n_subjects,
          sw_test = if (!is.na(refvar_result$s2_wT)) sqrt(refvar_result$s2_wT) else NA,
          sw_reference = sqrt(refvar_result$s2_wR),
          sw_ratio = if (!is.na(refvar_result$s2_wT)) sqrt(refvar_result$s2_wT / refvar_result$s2_wR) else NA,
          # RSABE decision components
          rsabe_scaling = TRUE,
          pe_constraint_pass = pe_within_constraint,
          rsabe_criterion_pass = rsabe_test$rsabe_pass,
          is_high_variability = TRUE
        )
        
        rsabe_details[[base_param_name]] <- list(
          rsabe_test = rsabe_test,
          refvar_result = refvar_result,
          model_result = model_result,
          is_hv = TRUE,
          pe_constraint_pass = pe_within_constraint,
          rsabe_method = rsabe_method
        )
        
      } else {
        # =====================================================================
        # LOW VARIABILITY: Fall back to standard ABE (fixed 80-125%)
        #
        # Per FDA guidance Step 1a, this is literally "use the two one-sided
        # tests procedure" — i.e. the SAME treatment-effect ANOVA (auto-
        # selected Fixed/Mixed by design) that ABE/ABEL use for this dataset,
        # not the FDA I_ij contrast model (that model exists only to feed the
        # Howe UCB/ncTOST scaling math, which isn't used once a parameter
        # isn't high-variability). PE/CI here are pulled from the SAME
        # replicateBE-based fit (`abel_style_anova`) already computed above
        # for the ANOVA Results display, so ABE, ABEL, and RSABE report an
        # identical PE/CI for a given dataset+model whenever RSABE falls back
        # to unscaled ABE. Falls back to the FDA contrast model's own CI only
        # if that fit is unavailable (e.g. replicateBE errored).
        # =====================================================================

        abel_ci <- if (!is.null(abel_style_anova)) {
          abel_style_anova$confidence_intervals[[base_param_name]]
        } else NULL

        if (!is.null(abel_ci)) {
          pe_val       <- abel_ci$point_estimate
          ci_lower_val <- abel_ci$ci_lower
          ci_upper_val <- abel_ci$ci_upper
          df_display   <- abel_param_result$residual_df %||% model_result$df_d
          n_display    <- abel_param_result$n_observations %||% model_result$n_subjects
          bioeq_log(sprintf("Using standard ABE limits (80-125%%) for %s - PE/CI from the same %s ANOVA as ABE/ABEL",
                            base_param_name, anova_model_label), "DEBUG")
        } else {
          # Fallback: FDA contrast model's own CI (only reached if replicateBE failed)
          t_crit <- qt(1 - alpha, model_result$df_d)
          pe_val       <- model_result$pe_ratio
          ci_lower_val <- exp(model_result$d_hat - t_crit * model_result$se_d) * 100
          ci_upper_val <- exp(model_result$d_hat + t_crit * model_result$se_d) * 100
          df_display   <- model_result$df_d
          n_display    <- model_result$n_subjects
          bioeq_log(sprintf(
            "Using standard ABE limits (80-125%%) for %s - replicateBE fit unavailable, using FDA contrast model's CI instead",
            base_param_name), "WARNING")
        }

        be_pass <- (ci_lower_val >= 80.0) && (ci_upper_val <= 125.0)

        bioeq_log(sprintf("[%s] ABE: PE=%.2f%%, CI [%.2f%%, %.2f%%], Limits [80%%, 125%%], BE=%s",
                          base_param_name, pe_val, ci_lower_val, ci_upper_val, ifelse(be_pass, "Pass", "Fail")), "DEBUG")

        ci_list[[base_param_name]] <- list(
          parameter = base_param_name,
          point_estimate = pe_val,
          ci_lower = ci_lower_val,
          ci_upper = ci_upper_val,
          confidence_level = (1 - 2 * alpha) * 100,
          geometric_mean_ratio = pe_val / 100,
          within_limits = be_pass,
          cv_wr = refvar_result$cv_wR,
          cv_wt = refvar_result$cv_wT %||% NA,
          scaled_lower_limit = 80.0,
          scaled_upper_limit = 125.0,
          limits_used = list(lower = 80, upper = 125, type = "fixed"),
          method = "ABE (low variability — RSABE not required)",
          regulator = "FDA",
          degrees_freedom = df_display,
          n_subjects = n_display,
          sw_test = if (!is.na(refvar_result$s2_wT)) sqrt(refvar_result$s2_wT) else NA,
          sw_reference = sqrt(refvar_result$s2_wR),
          sw_ratio = if (!is.na(refvar_result$s2_wT)) sqrt(refvar_result$s2_wT / refvar_result$s2_wR) else NA,
          rsabe_scaling = FALSE,
          pe_constraint_pass = NA,
          rsabe_criterion_pass = NA,
          is_high_variability = FALSE
        )

        rsabe_details[[base_param_name]] <- list(
          rsabe_test = NULL,
          refvar_result = refvar_result,
          model_result = model_result,
          is_hv = FALSE,
          pe_constraint_pass = NA,
          rsabe_method = rsabe_method
        )
      }
      
      # ANOVA Results display: use the replicateBE-based result (identical
      # calculation/formatting to ABEL) when available (abel_param_result was
      # computed earlier, right after the switching decision above). Falls
      # back to the FDA contrast-model's own ANOVA only if replicateBE failed
      # for this parameter — this fallback is display-only and never touches
      # the Howe UCB/ncTOST decision above, which always uses the FDA values
      # regardless of which branch fires here.
      all_results[[base_param_name]] <- if (!is.null(abel_param_result)) {
        abel_param_result
      } else {
        list(
          model = model_result$model,
          anova = model_result$anova_table,
          anova_comprehensive = NULL,
          subj_seq_analysis = NULL,
          type3_ss = NULL,
          lsmeans_result = NULL,
          treatment_coef = model_result$d_hat,
          treatment_se = model_result$se_d,
          residual_mse = model_result$residual_mse,
          residual_df = model_result$df_d,
          n_observations = model_result$n_observations,
          anova_method = model_result$anova_model,
          anova_model_label = model_result$anova_model_label,
          cv_wr_percent = refvar_result$cv_wR,
          cv_wt_percent = refvar_result$cv_wT %||% NA,
          s2_wR = refvar_result$s2_wR,
          s2_wT = refvar_result$s2_wT,
          df_wR = refvar_result$df_wR,
          df_wT = refvar_result$df_wT,
          anova_wR = refvar_result$anova_wR,
          anova_wT = refvar_result$anova_wT,
          model_wR = refvar_result$model_wR,
          model_wT = refvar_result$model_wT,
          n_wR = refvar_result$n_R,
          n_wT = refvar_result$n_T
        )
      }

      conclusion_list[[base_param_name]] <- be_pass
      
    }, error = function(e) {
      bioeq_log(sprintf("Error analyzing %s: %s", base_param_name, e$message), "ERROR")
    })
  }
  
  if (length(ci_list) == 0) {
    stop("No parameters could be analyzed successfully for RSABE")
  }
  
  # Compile final results
  results <- list(
    confidence_intervals = ci_list,
    be_conclusions = conclusion_list,
    anova_results = list(
      anova_results = all_results,
      design = design_info$design_type,
      parameters = names(all_results),
      note = sprintf("RSABE analysis using %s with FDA guidance per-subject contrast estimation (I_ij/D_ij)", method_label)
    ),
    design_type = design_info$design_type,
    n_subjects = length(unique(data$Subject)),
    n_periods = design_info$n_periods,
    analysis_type = "RSABE",
    analysis_method = sprintf("Reference-Scaled Average Bioequivalence (%s)", method_label),
    be_method = sprintf("RSABE — %s", method_label),
    anova_model = anova_model,
    rsabe_method = rsabe_method,
    rsabe_details = rsabe_details,
    alpha_level = alpha,
    regulator = "FDA",
    theta_s = theta_s,
    s2_w0 = s2_w0,
    pe_constraint = c(pe_constraint_lower, pe_constraint_upper),
    limits_justification = sprintf("FDA RSABE: scaled limits for HV parameters (CV_wR > %.1f%%), fixed 80-125%% for non-HV", 
                                    sqrt(exp(s2_w0) - 1) * 100)
  )
  
  bioeq_log("RSABE analysis completed", "DEBUG")
  return(results)
}
