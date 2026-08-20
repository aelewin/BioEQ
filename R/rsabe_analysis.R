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
    cat(sprintf("    Heteroscedasticity ratio: z = s_WT/s_WR = %.4f\n", z))
  } else {
    z <- 1.0
    cat("    Assuming homoscedasticity (z = 1) — s²_wT not available\n")
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
    cat("    ⚠️  Unknown design type for K constant — estimating from SE_d/s_WR\n")
    K <- NA_real_  # Will be filled in by the caller from SE_d / s_WR
    df_nct <- n_R  # Best available DF for s_WR
  }
  
  cat(sprintf("    Design: %s\n", design_label))
  if (!is.na(K)) {
    cat(sprintf("    K = %.6f, df_nct = %d\n", K, df_nct))
  }
  
  return(list(
    K = K,
    z = z,
    df_nct = as.integer(df_nct),
    design_label = design_label
  ))
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
  
  cat(sprintf("  🔧 Fitting %s model for treatment effect...\n", anova_model))
  
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

    cat(sprintf("    Fixed model: d̂=%.6f, SE=%.6f, df=%d\n", d_hat, se_d, df_d))

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
      cat(sprintf("    ⚠️  RSABE does not support '%s' DF approximation. Using nlme (REML) instead.\n", anova_model))
      cat("    Note: RSABE's s2_wR/CVwR come from a separate Reference-only ANOVA, not this model.\n")
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

    cat(sprintf("    Mixed model: d̂=%.6f, SE=%.6f, df=%d\n", d_hat, se_d, df_d))
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
    cat("    ⚠️  Could not compute LSMeans via emmeans\n")
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

#' FDA Linearized RSABE Test
#'
#' The linearized criterion is:
#'   η = d² − θ²_s · s²_wR
#' where:
#'   d = μ_T − μ_R (log-scale treatment difference)
#'   θ_s = ln(1.25)/σ_w0 = 0.2231/0.25 ≈ 0.8924
#'   s²_wR = within-subject variance for Reference
#'
#' If η ≤ 0, the test product is within the scaled limits.
#' The 95% upper confidence bound (UCB) of η is computed using
#' the Howe approximation.
#'
#' Howe UCB:
#'   UCB = d² − t²_α · SE²_d + θ²_s · s²_wR · (df_wR / χ²_{α,df_wR} − 1)
#'         + max(0, d² − t²_α · SE²_d) - if using exact Howe
#'
#' Simplified (FDA recommended):
#'   UCB = (|d| + t_α · SE_d)² − θ²_s · s²_wR · (df_wR / χ²_{1-α,df_wR})
#'   -- wait, use the proper Howe formulation --
#' 
#' The proper Howe (1974) upper confidence bound for η = d² - θ²·σ² is:
#'   UCB = max(0, |d̂| - t_{α,df_d}·SE_d)² - θ² · s²_wR · df_wR / χ²_{1-α,df_wR}
#'   -- when UCB < 0, H0 rejected (RSABE demonstrated)
#'
#' Actually, the widely used formulation from the FDA SAS code and
#' Davit et al. (2012) is:
#'   Let x1 = d̂² - t²_{α,df_d} · SE²_d       (component for d²)
#'   Let x2 = θ² · s²_wR · df_wR / χ²_{α,df_wR} (lower bound for θ²·σ²)
#'   Let x3 = θ² · s²_wR                        (point estimate of θ²·σ²)
#'   Let x4 = d̂²                                (point estimate of d²)
#'   
#'   η̂ = x4 - x3   (point estimate of criterion)
#'   UCB = x1 - x2  (upper confidence bound via Howe) -- NO.
#'
#' Per the FDA progesterone guidance SAS code and Tóthfalusi (2016):
#' The linearized scaled criterion η = (μ_T - μ_R)² - θ²·σ²_wR
#' The UCB is constructed as:
#'   UCB = d̂² + SE²_d·(df_d/χ²_{1-α,df_d} - 1) - θ²·s²_wR·(df_wR / χ²_{α,df_wR})
#' 
#' Actually the most commonly cited implementation from Tóthfalusi (2016) eq. 6:
#'   UCB = d̂² - t²_{α,df_d}·SE²_d - θ²·s²_wR·(df_wR / χ²_{α,df_wR} − 1) 
#'   -- not this either. Let me use the definitive formulation.
#'
#' DEFINITIVE formulation per FDA SAS code (Davit 2012, FDA guidance):
#'   η̂ = d̂² - θ² · s²_wR  (point estimate)
#'   
#'   Variance components of η:
#'     Var(d̂²) approximated, Var(s²_wR) from chi-squared
#'   
#'   UCB uses Howe's method:
#'     C_L = 1 - df_wR / χ²_{α,df_wR}
#'     C_U = df_wR / χ²_{1-α,df_wR} - 1
#'     
#'     If d̂² ≥ t²_{α,df_d} · SE²_d:
#'       UCB = d̂² - t²_{α,df_d} · SE²_d + θ⁴ · s⁴_wR · C_U²  (or simpler form)
#'     Else:
#'       UCB = sqrt((d̂² - t²_{α,df_d}·SE²_d)² + (θ²·s²_wR·C_L)²)
#'
#' OK - let me just implement the EXACT formulation from FDA SAS code.
#' From the FDA guidance SAS code and PharmSci paper implementations:
#'
#' @param d_hat Treatment difference on log scale (μ_T - μ_R estimate)
#' @param se_d Standard error of d_hat
#' @param df_d Degrees of freedom for d_hat
#' @param s2_wR Within-subject variance for reference
#' @param df_wR Degrees of freedom for s2_wR
#' @param alpha Significance level (default 0.05 for 95% UCB)
#' @param theta_s Regulatory scaling constant (default ln(1.25)/sigma_w0 = 0.8924)
#' @return List with criterion value, UCB, conclusion
#' @export
rsabe_linearized_test <- function(d_hat, se_d, df_d, s2_wR, df_wR, 
                                   alpha = 0.05, theta_s = log(1.25) / 0.25) {
  
  cat("  🔬 FDA Linearized RSABE Test (Howe UCB)...\n")
  cat("    Per Tóthfalusi & Endrényi (2016) Eqs. 26-31 / Howe (1974)\n")
  
  # Regulatory constants
  theta2 <- theta_s^2
  sw_R <- sqrt(s2_wR)
  
  # ---------------------------------------------------------------------------
  # Step 1: Point estimates (Eqs. 27, 28a, 28b)
  #   Linearized criterion: η = d² - θ² · σ²_wR ≤ 0
  #   Em = d̂²                        (point estimate of d²)
  #   Es = θ² · s²_wR                 (point estimate of θ² · σ²_wR)
  #   η̂ = Em - Es                    (point estimate of criterion)
  # ---------------------------------------------------------------------------
  Em <- d_hat^2
  Es <- theta2 * s2_wR
  eta_hat <- Em - Es
  
  cat(sprintf("    d̂ = %.6f, Em = d̂² = %.6f\n", d_hat, Em))
  cat(sprintf("    Es = θ² · s²_wR = %.6f · %.6f = %.6f\n", theta2, s2_wR, Es))
  cat(sprintf("    η̂ (point estimate) = %.6f\n", eta_hat))
  
  # ---------------------------------------------------------------------------
  # Step 2: Individual upper confidence bounds (Eqs. 29a, 29b)
  #   Cm = (|d̂| + t_{1-α,df_d} · SE_d)²     — UCB for d² (one-sided)
  #   Cs = θ² · df_wR · s²_wR / χ²_{α,df_wR} — UCB for θ²·σ²_wR
  #
  # Note: Cm uses the UPPER bound (|d̂| + t·SE)², not the lower bound.
  # This is because we are bounding d² FROM ABOVE (conservative for
  # the 95% UCB of η = d² - θ²·σ²).
  # Cs uses the LOWER chi-squared quantile χ²_{α} to get the UCB of σ².
  # ---------------------------------------------------------------------------
  t_alpha <- qt(1 - alpha, df_d)
  chi2_alpha <- qchisq(alpha, df_wR)  # lower tail
  
  Cm <- (abs(d_hat) + t_alpha * se_d)^2
  Cs <- theta2 * df_wR * s2_wR / chi2_alpha
  
  cat(sprintf("    t_{1-α,df_d} = t_{%.3f,%d} = %.4f\n", 1 - alpha, df_d, t_alpha))
  cat(sprintf("    χ²_{α,df_wR} = χ²_{%.3f,%d} = %.4f\n", alpha, df_wR, chi2_alpha))
  cat(sprintf("    Cm = (|d̂| + t·SE)² = (%.6f + %.4f·%.6f)² = %.6f\n",
              abs(d_hat), t_alpha, se_d, Cm))
  cat(sprintf("    Cs = θ²·df·s²/χ² = %.6f·%d·%.6f/%.4f = %.6f\n",
              theta2, df_wR, s2_wR, chi2_alpha, Cs))
  
  # ---------------------------------------------------------------------------
  # Step 3: Squared confidence interval half-lengths (Eqs. 30a, 30b)
  #   Lm = (Cm - Em)²
  #   Ls = (Cs - Es)²
  # ---------------------------------------------------------------------------
  Lm <- (Cm - Em)^2
  Ls <- (Cs - Es)^2
  
  cat(sprintf("    Lm = (Cm - Em)² = (%.6f - %.6f)² = %.6f\n", Cm, Em, Lm))
  cat(sprintf("    Ls = (Cs - Es)² = (%.6f - %.6f)² = %.6f\n", Cs, Es, Ls))
  
  # ---------------------------------------------------------------------------
  # Step 4: Howe's combined UCB (Eq. 31)
  #   UCB = (Em - Es) + sqrt(Lm + Ls)
  #
  # RSABE is demonstrated if UCB ≤ 0.
  # ---------------------------------------------------------------------------
  ucb <- (Em - Es) + sqrt(Lm + Ls)
  
  cat(sprintf("    UCB = (Em - Es) + sqrt(Lm + Ls) = %.6f + sqrt(%.6f) = %.6f\n",
              Em - Es, Lm + Ls, ucb))
  
  # Decision
  rsabe_pass <- (ucb <= 0)
  cat(sprintf("    UCB ≤ 0? %s → %s\n", ucb <= 0, 
              ifelse(rsabe_pass, "RSABE DEMONSTRATED", "RSABE NOT DEMONSTRATED")))
  
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
    chi2_alpha = chi2_alpha,
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
  
  cat("  🔬 Non-Central TOST (ncTOST) — Exact Method (Tóthfalusi & Endrényi 2016)...\n")
  cat("    Per Eqs. 11, 17a, 17b with Hedges' correction (Eq. 8)\n")
  
  sw_R <- sqrt(s2_wR)
  
  # =========================================================================
  # Step 1: Compute pivotal index d (Eq. 11 — Glass's estimator)
  #   d = d̂ / s_wR
  # =========================================================================
  d_index <- d_hat / sw_R
  
  cat(sprintf("    d̂ = %.6f, s_wR = %.6f\n", d_hat, sw_R))
  cat(sprintf("    d = d̂ / s_wR = %.6f (pivotal index, Eq. 11)\n", d_index))
  cat(sprintf("    K = %.6f, df_nct = %d\n", K, df_nct))
  
  # =========================================================================
  # Step 2: Hedges' bias correction (Eq. 8)
  #   cr(df) = 1 - 3 / (4·df - 1)
  #   The unbiased noncentrality parameter estimate is d / (K · cr)
  # =========================================================================
  cr <- hedges_correction(df_nct)
  stat <- d_index / (K * cr)  # bias-corrected test statistic
  
  cat(sprintf("    cr(df) = 1 - 3/(4·%d - 1) = %.6f (Hedges' correction)\n", df_nct, cr))
  cat(sprintf("    d/(K·cr) = %.6f / (%.6f · %.6f) = %.6f\n", d_index, K, cr, stat))
  
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
  
  cat(sprintf("    ncp_lower = -θ/K = %.6f, ncp_upper = θ/K = %.6f\n", ncp_lower, ncp_upper))
  cat(sprintf("    Lower test: P[t(%d, ncp=%.4f) ≤ %.4f] = %.6f (need > %.3f)\n",
              df_nct, ncp_lower, stat, p_lower, 1 - alpha))
  cat(sprintf("    Upper test: P[t(%d, ncp=%.4f) ≥ %.4f] = %.6f (need > %.3f)\n",
              df_nct, ncp_upper, stat, p_upper, 1 - alpha))
  
  # Decision: both must exceed 1-α
  lower_pass <- (p_lower > 1 - alpha)
  upper_pass <- (p_upper > 1 - alpha)
  rsabe_pass <- lower_pass && upper_pass
  
  # Overall p-value = max of the two one-sided p-values
  overall_p <- max(p1, p2)
  
  cat(sprintf("    Lower test: %s, Upper test: %s\n",
              ifelse(lower_pass, "PASS", "FAIL"),
              ifelse(upper_pass, "PASS", "FAIL")))
  cat(sprintf("    Overall p-value = max(p1, p2) = max(%.6f, %.6f) = %.6f\n", p1, p2, overall_p))
  cat(sprintf("    → %s\n", ifelse(rsabe_pass, "RSABE DEMONSTRATED", "RSABE NOT DEMONSTRATED")))
  
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
  anova_model <- params$anova_model %||% "fixed"
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
  
  cat(sprintf("🔬 Performing RSABE Analysis (%s)...\n", method_label))
  cat(sprintf("   ANOVA Model: %s\n", anova_model))
  cat(sprintf("   RSABE Method: %s\n", rsabe_method))
  cat(sprintf("   α = %.3f (%.0f%% CI)\n", alpha, (1 - 2 * alpha) * 100))
  cat(sprintf("   Criterion constant: σ_w0 = 0.25 (θ_s = %.4f)\n", theta_s))
  cat(sprintf("   Switching cutoff: s_wR ≥ %.3f (CV_wR ≈ %.1f%%)\n",
              s_wR_switch, sqrt(exp(s_wR_switch^2) - 1) * 100))
  
  # Detect replicate design
  design_info <- detect_replicate_design(data)
  
  if (!design_info$is_replicate) {
    stop("RSABE analysis requires a replicate design (2x2x3 or 2x2x4). Detected: ", design_info$design_type)
  }
  
  cat(sprintf("   Design: %s (%s)\n", design_info$design_type, design_info$design_name))
  
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
      cat(sprintf("⏭️  Skipping '%s' - already processed as '%s'\n", param, base_param_name))
      next
    }
    
    # Resolve parameter: prefer raw data (we'll log-transform ourselves)
    if (is_log_param) {
      raw_param <- base_param_name
      log_param <- param
      
      if (raw_param %in% names(data)) {
        param_to_use <- raw_param
        need_log_transform <- TRUE
        cat(sprintf("ℹ️  Using raw '%s' (will log-transform)\n", raw_param))
      } else if (log_param %in% names(data)) {
        param_to_use <- log_param
        need_log_transform <- FALSE
        cat(sprintf("ℹ️  Using pre-logged '%s'\n", log_param))
      } else {
        cat(sprintf("⚠️  Parameter '%s' not found, skipping\n", param))
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
          cat(sprintf("⚠️  Parameter '%s' not found, skipping\n", param))
          next
        }
      }
    }
    
    processed_params <- c(processed_params, base_param_name)
    
    cat(sprintf("\n--- RSABE Analysis for %s ---\n", base_param_name))
    
    tryCatch({
      
      # Prepare data: ensure log-transformed column exists
      analysis_data <- data
      log_col_name <- paste0("ln_", base_param_name, "_rsabe")
      
      if (need_log_transform) {
        raw_vals <- as.numeric(analysis_data[[param_to_use]])
        valid_mask <- !is.na(raw_vals) & raw_vals > 0
        analysis_data[[log_col_name]] <- NA_real_
        analysis_data[[log_col_name]][valid_mask] <- log(raw_vals[valid_mask])
        cat(sprintf("  Log-transformed %s → %s (%d valid values)\n", 
                    param_to_use, log_col_name, sum(valid_mask)))
      } else {
        analysis_data[[log_col_name]] <- as.numeric(analysis_data[[param_to_use]])
      }
      
      # Remove rows with NA in the analysis column
      analysis_data <- analysis_data[!is.na(analysis_data[[log_col_name]]), ]
      
      if (nrow(analysis_data) < 6) {
        cat(sprintf("  ⚠️  Insufficient data for %s (%d rows), skipping\n", base_param_name, nrow(analysis_data)))
        next
      }
      
      # Step 1: Fit ANOVA model for treatment effect
      model_result <- fit_rsabe_model(analysis_data, log_col_name, anova_model)
      
      # Step 2: Compute intra-subject Reference (and Test) variance via a
      # Reference-only (Test-only) ANOVA — Sequence + Subject(Sequence) +
      # Period, fit separately on each arm, restricted to subjects with >= 2
      # periods of that treatment. Verified to exactly reproduce
      # replicateBE::method.A()'s own CVwR/CVwT to machine precision on real
      # data (see R/simple_anova.R's compute_reference_anova_variance()) —
      # this is the same intra-subject-variance step ABEL uses (via
      # replicateBE internally), applied directly here since RSABE doesn't
      # route through replicateBE for its Howe UCB/ncTOST scaling decision.
      # This REPLACES the previous ISC (individual-subject-contrast) method
      # (that function has since been removed) — the ANOVA-based estimate is
      # the one FDA/SAS-style analyses report and the one now used for the
      # switching decision.
      refvar_result <- compute_reference_anova_variance(analysis_data, log_col_name)

      # Step 3: Check switching condition — FDA applies reference scaling only
      # when s_wR ≥ 0.294 (CV_wR ≈ 30%), NOT at σ_w0 = 0.25 (CV 25.4%). ONLY
      # Reference variability determines this — Test variability (s2_wT) is
      # not part of the switching criterion; it factors into the scaled
      # criterion/bound computation for heteroscedastic designs (see
      # compute_K_constant()), which is a separate step below.
      is_hv <- (refvar_result$s2_wR >= s2_wR_switch)

      cat(sprintf("  Switching: s_wR (%.4f) %s cutoff (%.3f) → %s\n",
                  sqrt(refvar_result$s2_wR), ifelse(is_hv, "≥", "<"), s_wR_switch,
                  ifelse(is_hv, "HIGH VARIABILITY → Use RSABE", "LOW VARIABILITY → Use ABE")))
      
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
            cat(sprintf("    K estimated from SE_d/s_wR: %.6f / %.6f = %.6f\n",
                        model_result$se_d, sqrt(refvar_result$s2_wR), K_val))
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
        
        cat(sprintf("  PE constraint: %.2f%% within [%.0f%%, %.0f%%]? %s\n",
                    model_result$pe_ratio, pe_constraint_lower, pe_constraint_upper,
                    ifelse(pe_within_constraint, "YES", "NO")))
        cat(sprintf("  Overall RSABE: %s (scaling: %s, PE: %s)\n",
                    ifelse(be_pass, "PASS", "FAIL"),
                    ifelse(rsabe_test$rsabe_pass, "PASS", "FAIL"),
                    ifelse(pe_within_constraint, "PASS", "FAIL")))
        
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
        # =====================================================================
        
        cat(sprintf("  📋 Using standard ABE limits (80-125%%) for %s\n", base_param_name))
        
        # Compute standard ABE CI
        t_crit <- qt(1 - alpha, model_result$df_d)
        ci_lower_val <- exp(model_result$d_hat - t_crit * model_result$se_d) * 100
        ci_upper_val <- exp(model_result$d_hat + t_crit * model_result$se_d) * 100
        
        be_pass <- (ci_lower_val >= 80.0) && (ci_upper_val <= 125.0)
        
        cat(sprintf("  ABE: PE=%.2f%%, CI [%.2f%%, %.2f%%], Limits [80%%, 125%%], BE=%s\n",
                    model_result$pe_ratio, ci_lower_val, ci_upper_val, ifelse(be_pass, "Pass", "Fail")))
        
        ci_list[[base_param_name]] <- list(
          parameter = base_param_name,
          point_estimate = model_result$pe_ratio,
          ci_lower = ci_lower_val,
          ci_upper = ci_upper_val,
          confidence_level = (1 - 2 * alpha) * 100,
          geometric_mean_ratio = model_result$pe_ratio / 100,
          within_limits = be_pass,
          cv_wr = refvar_result$cv_wR,
          cv_wt = refvar_result$cv_wT %||% NA,
          scaled_lower_limit = 80.0,
          scaled_upper_limit = 125.0,
          limits_used = list(lower = 80, upper = 125, type = "fixed"),
          method = "ABE (low variability — RSABE not required)",
          regulator = "FDA",
          degrees_freedom = model_result$df_d,
          n_subjects = model_result$n_subjects,
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
      
      # Store ANOVA-like results for compatibility
      all_results[[base_param_name]] <- list(
        model = model_result$model,
        anova = model_result$anova_table,
        # Parallel/accompanying comprehensive ANOVA table (SAS-style Source/DF/SS/MS/F/p
        # breakdown for fixed; Type III marginal tests for nlme) — alongside, not replacing,
        # the RSABE-specific Howe UCB/ncTOST scaling decision computed elsewhere in this
        # function (which uses s2_wR from the Reference-only ANOVA below, not this table).
        # See build_crossover_anova_tables() in R/simple_anova.R.
        anova_comprehensive = model_result$anova_comprehensive,
        subj_seq_analysis = model_result$subj_seq_analysis,
        type3_ss = model_result$type3_ss,
        lsmeans_result = model_result$lsmeans_result,
        treatment_coef = model_result$d_hat,
        treatment_se = model_result$se_d,
        residual_mse = model_result$residual_mse,
        residual_df = model_result$df_d,
        n_observations = model_result$n_observations,
        anova_method = model_result$anova_model,
        cv_wr_percent = refvar_result$cv_wR,
        cv_wt_percent = refvar_result$cv_wT %||% NA,
        s2_wR = refvar_result$s2_wR,
        s2_wT = refvar_result$s2_wT,
        df_wR = refvar_result$df_wR,
        df_wT = refvar_result$df_wT,
        # Reference-only / Test-only ANOVA tables (Sequence + Subject(Sequence) +
        # Period, no Treatment term) — the actual computation behind cv_wr_percent/
        # cv_wt_percent above, shown so the user can see the ANOVA that produced
        # the intra-subject variance, not just the resulting number.
        anova_wR = refvar_result$anova_wR,
        anova_wT = refvar_result$anova_wT,
        n_wR = refvar_result$n_R,
        n_wT = refvar_result$n_T
      )
      
      conclusion_list[[base_param_name]] <- be_pass
      
    }, error = function(e) {
      cat(sprintf("  ❌ Error analyzing %s: %s\n", base_param_name, e$message))
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
      note = sprintf("RSABE analysis using %s with Reference-only-ANOVA variance estimation", method_label)
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
  
  cat("✅ RSABE analysis completed!\n")
  return(results)
}
