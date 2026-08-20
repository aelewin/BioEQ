#' Enhanced ANOVA Analysis for Bioequivalence
#' 
#' Performs ANOVA analysis using four different methods:
#' 1. Fixed Effects (lm) - All effects fixed 
#' 2. Mixed Effects nlme (lme) - Subjects random using nlme
#' 3. Mixed Effects Satterthwaite (lmer) - Subjects random with Satterthwaite DF
#' 4. Mixed Effects Kenward-Roger (lmer) - Subjects random with Kenward-Roger DF
#'
#' @param nca_data Data frame containing NCA results with design variables
#' @param parameters Vector of parameter names to analyze (from NCA table columns)  
#' @param anova_model ANOVA model type: "fixed", "nlme", "satterthwaite", "kenward-roger"
#' @param random_effects For mixed models, specification of random effects (default: "(1|subject)")
#' @return List containing ANOVA results#' Detect study design based on data structure
#' 
#' @param nca_data NCA results data frame
#' @return Character string indicating design type ("parallel", "crossover")
detect_anova_design <- function(nca_data) {
  
  # Check if we have a period column (crossover design)
  if ("period" %in% names(nca_data)) {
    # Check if each subject appears in multiple periods
    subject_periods <- nca_data %>%
      group_by(subject) %>%
      summarise(n_periods = n_distinct(period), .groups = "drop")
    
    # If any subject has more than 1 period, it's a crossover design
    if (any(subject_periods$n_periods > 1)) {
      return("crossover")
    } else {
      return("parallel")
    }
  } else {
    # No period column - check treatments per subject
    subject_treatments <- nca_data %>%
      group_by(subject) %>%
      summarise(n_treatments = n_distinct(treatment), .groups = "drop")
    
    # If any subject has more than 1 treatment, it's likely crossover
    # If all subjects have exactly 1 treatment, it's parallel
    if (any(subject_treatments$n_treatments > 1)) {
      return("crossover")
    } else {
      return("parallel")
    }
  }
}

#' Build SAS-style comprehensive ANOVA tables for a fixed-effects 2x2 crossover model
#'
#' Given an `lm(y ~ seq + subj:seq + prd + drug, ...)` fit — the standard fixed-effects
#' crossover model shared by ABE (`perform_simple_anova`) and RSABE
#' (`fit_rsabe_model` in R/rsabe_analysis.R) — builds the SAS PROC GLM Type III
#' Source/DF/Sum-Sq/Mean-Sq/F/Pr(>F) breakdown (Sequence, Subject(Sequence), Period,
#' Treatment, Residual — all tested against Residual MS, matching SAS's default) plus
#' the "Tests of Hypotheses Using the Type III MS for Subject(Seq) as Error Term"
#' table (Sequence tested against the between-subject Subject(Sequence) MS instead).
#'
#' Extracted from `perform_simple_anova()`'s fixed-effects branch so RSABE can reuse
#' the exact same SAS-style decomposition on its own already-fit model, instead of
#' only ever showing a plain Type I `anova()` table. Pure refactor — behavior for
#' `perform_simple_anova()` callers is unchanged.
#'
#' @param model An `lm` object fit as `y ~ seq + subj:seq + prd + drug`
#' @param type3_ss Unused; kept for backward compatibility with existing callers
#'   that still pass a pre-computed `drop1()` result. Type III SS is always
#'   computed via `emmeans::joint_tests()` now (see below), which is the only
#'   approach that reproduces SAS PROC GLM's Type III SS exactly, including for
#'   the Sequence term (see next paragraph).
#' @return list(comprehensive_anova = data.frame, subj_seq_analysis = list), or NULL
#'   if the model's terms don't match the expected crossover shape (caller should
#'   fall back to a generic Model/Error/Corrected-Total summary in that case).
#' @export
build_crossover_anova_tables <- function(model, type3_ss = NULL) {
  anova_table <- tryCatch(anova(model), error = function(e) NULL)
  if (is.null(anova_table)) return(NULL)

  at_rows   <- rownames(anova_table)
  resid_row <- at_rows[grepl("^Resid", at_rows)]
  if (length(resid_row) != 1) return(NULL)
  df_resid <- anova_table[resid_row, "Df"]
  ss_resid <- anova_table[resid_row, "Sum Sq"]
  ms_resid <- anova_table[resid_row, "Mean Sq"]

  # ── Type III (marginal) SS for every other term, via emmeans' own estimable-
  # contrast machinery ─────────────────────────────────────────────────────
  # `drop1()` cannot test `seq` on its own here: with `subj:seq` (Subject nested
  # in Sequence) also in the model, dropping `seq` alone while keeping `subj:seq`
  # is not a valid marginality-respecting comparison, and naive fallbacks (e.g.
  # F = t² from the raw coefficient, or plain Type I SS) do NOT reproduce SAS's
  # actual Type III SS for Sequence once the design is even slightly unbalanced
  # (verified against a real SAS PROC GLM run: those approaches gave 0.1280
  # instead of SAS's 0.07860991 for a 44-subject, one-partial-subject dataset).
  # `emmeans::joint_tests()` computes the correct estimable linear contrast for
  # each term — including Sequence vs. the nested Subject(Sequence) term — via
  # the same general "estimable functions" approach SAS uses internally. Its own
  # printed F-ratio is pre-rounded to 3 decimals, so the Wald F-test is redone
  # here at full precision using the contrast matrices it exposes via
  # `attr(., "est.fcns")`, filtered to the model's non-aliased coefficients.
  # Verified to reproduce SAS's Type III SS for Sequence, Subject(Sequence),
  # Period, and Treatment to 6+ decimal places on real data.
  jt <- tryCatch(emmeans::joint_tests(model), error = function(e) NULL)
  if (is.null(jt)) return(NULL)
  ef <- attr(jt, "est.fcns")
  beta <- coef(model)
  V <- tryCatch(vcov(model), error = function(e) NULL)
  if (is.null(ef) || is.null(V)) return(NULL)
  ok <- !is.na(beta)
  beta_ok <- beta[ok]
  V_ok <- V[ok, ok, drop = FALSE]

  compute_term <- function(term_name) {
    L <- ef[[term_name]]
    if (is.null(L)) return(NULL)
    L <- L[, ok, drop = FALSE]
    Lb <- L %*% beta_ok
    df1 <- nrow(L)
    vv <- tryCatch(solve(L %*% V_ok %*% t(L)), error = function(e) NULL)
    if (is.null(vv)) return(NULL)
    f_stat <- as.numeric(t(Lb) %*% vv %*% Lb) / df1
    ss <- f_stat * df1 * ms_resid
    list(df = df1, ss = ss, ms = ss / df1, f = f_stat,
         p = pf(f_stat, df1, df_resid, lower.tail = FALSE))
  }
  find_term <- function(pattern) {
    hit <- names(ef)[grepl(pattern, names(ef))]
    if (length(hit) != 1) return(NULL)
    compute_term(hit)
  }

  seq_t  <- find_term("^seq$")
  subj_t <- find_term("subj.*seq|seq.*subj")
  prd_t  <- find_term("^prd$")
  drug_t <- find_term("^drug$")
  if (is.null(seq_t) || is.null(subj_t) || is.null(prd_t) || is.null(drug_t)) return(NULL)

  comprehensive_anova <- data.frame(
    Source = c("Sequence", "Subject(Sequence)", "Period", "Treatment", "Residual"),
    Df = c(seq_t$df, subj_t$df, prd_t$df, drug_t$df, df_resid),
    `Sum Sq` = c(seq_t$ss, subj_t$ss, prd_t$ss, drug_t$ss, ss_resid),
    `Mean Sq` = c(seq_t$ms, subj_t$ms, prd_t$ms, drug_t$ms, ms_resid),
    `F value` = c(seq_t$f, subj_t$f, prd_t$f, drug_t$f, NA),
    `Pr(>F)`  = c(seq_t$p, subj_t$p, prd_t$p, drug_t$p, NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(comprehensive_anova) <- comprehensive_anova$Source

  # Second table: "Tests of Hypotheses Using the Type III MS for Subject(Seq) as Error Term"
  # The SAME Sequence SS/MS as above (a single fixed quantity, per SAS convention),
  # but F-tested against Subject(Sequence) MS instead of Residual MS — the
  # standard, correct significance test for the Sequence effect in a crossover
  # design (matches SAS PROC GLM's second table exactly).
  f_seq_vs_subj <- seq_t$ms / subj_t$ms
  p_seq_vs_subj <- pf(f_seq_vs_subj, seq_t$df, subj_t$df, lower.tail = FALSE)

  subj_seq_analysis <- list(
    error_term = list(
      df = subj_t$df,
      ss = subj_t$ss,
      ms = subj_t$ms
    ),
    hypothesis_tests = list(
      seq = list(
        df      = seq_t$df,
        ss      = seq_t$ss,
        ms      = seq_t$ms,
        f_value = f_seq_vs_subj,
        p_value = p_seq_vs_subj
      )
    )
  )

  list(comprehensive_anova = comprehensive_anova, subj_seq_analysis = subj_seq_analysis)
}

#' Least-Squares Means (via emmeans) for the treatment factor, with CI
#'
#' Computes the population-average log-scale LSMean for each treatment level
#' (Reference and Test) from a fitted crossover model, marginalized over the
#' other model terms (sequence/period/subject nesting) — matching what SAS
#' PROC GLM/MIXED reports as "Least Squares Means" for the treatment (FORM)
#' effect. Works generically for lm, nlme::lme, and lmerTest::lmer fits since
#' emmeans dispatches on model class.
#'
#' @param model Fitted model with a `drug` factor term (2 levels: Reference, Test)
#' @param ref_level,test_level The factor level strings for Reference and Test
#'   (e.g. "R"/"T" or "Reference"/"Test") — passed in explicitly rather than
#'   re-derived from the fitted model object, since that differs by model class.
#' @param level Confidence level for the LSMean CIs (default 0.90, matching the
#'   90% CI convention used throughout BE analysis — NOT emmeans' 0.95 default).
#' @return A list with per-level LSMean (log + back-transformed geometric scale),
#'   SE, df, and CI, or NULL if emmeans fails (e.g. model too degenerate).
compute_lsmeans_ci <- function(model, ref_level, test_level, level = 0.90) {
  tryCatch({
    emm <- emmeans::emmeans(model, ~ drug, level = level)
    emm_df <- as.data.frame(emm)

    row_ref  <- emm_df[as.character(emm_df$drug) == ref_level, , drop = FALSE]
    row_test <- emm_df[as.character(emm_df$drug) == test_level, , drop = FALSE]
    if (nrow(row_ref) != 1 || nrow(row_test) != 1) return(NULL)

    get_col <- function(row, nm) if (nm %in% names(row)) row[[nm]] else NA_real_

    list(
      ref_level = ref_level, test_level = test_level,
      lsmean_ref_log  = row_ref$emmean,  lsmean_test_log  = row_test$emmean,
      lsmean_ref_geo  = exp(row_ref$emmean), lsmean_test_geo  = exp(row_test$emmean),
      se_ref  = get_col(row_ref, "SE"),  se_test  = get_col(row_test, "SE"),
      df_ref  = get_col(row_ref, "df"),  df_test  = get_col(row_test, "df"),
      ci_lower_ref_log  = get_col(row_ref, "lower.CL"),  ci_upper_ref_log  = get_col(row_ref, "upper.CL"),
      ci_lower_test_log = get_col(row_test, "lower.CL"), ci_upper_test_log = get_col(row_test, "upper.CL"),
      ci_lower_ref_geo  = exp(get_col(row_ref, "lower.CL")),  ci_upper_ref_geo  = exp(get_col(row_ref, "upper.CL")),
      ci_lower_test_geo = exp(get_col(row_test, "lower.CL")), ci_upper_test_geo = exp(get_col(row_test, "upper.CL")),
      level = level
    )
  }, error = function(e) NULL)
}

#' Build a Sequence + Subject(Sequence) + Period ANOVA table for a single-arm
#' (Reference-only or Test-only) fixed-effects model
#'
#' Same Type III SAS-style construction as build_crossover_anova_tables(), but
#' for a model with no Treatment term (`y ~ seq + subj:seq + prd`), fit on just
#' one treatment arm's rows. Used to build the intra-subject variance ANOVA
#' table alongside compute_reference_anova_variance().
#'
#' @param model An `lm` object fit as `y ~ seq + subj:seq + prd`
#' @return A Source/Df/Sum Sq/Mean Sq/F value/Pr(>F) data.frame (Sequence,
#'   Subject(Sequence), Period, Residual), or NULL if the model's row names
#'   don't match the expected single-arm crossover shape.
#' @export
build_single_arm_anova_table <- function(model) {
  anova_table <- tryCatch(anova(model), error = function(e) NULL)
  if (is.null(anova_table)) return(NULL)
  type3_ss <- tryCatch(drop1(model, test = "F"), error = function(e) NULL)

  at_rows      <- rownames(anova_table)
  seq_row      <- at_rows[at_rows == "seq"]
  subj_seq_row <- at_rows[grepl("subj.*seq|seq.*subj", at_rows) & at_rows != "seq"]
  prd_row      <- at_rows[at_rows == "prd"]
  resid_row    <- at_rows[grepl("^Resid", at_rows)]

  if (!(length(seq_row) == 1 && length(subj_seq_row) == 1 &&
        length(prd_row) == 1 && length(resid_row) == 1)) {
    return(NULL)
  }

  ss3_seq  <- NA_real_
  ss3_subj <- NA_real_
  ss3_prd  <- NA_real_

  if (!is.null(type3_ss)) {
    tryCatch({
      drop1_df   <- as.data.frame(type3_ss)
      drop1_rows <- rownames(drop1_df)
      sub_r <- drop1_rows[grepl("subj.*seq|seq.*subj", drop1_rows) & drop1_rows != "<none>"]
      prd_r <- drop1_rows[drop1_rows == "prd"]
      if (length(sub_r) == 1) ss3_subj <- drop1_df[sub_r, "Sum of Sq"]
      if (length(prd_r) == 1) ss3_prd  <- drop1_df[prd_r, "Sum of Sq"]
    }, error = function(e) NULL)
  }

  # Type III SS for seq via full-model t-statistic (F = t^2 for 1 DF) — same
  # trick as build_crossover_anova_tables(), needed since drop1() can't drop
  # seq while subj:seq depends on it.
  tryCatch({
    cs <- summary(model)$coefficients
    seq_coef_rows <- grep("^seq[^:]", rownames(cs), value = TRUE)
    if (length(seq_coef_rows) == 1) {
      t_seq_val <- cs[seq_coef_rows, "t value"]
      ms_resid_tmp <- anova_table[resid_row, "Mean Sq"]
      ss3_seq <- t_seq_val^2 * ms_resid_tmp
    }
  }, error = function(e) NULL)

  if (is.na(ss3_seq))  ss3_seq  <- anova_table[seq_row,      "Sum Sq"]
  if (is.na(ss3_subj)) ss3_subj <- anova_table[subj_seq_row, "Sum Sq"]
  if (is.na(ss3_prd))  ss3_prd  <- anova_table[prd_row,      "Sum Sq"]

  df_seq      <- anova_table[seq_row,      "Df"]
  df_subj_seq <- anova_table[subj_seq_row, "Df"]
  df_prd      <- anova_table[prd_row,      "Df"]
  df_resid    <- anova_table[resid_row,    "Df"]
  ss_resid    <- anova_table[resid_row,    "Sum Sq"]
  ms_resid    <- anova_table[resid_row,    "Mean Sq"]

  ms_seq      <- ss3_seq  / df_seq
  ms_subj_seq <- ss3_subj / df_subj_seq
  ms_prd      <- ss3_prd  / df_prd

  f_seq  <- ms_seq      / ms_resid
  f_subj <- ms_subj_seq / ms_resid
  f_prd  <- ms_prd      / ms_resid
  p_seq  <- pf(f_seq,  df_seq,      df_resid, lower.tail = FALSE)
  p_subj <- pf(f_subj, df_subj_seq, df_resid, lower.tail = FALSE)
  p_prd  <- pf(f_prd,  df_prd,      df_resid, lower.tail = FALSE)

  out <- data.frame(
    Source = c("Sequence", "Subject(Sequence)", "Period", "Residual"),
    Df = c(df_seq, df_subj_seq, df_prd, df_resid),
    `Sum Sq` = c(ss3_seq, ss3_subj, ss3_prd, ss_resid),
    `Mean Sq` = c(ms_seq, ms_subj_seq, ms_prd, ms_resid),
    `F value` = c(f_seq, f_subj, f_prd, NA),
    `Pr(>F)`  = c(p_seq, p_subj, p_prd, NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- out$Source
  out
}

#' Compute intra-subject (Reference and Test) variance via a reference-only /
#' test-only ANOVA — the SAS PROC GLM / EMA Method A approach
#'
#' Fits `y ~ seq + subj:seq + prd` (Sequence + Subject(Sequence) + Period, NO
#' Treatment term — there's only one treatment in each arm) separately on the
#' Reference-arm rows and the Test-arm rows, restricted in each case to
#' subjects with >= 2 periods of that treatment (a within-subject variance
#' component cannot be estimated from a single observation — same requirement
#' RSABE's ISC method already enforces). The residual mean square of each
#' fit is s²_w for that arm; CV_w = sqrt(exp(s²_w) - 1) * 100.
#'
#' This is the SAME intra-subject-variance step ABEL and RSABE both need
#' (only the downstream decision — expanded ABEL limits vs. RSABE's Howe UCB/
#' ncTOST criterion — differs by design). The COMPLETE dataset (all subjects,
#' every period) is used separately, for the treatment-effect ANOVA — see
#' build_crossover_anova_tables() — never for this variance step.
#'
#' @param data Data frame with Subject, Treatment, Period, Sequence, and the
#'   (log-transformed) parameter column
#' @param param_col Name of the log-transformed parameter column
#' @return list(s2_wR, cv_wR, df_wR, n_R, anova_wR, ref_subjects,
#'              s2_wT, cv_wT, df_wT, n_T, anova_wT, test_subjects)
#'   s2_w*/cv_w*/df_w* are NA and anova_w* is NULL for an arm with fewer than
#'   2 qualifying subjects.
#' @export
compute_reference_anova_variance <- function(data, param_col) {

  fit_one_arm <- function(arm_data) {
    arm_data$Subject <- as.character(arm_data$Subject)
    arm_data <- arm_data[!is.na(arm_data[[param_col]]), ]
    counts <- table(arm_data$Subject)
    qualifying <- names(counts)[counts >= 2]

    result <- list(s2_w = NA_real_, cv_w = NA_real_, df_w = NA_real_,
                    n = length(qualifying), anova = NULL, subjects = qualifying)

    if (length(qualifying) < 2) return(result)

    sub <- arm_data[arm_data$Subject %in% qualifying, ]
    sub$subj <- as.factor(sub$Subject)
    sub$seq  <- as.factor(sub$Sequence)
    sub$prd  <- as.factor(sub$Period)
    sub$y    <- sub[[param_col]]

    model <- tryCatch(lm(y ~ seq + subj:seq + prd, data = sub, na.action = na.omit),
                       error = function(e) NULL)
    if (is.null(model)) return(result)

    at <- tryCatch(anova(model), error = function(e) NULL)
    if (is.null(at) || !("Residuals" %in% rownames(at))) return(result)

    s2_w <- at["Residuals", "Mean Sq"]
    df_w <- at["Residuals", "Df"]
    cv_w <- sqrt(exp(s2_w) - 1) * 100

    result$s2_w  <- s2_w
    result$df_w  <- df_w
    result$cv_w  <- cv_w
    result$anova <- tryCatch(build_single_arm_anova_table(model), error = function(e) NULL)
    result
  }

  ref  <- fit_one_arm(data[data$Treatment == "R", ])
  test <- fit_one_arm(data[data$Treatment == "T", ])

  list(
    s2_wR = ref$s2_w,  cv_wR = ref$cv_w,  df_wR = ref$df_w,  n_R = ref$n,
    anova_wR = ref$anova, ref_subjects = ref$subjects,
    s2_wT = test$s2_w, cv_wT = test$cv_w, df_wT = test$df_w, n_T = test$n,
    anova_wT = test$anova, test_subjects = test$subjects
  )
}

#'
perform_simple_anova <- function(nca_data, parameters, anova_model = "fixed", random_effects = "(1|subject)",
                                include_group_fixed = FALSE, include_group_random = FALSE, 
                                include_group_treatment_interaction = FALSE,
                                alpha = 0.1) {
  
  # Validate alpha and derive CI level
  if (is.null(alpha) || is.na(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.1
  ci_level <- 1 - alpha  # e.g., alpha=0.1 -> 90% CI
  
  cat("\n=== ANOVA Analysis ===\n")
  
  # Standardize column names to lowercase for consistent processing
  # This handles both capitalized (Subject, Treatment) and lowercase (subject, treatment) inputs
  col_mapping <- c(
    "Subject" = "subject",
    "Treatment" = "treatment", 
    "Period" = "period",
    "Sequence" = "sequence",
    "Group" = "group"
  )
  
  for (old_name in names(col_mapping)) {
    new_name <- col_mapping[[old_name]]
    if (old_name %in% names(nca_data) && !new_name %in% names(nca_data)) {
      nca_data[[new_name]] <- nca_data[[old_name]]
      cat(sprintf("  📝 Standardized column: %s -> %s\n", old_name, new_name))
    }
  }
  
  # Detect study design
  study_design <- detect_anova_design(nca_data)
  cat(sprintf("Detected Study Design: %s\n", study_design))
  
  # Display model type and formula based on method and design
  switch(anova_model,
    "fixed" = {
      cat("Model Type: Fixed Effects (lm)\n")
      if (study_design == "parallel") {
        cat("Model: Parameter ~ treatment\n")
      } else {
        cat("Model: Parameter ~ seq + subj:seq + prd + drug\n")
      }
      cat("Package: stats\n")
    },
    "nlme" = {
      cat("Model Type: Mixed Effects (lme - nlme)\n")
      if (study_design == "parallel") {
        cat("Model: Parameter ~ treatment\n")
      } else {
        cat("Model: Parameter ~ sequence + period + treatment\n")
      }
      cat(sprintf("Random Effects: %s\n", random_effects))
      cat("Package: nlme\n")
    },
    "satterthwaite" = {
      cat("Model Type: Mixed Effects (lmer - Satterthwaite)\n")
      if (study_design == "parallel") {
        cat("Model: Parameter ~ treatment + random_effects\n")
      } else {
        cat("Model: Parameter ~ sequence + period + treatment + random_effects\n")
      }
      cat(sprintf("Random Effects: %s\n", random_effects))
      cat("Package: lmerTest (Satterthwaite DF)\n")
    },
    "kenward-roger" = {
      cat("Model Type: Mixed Effects (lmer - Kenward-Roger)\n")
      if (study_design == "parallel") {
        cat("Model: Parameter ~ treatment + random_effects\n")
      } else {
        cat("Model: Parameter ~ sequence + period + treatment + random_effects\n")
      }
      cat(sprintf("Random Effects: %s\n", random_effects))
      cat("Package: lmerTest (Kenward-Roger DF)\n")
    }
  )
  
  cat(sprintf("Analyzing parameters: %s\n", paste(parameters, collapse = ", ")))
  cat("========================\n\n")
  
  # Check required design variables based on study design
  if (study_design == "crossover") {
    required_vars <- c("sequence", "subject", "period", "treatment")
  } else {
    # For parallel design, only need subject and treatment
    required_vars <- c("subject", "treatment")
  }
  
  missing_vars <- required_vars[!required_vars %in% names(nca_data)]
  
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing required design variables for %s design: %s", 
                 study_design, paste(missing_vars, collapse = ", ")))
  }
  
  # Map column names to expected ANOVA model names based on design
  if (study_design == "crossover") {
    nca_data$seq <- as.factor(nca_data$sequence)
    nca_data$prd <- as.factor(nca_data$period)
  }
  nca_data$subj <- as.factor(nca_data$subject)
  
  # Explicitly set drug factor levels with R as reference to ensure T/R ratio calculation
  # This ensures drugT coefficient represents ln(Test/Reference)
  unique_treatments <- unique(nca_data$treatment)
  
  if (all(c("R", "T") %in% unique_treatments)) {
    nca_data$drug <- factor(nca_data$treatment, levels = c("R", "T"))
    cat("  ✓ Drug factor set with R as reference (R, T)\n")
  } else if (all(c("Reference", "Test") %in% unique_treatments)) {
    nca_data$drug <- factor(nca_data$treatment, levels = c("Reference", "Test"))
    cat("  ✓ Drug factor set with Reference as reference (Reference, Test)\n")
  } else {
    # Fallback to automatic assignment but warn user
    nca_data$drug <- as.factor(nca_data$treatment)
    cat(sprintf("  ⚠️  Warning: Could not identify R/T or Reference/Test treatments. Found: %s\n", 
                paste(unique_treatments, collapse = ", ")))
    cat("  ⚠️  Factor levels will be set alphabetically - verify T/R ratio interpretation\n")
  }
  
  # Determine the drug coefficient name dynamically based on factor levels
  # When levels are c("R", "T"), coefficient is "drugT"
  # When levels are c("Reference", "Test"), coefficient is "drugTest"
  drug_levels <- levels(nca_data$drug)
  drug_coef_name <- paste0("drug", drug_levels[length(drug_levels)])  # Non-reference level
  cat(sprintf("  ✓ Drug coefficient name: %s\n", drug_coef_name))
  lsmeans_result <- NULL  # populated after the model is fit, below
  
  # Check for group effects and prepare group factor
  has_groups <- FALSE
  if ("group" %in% names(nca_data) && (include_group_fixed || include_group_random)) {
    unique_groups <- unique(nca_data$group[!is.na(nca_data$group)])
    if (length(unique_groups) > 1) {
      has_groups <- TRUE
      nca_data$grp <- as.factor(nca_data$group)
      cat(sprintf("  ✓ Group factor detected with %d levels: %s\n", 
                  length(unique_groups), paste(unique_groups, collapse = ", ")))
      
      if (include_group_fixed) {
        cat("  📊 Including Group as fixed effect\n")
      }
      if (include_group_random) {
        cat("  📊 Including Group as random effect\n")
      }
      if (include_group_treatment_interaction) {
        cat("  📊 Including Group × Treatment interaction\n")
      }
    } else {
      cat("  ℹ️  Group column found but only one group detected - ignoring group effects\n")
    }
  }
  
  # Initialize results list
  anova_results <- list()
  
  # Loop through each parameter
  for (param in parameters) {
    
    cat(sprintf("\n--- Analyzing %s ---\n", param))
    
    # Check if parameter exists in data
    if (!param %in% names(nca_data)) {
      cat(sprintf("Warning: Parameter %s not found in NCA data. Skipping.\n", param))
      next
    }
    
    # Extract parameter values
    param_values <- nca_data[[param]]
    
    # Check for missing values
    if (all(is.na(param_values))) {
      cat(sprintf("Warning: All values are NA for %s. Skipping.\n", param))
      next
    }
    
    # Remove rows with missing parameter values
    complete_data <- nca_data[!is.na(param_values), ]
    
    if (nrow(complete_data) < 4) {
      cat(sprintf("Warning: Insufficient data for %s (n=%d). Skipping.\n", param, nrow(complete_data)))
      next
    }
    
    # Fit the appropriate model based on anova_model selection
    tryCatch({
      
      # Set options for numerical precision
      old_digits <- options("digits")
      old_contrasts <- options("contrasts")
      options(digits = 12)
      options(contrasts = c("contr.treatment", "contr.poly"))
      on.exit({
        options(old_digits)
        options(old_contrasts)
      })
      
      # Initialize model variables
      model <- NULL
      anova_table <- NULL
      type3_ss <- NULL
      model_summary <- NULL
      model_aic <- NULL
      pe_estimate <- NULL
      ci_lower <- NULL
      ci_upper <- NULL
      df_residual <- NULL
      treatment_coef <- NA
      treatment_se <- NA
      treatment_pval <- NA
      subj_seq_analysis <- NULL
      
      # Method-specific model fitting
      switch(anova_model,
        
        "fixed" = {
          # Fixed Effects Model using lm (all effects fixed)
          cat(sprintf("  🔧 Using Fixed Effects Model for %s...\n", param))
          
          # Choose formula based on study design and group effects
          if (study_design == "parallel") {
            # For parallel design: Parameter ~ treatment [+ group effects]
            formula_parts <- c("drug")
            
            if (has_groups && include_group_fixed) {
              formula_parts <- c(formula_parts, "grp")
              if (include_group_treatment_interaction) {
                formula_parts <- c(formula_parts, "grp:drug")
              }
            }
            
            formula_str <- sprintf("%s ~ %s", param, paste(formula_parts, collapse = " + "))
            cat(sprintf("  📋 Parallel design model: %s\n", formula_str))
            
          } else {
            # For crossover design: Parameter ~ sequence + subject %in% sequence + period + treatment [+ group effects]
            if (has_groups && include_group_fixed) {
              # For crossover with groups: subjects are nested within groups
              # Model: Parameter ~ group + sequence %in% group + subject %in% (group:sequence) + period + treatment
              formula_parts <- c("grp", "seq:grp", "subj:(grp:seq)", "prd", "drug")
              
              if (include_group_treatment_interaction) {
                formula_parts <- c(formula_parts, "grp:drug")
              }
              
              formula_str <- sprintf("%s ~ %s", param, paste(formula_parts, collapse = " + "))
              cat(sprintf("  📋 Crossover design with groups model: %s\n", formula_str))
            } else {
              # Standard crossover without groups
              formula_str <- sprintf("%s ~ seq + subj:seq + prd + drug", param)
              cat(sprintf("  📋 Crossover design model: %s\n", formula_str))
            }
          }
          
          model_formula <- as.formula(formula_str)
          
          # Fit model
          model <- lm(model_formula, data = complete_data, na.action = na.omit)
          lsmeans_result <- compute_lsmeans_ci(model, drug_levels[1], drug_levels[length(drug_levels)], level = ci_level)

          # Get ANOVA table and summaries
          anova_table <- anova(model)  # This gives Type I SS (sequential)
          type3_ss <- drop1(model, test = "F")  # This gives Type III SS (marginal)
          model_summary <- summary(model)
          model_aic <- AIC(model)
          
          # Build SAS-style source-level ANOVA table for crossover; generic fallback otherwise
          if (study_design == "crossover" && !has_groups) {
            built <- build_crossover_anova_tables(model, type3_ss)
            if (!is.null(built)) {
              comprehensive_anova <- built$comprehensive_anova
              subj_seq_analysis   <- built$subj_seq_analysis
            } else {
              # Unexpected row names: fall back to generic 3-row summary
              cat(sprintf("  ⚠️  Expected crossover ANOVA rows not found (rows: %s); using generic summary\n",
                          paste(rownames(anova_table), collapse = ", ")))
              comprehensive_anova <- data.frame(
                Source = c("Model", "Error", "Corrected Total"),
                Df = c(
                  sum(anova_table$Df[-nrow(anova_table)]),
                  anova_table$Df[nrow(anova_table)],
                  sum(anova_table$Df)
                ),
                `Sum Sq` = c(
                  sum(anova_table$`Sum Sq`[-nrow(anova_table)]),
                  anova_table$`Sum Sq`[nrow(anova_table)],
                  sum(anova_table$`Sum Sq`)
                ),
                `Mean Sq` = c(
                  sum(anova_table$`Sum Sq`[-nrow(anova_table)]) / sum(anova_table$Df[-nrow(anova_table)]),
                  anova_table$`Mean Sq`[nrow(anova_table)],
                  NA
                ),
                `F value` = c(
                  (sum(anova_table$`Sum Sq`[-nrow(anova_table)]) / sum(anova_table$Df[-nrow(anova_table)])) /
                    anova_table$`Mean Sq`[nrow(anova_table)],
                  NA, NA
                ),
                `Pr(>F)` = c(
                  pf((sum(anova_table$`Sum Sq`[-nrow(anova_table)]) / sum(anova_table$Df[-nrow(anova_table)])) /
                       anova_table$`Mean Sq`[nrow(anova_table)],
                     sum(anova_table$Df[-nrow(anova_table)]),
                     anova_table$Df[nrow(anova_table)], lower.tail = FALSE),
                  NA, NA
                ),
                check.names = FALSE,
                stringsAsFactors = FALSE
              )
              rownames(comprehensive_anova) <- comprehensive_anova$Source
            }

          } else {
            # Parallel design or crossover with groups: generic 3-row summary
            comprehensive_anova <- data.frame(
              Source = c("Model", "Error", "Corrected Total"),
              Df = c(
                sum(anova_table$Df[-nrow(anova_table)]),
                anova_table$Df[nrow(anova_table)],
                sum(anova_table$Df)
              ),
              `Sum Sq` = c(
                sum(anova_table$`Sum Sq`[-nrow(anova_table)]),
                anova_table$`Sum Sq`[nrow(anova_table)],
                sum(anova_table$`Sum Sq`)
              ),
              `Mean Sq` = c(
                sum(anova_table$`Sum Sq`[-nrow(anova_table)]) / sum(anova_table$Df[-nrow(anova_table)]),
                anova_table$`Mean Sq`[nrow(anova_table)],
                NA
              ),
              `F value` = c(
                (sum(anova_table$`Sum Sq`[-nrow(anova_table)]) / sum(anova_table$Df[-nrow(anova_table)])) /
                  anova_table$`Mean Sq`[nrow(anova_table)],
                NA, NA
              ),
              `Pr(>F)` = c(
                pf((sum(anova_table$`Sum Sq`[-nrow(anova_table)]) / sum(anova_table$Df[-nrow(anova_table)])) /
                     anova_table$`Mean Sq`[nrow(anova_table)],
                   sum(anova_table$Df[-nrow(anova_table)]),
                   anova_table$Df[nrow(anova_table)], lower.tail = FALSE),
                NA, NA
              ),
              check.names = FALSE,
              stringsAsFactors = FALSE
            )
            rownames(comprehensive_anova) <- comprehensive_anova$Source
          }

          # Extract treatment effect
          coeffs <- coef(model)
          if (drug_coef_name %in% names(coeffs)) {
            pe_estimate <- 100 * exp(coeffs[[drug_coef_name]])
            ci <- 100 * exp(confint(model, drug_coef_name, level = ci_level))
            ci_lower <- ci[1]
            ci_upper <- ci[2]
            df_residual <- anova_table["Residuals", "Df"]
          }
        },
        
        "nlme" = {
          # Mixed Effects Model using nlme::lme
          cat(sprintf("  🔧 Using Mixed Effects Model (nlme) for %s...\n", param))
          
          if (!requireNamespace("nlme", quietly = TRUE)) {
            stop("nlme package not available. Please install nlme package.")
          }
          
          # Parse random effects specification
          # For nlme, we need to create the proper random effects formula
          # Handle group effects in random effects specification
          if (has_groups && include_group_random) {
            # Modify random effects to include groups
            if (random_effects == "(1|subject)") {
              # Add group nesting: (1|group/subject) or (1|subject) with group in fixed effects
              random_formula <- ~ 1 | grp/subj
              grouping_var <- "grp"
              cat("  📊 Modified random effects to include groups: ~ 1 | grp/subj\n")
            } else {
              # Try to parse user-specified random effects and add group
              random_formula <- as.formula(paste("~", gsub("^\\(|\\)$", "", random_effects)))
              grouping_var <- all.vars(random_formula)[length(all.vars(random_formula))]
            }
          } else {
            # Standard random effects without groups
            if (random_effects == "(1|subject)") {
              random_formula <- ~ 1 | subj
              grouping_var <- "subj"
            } else {
              random_formula <- as.formula(paste("~", gsub("^\\(|\\)$", "", random_effects)))
              grouping_var <- all.vars(random_formula)[length(all.vars(random_formula))]
            }
          }
          
          # Check if grouping variable exists in data
          if (!grouping_var %in% names(complete_data)) {
            stop(sprintf("Grouping variable '%s' not found in data. Available variables: %s", 
                        grouping_var, paste(names(complete_data), collapse=", ")))
          }
          
          # Choose formula based on study design and group effects
          if (study_design == "parallel") {
            # For parallel design: Parameter ~ treatment [+ group effects]
            formula_parts <- c("drug")
            
            if (has_groups && include_group_fixed) {
              formula_parts <- c(formula_parts, "grp")
              if (include_group_treatment_interaction) {
                formula_parts <- c(formula_parts, "grp:drug")
              }
            }
            
            formula_str <- sprintf("%s ~ %s", param, paste(formula_parts, collapse = " + "))
            cat(sprintf("  📋 Parallel design model: %s\n", formula_str))
            
          } else {
            # For crossover design: Parameter ~ sequence + period + treatment [+ group effects]
            formula_parts <- c("seq", "prd", "drug")
            
            if (has_groups && include_group_fixed) {
              formula_parts <- c("grp", formula_parts)
              if (include_group_treatment_interaction) {
                formula_parts <- c(formula_parts, "grp:drug")
              }
            }
            
            formula_str <- sprintf("%s ~ %s", param, paste(formula_parts, collapse = " + "))
            cat(sprintf("  📋 Crossover design model: %s\n", formula_str))
          }
          model_formula <- as.formula(formula_str)
          cat(sprintf("  [DEBUG] Model formula: %s\n", formula_str))
          cat(sprintf("  [DEBUG] Data dimensions: %d rows, %d cols\n", nrow(complete_data), ncol(complete_data)))
          cat(sprintf("  [DEBUG] Available variables: %s\n", paste(names(complete_data), collapse=", ")))
          cat(sprintf("  [DEBUG] Grouping variable '%s' has %d levels\n", grouping_var, length(unique(complete_data[[grouping_var]]))))
          
          # Fit model with error handling
          cat("  [DEBUG] Fitting nlme model...\n")
          tryCatch({
            model <- nlme::lme(model_formula, 
                             random = random_formula,
                             data = complete_data, 
                             na.action = na.omit,
                             method = "REML")
            cat("  [DEBUG] Model fitted successfully\n")
          }, error = function(e) {
            cat(sprintf("  [ERROR] nlme model fitting failed: %s\n", e$message))
            stop(sprintf("nlme model fitting failed for %s: %s", param, e$message))
          })
          lsmeans_result <- compute_lsmeans_ci(model, drug_levels[1], drug_levels[length(drug_levels)], level = ci_level)

          cat("  [DEBUG] Getting summaries...\n")
          
          # Get summaries
          model_summary <- summary(model)
          model_aic <- AIC(model)
          cat("  [DEBUG] Summaries obtained\n")
          
          # Extract treatment effect
          tTable <- model_summary$tTable
          cat(sprintf("  [DEBUG] tTable rownames: %s\n", paste(rownames(tTable), collapse=", ")))
          
          if (drug_coef_name %in% rownames(tTable)) {
            cat(sprintf("  [DEBUG] Found %s coefficient\n", drug_coef_name))
            pe_estimate <- 100 * exp(tTable[drug_coef_name, "Value"])
            
            # Get confidence intervals for nlme model 
            cat("  [DEBUG] Getting confidence intervals...\n")
            tryCatch({
              ci_obj <- nlme::intervals(model, which = "fixed", level = ci_level)
              cat("  [DEBUG] Intervals object obtained\n")
              if (drug_coef_name %in% rownames(ci_obj$fixed)) {
                ci_vals <- 100 * exp(ci_obj$fixed[drug_coef_name, c("lower", "upper")])
                ci_lower <- ci_vals[1]
                ci_upper <- ci_vals[2]
                cat("  [DEBUG] Confidence intervals extracted from intervals() function\n")
              } else {
                cat(sprintf("  [DEBUG] %s not found in intervals object, using manual calculation\n", drug_coef_name))
                # Fallback: calculate CI manually using t-distribution
                coef_val <- tTable[drug_coef_name, "Value"]
                se_val <- tTable[drug_coef_name, "Std.Error"]
                df_val <- tTable[drug_coef_name, "DF"]
                t_crit <- qt(1 - alpha/2, df_val)  # CI based on alpha
                ci_lower <- 100 * exp(coef_val - t_crit * se_val)
                ci_upper <- 100 * exp(coef_val + t_crit * se_val)
              }
            }, error = function(e) {
              cat(sprintf("  [DEBUG] Error in intervals(): %s\n", e$message))
              # Fallback calculation if intervals() fails
              coef_val <- tTable[drug_coef_name, "Value"]
              se_val <- tTable[drug_coef_name, "Std.Error"]
              df_val <- tTable[drug_coef_name, "DF"]
              t_crit <- qt(1 - alpha/2, df_val)  # CI based on alpha
              ci_lower <- 100 * exp(coef_val - t_crit * se_val)
              ci_upper <- 100 * exp(coef_val + t_crit * se_val)
              cat("  [DEBUG] Used fallback CI calculation\n")
            })
            
            df_residual <- tTable[drug_coef_name, "DF"]
          } else {
            cat(sprintf("  [DEBUG] %s not found in tTable\n", drug_coef_name))
          }
          
          # Create anova-like table for nlme
          cat("  [DEBUG] Getting ANOVA table...\n")
          anova_table <- anova(model)  # This gives Type I SS for nlme
          cat("  [DEBUG] ANOVA table obtained\n")
          cat(sprintf("  [DEBUG] ANOVA table dimensions: %d rows, %d cols\n", nrow(anova_table), ncol(anova_table)))
          
          # Type III (marginal) tests of fixed effects — nlme supports this via
          # anova(model, type = "marginal"); this is the appropriate per-term
          # significance table for a mixed model (SAS PROC MIXED "Type 3 Tests
          # of Fixed Effects" equivalent): Sequence, Period, Treatment each with
          # numDF/denDF/F-value/p-value.
          cat("  [DEBUG] Computing Type III (marginal) fixed-effects tests...\n")
          type3_ss <- tryCatch({
            anova(model, type = "marginal")
          }, error = function(e) {
            cat(sprintf("  [DEBUG] Marginal anova failed (%s); falling back to sequential\n", e$message))
            anova_table
          })
          cat("  [DEBUG] Type III SS table created\n")

          # Create comprehensive ANOVA table (Model/Error/Corrected Total) for nlme
          # For mixed models, we focus on fixed effects.
          # NOTE: nlme's `model$dims` does not expose a `$p` (fixed-effect count)
          # element in current nlme versions — use length(fixef(model)) instead,
          # which previously caused residual_df/model_df to silently become NA.
          cat("  [DEBUG] Creating comprehensive ANOVA table...\n")
          n_fixef <- length(nlme::fixef(model))
          residual_df <- as.numeric(model$dims$N - n_fixef)
          residual_ss <- sum(resid(model)^2)
          residual_ms <- residual_ss / residual_df
          cat("  [DEBUG] Residual calculations done\n")
          
          # Calculate model SS as difference from total
          # For nlme models, we need to extract y values differently
          y_values <- tryCatch({
            cat("  [DEBUG] Attempting model.frame approach...\n")
            model.response(model.frame(model))
          }, error = function(e) {
            cat(sprintf("  [DEBUG] model.frame failed (%s), trying alternative...\n", e$message))
            # Alternative: get response from model data
            tryCatch({
              # Get the response variable from the model formula
              response_name <- all.vars(formula(model))[1]
              cat(sprintf("  [DEBUG] Response variable name: %s\n", response_name))
              model$data[[response_name]]
            }, error = function(e2) {
              cat(sprintf("  [DEBUG] Alternative failed (%s), using complete_data...\n", e2$message))
              # Final fallback: use original data
              complete_data[[param]]
            })
          })
          cat(sprintf("  [DEBUG] Y values extracted, length: %d\n", length(y_values)))
          
          # Ensure we have valid y_values
          if (is.null(y_values) || length(y_values) == 0) {
            stop("Could not extract response values from model")
          }
          
          total_ss <- sum((y_values - mean(y_values, na.rm = TRUE))^2, na.rm = TRUE)
          model_ss <- total_ss - residual_ss
          model_df <- as.numeric(n_fixef - 1)  # Excluding intercept
          model_ms <- model_ss / model_df
          cat("  [DEBUG] Model calculations done\n")
          
          # Debug the values before creating data frame
          cat(sprintf("  [DEBUG] comprehensive_anova values:\n"))
          cat(sprintf("    model_df: %s (length: %d)\n", toString(model_df), length(model_df)))
          cat(sprintf("    residual_df: %s (length: %d)\n", toString(residual_df), length(residual_df)))
          cat(sprintf("    model_ss: %s (length: %d)\n", toString(model_ss), length(model_ss)))
          cat(sprintf("    residual_ss: %s (length: %d)\n", toString(residual_ss), length(residual_ss)))
          cat(sprintf("    total_ss: %s (length: %d)\n", toString(total_ss), length(total_ss)))
          cat(sprintf("    model_ms: %s (length: %d)\n", toString(model_ms), length(model_ms)))
          cat(sprintf("    residual_ms: %s (length: %d)\n", toString(residual_ms), length(residual_ms)))
          
          # Ensure all values are single scalars
          model_df <- as.numeric(model_df)[1]
          residual_df <- as.numeric(residual_df)[1]
          model_ss <- as.numeric(model_ss)[1]
          residual_ss <- as.numeric(residual_ss)[1]
          total_ss <- as.numeric(total_ss)[1]
          model_ms <- as.numeric(model_ms)[1]
          residual_ms <- as.numeric(residual_ms)[1]
          
          comprehensive_anova <- data.frame(
            Source = c("Model", "Error", "Corrected Total"),
            Df = c(model_df, residual_df, model_df + residual_df),
            `Sum Sq` = c(model_ss, residual_ss, total_ss),
            `Mean Sq` = c(model_ms, residual_ms, NA),
            `F value` = c(model_ms / residual_ms, NA, NA),
            `Pr(>F)` = c(
              pf(model_ms / residual_ms, model_df, residual_df, lower.tail = FALSE),
              NA, NA
            ),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          rownames(comprehensive_anova) <- comprehensive_anova$Source
          cat("  [DEBUG] Comprehensive ANOVA table created successfully\n")
        },
        
        "satterthwaite" = {
          # Mixed Effects Model using lmerTest with Satterthwaite DF
          cat(sprintf("  🔧 Using Mixed Effects Model (Satterthwaite) for %s...\n", param))
          
          if (!requireNamespace("lme4", quietly = TRUE) || !requireNamespace("lmerTest", quietly = TRUE)) {
            stop("lme4 and lmerTest packages required. Please install both packages.")
          }
          
          # Choose formula based on study design
          if (study_design == "parallel") {
            # For parallel design: Parameter ~ treatment + random_effects
            formula_str <- sprintf("%s ~ drug + %s", param, random_effects)
            cat(sprintf("  📋 Parallel design model: %s\n", formula_str))
          } else {
            # For crossover design: Parameter ~ sequence + period + treatment + random_effects
            formula_str <- sprintf("%s ~ seq + prd + drug + %s", param, random_effects)
            cat(sprintf("  📋 Crossover design model: %s\n", formula_str))
          }
          model_formula <- as.formula(formula_str)
          
          # Fit model
          model <- lmerTest::lmer(model_formula, data = complete_data, na.action = na.omit)
          lsmeans_result <- compute_lsmeans_ci(model, drug_levels[1], drug_levels[length(drug_levels)], level = ci_level)

          # Get summary with Satterthwaite DF
          model_summary <- summary(model, ddf = "Satterthwaite")
          model_aic <- AIC(model)
          anova_table <- anova(model)  # This gives Type I SS
          
          # Generate Type III SS for lmerTest
          type3_ss <- tryCatch({
            anova(model, type = "III")
          }, error = function(e) {
            # Fallback: use Type I as approximation
            anova_table
          })
          
          # Extract treatment DF (Satterthwaite approximation) before building comprehensive table
          coeffs_tmp      <- model_summary$coefficients
          df_val_comp     <- if (drug_coef_name %in% rownames(coeffs_tmp)) {
            as.numeric(coeffs_tmp[drug_coef_name, "df"])
          } else {
            nrow(complete_data) - length(lme4::fixef(model))
          }

          # Create comprehensive ANOVA table for lmerTest (Satterthwaite)
          residuals_vec <- residuals(model)
          residual_ss <- sum(residuals_vec^2)
          residual_df <- df_val_comp  # Satterthwaite DF for within-subject residual
          residual_ms <- residual_ss / residual_df
          
          # Calculate model SS
          y_values <- complete_data[[param]]
          total_ss <- sum((y_values - mean(y_values))^2)
          model_ss <- total_ss - residual_ss
          model_df <- length(fixef(model)) - 1  # Excluding intercept
          model_ms <- model_ss / model_df
          
          comprehensive_anova <- data.frame(
            Source = c("Model", "Error", "Corrected Total"),
            Df = c(model_df, residual_df, model_df + residual_df),
            `Sum Sq` = c(model_ss, residual_ss, total_ss),
            `Mean Sq` = c(model_ms, residual_ms, NA),
            `F value` = c(model_ms / residual_ms, NA, NA),
            `Pr(>F)` = c(
              pf(model_ms / residual_ms, model_df, residual_df, lower.tail = FALSE),
              NA, NA
            ),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          rownames(comprehensive_anova) <- comprehensive_anova$Source
          
          # Extract treatment effect
          coeffs <- model_summary$coefficients
          if (drug_coef_name %in% rownames(coeffs)) {
            pe_log <- coeffs[drug_coef_name, "Estimate"]
            se_log <- coeffs[drug_coef_name, "Std. Error"]
            df_val <- coeffs[drug_coef_name, "df"]
            
            pe_estimate <- 100 * exp(pe_log)
            t_crit <- qt(1 - alpha/2, df_val)
            ci_lower <- 100 * exp(pe_log - t_crit * se_log)
            ci_upper <- 100 * exp(pe_log + t_crit * se_log)
            df_residual <- df_val
          }
        },
        
        "kenward-roger" = {
          # Mixed Effects Model using lmerTest with Kenward-Roger DF
          cat(sprintf("  🔧 Using Mixed Effects Model (Kenward-Roger) for %s...\n", param))
          
          if (!requireNamespace("lme4", quietly = TRUE) || !requireNamespace("lmerTest", quietly = TRUE)) {
            stop("lme4 and lmerTest packages required. Please install both packages.")
          }
          
          # Choose formula based on study design
          if (study_design == "parallel") {
            # For parallel design: Parameter ~ treatment + random_effects
            formula_str <- sprintf("%s ~ drug + %s", param, random_effects)
            cat(sprintf("  📋 Parallel design model: %s\n", formula_str))
          } else {
            # For crossover design: Parameter ~ sequence + period + treatment + random_effects
            formula_str <- sprintf("%s ~ seq + prd + drug + %s", param, random_effects)
            cat(sprintf("  📋 Crossover design model: %s\n", formula_str))
          }
          model_formula <- as.formula(formula_str)
          
          # Fit model
          model <- lmerTest::lmer(model_formula, data = complete_data, na.action = na.omit)
          lsmeans_result <- compute_lsmeans_ci(model, drug_levels[1], drug_levels[length(drug_levels)], level = ci_level)

          # Get summary with Kenward-Roger DF
          model_summary <- summary(model, ddf = "Kenward-Roger")
          model_aic <- AIC(model)
          anova_table <- anova(model)  # This gives Type I SS
          
          # Generate Type III SS for lmerTest
          type3_ss <- tryCatch({
            anova(model, type = "III")
          }, error = function(e) {
            # Fallback: use Type I as approximation
            anova_table
          })
          
          # Extract treatment DF (Kenward-Roger approximation) before building comprehensive table
          coeffs_tmp      <- model_summary$coefficients
          df_val_comp     <- if (drug_coef_name %in% rownames(coeffs_tmp)) {
            as.numeric(coeffs_tmp[drug_coef_name, "df"])
          } else {
            nrow(complete_data) - length(lme4::fixef(model))
          }

          # Create comprehensive ANOVA table for lmerTest (Kenward-Roger)
          residuals_vec <- residuals(model)
          residual_ss <- sum(residuals_vec^2)
          residual_df <- df_val_comp  # Kenward-Roger DF for within-subject residual
          residual_ms <- residual_ss / residual_df
          
          # Calculate model SS
          y_values <- complete_data[[param]]
          total_ss <- sum((y_values - mean(y_values))^2)
          model_ss <- total_ss - residual_ss
          model_df <- length(fixef(model)) - 1  # Excluding intercept
          model_ms <- model_ss / model_df
          
          comprehensive_anova <- data.frame(
            Source = c("Model", "Error", "Corrected Total"),
            Df = c(model_df, residual_df, model_df + residual_df),
            `Sum Sq` = c(model_ss, residual_ss, total_ss),
            `Mean Sq` = c(model_ms, residual_ms, NA),
            `F value` = c(model_ms / residual_ms, NA, NA),
            `Pr(>F)` = c(
              pf(model_ms / residual_ms, model_df, residual_df, lower.tail = FALSE),
              NA, NA
            ),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          rownames(comprehensive_anova) <- comprehensive_anova$Source
          
          # Extract treatment effect
          coeffs <- model_summary$coefficients
          if (drug_coef_name %in% rownames(coeffs)) {
            pe_log <- coeffs[drug_coef_name, "Estimate"]
            se_log <- coeffs[drug_coef_name, "Std. Error"]
            df_val <- coeffs[drug_coef_name, "df"]
            
            pe_estimate <- 100 * exp(pe_log)
            t_crit <- qt(1 - alpha/2, df_val)
            ci_lower <- 100 * exp(pe_log - t_crit * se_log)
            ci_upper <- 100 * exp(pe_log + t_crit * se_log)
            df_residual <- df_val
          }
        }
      )
      
      # Validate model was fitted successfully
      if (is.null(model)) {
        cat(sprintf("  ❌ Model fitting failed for %s\n", param))
        next
      }
      
      cat("✓ Model fitted successfully\n")
      cat(sprintf("  Observations: %d\n", nrow(complete_data)))
      
      # Common result extraction
      if (!is.null(model_summary)) {
        if (anova_model == "fixed") {
          cat(sprintf("  R-squared: %.4f (Adj: %.4f)\n", 
                      model_summary$r.squared %||% NA, model_summary$adj.r.squared %||% NA))
        }
        if (!is.null(model_aic)) {
          cat(sprintf("  AIC: %.2f\n", model_aic))
        }
      }
      
      # Extract method-specific diagnostics
      residual_mse <- switch(anova_model,
        "fixed" = model_summary$sigma^2,
        "nlme" = model_summary$sigma^2,
        "satterthwaite" = attr(lme4::VarCorr(model), "sc")^2,
        "kenward-roger" = attr(lme4::VarCorr(model), "sc")^2,
        NA  # Default case
      )
      
      residual_df <- switch(anova_model,
        "fixed" = df_residual,  # Use the extracted df_residual, not model_summary$df[2]
        "nlme" = df_residual,
        "satterthwaite" = df_residual,
        "kenward-roger" = df_residual,
        NA  # Default case
      )
      
      # Print treatment effect info
      if (!is.null(pe_estimate)) {
        cat(sprintf("  Residual MSE: %s\n", if(is.null(residual_mse)) "NA" else format(residual_mse, digits=6)))
        
        # Extract treatment coefficient based on model type
        treatment_coef <- NA
        treatment_se <- NA
        treatment_pval <- NA
        
        if (anova_model == "nlme") {
          # For nlme models, use fixef() and tTable
          if (drug_coef_name %in% rownames(model_summary$tTable)) {
            treatment_coef <- model_summary$tTable[drug_coef_name, "Value"]
            treatment_se <- model_summary$tTable[drug_coef_name, "Std.Error"]
            treatment_pval <- model_summary$tTable[drug_coef_name, "p-value"]
          }
        } else if (anova_model %in% c("satterthwaite", "kenward-roger")) {
          # For lmer models
          if (!is.null(model_summary$coefficients) && drug_coef_name %in% rownames(model_summary$coefficients)) {
            treatment_coef <- model_summary$coefficients[drug_coef_name, "Estimate"]
            treatment_se <- model_summary$coefficients[drug_coef_name, "Std. Error"]
            treatment_pval <- model_summary$coefficients[drug_coef_name, 5]  # p-value column
          }
        } else {
          # For fixed effects models
          if (!is.null(model_summary$coefficients) && drug_coef_name %in% rownames(model_summary$coefficients)) {
            treatment_coef <- model_summary$coefficients[drug_coef_name, "Estimate"]
            treatment_se <- model_summary$coefficients[drug_coef_name, "Std. Error"]
            treatment_pval <- model_summary$coefficients[drug_coef_name, 4]  # p-value column
          }
        }
        
        cat(sprintf("  Treatment effect: %s (SE: %s, p: %s)\n", 
                    format(treatment_coef, digits=6),
                    format(treatment_se, digits=6),
                    format(treatment_pval, digits=4)))
      }
      
      # Calculate parameter mean from observed data or fitted values
      param_mean <- tryCatch({
        if (!is.null(model)) {
          fitted_vals <- fitted(model)
          mean(fitted_vals, na.rm = TRUE)
        } else {
          mean(complete_data[[param]], na.rm = TRUE)
        }
      }, error = function(e) {
        # Fallback to raw data mean
        mean(complete_data[[param]], na.rm = TRUE)
      })
      
      cat(sprintf("  [DEBUG] Parameter mean calculated: %s\n", param_mean))
      
      # Calculate Root MSE
      root_mse <- if (!is.na(residual_mse)) sqrt(residual_mse) else NA
      
      # Calculate Coefficient of Variation (C.V.)
      cv_percent <- if (!is.na(param_mean) && !is.na(root_mse) && param_mean != 0) {
        (root_mse / param_mean) * 100
      } else NA

      # ── Intra-subject CV% (within-subject) ──────────────────────────────────
      # CV% = sqrt(Residual MSE) * 100  (SAS/WinNonlin/Phoenix convention)
      cv_intra_pct <- if (!is.na(residual_mse) && residual_mse > 0) {
        sqrt(residual_mse) * 100
      } else NA

      # ── Inter-subject stats (between-subject) ────────────────────────────────
      # For fixed model: MSE_inter = Subject(Seq) MS (from ANOVA table directly)
      #   CV% = sqrt(MS_subj_seq) * 100; F = MS_subj_seq / MS_residual
      # For mixed models: MSE_inter = random-intercept variance component (REML)
      cv_inter_pct <- NA
      mse_inter    <- NA
      df_inter     <- NA
      f_inter      <- NA
      p_inter      <- NA
      n_subjects   <- length(unique(complete_data$subj))
      if (anova_model == "fixed") {
        ms_ss <- tryCatch(subj_seq_analysis$error_term$ms, error = function(e) NA)
        df_ss <- tryCatch(subj_seq_analysis$error_term$df, error = function(e) NA)
        if (!is.null(ms_ss) && !is.na(ms_ss) && !is.na(residual_mse) &&
            !is.null(df_ss)  && !is.na(df_ss)  && !is.na(residual_df)) {
          mse_inter    <- ms_ss
          cv_inter_pct <- sqrt(ms_ss) * 100
          df_inter     <- df_ss
          f_inter      <- ms_ss / residual_mse
          p_inter      <- pf(f_inter, df_ss, residual_df, lower.tail = FALSE)
        }
      } else if (anova_model == "nlme") {
        tryCatch({
          vc           <- nlme::VarCorr(model)
          mse_inter    <- as.numeric(vc[1, "Variance"])
          cv_inter_pct <- sqrt(mse_inter) * 100
          df_inter     <- n_subjects - nlevels(complete_data$seq)
        }, error = function(e) NULL)
      } else if (anova_model %in% c("satterthwaite", "kenward-roger")) {
        tryCatch({
          vc           <- lme4::VarCorr(model)
          mse_inter    <- as.numeric(vc$subject[1, 1])
          cv_inter_pct <- sqrt(mse_inter) * 100
          df_inter     <- n_subjects - nlevels(complete_data$seq)
        }, error = function(e) NULL)
      }

      cat(sprintf("  [DEBUG] About to create results list for %s\n", param))
      cat(sprintf("  [DEBUG] Variable checks before assignment:\n"))
      cat(sprintf("    pe_estimate: %s (exists: %s)\n", 
                 if(exists("pe_estimate")) paste(pe_estimate, collapse=",") else "MISSING", 
                 exists("pe_estimate")))
      cat(sprintf("    ci_lower: %s (exists: %s)\n", 
                 if(exists("ci_lower")) paste(ci_lower, collapse=",") else "MISSING", 
                 exists("ci_lower")))
      cat(sprintf("    ci_upper: %s (exists: %s)\n", 
                 if(exists("ci_upper")) paste(ci_upper, collapse=",") else "MISSING", 
                 exists("ci_upper")))
      cat(sprintf("    treatment_coef: %s (exists: %s)\n", 
                 if(exists("treatment_coef")) paste(treatment_coef, collapse=",") else "MISSING", 
                 exists("treatment_coef")))
      cat(sprintf("    comprehensive_anova: %s (exists: %s)\n", 
                 if(exists("comprehensive_anova")) "data.frame" else "MISSING", 
                 exists("comprehensive_anova")))
      cat(sprintf("    type3_ss: %s (exists: %s)\n", 
                 if(exists("type3_ss")) "data.frame" else "MISSING", 
                 exists("type3_ss")))
      
      # Store comprehensive results
      anova_results[[param]] <- list(
        parameter = param,
        model = model,
        anova = anova_table,                    # Type I SS (sequential)
        anova_comprehensive = comprehensive_anova,  # Model/Error/Corrected Total
        type3_ss = type3_ss,                    # Type III SS (marginal)
        summary = model_summary,
        aic = model_aic,
        n_observations = nrow(complete_data),
        residual_mse = residual_mse,
        residual_df = residual_df,
        r_squared = if(anova_model == "fixed") model_summary$r.squared else NA,
        anova_method = anova_model,
        random_effects = if(anova_model != "fixed") random_effects else NULL,
        pe_estimate = pe_estimate,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        treatment_coef = treatment_coef,
        param_mean = param_mean,
        root_mse = root_mse,
        cv_percent = cv_percent,
        cv_intra_pct = cv_intra_pct,
        cv_inter_pct = cv_inter_pct,
        mse_inter = mse_inter,
        df_inter = df_inter,
        f_inter = f_inter,
        p_inter = p_inter,
        n_subjects = n_subjects,
        treatment_se = treatment_se,
        treatment_pval = treatment_pval,
        subj_seq_analysis = subj_seq_analysis,
        lsmeans_result = lsmeans_result
      )
      
    }, error = function(e) {
      cat(sprintf("✗ Error fitting model for %s: %s\n", param, e$message))
      anova_results[[param]] <- list(
        parameter = param,
        error = e$message,
        anova_method = anova_model
      )
    })
  }
  
  cat(sprintf("[DEBUG] ✓ Simple ANOVA completed for %d parameters\n", length(anova_results)))
  return(anova_results)
}


