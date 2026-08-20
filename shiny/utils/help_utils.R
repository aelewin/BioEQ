# Help and Tooltip Utilities
# Functions to create consistent help icons and modals throughout the application

# Create a help icon with tooltip
help_icon <- function(id, tooltip_text, modal_title = NULL, modal_content = NULL) {
  ns <- NS(id)
  
  if (!is.null(modal_content)) {
    # Create help icon with modal
    tags$span(
      icon("question-circle"),
      id = paste0("help_", id),
      class = "help-icon",
      style = "color: #007bff; cursor: pointer; margin-left: 5px;",
      title = tooltip_text,
      `data-toggle` = "tooltip",
      `data-placement` = "top",
      onclick = paste0("Shiny.setInputValue('", id, "_help', Math.random())")
    )
  } else {
    # Simple tooltip only
    tags$span(
      icon("question-circle"),
      class = "help-icon",
      style = "color: #007bff; cursor: help; margin-left: 5px;",
      title = tooltip_text,
      `data-toggle` = "tooltip",
      `data-placement` = "top"
    )
  }
}

# Create help modal content
create_help_modal <- function(session, input, id, title, content) {
  observeEvent(input[[paste0(id, "_help")]], {
    showModal(modalDialog(
      title = title,
      content,
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

# Standard help texts for common fields
help_texts <- list(
  study_design = list(
    tooltip = "Study design determines the statistical model and analysis approach",
    title = "Study Design Selection",
    content = div(
      p("The study design determines how data is analyzed and which statistical model is applied:"),
      tags$ul(
        tags$li(strong("2×2×2 Crossover:"), " Standard two-period, two-treatment crossover"),
        tags$li(strong("Replicate Designs:"), " Multiple administrations of reference and/or test"),
        tags$li(strong("Parallel Group:"), " Single administration, different subjects per treatment"),
        tags$li(strong("Auto-detect:"), " Automatically determines design from uploaded data")
      )
    )
  ),
  
  auc_method = list(
    tooltip = "Method for calculating area under the concentration-time curve",
    title = "AUC Calculation Methods",
    content = div(
      tags$ul(
        tags$li(strong("Linear up/Log down:"), " Uses linear trapezoidal rule when concentration is increasing (C₂ ≥ C₁) and log trapezoidal rule when concentration is decreasing (C₂ < C₁). FDA-preferred method."),
        tags$li(strong("Linear:"), " Uses linear trapezoidal rule throughout, regardless of whether concentrations are increasing or decreasing. EMA-preferred for regulatory submissions."),
        tags$li(strong("Log:"), " Uses log trapezoidal rule for all consecutive data points where both concentrations are positive. Cannot be used when C₁ or C₂ equals zero."),
        tags$li(strong("Linear/Log:"), " Uses linear rule up to Cmax (absorption phase), then log rule after Cmax (elimination phase). Suitable for drugs with first-order kinetics.")
      )
    )
  ),
  
  lambda_z = list(
    tooltip = "Method for determining terminal elimination rate constant",
    title = "Lambda_z Estimation Methods",
    content = div(
      p("Lambda_z (λz) is the terminal elimination rate constant, crucial for calculating AUC₀-∞ and half-life. Different methods exist for selecting which data points to use in the log-linear regression:"),
      tags$ul(
        tags$li(
          tags$strong("Manual (Fixed points):"), 
          " User specifies the exact number of terminal points to include in the regression. Allows for consistent application across all subjects when the terminal phase is well-defined. Simple and transparent approach."
        ),
        tags$li(
          tags$strong("ARS (Adjusted R-squared):"), 
          " Automatically selects points by maximizing the adjusted R² of the log-linear regression. Starting from the last 3 non-zero concentrations, it iteratively adds earlier points as long as the adjusted R² improves. Balances goodness-of-fit with the number of points included."
        ),
        tags$li(
          tags$strong("AIC (Akaike Information Criterion):"), 
          " Uses information theory to select the optimal number of points. Minimizes AIC = n × ln(RSS/n) + 2k, where RSS is residual sum of squares, n is number of points, and k is number of parameters. Can be useful but may not always select the most pharmacologically relevant points."
        ),
        tags$li(
          tags$strong("TTT (Two-Times-Tmax):"), 
          " Includes all concentration points from 2×Tmax onwards in the regression. Based on the assumption that absorption is negligible after twice the time of maximum concentration. Simple rule but may include too many or too few points depending on the drug's kinetics."
        )
      ),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("Important Considerations:"), 
        tags$ul(style = "margin-bottom: 0; margin-top: 5px;",
          tags$li("Cmax/Tmax points should be excluded from terminal phase selection"),
          tags$li("Selection should be done blinded to treatment assignment"),
          tags$li("Apply the same method consistently across all subjects"),
          tags$li("Minimum of 3 points required for reliable estimation"),
          tags$li("Regression should be statistically significant (p ≤ 0.05)")
        )
      )
    )
  ),
  
  missing_data = list(
    tooltip = "Strategy for handling missing concentration data in NCA analysis",
    title = "Missing Data Handling for NCA Analysis",
    content = div(
      p("For Non-Compartmental Analysis, missing concentration measurements can significantly impact pharmacokinetic parameter estimation. Choose an appropriate imputation strategy:"),
      tags$ul(
        tags$li(
          tags$strong("Last observation carried forward (LOCF):"), 
          " Replace missing values with the last observed concentration for that subject. Simple approach that preserves the subject in analysis. May introduce bias if concentrations are declining rapidly, but maintains conservative estimates for terminal phase parameters."
        ),
        tags$li(
          tags$strong("Linear interpolation:"), 
          " Estimate missing values using linear interpolation between adjacent time points. Most appropriate for isolated missing points in the absorption or distribution phases. Assumes linear concentration changes between time points."
        ),
        tags$li(
          tags$strong("Multiple imputation:"), 
          " Generate multiple plausible values for missing data using statistical modeling that accounts for the pharmacokinetic profile. Most sophisticated approach that incorporates uncertainty in imputed values. Recommended when missing data is substantial (>10%)."
        )
      ),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 4px;",
        tags$strong("NCA Considerations:"), 
        " For NCA analysis, preserving the concentration-time profile is critical for accurate parameter estimation. Complete case deletion is not recommended as it can eliminate entire subjects and reduce statistical power. The chosen method should be pre-specified and justified based on the missing data pattern."
      ),
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("Recommendation:"), 
        " Linear interpolation is generally preferred for isolated missing points. Use LOCF for terminal phase missingness. Consider multiple imputation when >10% of concentration data is missing."
      )
    )
  ),
  
  carryover_effect = list(
    tooltip = "Assessment of carryover effects in crossover bioequivalence studies",
    title = "Carryover Assessment (ICH M13A)",
    content = div(
      p("According to ICH M13A guidelines, carryover in crossover studies should be assessed through examination of pre-treatment plasma concentrations rather than statistical testing:"),
      tags$ul(
        tags$li(
          tags$strong("ICH M13A Position:"), 
          " A statistical test for carryover is not considered relevant and no decisions regarding the analysis (e.g., analysis of the first period only) should be made based on such a test."
        ),
        tags$li(
          tags$strong("Assessment Method:"), 
          " Carryover potential should be directly addressed by examination of pre-treatment plasma concentrations in period 2 and beyond (e.g., period 3 in a 3-period study)."
        ),
        tags$li(
          tags$strong("Decision Criteria:"), 
          " In single-dose studies, if pre-dose concentration is greater than 5% of the Cmax value for the subject in that period, the data from that period should be excluded from the primary statistical analysis."
        ),
        tags$li(
          tags$strong("Impact on Analysis:"), 
          " Exclusion of a period may result in the exclusion of the entire subject from the bioequivalence analysis, depending on study design requirements."
        )
      ),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("Implementation:"), 
        " This approach focuses on objective measurement rather than statistical inference, providing a more direct assessment of potential carryover effects based on actual drug concentrations."
      )
    )
  ),
  
  pk_parameters = list(
    tooltip = "Pharmacokinetic parameters for bioequivalence assessment per ICH M13A",
    title = "PK Parameters for BE Assessment",
    content = div(
      p("ICH M13A defines specific pharmacokinetic parameters for bioequivalence analysis:"),
      
      div(style = "margin-bottom: 15px;",
        h6(tags$strong("Primary Parameters for BE Analysis:"), style = "color: #2c3e50; margin-bottom: 8px;"),
        tags$ul(style = "margin-left: 15px;",
          tags$li(tags$strong("AUC(0-t):"), " Area under the curve from time zero to the last measurable concentration"),
          tags$li(tags$strong("Cmax:"), " Maximum observed concentration")
        )
      ),
      
      div(style = "margin-bottom: 15px;",
        h6(tags$strong("Additional Parameters for Study Acceptability:"), style = "color: #2c3e50; margin-bottom: 8px;"),
        tags$ul(style = "margin-left: 15px;",
          tags$li(tags$strong("AUC(0-inf):"), " Area under the curve extrapolated to infinity"),
          tags$li(tags$strong("AUC(0-t)/AUC(0-inf):"), " Percentage of AUC captured by AUC(0-t)"),
          tags$li(tags$strong("Tmax:"), " Time to maximum concentration"),
          tags$li(tags$strong("kel:"), " Elimination rate constant"),
          tags$li(tags$strong("t1/2:"), " Elimination half-life"),
          tags$li(tags$strong("pAUC:"), " Partial AUC for early exposure assessment when clinically relevant")
        )
      ),
      
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 3px solid #f39c12; border-radius: 4px;",
        tags$strong("Study Validity Requirements:"),
        tags$ul(style = "margin: 5px 0 0 15px;",
          tags$li("AUC(0-t) should cover at least 80% of AUC(0-inf) for single-dose studies"),
          tags$li("If AUC(0-t)/AUC(0-inf) < 80% in >20% of observations, study validity requires discussion")
        )
      ),
      
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("Long Half-life Drugs:"),
        " For drugs with long half-lives, AUC0-t automatically reflects the last sampling time (e.g., AUC0-72h if sampling ends at 72 hours). AUC0-inf may not be reliably estimated and can be excluded from BE assessment."
      ),
      
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #f0f8e8; border-left: 3px solid #28a745; border-radius: 4px;",
        tags$strong("Early Exposure Assessment:"),
        " When early onset of action is clinically relevant, additional parameters may be needed, such as pAUC and Tmax."
      )
    )
  ),
  
  confidence_level = list(
    tooltip = "Confidence level for bioequivalence assessment (regulatory standard: 90%)",
    title = "Confidence Level Selection",
    content = div(
      p("Statistical confidence level for bioequivalence assessment:"),
      tags$ul(
        tags$li(strong("90%:"), " FDA and EMA regulatory standard"),
        tags$li(strong("95%:"), " Higher confidence, more conservative"),
        tags$li("Corresponds to α = 0.05 for two one-sided tests (TOST)")
      )
    )
  ),
  
  alpha_level_abe = list(
    tooltip = "One-sided significance level for the TOST procedure",
    title = "Alpha Level (α) for ABE",
    content = div(
      p("Alpha (α) is the", tags$strong("one-sided"), "significance level used in the Two One-Sided Tests (TOST) procedure for bioequivalence:"),
      tags$ul(
        tags$li(tags$strong("α = 0.05"), " → 90% confidence interval (regulatory standard)"),
        tags$li(tags$strong("α = 0.025"), " → 95% confidence interval (more conservative)"),
        tags$li(tags$strong("Formula:"), " CI% = 100 × (1 - 2α)")
      ),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("TOST Procedure:"), br(),
        "The TOST procedure uses", tags$strong("two"), "one-sided tests:", br(),
        "• H₁: μT/μR > lower limit", br(),
        "• H₂: μT/μR < upper limit", br(),
        "Each test is performed at significance level α (one-sided).", br(),
        "Together they create a (1-2α) confidence interval."
      )
    )
  ),
  
  alpha_level_sabe = list(
    tooltip = "One-sided significance level for replicate BE analysis",
    title = "Alpha Level (α) for Scaled BE",
    content = div(
      p("For replicate BE studies (ABEL/RSABE), alpha (α) represents the", tags$strong("one-sided"), 
        "Type I Error probability:"),
      tags$ul(
        tags$li(tags$strong("α = 0.05"), " → 90% confidence interval (regulatory standard)"),
        tags$li(tags$strong("α = 0.025"), " → 95% confidence interval"),
        tags$li(tags$strong("Formula:"), " CI% = 100 × (1 - 2α)")
      ),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 4px;",
        tags$strong(icon("info-circle"), " Important for Replicate Studies:"), br(),
        "In ABEL/RSABE, the confidence interval is based on the within-subject variability of the reference (s", 
        tags$sub("wR"), "). The one-sided alpha remains 0.05 for a 90% CI, but the ", 
        tags$strong("acceptance limits expand"), " based on CV", tags$sub("wR"), "."
      ),
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("replicateBE Package:"), br(),
        "The replicateBE package uses α as the one-sided significance level. ",
        "Setting α = 0.05 will produce a 90% CI using the formula 100(1-2α) = 90%."
      )
    )
  ),
  
  abel_method = list(
    tooltip = "Information about ABEL method selection",
    title = "ABEL Method Selection",
    content = div(
      p("The ANOVA Model selection above determines which replicateBE method will be used for ABEL analysis:"),
      tags$ul(
        tags$li(tags$strong("Fixed Effects"), " → Method A (Linear Model/ANOVA)"),
        tags$li(tags$strong("Mixed Effects - nlme"), " → Method B (SAS default DF approximation)"),
        tags$li(tags$strong("Mixed Effects - Satterthwaite"), " → Method B (Satterthwaite DF)"),
        tags$li(tags$strong("Mixed Effects - Kenward-Roger"), " → Method B (Kenward-Roger DF)")
      ),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("Method A vs Method B:"), br(),
        tags$strong("Method A:"), " Linear model (lm), faster, suitable for balanced designs", br(),
        tags$strong("Method B:"), " Mixed-effects model (lme), handles unbalanced designs, allows different DF approximations"
      )
    )
  ),
  
  abel_upper_cap = list(
    tooltip = "Select the regulatory approach for ABEL",
    title = "ABEL Regulatory Approaches",
    content = div(
      p("Different regulatory agencies use different approaches for Average Bioequivalence with Expanding Limits."),

      tags$h6(tags$strong("EMA (European Medicines Agency)"), style = "margin-top: 15px;"),
      tags$ul(
        tags$li(tags$strong("Scaling with 50% cap")),
        tags$li("Limits expand based on reference variability (CV", tags$sub("wR"), ")"),
        tags$li("Maximum expanded limits: 69.84% - 143.19%"),
        tags$li("Cap reached when CV", tags$sub("wR"), " ≈ 50%")
      ),
      
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #e7f3ff; border-left: 4px solid #2196F3; border-radius: 4px;",
        tags$strong(icon("calculator"), " EMA Scaling Formula:"), br(),
        "Expanded limits = 100 × exp(±k × s", tags$sub("wR"), ")", br(),
        "where s", tags$sub("wR"), " = √(ln(CV", tags$sub("wR"), tags$sup("2"), " + 1))", br(),
        "and k = 0.76 (regulatory constant)"
      ),
      
      tags$h6(tags$strong("GCC (Gulf Cooperation Council, pre-2022)"), style = "margin-top: 15px;"),
      tags$ul(
        tags$li(tags$strong("Fixed widened limits: 75.00% - 133.33%")),
        tags$li("Limits do NOT scale with CV", tags$sub("wR")),
        tags$li("Applied when CV", tags$sub("wR"), " > 30%"),
        tags$li("If CV", tags$sub("wR"), " ≤ 30%, standard 80-125% limits apply")
      ),

      tags$h6(tags$strong("Health Canada"), style = "margin-top: 15px;"),
      tags$ul(
        tags$li(tags$strong("Scaling with 57.4% cap")),
        tags$li("Limits expand based on reference variability (CV", tags$sub("wR"), "), same formula as EMA"),
        tags$li("Maximum expanded limits: 66.7% - 150.0%"),
        tags$li("Cap reached when CV", tags$sub("wR"), " ≈ 57.4%")
      ),

      div(
        style = "margin-top: 10px; padding: 10px; background-color: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 4px;",
        tags$strong(icon("calculator"), " Health Canada Scaling Formula:"), br(),
        "Expanded limits = 100 × exp(±k × s", tags$sub("wR"), ")", br(),
        "where s", tags$sub("wR"), " = √(ln(CV", tags$sub("wR"), tags$sup("2"), " + 1))", br(),
        "and k = 0.76 (same regulatory constant as EMA, wider cap)"
      )
    )
  ),
  
  be_analysis_type = list(
    tooltip = "Select the type of bioequivalence analysis approach",
    title = "BE Analysis Types",
    content = div(
      h4("Average Bioequivalence (ABE)"),
      p("Standard BE assessment with fixed limits (typically 80-125%). Used for:"),
      tags$ul(
        tags$li("Drugs with normal variability (CV < 30%)"),
        tags$li("Standard 2×2×2 crossover studies"),
        tags$li("Parallel group studies"),
        tags$li("Most conventional pharmaceutical products")
      ),
      
      h4("Reference-Scaled Average BE (RSABE)", style = "margin-top: 20px;"),
      p("Approach for highly variable drugs (HVDs) with CV ≥ 30%. Features:"),
      tags$ul(
        tags$li("BE limits scale with reference formulation variability"),
        tags$li("Point estimate constraint (80-125%) still required"),
        tags$li("Uses within-subject variability of reference (s", tags$sub("WR"), ")"),
        tags$li("Applied when s", tags$sub("WR"), " > 0.294 (CV ≈ 30%)")
      ),
      div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
        strong("RSABE Formula:"), br(),
        "Upper limit = exp(+0.893 × s", tags$sub("WR"), ")", br(),
        "Lower limit = exp(-0.893 × s", tags$sub("WR"), ")"
      ),
      
      h4("Average BE with Expanding Limits (ABEL)", style = "margin-top: 20px;"),
      p("Alternative approach for HVDs with expanding acceptance limits. Features:"),
      tags$ul(
        tags$li("Widened limits based on CV (up to 69.84%-143.19%)"),
        tags$li("Geometric mean ratio (GMR) constraint (80-125%)"),
        tags$li("Different scaling formula than RSABE"),
        tags$li("Applied when CV > 30% for Cmax")
      ),
      div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
        strong("ABEL Formula:"), br(),
        "Upper/Lower limits = exp(±0.76 × s", tags$sub("WR"), ")", br(),
        "Maximum expansion: 69.84% - 143.19%"
      ),
      
      div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-top: 20px;",
        h5(icon("info-circle"), " Implementation Status"),
        p("Currently, only ABE is fully implemented. RSABE and ABEL selection will use ABE methodology with appropriate notifications until full implementation is complete.")
      )
    )
  ),
  
  be_limits = list(
    tooltip = "Bioequivalence acceptance limits (regulatory standard: 80.00% - 125.00%)",
    title = "Bioequivalence Limits",
    content = div(
      p("Acceptance criteria for bioequivalence:"),
      tags$ul(
        tags$li(strong("80.00% - 125.00%:"), " Standard FDA/EMA limits"),
        tags$li(strong("Narrow therapeutic index:"), " May require tighter limits (90.00% - 111.11%)"),
        tags$li("90% CI for geometric mean ratio must fall within these limits")
      )
    )
  ),
  
  analysis_model = list(
    tooltip = "Statistical model for ANOVA analysis including primary parameters and calculations",
    title = "ANOVA Analysis Model",
    content = div(
      p("The ANOVA analysis includes comprehensive bioequivalence evaluation with the following components:"),
      tags$h6("Analysis Includes", style = "margin-top: 15px; margin-bottom: 10px; font-weight: bold;"),
      tags$ul(
        tags$li("Primary PK parameters: Cmax, AUC0-t, AUC0-inf"),
        tags$li("Log-transformed analysis for each parameter"),
        tags$li("Type I and Type III Sum of Squares analysis"),
        tags$li("Intra- and inter-subject CV calculations"),
        tags$li("90% confidence intervals for T/R ratios"),
        tags$li("Point estimates and geometric means")
      ),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("Model Selection:"), 
        " Fixed Effects Model uses conventional ANOVA approach. Mixed Effects Model (REML) accounts for random subject effects and is recommended for crossover designs."
      )
    )
  ),
  
  reference_scaling = list(
    tooltip = "Use reference-scaled bioequivalence for highly variable drugs",
    title = "Reference-Scaled Bioequivalence",
    content = div(
      p("Alternative approach for highly variable drugs (CV > 30%):"),
      tags$ul(
        tags$li("Allows wider acceptance limits based on reference variability"),
        tags$li("FDA: Cmax only, requires replicate design"),
        tags$li("EMA: AUC and Cmax, requires replicate design"),
        tags$li("Must demonstrate comparable variability")
      )
    )
  ),
  
  rsabe_method = list(
    tooltip = "Select the RSABE statistical method",
    title = "RSABE Statistical Methods",
    content = div(
      tags$p(tags$em("U.S. Food and Drug Administration (FDA), Center for Drug Evaluation and Research (CDER). "),
             tags$em("Statistical Approaches to Establishing Bioequivalence. Guidance for Industry."),
             " December 2022.",
             style = "font-size: 0.85em; color: #555;"),
      tags$h6(tags$strong("FDA Linearized Scaled Criterion (Howe UCB)"), style = "margin-top: 10px;"),
      tags$p("Linearizes the scaled BE criterion \u03B7 = d\u00B2 \u2212 \u03B8\u00B2\u209B\u00B7s\u00B2wR and tests whether its ",
             "95% upper confidence bound (UCB) \u2264 0, using Howe\u2019s method for combining ",
             "independent confidence intervals (Eq. 31 in T\u00F3thfalusi & Endr\u00E9nyi 2016). ",
             "This is the default FDA-recommended method for highly variable drugs and drug products (HVDs/HVDPs)."),
      tags$p(tags$em("Howe WG (1974). Approximate Confidence Limits on the Mean of X + Y ",
                     "Where X and Y Are Two Tabled Independent Random Variables. "),
             tags$em("Journal of the American Statistical Association"), ", 69, 789\u2013794.",
             style = "font-size: 0.85em; color: #555;"),
      tags$hr(style = "margin: 10px 0;"),
      tags$h6(tags$strong("Non-Central TOST (ncTOST) \u2014 Exact Method"), style = "margin-top: 10px;"),
      tags$p("Uses the noncentral t distribution directly to perform two one-sided tests ",
             "against the scaled limits \u00B1\u03B8\u209B\u00B7\u03C3", tags$sub("wR"),
             ". Computes the pivotal index d = d\u0302/s", tags$sub("wR"),
             " (Glass\u2019s effect size), applies Hedges\u2019 bias correction c", tags$sub("r"),
             "(df), and evaluates p-values from the noncentral t CDF with ",
             "design-dependent constant K and noncentrality parameter \u00B1\u03B8/K."),
      tags$p(tags$em("T\u00F3thfalusi L, Endr\u00E9nyi L (2016). An Exact Procedure for the ",
                     "Evaluation of Reference-Scaled Average Bioequivalence. "),
             tags$em("The AAPS Journal"), ", 18(2), 476\u2013489. DOI: 10.1208/s12248-016-9873-6.",
             style = "font-size: 0.85em; color: #555;"),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("Common to both methods:"),
        tags$ul(style = "margin-bottom: 0; margin-top: 5px;",
          tags$li("Scaling constant \u03B8\u209B = ln(1.25)/\u03C3\u2080 \u2248 0.8924 (\u03C3\u2080 = 0.25, the regulatory criterion constant)"),
          tags$li("Switching threshold: s", tags$sub("wR"), " \u2265 0.294 (CV", tags$sub("wR"), " \u2248 30%) triggers reference-scaling \u2014 distinct from \u03C3\u2080 above"),
          tags$li("FDA point estimate constraint: 80\u2013125%")
        )
      )
    )
  ),

  welch_correction = list(
    tooltip = "Statistical method for t-tests in parallel group bioequivalence studies",
    title = "Welch Correction for Parallel Designs",
    content = div(
      p("For parallel group bioequivalence studies, two statistical approaches are available for comparing treatment groups:"),
      tags$ul(
        tags$li(
          tags$strong("Welch correction (unequal variances) - Recommended:"), 
          " Performs Welch's t-test which does not assume equal variances between treatment groups. This is more robust and appropriate for most bioequivalence studies as it accounts for potential differences in variability between test and reference treatments."
        ),
        tags$li(
          tags$strong("Equal variances assumption:"), 
          " Performs Student's t-test assuming equal variances between groups. This method requires the assumption that both treatment groups have identical variability, which may not hold in practice."
        )
      ),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 4px;",
        tags$strong("Regulatory Recommendation:"), 
        " The Welch correction is widely accepted and recommended by regulatory agencies for parallel group bioequivalence studies. It provides more conservative and reliable results without requiring the equal variances assumption."
      ),
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #d5f4e6; border-left: 3px solid #27ae60; border-radius: 4px;",
        tags$strong("Default Setting:"), 
        " This application defaults to using the Welch correction for all parallel group analyses, consistent with current best practices and regulatory expectations."
      )
    )
  )
)

# Initialize tooltips JavaScript
tooltip_js <- "
$(document).ready(function(){
  // Initialize Bootstrap tooltips
  $('[data-toggle=\"tooltip\"]').tooltip({
    container: 'body',
    html: true
  });
  
  // Re-initialize tooltips after content updates
  $(document).on('shiny:bound', function() {
    $('[data-toggle=\"tooltip\"]').tooltip({
      container: 'body',
      html: true
    });
  });
  
  // Handle dynamic content
  $(document).on('shiny:value', function() {
    setTimeout(function() {
      $('[data-toggle=\"tooltip\"]').tooltip({
        container: 'body',
        html: true
      });
    }, 100);
  });
});
"
