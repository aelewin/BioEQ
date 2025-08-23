# Interpretation Guides Module
# Provides detailed interpretation guidelines for bioequivalence results

#' Generate interpretation guide for PK parameters
#' 
#' @param parameter_name Name of the PK parameter
#' @return HTML content for interpretation
generate_pk_interpretation_guide <- function(parameter_name = NULL) {
  
  if (is.null(parameter_name)) {
    # General PK interpretation
    return(tagList(
      h5("📊 Pharmacokinetic Parameter Interpretation"),
      
      div(class = "interpretation-section",
        h6("AUC0t (Area Under the Curve from time 0 to last measurable concentration)"),
        p("• Represents the extent of drug absorption"),
        p("• Primary endpoint for bioequivalence assessment"),
        p("• Must fall within 80.00% - 125.00% for bioequivalence"),
        tags$br()
      ),
      
      div(class = "interpretation-section", 
        h6("Cmax (Maximum Observed Concentration)"),
        p("• Represents the rate of drug absorption"),
        p("• Primary endpoint for bioequivalence assessment"),
        p("• Must fall within 80.00% - 125.00% for bioequivalence"),
        tags$br()
      ),
      
      div(class = "interpretation-section",
        h6("AUC0inf (Area Under the Curve from time 0 to infinity)"),
        p("• Represents total drug exposure"),
        p("• Secondary endpoint when adequately extrapolated"),
        p("• Should be consistent with AUC0t findings"),
        tags$br()
      ),
      
      div(class = "interpretation-section",
        h6("Tmax (Time to Maximum Concentration)"),
        p("• Indicates absorption rate"),
        p("• Usually analyzed non-parametrically"),
        p("• Not typically a primary BE endpoint"),
        tags$br()
      ),
      
      div(class = "alert alert-info",
        icon("lightbulb"),
        strong(" Key Points:"),
        tags$ul(
          tags$li("90% confidence intervals must be entirely within 80.00% - 125.00%"),
          tags$li("Both AUC and Cmax must pass for bioequivalence"),
          tags$li("Lower CV% indicates less variability and more reliable results"),
          tags$li("Point estimates closer to 100% indicate similar formulations")
        )
      )
    ))
  }
  
  # Parameter-specific interpretation
  switch(parameter_name,
    "AUC0t" = generate_auc0t_interpretation(),
    "Cmax" = generate_cmax_interpretation(),
    "AUC0inf" = generate_aucinf_interpretation(),
    "Tmax" = generate_tmax_interpretation(),
    generate_pk_interpretation_guide()  # Default to general interpretation
  )
}

#' Generate ANOVA interpretation guide
#' 
#' @param anova_results ANOVA results (optional)
#' @return HTML content for ANOVA interpretation
generate_anova_interpretation_guide <- function(anova_results = NULL) {
  
  tagList(
    h5("📈 ANOVA Statistical Interpretation"),
    
    div(class = "interpretation-section",
      h6("Formulation Effect"),
      p("• Tests for differences between Test and Reference formulations"),
      p("• P > 0.05 supports bioequivalence (no significant difference)"),
      p("• This is the key test for bioequivalence assessment"),
      tags$br()
    ),
    
    div(class = "interpretation-section",
      h6("Subject Effect"),
      p("• Tests for between-subject variability"),
      p("• Usually significant (P < 0.05) in BE studies"),
      p("• Indicates individual differences in drug response"),
      tags$br()
    ),
    
    div(class = "interpretation-section",
      h6("Period Effect"),
      p("• Tests for time-related effects between study periods"),
      p("• Should be non-significant (P > 0.05) for valid crossover"),
      p("• Significant period effects may indicate carryover"),
      tags$br()
    ),
    
    div(class = "interpretation-section",
      h6("Sequence Effect"),
      p("• Tests for treatment sequence effects"),
      p("• Should be non-significant (P > 0.05)"),
      p("• Significant sequence effects may indicate carryover"),
      tags$br()
    ),
    
    div(class = "alert alert-warning",
      icon("exclamation-triangle"),
      strong(" Important Considerations:"),
      tags$ul(
        tags$li("The Formulation P-value is NOT the bioequivalence test"),
        tags$li("Bioequivalence is determined by the 90% confidence interval"),
        tags$li("ANOVA provides the variance estimates for CI calculation"),
        tags$li("Significant carryover effects may invalidate crossover analysis")
      )
    )
  )
}

#' Generate regulatory guidance interpretation
#' 
#' @param regulatory_standard Regulatory standard ("FDA", "EMA", "ICH")
#' @return HTML content for regulatory interpretation
generate_regulatory_interpretation <- function(regulatory_standard = "FDA") {
  
  switch(regulatory_standard,
    "FDA" = tagList(
      h5("🏛️ FDA Bioequivalence Guidance"),
      
      div(class = "interpretation-section",
        h6("Acceptance Criteria"),
        p("• 90% confidence interval for AUC and Cmax must be within 80.00% - 125.00%"),
        p("• Based on FDA Guidance for Industry: Bioequivalence Studies"),
        p("• Applies to immediate-release solid oral dosage forms"),
        tags$br()
      ),
      
      div(class = "interpretation-section", 
        h6("Statistical Requirements"),
        p("• ANOVA on log-transformed data (multiplicative model)"),
        p("• Mixed-effects model accounting for study design"),
        p("• Two one-sided tests (TOST) approach equivalent to 90% CI"),
        tags$br()
      ),
      
      div(class = "interpretation-section",
        h6("Study Design Recommendations"),
        p("• 2×2×2 crossover preferred for immediate-release products"),
        p("• Adequate washout period between treatments"),
        p("• Sample size to ensure 80% power"),
        tags$br()
      )
    ),
    
    "EMA" = tagList(
      h5("🇪🇺 EMA Bioequivalence Guideline"),
      
      div(class = "interpretation-section",
        h6("Acceptance Criteria"),
        p("• 90% confidence interval must be within 80.00% - 125.00%"),
        p("• Based on EMA Guideline on Bioequivalence (CHMP/EWP/QWP/1401/98)"),
        p("• Additional requirements for highly variable drugs"),
        tags$br()
      ),
      
      div(class = "interpretation-section",
        h6("Scaled Bioequivalence"),
        p("• May apply to highly variable drugs (CV > 30%)"),
        p("• Reference-scaled acceptance criteria"),
        p("• Point estimate constraint (90.00% - 111.11%)"),
        tags$br()
      )
    ),
    
    "ICH" = tagList(
      h5("🌐 ICH M13A Bioanalytical Method Validation"),
      
      div(class = "interpretation-section",
        h6("International Harmonization"),
        p("• Harmonized approach across ICH regions"),
        p("• Focus on bioanalytical method validation"),
        p("• Quality-by-design principles"),
        tags$br()
      ),
      
      div(class = "interpretation-section",
        h6("Method Performance"),
        p("• Accuracy and precision requirements"),
        p("• Stability validation across study duration"),
        p("• Matrix effect assessment"),
        tags$br()
      )
    ),
    
    # Default general interpretation
    generate_regulatory_interpretation("FDA")
  )
}

#' Generate bioequivalence conclusion interpretation
#' 
#' @param be_conclusions BE conclusion results
#' @return HTML content for BE conclusion interpretation
generate_be_conclusion_interpretation <- function(be_conclusions) {
  
  if (is.null(be_conclusions) || length(be_conclusions) == 0) {
    return(p("Bioequivalence conclusions not available for interpretation."))
  }
  
  # Calculate overall status
  all_pass <- all(sapply(be_conclusions, function(x) x$bioequivalent), na.rm = TRUE)
  any_pass <- any(sapply(be_conclusions, function(x) x$bioequivalent), na.rm = TRUE)
  
  conclusion_html <- tagList()
  
  if (all_pass) {
    conclusion_html <- tagList(
      conclusion_html,
      div(class = "alert alert-success",
        icon("check-circle"),
        h6("✅ BIOEQUIVALENT FORMULATIONS"),
        p("Both primary endpoints (AUC and Cmax) meet bioequivalence criteria."),
        p(strong("Regulatory Implication: "), "The test formulation can be considered bioequivalent to the reference formulation.")
      )
    )
  } else if (any_pass) {
    conclusion_html <- tagList(
      conclusion_html,
      div(class = "alert alert-warning",
        icon("exclamation-triangle"),
        h6("⚠️ PARTIAL BIOEQUIVALENCE"),
        p("Some parameters meet bioequivalence criteria, but not all primary endpoints."),
        p(strong("Regulatory Implication: "), "Additional investigation may be required.")
      )
    )
  } else {
    conclusion_html <- tagList(
      conclusion_html,
      div(class = "alert alert-danger",
        icon("times-circle"),
        h6("❌ NOT BIOEQUIVALENT"),
        p("Primary endpoints do not meet bioequivalence criteria."),
        p(strong("Regulatory Implication: "), "Formulation modifications may be necessary.")
      )
    )
  }
  
  # Add parameter-specific details
  param_details <- div(
    h6("Parameter-Specific Results:"),
    tags$ul(
      lapply(names(be_conclusions), function(param) {
        conclusion <- be_conclusions[[param]]
        status_text <- ifelse(conclusion$bioequivalent, "PASS", "FAIL")
        status_icon <- ifelse(conclusion$bioequivalent, "✅", "❌")
        
        tags$li(
          strong(param, ": "), status_icon, " ", status_text,
          sprintf(" (%.1f%%, CI: %.1f%% - %.1f%%)",
                 conclusion$point_estimate * 100,
                 conclusion$ci_lower * 100,
                 conclusion$ci_upper * 100)
        )
      })
    )
  )
  
  return(tagList(conclusion_html, param_details))
}

#' Generate study design interpretation
#' 
#' @param design Study design
#' @param n_subjects Number of subjects
#' @return HTML content for study design interpretation
generate_study_design_interpretation <- function(design, n_subjects) {
  
  design_info <- switch(design,
    "2x2x2" = list(
      name = "Two-Treatment, Two-Period, Two-Sequence Crossover",
      description = "Each subject receives both treatments in different periods",
      advantages = c(
        "Each subject serves as their own control",
        "Eliminates between-subject variability", 
        "Most efficient design for BE studies",
        "Recommended by regulatory agencies"
      ),
      considerations = c(
        "Requires adequate washout between periods",
        "Assumes no carryover effects",
        "Subjects must complete both periods"
      )
    ),
    "parallel" = list(
      name = "Parallel Group Design",
      description = "Subjects receive only one treatment",
      advantages = c(
        "No carryover effects possible",
        "Suitable for long half-life drugs",
        "Simple study conduct"
      ),
      considerations = c(
        "Requires larger sample sizes",
        "Between-subject variability affects precision",
        "Less efficient than crossover designs"
      )
    ),
    "replicate" = list(
      name = "Replicate Crossover Design",
      description = "Multiple administrations of reference treatment",
      advantages = c(
        "Allows assessment of within-subject variability",
        "Enables scaled bioequivalence for highly variable drugs",
        "More information per subject"
      ),
      considerations = c(
        "More complex study design",
        "Longer study duration",
        "Higher subject burden"
      )
    ),
    list(
      name = "Custom Study Design",
      description = "Non-standard design configuration",
      advantages = character(0),
      considerations = character(0)
    )
  )
  
  # Sample size assessment
  sample_size_assessment <- if (!is.null(n_subjects) && is.numeric(n_subjects)) {
    if (design == "2x2x2" && n_subjects >= 12) {
      "Adequate sample size for crossover design"
    } else if (design == "parallel" && n_subjects >= 24) {
      "Adequate sample size for parallel design"
    } else {
      "Sample size may be limited for reliable conclusions"
    }
  } else {
    "Sample size information not available"
  }
  
  tagList(
    h6("Study Design: ", design_info$name),
    p(design_info$description),
    
    if (length(design_info$advantages) > 0) {
      div(
        h6("Advantages:"),
        tags$ul(lapply(design_info$advantages, tags$li))
      )
    },
    
    if (length(design_info$considerations) > 0) {
      div(
        h6("Considerations:"),
        tags$ul(lapply(design_info$considerations, tags$li))
      )
    },
    
    div(class = "alert alert-info",
      icon("info-circle"),
      strong("Sample Size Assessment: "), sample_size_assessment
    )
  )
}

# Helper functions for specific parameter interpretations
generate_auc0t_interpretation <- function() {
  tagList(
    h6("AUC0t - Area Under the Curve (0 to last measurable time)"),
    p("Primary measure of extent of absorption and bioavailability."),
    div(class = "interpretation-details",
      strong("Clinical Significance: "),
      p("Represents the total amount of drug that reaches systemic circulation."),
      strong("Regulatory Importance: "),
      p("Must demonstrate bioequivalence for generic drug approval."),
      strong("Interpretation: "),
      p("Point estimate near 100% indicates similar bioavailability.")
    )
  )
}

generate_cmax_interpretation <- function() {
  tagList(
    h6("Cmax - Maximum Observed Concentration"),
    p("Primary measure of rate of absorption."),
    div(class = "interpretation-details",
      strong("Clinical Significance: "),
      p("Indicates how quickly and how much drug reaches peak levels."),
      strong("Safety Relevance: "),
      p("Higher Cmax may be associated with increased side effects."),
      strong("Interpretation: "),
      p("Point estimate near 100% indicates similar absorption profiles.")
    )
  )
}

generate_aucinf_interpretation <- function() {
  tagList(
    h6("AUC0inf - Area Under the Curve (0 to infinity)"),
    p("Total drug exposure extrapolated to infinity."),
    div(class = "interpretation-details",
      strong("Clinical Significance: "),
      p("Complete measure of drug exposure including terminal elimination."),
      strong("Quality Considerations: "),
      p("Should be well-determined (extrapolated portion < 20%)."),
      strong("Interpretation: "),
      p("Should be consistent with AUC0t findings.")
    )
  )
}

generate_tmax_interpretation <- function() {
  tagList(
    h6("Tmax - Time to Maximum Concentration"),
    p("Time when peak concentration is reached."),
    div(class = "interpretation-details",
      strong("Clinical Significance: "),
      p("Indicates absorption rate characteristics."),
      strong("Statistical Analysis: "),
      p("Typically analyzed using non-parametric methods."),
      strong("Interpretation: "),
      p("Usually not a primary endpoint for bioequivalence.")
    )
  )
}
