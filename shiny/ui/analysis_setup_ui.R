# Analysis Setup UI
# This file contains the analysis configuration interface

# Source help utilities
source("utils/help_utils.R", local = TRUE)

tagList(
  # ---- Header --------------------------------------------------------
  div(
    class = "setup-header",
    style = "padding: 12px 18px; margin-bottom: 14px; background: linear-gradient(135deg, #1e3a5f 0%, #2c5282 100%); border-radius: 8px; color: white;",
    h3(icon("cogs"), " Analysis Setup",
       style = "margin: 0; font-weight: 700;"),
    p("Configure study design, PK parameters, statistical method, and analysis options.",
      style = "margin: 4px 0 0 0; font-size: 13px; color: #e2e8f0;")
  ),

  fluidRow(
    # Main configuration panel
    column(
      width = 12,
      
      # Study design detection
      box(
        title = NULL,
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        
        fluidRow(
          # Green detection box - left side when detected
          conditionalPanel(
            condition = "output.study_design_detected",
            column(6,
              div(
                style = "background-color: #d5f4e6; padding: 15px; border-radius: 8px; height: 100%;",
                h4(icon("check-circle", style = "color: #27ae60;"), " Study Design Detected", 
                   style = "color: #27ae60; margin-top: 0;"),
                uiOutput("detected_design")
              )
            )
          ),
          
          # Main configuration - adaptive width
          conditionalPanel(
            condition = "output.study_design_detected",
            column(6,
              h5("Study Design", 
                 help_icon("study_design", help_texts$study_design$tooltip, 
                          help_texts$study_design$title, help_texts$study_design$content)
              ),
              selectInput(
                "study_design",
                label = NULL,
                choices = list(
                  "2×2×2 Crossover" = "2x2x2",
                  "2×2×3 Replicate" = "2x2x3",
                  "2×2×4 Replicate" = "2x2x4", 
                  "Parallel Group" = "parallel",
                  "Auto-detect" = "auto"
                ),
                selected = "auto"
              ),
              conditionalPanel(
                condition = "input.study_design == 'auto'",
                div(style = "color: #6c757d; font-style: italic; margin-top: 10px;",
                  icon("info-circle"), " Design will be automatically detected from data"
                )
              )
            )
          ),
          
          # Full width when no detection
          conditionalPanel(
            condition = "!output.study_design_detected",
            column(12,
              h5("Study Design", 
                 help_icon("study_design", help_texts$study_design$tooltip, 
                          help_texts$study_design$title, help_texts$study_design$content)
              ),
              selectInput(
                "study_design",
                label = NULL,
                choices = list(
                  "2×2×2 Crossover" = "2x2x2",
                  "2×2×3 Replicate" = "2x2x3",
                  "2×2×4 Replicate" = "2x2x4", 
                  "Parallel Group" = "parallel",
                  "Auto-detect" = "auto"
                ),
                selected = "auto"
              ),
              conditionalPanel(
                condition = "input.study_design == 'auto'",
                div(style = "color: #6c757d; font-style: italic; margin-top: 10px;",
                  icon("info-circle"), " Design will be automatically detected from data"
                )
              )
            )
          )
        )
      ), # Close Study Design box
      
      div(style = "clear: both; width: 100%;"),
      
      # Analysis parameters - 3-step structure based on data type
      # Step 1: NCA Analysis Setup (only for concentration data)
      conditionalPanel(
          condition = "input.data_type == 'concentration'",
          div(
            style = "border: 2px solid #3498db; border-radius: 10px; padding: 20px; margin-bottom: 20px; background-color: #f8f9ff;",
            h4(icon("flask"), " Step 1: NCA Analysis Setup", style = "color: #3498db; margin-top: 0;"),
            
            fluidRow(
              column(6,
                h5(tags$strong("AUC Calculation Method"),
                   help_icon("auc_method", help_texts$auc_method$tooltip, 
                            help_texts$auc_method$title, help_texts$auc_method$content)
                ),
                selectInput(
                  "auc_method",
                  label = NULL,
                  choices = list(
                    "Linear-up/Log-down (Mixed)" = "mixed",
                    "Linear trapezoidal" = "linear"
                  ),
                  selected = "mixed"
                ),

                h5(tags$strong("Lambda_z Estimation"),
                   help_icon("lambda_z", help_texts$lambda_z$tooltip, 
                            help_texts$lambda_z$title, help_texts$lambda_z$content)
                ),
                selectInput(
                  "lambda_z_method",
                  label = NULL,
                  choices = list(
                    "Manual (Fixed points)" = "manual",
                    "ARS (Adjusted R-squared)" = "ars",
                    "TTT (Two-Times-Tmax)" = "ttt"
                  ),
                  selected = "ttt"
                ),
                
                # Conditional input for manual point selection
                conditionalPanel(
                  condition = "input.lambda_z_method == 'manual'",
                  div(style = "margin-top: 10px; margin-left: 20px; padding-left: 15px; border-left: 2px solid #dee2e6;",
                    h6("Number of points to include:", style = "font-weight: normal;"),
                    numericInput(
                      "lambda_z_points",
                      label = NULL,
                      value = 3,
                      min = 3,
                      max = 10,
                      step = 1
                    ),
                    helpText("Minimum 3 points required for reliable estimation")
                  )
                ),

                h5(tags$strong("Optional PK Parameters")),
                
                div(style = "margin-left: 20px;",
                  # pAUC Configuration
                  div(style = "margin-bottom: 15px;",
                    checkboxInput(
                      "calculate_pAUC",
                      "Calculate pAUC (Partial AUC for early exposure)",
                      value = FALSE
                    ),
                    
                    conditionalPanel(
                      condition = "input.calculate_pAUC",
                      div(style = "margin-top: 10px; margin-left: 20px; padding-left: 15px; border-left: 2px solid #dee2e6;",
                        h6("pAUC Time Points Configuration:", style = "font-weight: normal;"),
                        
                        fluidRow(
                          column(6,
                            numericInput(
                              "pAUC_start",
                              "Start time (h):",
                              value = 0,
                              min = 0,
                              step = 0.25
                            )
                          ),
                          column(6,
                            numericInput(
                              "pAUC_end", 
                              "End time (h):",
                              value = 2,
                              min = 0.25,
                              step = 0.25
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              column(6,
                h5(tags$strong("Missing Data Handling")),
                tags$label("Middle Points", style = "font-weight: 600; font-size: 13px; color: #4a5568;"),
                selectInput(
                  "missing_data_middle",
                  label = NULL,
                  choices = list(
                    "Exclude point" = "complete",
                    "Linear interpolation" = "interpolate",
                    "Last observation carried forward" = "locf"
                  ),
                  selected = "complete"
                ),
                tags$label("Terminal Points", style = "font-weight: 600; font-size: 13px; color: #4a5568;"),
                selectInput(
                  "missing_data_terminal",
                  label = NULL,
                  choices = list(
                    "Exclude point" = "complete",
                    "Last observation carried forward" = "locf"
                  ),
                  selected = "complete"
                ),
                helpText("BLQ values are always set to 0."),
                
                # Carryover Detection section
                div(
                  # Add conditional styling for parallel designs
                  style = "transition: all 0.3s ease;",
                  conditionalPanel(
                    condition = "output.is_parallel_design == true",
                    div(style = "opacity: 0.4; pointer-events: none; background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px dashed #dee2e6;",
                      h5(tags$strong("Carryover Detection (ICH M13A)"),
                         tags$span(" (Not available for parallel studies)", style = "color: #6c757d; font-weight: normal; font-size: 0.9em;")
                      ),
                      
                      div(style = "margin-left: 20px;",
                        checkboxInput(
                          "test_carryover",
                          "Perform carryover assessment",
                          value = FALSE
                        ),
                        
                        div(style = "margin-top: 10px; margin-left: 20px; padding-left: 15px; border-left: 2px solid #dee2e6;",
                          fluidRow(
                            column(6,
                              numericInput(
                                "carryover_threshold",
                                "Carryover threshold (% of Cmax):",
                                value = 5,
                                min = 1,
                                max = 10,
                                step = 0.5
                              )
                            ),
                            column(6,
                              div(style = "margin-top: 25px;",
                                checkboxInput(
                                  "exclude_carryover_subjects",
                                  "Automatically exclude subjects with carryover",
                                  value = TRUE
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.is_parallel_design == false",
                    h5(tags$strong("Carryover Detection (ICH M13A)")),
                    
                    div(style = "margin-left: 20px;",
                      checkboxInput(
                        "test_carryover",
                        "Perform carryover assessment",
                        value = FALSE
                      ),
                      
                      conditionalPanel(
                        condition = "input.test_carryover == true",
                        div(style = "margin-top: 10px; margin-left: 20px; padding-left: 15px; border-left: 2px solid #dee2e6;",
                          fluidRow(
                            column(6,
                              numericInput(
                                "carryover_threshold",
                                "Carryover threshold (% of Cmax):",
                                value = 5,
                                min = 1,
                                max = 10,
                                step = 0.5
                              )
                            ),
                            column(6,
                              div(style = "margin-top: 25px;",
                                checkboxInput(
                                  "exclude_carryover_subjects",
                                  "Automatically exclude subjects with carryover",
                                  value = TRUE
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        # Skip Step 1 message for PK parameters data
        conditionalPanel(
          condition = "input.data_type == 'pk_parameters'",
          div(
            style = "background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
            h5(icon("info-circle", style = "color: #f39c12;"), " Step 1: NCA Analysis Setup - Skipped", 
               style = "color: #856404; margin-top: 0;"),
            p("NCA calculations have already been completed for pre-calculated PK parameters.",
              style = "color: #856404; margin-bottom: 0;")
          )
        ),
        
        # Step 2: BE Statistical Analysis (for all data types) — choose BE type FIRST
        div(
          style = "border: 2px solid #27ae60; border-radius: 10px; padding: 20px; margin-bottom: 20px; background-color: #f8fff8;",
          h4(icon("calculator"), " Step 2: BE Statistical Analysis", style = "color: #27ae60; margin-top: 0;"),
          
          fluidRow(
            # BE Analysis Type Selection — left column
            column(6,
              h5("BE Analysis Type"),
              radioButtons("be_analysis_type", 
                label = NULL,
                choices = list(
                  "Average Bioequivalence (ABE)" = "ABE",
                  "Average BE with Expanding Limits (ABEL)" = "ABEL",
                  "Reference-Scaled Average BE (RSABE)" = "RSABE"
                ),
                selected = "ABE",
                inline = FALSE
              ),
              
              # Design compatibility warning — shown when RSABE/ABEL selected with non-replicate
              conditionalPanel(
                condition = "(input.be_analysis_type == 'RSABE' || input.be_analysis_type == 'ABEL') && output.is_non_replicate_design == true",
                div(style = "padding: 10px; background-color: #f8d7da; border-left: 3px solid #dc3545; border-radius: 4px; margin-top: 10px;",
                  tags$small(style = "color: #721c24;",
                    icon("exclamation-triangle"), " ",
                    tags$strong("Design Incompatibility: "),
                    "RSABE and ABEL require a replicate design (2\u00D72\u00D73 or 2\u00D72\u00D74). ",
                    "The detected study design does not support this analysis type. ",
                    "Please select ABE or change the study design."
                  )
                )
              )
            ),
            
            # BE type configuration — right column
            column(6,

              # ABE acceptance limits (fixed)
              conditionalPanel(
                condition = "input.be_analysis_type == 'ABE'",
                div(style = "padding: 10px; background-color: #e8f5e9; border-left: 3px solid #28a745; border-radius: 4px; margin-top: 10px;",
                  tags$small(style = "color: #2e7d32;",
                    icon("check-circle"), " ",
                    tags$strong("Acceptance Limits: "),
                    "80.00% \u2013 125.00% (regulatory standard for all parameters)"
                  )
                )
              ),
              
              # RSABE: Method selector
              conditionalPanel(
                condition = "input.be_analysis_type == 'RSABE'",
                h5("RSABE Statistical Method",
                   help_icon("rsabe_method", help_texts$rsabe_method$tooltip,
                            help_texts$rsabe_method$title, help_texts$rsabe_method$content)
                ),
                radioButtons("rsabe_method",
                  label = NULL,
                  choices = list(
                    "FDA Linearized Scaled Criterion (Howe UCB)" = "fda_linearized",
                    "Non-Central TOST (ncTOST)" = "nctost"
                  ),
                  selected = "fda_linearized",
                  inline = FALSE
                )
              ),
              
              # ABEL: Expanded limits scope selection — which PK parameters are evaluated
              # for reference-scaling (CVwR > 30% triggers expanded limits). All other
              # parameters always use fixed 80–125% limits. This is independent of the
              # CVwR cap and the ANOVA model (Method A vs B).
              conditionalPanel(
                condition = "input.be_analysis_type == 'ABEL'",
                h5("Parameters Eligible for Expanded Limits"),
                checkboxGroupInput("abel_eligible_params",
                  label = NULL,
                  choices = list(
                    "Cmax" = "Cmax",
                    "AUC0-t" = "AUC0t"
                  ),
                  selected = c("Cmax"),
                  inline = TRUE
                ),
                tags$small(style = "color: #6c757d;",
                  "Selected parameters use ABEL (expanded if CV", tags$sub("wR"), " > 30%); ",
                  "other PK parameters always use fixed 80–125% limits."
                )
              ),
              
              # Conditional panel for ABEL cap selection — controls the CVwR cap regardless
              # of which parameters are eligible above. Maps to replicateBE regulator code.
              conditionalPanel(
                condition = "input.be_analysis_type == 'ABEL'",
                h5("CV", tags$sub("wR"), " Cap on Expanded Limits",
                   help_icon("abel_upper_cap", help_texts$abel_upper_cap$tooltip, 
                            help_texts$abel_upper_cap$title, help_texts$abel_upper_cap$content)
                ),
                div(style = "border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; background: #f8f9fa;",
                  selectInput(
                    "abel_upper_cap",
                    label = NULL,
                    choices = list(
                      "EMA: cap at CVwR = 50% (limits 69.84% – 143.19%)" = "50",
                      "EMA: no cap" = "none",
                      "Health Canada: cap at CVwR = 57.4% (limits 66.67% – 150.00%)" = "HC",
                      "Fixed widened limits (75.00% – 133.33%)" = "fixed"
                    ),
                    selected = "50"
                  )
                )
              ),
              
              # Parallel Design Statistical Method Selection
              conditionalPanel(
                condition = "output.is_parallel_design == true",
                div(style = "margin-top: 20px; padding: 15px; background-color: #f1f8ff; border: 1px solid #b8daff; border-radius: 5px;",
                  h6(tags$strong("Parallel Design Statistical Method"), 
                     help_icon("welch_correction", help_texts$welch_correction$tooltip, 
                              help_texts$welch_correction$title, help_texts$welch_correction$content),
                     style = "color: #004085; margin-bottom: 10px;"
                  ),
                  radioButtons(
                    "welch_correction_be",
                    "Variance assumption for bioequivalence testing:",
                    choices = list(
                      "Welch correction (recommended)" = TRUE,
                      "Equal variances assumption" = FALSE
                    ),
                    selected = TRUE,
                    inline = FALSE
                  ),
                  helpText("Welch correction is preferred for parallel designs as it does not assume equal variances between groups.")
                )
              )
            )
          )
        ),
        
        # Step 3: ANOVA Configuration (for all data types) — model depends on BE type
        div(
          style = "border: 2px solid #e67e22; border-radius: 10px; padding: 20px; margin-bottom: 20px; background-color: #fef9f3;",
          h4(icon("chart-bar"), " Step 3: ANOVA Configuration", style = "color: #e67e22; margin-top: 0;"),
          
          fluidRow(
            column(6,
              h5("Analysis Model"),
              
              # ===============================================================
              # PARALLEL DESIGNS: Fixed effects only (all BE types)
              # NOTE: own id (anova_model_parallel) - this used to reuse
              # "anova_model" (same id as the ABEL picker below), which meant
              # both selectInputs existed in the DOM at once and could
              # silently overwrite each other's value when switching
              # be_analysis_type without ever touching the visible dropdown.
              # See the analysis_config$anova_model assembly in
              # analysis_setup_server.R for how each id is read back out.
              # ===============================================================
              conditionalPanel(
                condition = "output.study_design_detected && output.detected_design_type == 'parallel'",
                div(
                  selectInput(
                    "anova_model_parallel",
                    label = NULL,
                    choices = list("Fixed Effects (lm)" = "fixed"),
                    selected = "fixed"
                  ),
                  div(style = "font-size: 11px; color: #666; margin-top: 5px;",
                    "\U0001F512 Fixed effects model automatically selected for parallel designs."
                  )
                )
              ),
              
              # ===============================================================
              # ABE: All four ANOVA model options (our own simple_anova.R)
              # Keeps the plain "anova_model" id (the Random Effects Structure
              # and Group Effects conditionalPanels below key off
              # input.anova_model and are both scoped to be_analysis_type ==
              # 'ABE' already, so this is the only picker they should ever
              # reflect).
              # ===============================================================
              conditionalPanel(
                condition = "(!output.study_design_detected || output.detected_design_type != 'parallel') && input.be_analysis_type == 'ABE'",
                selectInput(
                  "anova_model",
                  label = NULL,
                  choices = list(
                    "Fixed Effects (lm)" = "fixed",
                    "Mixed Effects - nlme (REML)" = "nlme", 
                    "Mixed Effects - Satterthwaite DF" = "satterthwaite",
                    "Mixed Effects - Kenward-Roger DF" = "kenward-roger"
                  ),
                  selected = "fixed"
                ),
                div(style = "font-size: 11px; color: #666; margin-top: 5px;",
                  "ABE uses our ANOVA engine for treatment comparison and 90% CI construction."
                )
              ),
              
              # ===============================================================
              # ABEL: Maps to replicateBE Method A (fixed) or Method B (mixed)
              # method.B option: 1=Satterthwaite, 2=nlme/SAS, 3=Kenward-Roger
              # NOTE: own id (anova_model_abel) - see the parallel-design
              # panel's comment above for why this can't share "anova_model".
              # ===============================================================
              conditionalPanel(
                condition = "(!output.study_design_detected || output.detected_design_type != 'parallel') && input.be_analysis_type == 'ABEL'",
                selectInput(
                  "anova_model_abel",
                  label = NULL,
                  choices = list(
                    "Method A — Fixed Effects (ANOVA)" = "fixed",
                    "Method B — Mixed Effects, nlme (SAS CONTAIN DF)" = "nlme",
                    "Method B — Mixed Effects, Satterthwaite DF" = "satterthwaite",
                    "Method B — Mixed Effects, Kenward-Roger DF" = "kenward-roger"
                  ),
                  selected = "fixed"
                ),
                div(style = "font-size: 11px; color: #666; margin-top: 5px;",
                  icon("info-circle"), " ABEL analysis is performed by the replicateBE package. ",
                  tags$b("Method A"), " uses a linear fixed-effects model (ANOVA). ",
                  tags$b("Method B"), " uses a linear mixed-effects model with subjects as random effect. ",
                  "The random effects structure is fixed at (1|subject) as required by the EMA guideline."
                )
              ),
              
              # ===============================================================
              # RSABE: model auto-selected by replicate design (not user-
              # editable). See R/rsabe_analysis.R::perform_rsabe().
              # ===============================================================
              conditionalPanel(
                condition = "(!output.study_design_detected || output.detected_design_type != 'parallel') && input.be_analysis_type == 'RSABE'",
                uiOutput("rsabe_anova_model_ui"),
                div(style = "font-size: 11px; color: #666; margin-top: 5px;",
                  "(Partial replicate requires Fixed Effects; full replicate requires Mixed Effects.)"
                )
              ),
              
              # ===============================================================
              # RANDOM EFFECTS — only for ABE with mixed models
              # (ABEL uses replicateBE's built-in (1|subject); RSABE uses nlme's built-in (1|subject))
              # Subject-as-random-intercept is the only structure used in standard
              # regulatory BE mixed models (FDA/EMA 2x2 crossover); random-slope
              # and period-nested variants are not part of the standard BE
              # specification, so they are not offered here.
              # ===============================================================
              conditionalPanel(
                condition = "input.anova_model != 'fixed' && input.be_analysis_type == 'ABE'",
                div(style = "margin-top: 10px;",
                  h6("Random Effects Structure", style = "margin-bottom: 5px; font-weight: bold;"),
                  selectInput(
                    "random_effects",
                    label = NULL,
                    choices = list(
                      "Random Intercept: (1|subject)" = "(1|subject)"
                    ),
                    selected = "(1|subject)"
                  ),
                  div(style = "font-size: 11px; color: #666; margin-top: 5px;",
                    "Subject as a random intercept — (1|subject) — is the standard random-effects structure for a bioequivalence mixed model."
                  ),
                  
                  # Group as random effect option (only for ABE mixed models,
                  # standard 2x2 crossover only for now)
                  conditionalPanel(
                    condition = "output.groups_detected && output.is_non_replicate_design && !output.is_parallel_design",
                    div(style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border-radius: 5px; border-left: 3px solid #ffc107;",
                      h6("Group Effects", style = "margin-bottom: 5px; font-weight: bold; color: #856404;"),
                      checkboxInput(
                        "include_group_random",
                        "Include Group as Random Effect",
                        value = FALSE
                      ),
                      div(style = "font-size: 11px; color: #856404; margin-top: 5px;",
                        "Groups detected in data (2×2 crossover). Check to include group-to-group variability in the mixed-effects model."
                      )
                    )
                  )
                )
              ),
              
              # Group effects for fixed AND mixed models (ABE, standard 2x2 crossover only for now —
              # ABEL/RSABE handle their own models; replicate/parallel designs not yet supported).
              conditionalPanel(
                condition = "(input.anova_model == 'fixed' || input.anova_model == 'nlme') && output.groups_detected && input.be_analysis_type == 'ABE' && output.is_non_replicate_design && !output.is_parallel_design",
                div(style = "margin-top: 10px; padding: 10px; background-color: #d1ecf1; border-radius: 5px; border-left: 3px solid #17a2b8;",
                  h6("Group Effects", style = "margin-bottom: 5px; font-weight: bold; color: #0c5460;"),
                  checkboxInput(
                    "include_group_fixed",
                    "Include Group as Fixed Effect",
                    value = TRUE
                  ),
                  checkboxInput(
                    "include_group_treatment_interaction",
                    "Include Group \u00D7 Treatment Interaction",
                    value = FALSE
                  ),
                  div(style = "font-size: 11px; color: #0c5460; margin-top: 5px;",
                    "Groups detected in data (2×2 crossover). Adds a Group term to the ANOVA to assess whether dosing/facility group explains any variance."
                  )
                )
              )
            ),
            column(6,
              h5("PK Parameters for Analysis"),
              
              fluidRow(
                column(6,
                  h6("Primary Parameters", style = "color: #2c3e50; font-weight: bold;"),
                  checkboxGroupInput(
                    "primary_pk_params",
                    label = NULL,
                    choices = list(
                      "Cmax" = "Cmax",
                      "AUC0-t" = "AUC0t"
                    ),
                    selected = c("Cmax", "AUC0t")
                  )
                ),
                column(6,
                  h6("Secondary Parameters", style = "color: #2c3e50; font-weight: bold;"),
                  checkboxGroupInput(
                    "secondary_pk_params",
                    label = NULL,
                    choices = list(
                      "AUC0-inf" = "AUC0inf",
                      "pAUC" = "pAUC",
                      "Tmax" = "Tmax"
                    ),
                    selected = NULL
                  )
                )
              ),
              
              # Note about Tmax methodology
              conditionalPanel(
                condition = "input.secondary_pk_params && input.secondary_pk_params.indexOf('Tmax') > -1",
                div(
                  style = "margin-top: 8px; padding: 8px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 3px;",
                  tags$small(
                    icon("exclamation-triangle"), 
                    " Note: Tmax requires non-parametric analysis methods (median differences, Wilcoxon tests) per regulatory guidance and is not suitable for standard confidence interval-based bioequivalence assessment.",
                    style = "color: #856404;"
                  )
                )
              )
            )
          )
        ),
        
        # Advanced Options Section (ABE)
        conditionalPanel(
          condition = "input.be_analysis_type == 'ABE'",
          div(
            style = "border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-top: 15px; background: #f8f9fa;",
            h5("Advanced Analysis Options"),
            checkboxInput(
              "enable_advanced_options",
              "Show advanced options",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.enable_advanced_options",
              div(style = "margin-top: 15px; padding: 15px; background-color: #e8f4fd; border-radius: 5px;",
                h6("Alpha Level (\u03b1)",
                   help_icon("alpha_level", help_texts$confidence_level$tooltip, 
                            help_texts$confidence_level$title, help_texts$confidence_level$content),
                   style = "font-weight: bold; color: #2c3e50;"
                ),
                p(style = "font-size: 0.85em; color: #666; margin-bottom: 8px;",
                  "One-sided significance level for TOST. Default 0.05 yields a 90% confidence interval."),
                fluidRow(
                  column(6,
                    numericInput(
                      "alpha_level",
                      label = NULL,
                      value = 0.05,
                      min = 0.01,
                      max = 0.20,
                      step = 0.01
                    )
                  ),
                  column(6,
                    div(id = "confidence_display", style = "margin-top: 8px; color: #6c757d;",
                      uiOutput("alpha_display_text")
                    )
                  )
                ),
                hr(style = "margin: 15px 0;"),
                h6("Per-Parameter BE Limits", style = "font-weight: bold; color: #2c3e50;"),
                p(style = "font-size: 0.85em; color: #666; margin-bottom: 12px;",
                  "Set different acceptance limits for each PK parameter category. Defaults: 80.00% - 125.00%."),
                fluidRow(
                  column(6,
                    div(style = "padding: 10px; background: #fff; border-radius: 4px; border: 1px solid #dee2e6;",
                      h6(tags$strong("Cmax Limits (%)"), style = "margin-top: 0;"),
                      fluidRow(
                        column(6, numericInput("be_lower_cmax", "Lower", value = 80, min = 70, max = 90, step = 1)),
                        column(6, numericInput("be_upper_cmax", "Upper", value = 125, min = 110, max = 140, step = 1))
                      )
                    )
                  ),
                  column(6,
                    div(style = "padding: 10px; background: #fff; border-radius: 4px; border: 1px solid #dee2e6;",
                      h6(tags$strong("AUC Limits (%)"), style = "margin-top: 0;"),
                      fluidRow(
                        column(6, numericInput("be_lower_auc", "Lower", value = 80, min = 70, max = 90, step = 1)),
                        column(6, numericInput("be_upper_auc", "Upper", value = 125, min = 110, max = 140, step = 1))
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        # ABEL Advanced Options
        conditionalPanel(
          condition = "input.be_analysis_type == 'ABEL'",
          div(
            style = "border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-top: 15px; background: #f8f9fa;",
            h5("Advanced ABEL Options"),
            checkboxInput(
              "abel_show_advanced",
              "Show advanced options",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.abel_show_advanced",
              div(style = "padding: 15px; background-color: #fff8e1; border: 1px solid #ffd54f; border-radius: 5px; margin-top: 10px;",
                h6(tags$strong(tags$i(class="fa fa-exclamation-triangle"), " Advanced Options"), 
                   style = "color: #f57f17; margin-bottom: 10px;"),
                p(style = "font-size: 0.85em; color: #666; margin-bottom: 12px;",
                  "These options allow customization of ABEL parameters. Use only if required by specific regulatory guidance."),
                
                # Alpha Level for ABEL
                h6("Alpha Level (\u03b1)", style = "font-weight: bold; color: #2c3e50;"),
                p(style = "font-size: 0.85em; color: #666; margin-bottom: 8px;",
                  "One-sided significance level for TOST. Default 0.05 yields a 90% confidence interval."),
                fluidRow(
                  column(6,
                    numericInput(
                      "alpha_level_abel",
                      label = NULL,
                      value = 0.05,
                      min = 0.01,
                      max = 0.20,
                      step = 0.01
                    )
                  ),
                  column(6,
                    div(id = "confidence_display_abel", style = "margin-top: 8px; color: #6c757d;")
                  )
                ),
                hr(style = "margin: 15px 0;"),
                
                # TIE adjustment
                checkboxInput(
                  "abel_adjust_tie",
                  "Adjust alpha to control Type I Error inflation",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.abel_adjust_tie",
                  div(style = "margin-left: 20px; margin-top: 8px; padding: 10px; background-color: #fff; border-radius: 4px;",
                    helpText("Iteratively adjusts alpha to control TIE inflation at the nominal level (Labes & Schütz 2016).")
                  )
                ),
                
                # Outlier analysis
                checkboxInput(
                  "abel_outlier_analysis",
                  "Enable outlier detection (studentized residuals)",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.abel_outlier_analysis",
                  div(style = "margin-left: 20px; margin-top: 8px; padding: 10px; background-color: #fff; border-radius: 4px;",
                    numericInput(
                      "abel_outlier_fence",
                      "Outlier detection fence (IQR multiplier):",
                      value = 2,
                      min = 1.5,
                      max = 3,
                      step = 0.5
                    ),
                    helpText("Multiplier of the interquartile range. Higher values = fewer outliers detected. Default: 2 (Tukey's rule).")
                  )
                )
              )
            )
          )
        ),
        
        # RSABE Advanced Options
        conditionalPanel(
          condition = "input.be_analysis_type == 'RSABE'",
          div(
            style = "border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-top: 15px; background: #f8f9fa;",
            h5("Advanced RSABE Options"),
            checkboxInput(
              "rsabe_show_advanced",
              "Show advanced options",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.rsabe_show_advanced",
              div(style = "padding: 15px; background-color: #e8f4fd; border: 1px solid #90caf9; border-radius: 5px; margin-top: 10px;",
                h6("Alpha Level (\u03b1)",
                   style = "font-weight: bold; color: #2c3e50;"
                ),
                p(style = "font-size: 0.85em; color: #666; margin-bottom: 8px;",
                  "One-sided significance level. Default 0.05 yields a 95% upper confidence bound (linearized) or 90% CI (ncTOST)."),
                fluidRow(
                  column(6,
                    numericInput(
                      "alpha_level_rsabe",
                      label = NULL,
                      value = 0.05,
                      min = 0.01,
                      max = 0.20,
                      step = 0.01
                    )
                  ),
                  column(6,
                    div(id = "confidence_display_rsabe", style = "margin-top: 8px; color: #6c757d;")
                  )
                )
              )
            )
          )
        ),
      # Analysis run button
      div(style = "text-align: center; padding: 20px; margin-top: 20px;",
        actionButton(
          "run_analysis", 
          "Run Bioequivalence Analysis",
          class = "btn btn-success btn-lg",
          icon = icon("calculator"),
          style = "font-size: 18px; padding: 15px 30px;"
        )
      )
    ) # Close main configuration column
  ), # Close main fluidRow
  
  # JavaScript for alpha to confidence interval conversion
  tags$script(HTML("
    $(document).ready(function() {
      // Function to update confidence interval display
      function updateConfidenceInterval() {
        var alpha = $('#alpha_level').val();
        var beType = $('input[name=\"be_analysis_type\"]:checked').val();
        
        if (alpha && !isNaN(alpha)) {
          var ci = (1 - 2 * parseFloat(alpha)) * 100;
          var ciText = '';
          
          if (beType === 'ABEL' || beType === 'RSABE') {
            ciText = ci.toFixed(0) + '% CI (α = ' + alpha + ' one-sided for TOST)';
          } else {
            ciText = ci.toFixed(0) + '% CI (α = ' + alpha + ' one-sided for TOST)';
          }
          
          $('#confidence_display').text(ciText);
        }
      }
      
      // Update on page load
      updateConfidenceInterval();
      
      // Update when alpha level changes
      $('#alpha_level').on('input change', function() {
        updateConfidenceInterval();
      });
      
      // Update when BE analysis type changes
      $('input[name=\"be_analysis_type\"]').on('change', function() {
        updateConfidenceInterval();
      });
    });
  "))
) # Close tagList
