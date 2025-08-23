# Analysis Setup UI
# This file contains the analysis configuration interface

# Source help utilities
source("utils/help_utils.R", local = TRUE)

tagList(
  fluidRow(
    # Navigation breadcrumb
    column(12,
      div(style = "margin-bottom: 20px;",
        actionButton("back_to_upload", "← Back to Upload", class = "btn btn-outline-secondary"),
        span(" > Analysis Setup", style = "margin-left: 10px; font-weight: bold; color: #3498db;")
      )
    )
  ),

  fluidRow(
    # Main configuration panel
    column(
      width = 12,
      
      # Study design detection
      box(
        title = "Study Design Configuration", 
        status = "primary", 
        solidHeader = TRUE,
        width = 12,
        icon = icon("cogs"),
        
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
      
      # Analysis parameters - 3-step structure based on data type
      box(
        title = "Analysis Parameters", 
        status = "info", 
        solidHeader = TRUE,
        width = 12,
        icon = icon("sliders-h"),
        
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
                    "Linear up/Log down (Mixed)" = "mixed",
                    "Linear trapezoidal" = "linear",
                    "Log trapezoidal" = "log",
                    "Linear/Log trapezoidal" = "linear_log"
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
                    "AIC (Akaike Information Criterion)" = "aic",
                    "TTT (Two-Times-Tmax)" = "ttt"
                  ),
                  selected = "manual"
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

                h5(tags$strong("Missing Data Handling"),
                   help_icon("missing_data", help_texts$missing_data$tooltip, 
                            help_texts$missing_data$title, help_texts$missing_data$content)
                ),
                selectInput(
                  "missing_data",
                  label = NULL,
                  choices = list(
                    "Last observation carried forward" = "locf",
                    "Linear interpolation" = "interpolate", 
                    "Multiple imputation" = "multiple"
                  ),
                  selected = "interpolate"
                )
              ),
              column(6,
                h5(tags$strong("Optional PK Parameters"),
                   help_icon("pk_parameters", "Configure optional pharmacokinetic parameters for specialized analysis", 
                            "Optional PK Parameters", 
                            "Most standard PK parameters (AUC0t, AUC0inf, Cmax, Tmax, t½, etc.) are calculated automatically. Use this section to configure partial AUC (pAUC) for early exposure assessment when required by regulatory guidelines.")
                ),
                
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
                  ),
                  
                  # Special case for long half-life drugs
                  div(style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #dee2e6;",
                    checkboxInput(
                      "truncated_auc_72h",
                      "Use AUC(0-72h) for long half-life drugs",
                      value = FALSE
                    ),
                    conditionalPanel(
                      condition = "input.truncated_auc_72h",
                      div(style = "margin-left: 20px; color: #666; font-size: 0.9em;")
                    )
                  )
                ),
                
                # Carryover Detection section - aligned with Optional PK Parameters heading
                div(
                  # Add conditional styling for parallel designs
                  style = "transition: all 0.3s ease;",
                  conditionalPanel(
                    condition = "output.is_parallel_design == true",
                    div(style = "opacity: 0.4; pointer-events: none; background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px dashed #dee2e6;",
                      h5(tags$strong("Carryover Detection (ICH M13A)"),
                         help_icon("carryover_effect", "ICH M13A Section 2.2.3.3 carryover detection method", 
                                  "ICH M13A Carryover Detection", 
                                  "Proper carryover detection examines pre-dose samples in Period 2+ and compares to Cmax within the same period. If pre-dose > 5% of Cmax, significant carryover is detected and the subject should be excluded per regulatory guidelines."),
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
                    h5(tags$strong("Carryover Detection (ICH M13A)"),
                       help_icon("carryover_effect", "ICH M13A Section 2.2.3.3 carryover detection method", 
                                "ICH M13A Carryover Detection", 
                                "Proper carryover detection examines pre-dose samples in Period 2+ and compares to Cmax within the same period. If pre-dose > 5% of Cmax, significant carryover is detected and the subject should be excluded per regulatory guidelines.")
                    ),
                    
                    div(style = "margin-left: 20px;",
                      checkboxInput(
                        "test_carryover",
                        "Perform carryover assessment",
                        value = TRUE
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
        
        # Step 2: ANOVA Configuration (for all data types)
        div(
          style = "border: 2px solid #e67e22; border-radius: 10px; padding: 20px; margin-bottom: 20px; background-color: #fef9f3;",
          h4(icon("chart-bar"), " Step 2: ANOVA Configuration", style = "color: #e67e22; margin-top: 0;"),
          
          fluidRow(
            column(6,
              h5("Analysis Model",
                 help_icon("analysis_model", help_texts$analysis_model$tooltip, 
                          help_texts$analysis_model$title, help_texts$analysis_model$content)
              ),
              selectInput(
                "anova_model",
                label = NULL,
                choices = list(
                  "Fixed Effects (lm)" = "fixed",
                  "Mixed Effects - nlme" = "nlme", 
                  "Mixed Effects - Satterthwaite" = "satterthwaite",
                  "Mixed Effects - Kenward-Roger" = "kenward-roger"
                ),
                selected = "fixed"
              ),
              
              # Conditional random effects specification for mixed models
              conditionalPanel(
                condition = "input.anova_model != 'fixed'",
                div(style = "margin-top: 10px;",
                  h6("Random Effects Structure", style = "margin-bottom: 5px; font-weight: bold;"),
                  selectInput(
                    "random_effects",
                    label = NULL,
                    choices = list(
                      "Random Intercept: (1|subject)" = "(1|subject)",
                      "Random Intercept: (1|subject/period)" = "(1|subject/period)",
                      "Random Slope: (treatment|subject)" = "(treatment|subject)",
                      "Random Intercept + Slope: (1 + treatment|subject)" = "(1 + treatment|subject)"
                    ),
                    selected = "(1|subject)"
                  ),
                  div(style = "font-size: 11px; color: #666; margin-top: 5px;",
                    "For most bioequivalence studies, '(1|subject)' is appropriate."
                  )
                )
              )
            ),
            column(6,
              h5("PK Parameters for Analysis",
                 help_icon("pk_parameters", "Select which pharmacokinetic parameters to include in the ANOVA analysis. Both original and log-transformed versions will be analyzed automatically.", 
                          "PK Parameter Selection", "Choose the pharmacokinetic parameters you want to analyze. Primary parameters (Cmax, AUC0-t, AUC0-inf) are pre-selected as they are typically required for bioequivalence assessment. The system will automatically include both the original parameter and its log-transformed version (e.g., selecting Cmax will analyze both Cmax and lnCmax).")
              ),
              
              fluidRow(
                column(6,
                  h6("Primary Parameters", style = "color: #2c3e50; font-weight: bold;"),
                  checkboxGroupInput(
                    "primary_pk_params",
                    label = NULL,
                    choices = list(
                      "Cmax" = "Cmax",
                      "AUC0-t" = "AUC0t",
                      "AUC0-inf" = "AUC0inf"
                    ),
                    selected = c("Cmax", "AUC0t", "AUC0inf")
                  )
                ),
                column(6,
                  h6("Secondary Parameters", style = "color: #2c3e50; font-weight: bold;"),
                  checkboxGroupInput(
                    "secondary_pk_params",
                    label = NULL,
                    choices = list(
                      "Tmax" = "Tmax",
                      "pAUC" = "pAUC",
                      "AUC0-72" = "AUC072"
                    ),
                    selected = NULL
                  )
                )
              ),
              
              # Information about automatic log-transformation
              div(
                style = "margin-top: 10px; padding: 8px; background-color: #e8f4fd; border-left: 3px solid #3498db; border-radius: 3px;",
                tags$small(
                  icon("info-circle"), 
                  " Log-transformed versions (e.g., lnCmax, lnAUC0t, lnTmax) will be automatically included in the analysis.",
                  style = "color: #2980b9;"
                )
              )
            )
          )
        ),
        
        # Step 3: BE Statistical Analysis (for all data types)
        div(
          style = "border: 2px solid #27ae60; border-radius: 10px; padding: 20px; background-color: #f8fff8;",
          h4(icon("calculator"), " Step 3: BE Statistical Analysis", style = "color: #27ae60; margin-top: 0;"),
          
          fluidRow(
            # BE Analysis Type Selection (NEW - First element)
            column(6,
              h5("BE Analysis Type",
                 help_icon("be_analysis_type", help_texts$be_analysis_type$tooltip, 
                          help_texts$be_analysis_type$title, help_texts$be_analysis_type$content)
              ),
              radioButtons("be_analysis_type", 
                label = NULL,
                choices = list(
                  "Average Bioequivalence (ABE)" = "ABE",
                  "Reference-Scaled Average BE (RSABE)" = "RSABE", 
                  "Average BE with Expanding Limits (ABEL)" = "ABEL"
                ),
                selected = "ABE",
                inline = FALSE
              ),
              
              # Conditional panel for ABE limits
              conditionalPanel(
                condition = "input.be_analysis_type == 'ABE'",
                h5("Bioequivalence Limits (%)",
                   help_icon("be_limits", help_texts$be_limits$tooltip, 
                            help_texts$be_limits$title, help_texts$be_limits$content)
                ),
                div(style = "border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; background: #f8f9fa;",
                  fluidRow(
                    column(6,
                      numericInput("be_lower", "Lower (%)", value = 80, min = 70, max = 90, step = 1)
                    ),
                    column(6,
                      numericInput("be_upper", "Upper (%)", value = 125, min = 110, max = 140, step = 1)
                    )
                  ),
                  div(style = "margin-top: 10px; color: #6c757d; font-size: 0.9em;",
                    "• Standard: 80.00% - 125.00% for most drugs"
                  )
                )
              ),
              
              # Placeholder panels for RSABE and ABEL
              conditionalPanel(
                condition = "input.be_analysis_type == 'RSABE'",
                div(class = "alert alert-info",
                  h5(icon("info-circle"), " Reference-Scaled Average BE"),
                  p("RSABE analysis for highly variable drugs (CV > 30%)"),
                  p("Configuration options coming soon:"),
                  tags$ul(
                    tags$li("Regulatory constant (default: 0.893)"),
                    tags$li("CV threshold for scaling (default: 30%)"),
                    tags$li("Upper cap for scaling")
                  ),
                  helpText("Currently using ABE methodology. Full RSABE implementation pending.")
                )
              ),
              
              conditionalPanel(
                condition = "input.be_analysis_type == 'ABEL'",
                div(class = "alert alert-info",
                  h5(icon("info-circle"), " Average BE with Expanding Limits"),
                  p("ABEL analysis per EMA guidelines for HVDs"),
                  p("Configuration options coming soon:"),
                  tags$ul(
                    tags$li("CV threshold (default: 30%)"),
                    tags$li("Maximum expansion factor"),
                    tags$li("Widening approach selection")
                  ),
                  helpText("Currently using ABE methodology. Full ABEL implementation pending.")
                )
              )
            ),
            
            # Analysis configuration (moved to right column)
            column(6,
              h5("Confidence Interval Level",
                 help_icon("confidence_level", help_texts$confidence_level$tooltip, 
                          help_texts$confidence_level$title, help_texts$confidence_level$content)
              ),
              numericInput(
                "confidence_level",
                label = NULL,
                value = 90,
                min = 80,
                max = 99,
                step = 1
              ),
              helpText("90% CI corresponds to α = 0.05 for two one-sided tests"),
              
              h5("Analysis Options"),
              checkboxInput(
                "log_transform",
                "Apply log transformation",
                value = TRUE
              ),
              helpText("Log-transformation required for AUC and Cmax")
            )
          )
        ),
        
        # Advanced Options Section (previously reference scaling)
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
                h6("Experimental Reference Scaling"),
                p("Note: Advanced reference scaling features are experimental"),
                checkboxInput(
                  "reference_scaling",
                  "Enable experimental reference scaling",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.reference_scaling",
                  numericInput(
                    "scaling_threshold",
                    "CV threshold for scaling (%)",
                    value = 30,
                    min = 20,
                    max = 50,
                    step = 5
                  ),
                  helpText("Apply scaling when within-subject CV > threshold")
                )
              )
            )
          )
        )
      ), # Close Analysis Parameters box
      
      # Analysis summary and run button
      box(
        title = "Ready to Analyze", 
        status = "success", 
        solidHeader = TRUE,
        width = 12,
        icon = icon("play"),
        
        div(style = "text-align: center; padding: 20px;",
          h4("Analysis Configuration Complete", style = "color: #27ae60; margin-bottom: 20px;"),
          p("Review your settings and click the button below to start the analysis.", 
            style = "color: #7f8c8d; margin-bottom: 30px;"),
          
          actionButton(
            "run_analysis", 
            "Run Bioequivalence Analysis",
            class = "btn btn-success btn-lg",
            icon = icon("calculator"),
            style = "font-size: 18px; padding: 15px 30px;"
          )
        )
      ) # Close Ready to Analyze box
    ) # Close main configuration column
  ) # Close main fluidRow
) # Close tagList
