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
#' @return List containing ANOVA results for each parameter
#'
perform_simple_anova <- function(nca_data, parameters, anova_model = "fixed", random_effects = "(1|subject)") {
  
  cat("\n=== ANOVA Analysis ===\n")
  
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
  
  # Check required design variables are present
  required_vars <- c("sequence", "subject", "period", "treatment")
  missing_vars <- required_vars[!required_vars %in% names(nca_data)]
  
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing required design variables: %s", paste(missing_vars, collapse = ", ")))
  }
  
  # Map column names to expected ANOVA model names
  nca_data$seq <- as.factor(nca_data$sequence)
  nca_data$subj <- as.factor(nca_data$subject)
  nca_data$prd <- as.factor(nca_data$period)
  
  # Explicitly set drug factor levels with R as reference to ensure T/R ratio calculation
  # This ensures drugT coefficient represents ln(Test/Reference)
  unique_treatments <- unique(nca_data$treatment)
  cat(sprintf("  [DEBUG] Available treatments: %s\n", paste(unique_treatments, collapse = ", ")))
  
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
      
      # Method-specific model fitting
      switch(anova_model,
        
        "fixed" = {
          # Fixed Effects Model using lm (all effects fixed)
          cat(sprintf("  🔧 Using Fixed Effects Model for %s...\n", param))
          
          # Choose formula based on study design
          if (study_design == "parallel") {
            # For parallel design: Parameter ~ treatment
            formula_str <- sprintf("%s ~ drug", param)
            cat(sprintf("  📋 Parallel design model: %s\n", formula_str))
          } else {
            # For crossover design: Parameter ~ sequence + subject %in% sequence + period + treatment
            formula_str <- sprintf("%s ~ seq + subj:seq + prd + drug", param)
            cat(sprintf("  📋 Crossover design model: %s\n", formula_str))
          }
          
          model_formula <- as.formula(formula_str)
          
          # Fit model
          model <- lm(model_formula, data = complete_data, na.action = na.omit)
          
          # Get ANOVA table and summaries
          anova_table <- anova(model)  # This gives Type I SS (sequential)
          type3_ss <- drop1(model, test = "F")  # This gives Type III SS (marginal)
          model_summary <- summary(model)
          model_aic <- AIC(model)
          
          # Create comprehensive ANOVA table (Model/Error/Corrected Total)
          comprehensive_anova <- data.frame(
            Source = c("Model", "Error", "Corrected Total"),
            Df = c(
              sum(anova_table$Df[-nrow(anova_table)]),  # Model DF
              anova_table$Df[nrow(anova_table)],        # Error DF  
              sum(anova_table$Df)                       # Total DF
            ),
            `Sum Sq` = c(
              sum(anova_table$`Sum Sq`[-nrow(anova_table)]),  # Model SS
              anova_table$`Sum Sq`[nrow(anova_table)],        # Error SS
              sum(anova_table$`Sum Sq`)                       # Total SS
            ),
            `Mean Sq` = c(
              sum(anova_table$`Sum Sq`[-nrow(anova_table)]) / sum(anova_table$Df[-nrow(anova_table)]),  # Model MS
              anova_table$`Mean Sq`[nrow(anova_table)],  # Error MS
              NA                                          # Total MS (not applicable)
            ),
            `F value` = c(
              (sum(anova_table$`Sum Sq`[-nrow(anova_table)]) / sum(anova_table$Df[-nrow(anova_table)])) / 
              anova_table$`Mean Sq`[nrow(anova_table)],  # Model F
              NA,                                         # Error F (not applicable)
              NA                                          # Total F (not applicable)
            ),
            `Pr(>F)` = c(
              pf((sum(anova_table$`Sum Sq`[-nrow(anova_table)]) / sum(anova_table$Df[-nrow(anova_table)])) / 
                 anova_table$`Mean Sq`[nrow(anova_table)], 
                 sum(anova_table$Df[-nrow(anova_table)]), 
                 anova_table$Df[nrow(anova_table)], lower.tail = FALSE),  # Model p-value
              NA,                                                          # Error p-value (not applicable)
              NA                                                           # Total p-value (not applicable)
            ),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          rownames(comprehensive_anova) <- comprehensive_anova$Source
          
          # Extract treatment effect
          coeffs <- coef(model)
          if ("drugT" %in% names(coeffs)) {
            pe_estimate <- 100 * exp(coeffs[["drugT"]])
            ci <- 100 * exp(confint(model, "drugT", level = 0.9))
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
          cat(sprintf("  [DEBUG] Parsing random effects: %s\n", random_effects))
          
          # For nlme, we need to create the proper random effects formula
          # Default for 2x2x2 crossover: random intercept by subject
          if (random_effects == "(1|subject)") {
            # Standard random intercept model
            random_formula <- ~ 1 | subj
            grouping_var <- "subj"
          } else {
            # Try to parse user-specified random effects
            random_formula <- as.formula(paste("~", gsub("^\\(|\\)$", "", random_effects)))
            grouping_var <- all.vars(random_formula)[length(all.vars(random_formula))]
          }
          
          cat(sprintf("  [DEBUG] Random formula: %s, grouping var: %s\n", deparse(random_formula), grouping_var))
          
          # Check if grouping variable exists in data
          if (!grouping_var %in% names(complete_data)) {
            stop(sprintf("Grouping variable '%s' not found in data. Available variables: %s", 
                        grouping_var, paste(names(complete_data), collapse=", ")))
          }
          
          # Choose formula based on study design
          if (study_design == "parallel") {
            # For parallel design: Parameter ~ treatment (no period effect)
            formula_str <- sprintf("%s ~ drug", param)
            cat(sprintf("  📋 Parallel design model: %s\n", formula_str))
          } else {
            # For crossover design: Parameter ~ sequence + period + treatment (fixed effects)
            formula_str <- sprintf("%s ~ seq + prd + drug", param)
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
          
          cat("  [DEBUG] Getting summaries...\n")
          
          # Get summaries
          model_summary <- summary(model)
          model_aic <- AIC(model)
          cat("  [DEBUG] Summaries obtained\n")
          
          # Extract treatment effect
          tTable <- model_summary$tTable
          cat(sprintf("  [DEBUG] tTable rownames: %s\n", paste(rownames(tTable), collapse=", ")))
          
          if ("drugT" %in% rownames(tTable)) {
            cat("  [DEBUG] Found drugT coefficient\n")
            pe_estimate <- 100 * exp(tTable["drugT", "Value"])
            
            # Get confidence intervals for nlme model 
            cat("  [DEBUG] Getting confidence intervals...\n")
            tryCatch({
              ci_obj <- nlme::intervals(model, which = "fixed", level = 0.9)
              cat("  [DEBUG] Intervals object obtained\n")
              if ("drugT" %in% rownames(ci_obj$fixed)) {
                ci_vals <- 100 * exp(ci_obj$fixed["drugT", c("lower", "upper")])
                ci_lower <- ci_vals[1]
                ci_upper <- ci_vals[2]
                cat("  [DEBUG] Confidence intervals extracted from intervals() function\n")
              } else {
                cat("  [DEBUG] drugT not found in intervals object, using manual calculation\n")
                # Fallback: calculate CI manually using t-distribution
                coef_val <- tTable["drugT", "Value"]
                se_val <- tTable["drugT", "Std.Error"]
                df_val <- tTable["drugT", "DF"]
                t_crit <- qt(0.95, df_val)  # 90% CI
                ci_lower <- 100 * exp(coef_val - t_crit * se_val)
                ci_upper <- 100 * exp(coef_val + t_crit * se_val)
              }
            }, error = function(e) {
              cat(sprintf("  [DEBUG] Error in intervals(): %s\n", e$message))
              # Fallback calculation if intervals() fails
              coef_val <- tTable["drugT", "Value"]
              se_val <- tTable["drugT", "Std.Error"]
              df_val <- tTable["drugT", "DF"]
              t_crit <- qt(0.95, df_val)  # 90% CI
              ci_lower <- 100 * exp(coef_val - t_crit * se_val)
              ci_upper <- 100 * exp(coef_val + t_crit * se_val)
              cat("  [DEBUG] Used fallback CI calculation\n")
            })
            
            df_residual <- tTable["drugT", "DF"]
          } else {
            cat("  [DEBUG] drugT not found in tTable\n")
          }
          
          # Create anova-like table for nlme
          cat("  [DEBUG] Getting ANOVA table...\n")
          anova_table <- anova(model)  # This gives Type I SS for nlme
          cat("  [DEBUG] ANOVA table obtained\n")
          cat(sprintf("  [DEBUG] ANOVA table dimensions: %d rows, %d cols\n", nrow(anova_table), ncol(anova_table)))
          
          # For nlme, we need to create Type III SS manually since drop1() doesn't work well
          # We'll get the anova table and derive the other tables
          cat("  [DEBUG] Creating Type III SS table...\n")
          type3_ss <- tryCatch({
            # Try to get marginal effects
            cat("  [DEBUG] Attempting marginal anova...\n")
            anova(model, type = "marginal")
          }, error = function(e) {
            # Fallback: use sequential sums of squares as approximation
            cat("  [DEBUG] Marginal anova not available, using sequential as approximation\n")
            anova_table
          })
          cat("  [DEBUG] Type III SS table created\n")
          cat("  [DEBUG] Type III SS table created\n")
          
          # Create comprehensive ANOVA table (Model/Error/Corrected Total) for nlme
          # For mixed models, we focus on fixed effects
          cat("  [DEBUG] Creating comprehensive ANOVA table...\n")
          residual_df <- as.numeric(model$dims$N - model$dims$p)
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
          model_df <- as.numeric(model$dims$p - 1)  # Excluding intercept
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
          formula_str <- sprintf("%s ~ seq + prd + drug + %s", param, random_effects)
          model_formula <- as.formula(formula_str)
          
          # Fit model
          model <- lmerTest::lmer(model_formula, data = complete_data, na.action = na.omit)
          
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
          
          # Create comprehensive ANOVA table for lmerTest (Satterthwaite)
          residuals_vec <- residuals(model)
          residual_ss <- sum(residuals_vec^2)
          residual_df <- nrow(complete_data) - length(fixef(model))
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
          if ("drugT" %in% rownames(coeffs)) {
            pe_log <- coeffs["drugT", "Estimate"]
            se_log <- coeffs["drugT", "Std. Error"]
            df_val <- coeffs["drugT", "df"]
            
            pe_estimate <- 100 * exp(pe_log)
            alpha <- 0.1  # For 90% CI
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
          
          # Create comprehensive ANOVA table for lmerTest (Kenward-Roger)
          residuals_vec <- residuals(model)
          residual_ss <- sum(residuals_vec^2)
          residual_df <- nrow(complete_data) - length(fixef(model))
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
          if ("drugT" %in% rownames(coeffs)) {
            pe_log <- coeffs["drugT", "Estimate"]
            se_log <- coeffs["drugT", "Std. Error"]
            df_val <- coeffs["drugT", "df"]
            
            pe_estimate <- 100 * exp(pe_log)
            alpha <- 0.1  # For 90% CI
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
          if ("drugT" %in% rownames(model_summary$tTable)) {
            treatment_coef <- model_summary$tTable["drugT", "Value"]
            treatment_se <- model_summary$tTable["drugT", "Std.Error"]
            treatment_pval <- model_summary$tTable["drugT", "p-value"]
          }
        } else if (anova_model %in% c("satterthwaite", "kenward-roger")) {
          # For lmer models
          if (!is.null(model_summary$coefficients) && "drugT" %in% rownames(model_summary$coefficients)) {
            treatment_coef <- model_summary$coefficients["drugT", "Estimate"]
            treatment_se <- model_summary$coefficients["drugT", "Std. Error"]
            treatment_pval <- model_summary$coefficients["drugT", 5]  # p-value column
          }
        } else {
          # For fixed effects models
          if (!is.null(model_summary$coefficients) && "drugT" %in% rownames(model_summary$coefficients)) {
            treatment_coef <- model_summary$coefficients["drugT", "Estimate"]
            treatment_se <- model_summary$coefficients["drugT", "Std. Error"]
            treatment_pval <- model_summary$coefficients["drugT", 4]  # p-value column
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
        treatment_se = treatment_se,
        treatment_pval = treatment_pval
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


#' Detect Study Design for ANOVA Analysis
#' 
#' @param nca_data NCA results data frame
#' @return Character string indicating design type ("parallel", "crossover")
detect_anova_design <- function(nca_data) {
  
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
}
