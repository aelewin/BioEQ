# BioEQ - Comprehensive Validation Test with Subject-Level Statistics
# Based on basic_analysis_validation_data.R with enhanced per-subject reporting

# Load BioEQ
source("R/bioeq_main.R")
init_bioeq()

cat("🧪 BioEQ - Comprehensive Validation Test with Subject Statistics\n")
cat("==============================================================\n\n")

# Test configuration
REGULATORY_STANDARD <- "ICH_M13A"
BE_LIMITS <- c(0.8, 1.25)
ALPHA <- 0.05
PRIMARY_PARAMS <- c("AUC0t", "AUC0inf", "Cmax")
LAMBDA_POINTS <- 3

cat("🔧 Test Configuration:\n")
cat("   Standard:", REGULATORY_STANDARD, "\n")
cat("   BE Limits:", paste(BE_LIMITS * 100, collapse="%-", sep=""), "%\n")
cat("   Confidence Level:", (1 - 2 * ALPHA) * 100, "%\n")
cat("   Primary Parameters:", paste(PRIMARY_PARAMS, collapse=", "), "\n\n")

# Load validation data
cat("📊 Loading validation data...\n")
validation_data <- read.csv("data/user_validation_data.csv", stringsAsFactors = FALSE)

cat("   Data loaded:", nrow(validation_data), "concentration records\n")
cat("   Subjects:", length(unique(validation_data$Subject)), "\n")
cat("   Treatments:", paste(unique(validation_data$Treatment), collapse = ", "), "\n")
cat("   Time points:", length(unique(validation_data$Time)), "\n\n")

# Perform NCA analysis
cat("🔬 Performing NCA calculations...\n")
nca_data <- validation_data
colnames(nca_data) <- c("subj", "tmt", "time", "conc")
nca_data$tmt <- ifelse(nca_data$tmt == "R", 1, 2)

nca_results <- perform_enhanced_nca_analysis(nca_data, lambda_points = LAMBDA_POINTS)
cat("   NCA completed for", nrow(nca_results), "subject-treatment combinations\n\n")

# Prepare BE data
cat("📈 Preparing bioequivalence data...\n")
be_data <- nca_results
colnames(be_data)[colnames(be_data) == "subj"] <- "Subject"

be_data$percent_extrap <- ifelse(is.na(be_data$AUC0inf) | be_data$AUC0inf == 0, 
                                 NA, 
                                 ((be_data$AUC0inf - be_data$AUC0t) / be_data$AUC0inf) * 100)

be_data$Treatment <- ifelse(be_data$tmt == 1, "R", "T")
be_data$Formulation <- ifelse(be_data$tmt == 1, "Reference", "Test")

# Add Period and Sequence information
subjects <- unique(be_data$Subject)
be_data$Period <- NA
be_data$Sequence <- NA

for (subj in subjects) {
  subj_data <- be_data[be_data$Subject == subj, ]
  be_data[be_data$Subject == subj, "Period"] <- 1:nrow(subj_data)
  
  treatments <- be_data[be_data$Subject == subj, "Treatment"]
  if (length(treatments) >= 2) {
    if (treatments[1] == "T" && treatments[2] == "R") {
      be_data[be_data$Subject == subj, "Sequence"] <- "TR"
    } else if (treatments[1] == "R" && treatments[2] == "T") {
      be_data[be_data$Subject == subj, "Sequence"] <- "RT"
    } else {
      be_data[be_data$Subject == subj, "Sequence"] <- "Unknown"
    }
  }
}

cat("   BE data prepared for", length(subjects), "subjects\n\n")

# =============================================================================
# EXPECTED SUBJECT-LEVEL STATISTICS (for validation)
# =============================================================================

expected_subject_stats <- list(
  "1" = list(
    CMAX = list(n = 2, min = 1633.00, max = 1739.00, mean = 1686.00, std = 74.9533188),
    AUC0T = list(n = 2, min = 12294.00, max = 14445.00, mean = 13369.50, std = 1520.99),
    AUC0I = list(n = 2, min = 12972.00, max = 14933.00, mean = 13952.50, std = 1386.64),
    LNCMAX = list(n = 2, min = 7.4000000, max = 7.4600000, mean = 7.4300000, std = 0.0424264),
    LNAUC0T = list(n = 2, min = 9.4200000, max = 9.5800000, mean = 9.5000000, std = 0.1131371),
    LNAUC0I = list(n = 2, min = 9.4700000, max = 9.6100000, mean = 9.5400000, std = 0.0989949)
  ),
  "2" = list(
    CMAX = list(n = 2, min = 1481.00, max = 1837.00, mean = 1659.00, std = 251.7300141),
    AUC0T = list(n = 2, min = 12516.00, max = 15299.00, mean = 13907.50, std = 1967.88),
    AUC0I = list(n = 2, min = 13185.00, max = 16209.00, mean = 14697.00, std = 2138.29),
    LNCMAX = list(n = 2, min = 7.3000000, max = 7.5200000, mean = 7.4100000, std = 0.1555635),
    LNAUC0T = list(n = 2, min = 9.4300000, max = 9.6400000, mean = 9.5350000, std = 0.1484924),
    LNAUC0I = list(n = 2, min = 9.4900000, max = 9.6900000, mean = 9.5900000, std = 0.1414214)
  ),
  "3" = list(
    CMAX = list(n = 2, min = 1780.00, max = 2073.00, mean = 1926.50, std = 207.1822869),
    AUC0T = list(n = 2, min = 15184.00, max = 15371.00, mean = 15277.50, std = 132.2289681),
    AUC0I = list(n = 2, min = 15691.00, max = 16032.00, mean = 15861.50, std = 241.1234124),
    LNCMAX = list(n = 2, min = 7.4800000, max = 7.6400000, mean = 7.5600000, std = 0.1131371),
    LNAUC0T = list(n = 2, min = 9.6300000, max = 9.6400000, mean = 9.6350000, std = 0.0070711),
    LNAUC0I = list(n = 2, min = 9.6600000, max = 9.6800000, mean = 9.6700000, std = 0.0141421)
  ),
  "4" = list(
    CMAX = list(n = 2, min = 1374.00, max = 1629.00, mean = 1501.50, std = 180.3122292),
    AUC0T = list(n = 2, min = 11063.00, max = 13982.00, mean = 12522.50, std = 2064.04),
    AUC0I = list(n = 2, min = 11668.00, max = 14650.00, mean = 13159.00, std = 2108.59),
    LNCMAX = list(n = 2, min = 7.2300000, max = 7.4000000, mean = 7.3150000, std = 0.1202082),
    LNAUC0T = list(n = 2, min = 9.3100000, max = 9.5500000, mean = 9.4300000, std = 0.1697056),
    LNAUC0I = list(n = 2, min = 9.3600000, max = 9.5900000, mean = 9.4750000, std = 0.1626346)
  ),
  "5" = list(
    CMAX = list(n = 2, min = 1385.00, max = 1555.00, mean = 1470.00, std = 120.2081528),
    AUC0T = list(n = 2, min = 11852.00, max = 13971.00, mean = 12911.50, std = 1498.36),
    AUC0I = list(n = 2, min = 12550.00, max = 14557.00, mean = 13553.50, std = 1419.16),
    LNCMAX = list(n = 2, min = 7.2300000, max = 7.3500000, mean = 7.2900000, std = 0.0848528),
    LNAUC0T = list(n = 2, min = 9.3800000, max = 9.5400000, mean = 9.4600000, std = 0.1131371),
    LNAUC0I = list(n = 2, min = 9.4400000, max = 9.5900000, mean = 9.5150000, std = 0.1060660)
  ),
  "6" = list(
    CMAX = list(n = 2, min = 1522.00, max = 1756.00, mean = 1639.00, std = 165.4629868),
    AUC0T = list(n = 2, min = 13838.00, max = 15376.00, mean = 14607.00, std = 1087.53),
    AUC0I = list(n = 2, min = 14343.00, max = 15964.00, mean = 15153.50, std = 1146.22),
    LNCMAX = list(n = 2, min = 7.3300000, max = 7.4700000, mean = 7.4000000, std = 0.0989949),
    LNAUC0T = list(n = 2, min = 9.5400000, max = 9.6400000, mean = 9.5900000, std = 0.0707107),
    LNAUC0I = list(n = 2, min = 9.5700000, max = 9.6800000, mean = 9.6250000, std = 0.0777817)
  ),
  "7" = list(
    CMAX = list(n = 2, min = 1566.00, max = 1643.00, mean = 1604.50, std = 54.4472222),
    AUC0T = list(n = 2, min = 12361.00, max = 13442.00, mean = 12901.50, std = 764.3824305),
    AUC0I = list(n = 2, min = 12979.00, max = 14068.00, mean = 13523.50, std = 770.0392847),
    LNCMAX = list(n = 2, min = 7.3600000, max = 7.4000000, mean = 7.3800000, std = 0.0282843),
    LNAUC0T = list(n = 2, min = 9.4200000, max = 9.5100000, mean = 9.4650000, std = 0.0636396),
    LNAUC0I = list(n = 2, min = 9.4700000, max = 9.5500000, mean = 9.5100000, std = 0.0565685)
  ),
  "8" = list(
    CMAX = list(n = 2, min = 1615.00, max = 1939.00, mean = 1777.00, std = 229.1025971),
    AUC0T = list(n = 2, min = 13422.00, max = 14347.00, mean = 13884.50, std = 654.0737726),
    AUC0I = list(n = 2, min = 14001.00, max = 14681.00, mean = 14341.00, std = 480.8326112),
    LNCMAX = list(n = 2, min = 7.3900000, max = 7.5700000, mean = 7.4800000, std = 0.1272792),
    LNAUC0T = list(n = 2, min = 9.5000000, max = 9.5700000, mean = 9.5350000, std = 0.0494975),
    LNAUC0I = list(n = 2, min = 9.5500000, max = 9.5900000, mean = 9.5700000, std = 0.0282843)
  ),
  "9" = list(
    CMAX = list(n = 2, min = 1475.00, max = 1759.00, mean = 1617.00, std = 200.8183259),
    AUC0T = list(n = 2, min = 12410.00, max = 15804.00, mean = 14107.00, std = 2399.92),
    AUC0I = list(n = 2, min = 12915.00, max = 16565.00, mean = 14740.00, std = 2580.94),
    LNCMAX = list(n = 2, min = 7.3000000, max = 7.4700000, mean = 7.3850000, std = 0.1202082),
    LNAUC0T = list(n = 2, min = 9.4300000, max = 9.6700000, mean = 9.5500000, std = 0.1697056),
    LNAUC0I = list(n = 2, min = 9.4700000, max = 9.7200000, mean = 9.5950000, std = 0.1767767)
  ),
  "10" = list(
    CMAX = list(n = 2, min = 1388.00, max = 1483.00, mean = 1435.50, std = 67.1751442),
    AUC0T = list(n = 2, min = 11711.00, max = 13310.00, mean = 12510.50, std = 1130.66),
    AUC0I = list(n = 2, min = 12544.00, max = 13985.00, mean = 13264.50, std = 1018.94),
    LNCMAX = list(n = 2, min = 7.2400000, max = 7.3000000, mean = 7.2700000, std = 0.0424264),
    LNAUC0T = list(n = 2, min = 9.3700000, max = 9.5000000, mean = 9.4350000, std = 0.0919239),
    LNAUC0I = list(n = 2, min = 9.4400000, max = 9.5500000, mean = 9.4950000, std = 0.0777817)
  ),
  "11" = list(
    CMAX = list(n = 2, min = 1127.00, max = 1682.00, mean = 1404.50, std = 392.4442636),
    AUC0T = list(n = 2, min = 9353.00, max = 15371.00, mean = 12362.00, std = 4255.37),
    AUC0I = list(n = 2, min = 9750.00, max = 16029.00, mean = 12889.50, std = 4439.92),
    LNCMAX = list(n = 2, min = 7.0300000, max = 7.4300000, mean = 7.2300000, std = 0.2828427),
    LNAUC0T = list(n = 2, min = 9.1400000, max = 9.6400000, mean = 9.3900000, std = 0.3535534),
    LNAUC0I = list(n = 2, min = 9.1900000, max = 9.6800000, mean = 9.4350000, std = 0.3464823)
  ),
  "12" = list(
    CMAX = list(n = 2, min = 1247.00, max = 1542.00, mean = 1394.50, std = 208.5965005),
    AUC0T = list(n = 2, min = 10609.00, max = 15015.00, mean = 12812.00, std = 3115.51),
    AUC0I = list(n = 2, min = 11093.00, max = 15757.00, mean = 13425.00, std = 3297.95),
    LNCMAX = list(n = 2, min = 7.1300000, max = 7.3400000, mean = 7.2350000, std = 0.1484924),
    LNAUC0T = list(n = 2, min = 9.2700000, max = 9.6200000, mean = 9.4450000, std = 0.2474874),
    LNAUC0I = list(n = 2, min = 9.3100000, max = 9.6700000, mean = 9.4900000, std = 0.2545584)
  ),
  "13" = list(
    CMAX = list(n = 2, min = 1235.00, max = 1605.00, mean = 1420.00, std = 261.6295090),
    AUC0T = list(n = 2, min = 9723.00, max = 15428.00, mean = 12575.50, std = 4034.04),
    AUC0I = list(n = 2, min = 10375.00, max = 16308.00, mean = 13341.50, std = 4195.26),
    LNCMAX = list(n = 2, min = 7.1200000, max = 7.3800000, mean = 7.2500000, std = 0.1838478),
    LNAUC0T = list(n = 2, min = 9.1800000, max = 9.6400000, mean = 9.4100000, std = 0.3252691),
    LNAUC0I = list(n = 2, min = 9.2500000, max = 9.7000000, mean = 9.4750000, std = 0.3181981)
  ),
  "14" = list(
    CMAX = list(n = 2, min = 1598.00, max = 1718.00, mean = 1658.00, std = 84.8528137),
    AUC0T = list(n = 2, min = 14977.00, max = 17803.00, mean = 16390.00, std = 1998.28),
    AUC0I = list(n = 2, min = 15916.00, max = 18870.00, mean = 17393.00, std = 2088.79),
    LNCMAX = list(n = 2, min = 7.3800000, max = 7.4500000, mean = 7.4150000, std = 0.0494975),
    LNAUC0T = list(n = 2, min = 9.6100000, max = 9.7900000, mean = 9.7000000, std = 0.1272792),
    LNAUC0I = list(n = 2, min = 9.6800000, max = 9.8500000, mean = 9.7650000, std = 0.1202082)
  )
)

# =============================================================================
# SUBJECT-LEVEL STATISTICS CALCULATION AND VALIDATION
# =============================================================================

#' Calculate and Validate Subject-Level Statistics
#' @param be_data Bioequivalence data
#' @param expected_stats Expected statistics for validation
#' @param tolerance Tolerance for numerical comparisons
calculate_and_validate_subject_stats <- function(be_data, expected_stats, tolerance = 0.01) {
  
  cat("📊 SUBJECT-LEVEL STATISTICS CALCULATION & VALIDATION\n")
  cat("====================================================\n\n")
  
  subjects <- sort(unique(be_data$Subject))
  validation_results <- list()
  overall_pass <- TRUE
  
  for (subj in subjects) {
    subj_str <- as.character(subj)
    cat("---------------------------------- SUBJECT=", subj, " ---------------------------------\n", sep="")
    cat(" Parameter       N  Minimum   Maximum      Mean   Std Dev   Status\n")
    cat(" ---------------------------------------------------------------------------\n")
    
    # Get subject data
    subj_data <- be_data[be_data$Subject == subj, ]
    validation_results[[subj_str]] <- list()
    
    # Parameters to analyze
    parameters <- c("Cmax", "AUC0t", "AUC0inf")
    param_display <- c("CMAX", "AUC0T", "AUC0I")
    
    # Calculate statistics for each parameter
    for (i in seq_along(parameters)) {
      param <- parameters[i]
      param_disp <- param_display[i]
      
      if (param %in% colnames(subj_data)) {
        values <- subj_data[[param]]
        values <- values[!is.na(values)]
        
        if (length(values) > 0) {
          # Calculate actual statistics
          actual_stats <- list(
            n = length(values),
            min = min(values),
            max = max(values),
            mean = mean(values),
            std = if (length(values) > 1) sd(values) else 0
          )
          
          # Validate against expected values
          status <- "✅ PASS"
          if (subj_str %in% names(expected_stats) && param_disp %in% names(expected_stats[[subj_str]])) {
            expected <- expected_stats[[subj_str]][[param_disp]]
            
            # Check each statistic
            checks <- c(
              abs(actual_stats$min - expected$min) < tolerance * expected$min,
              abs(actual_stats$max - expected$max) < tolerance * expected$max,
              abs(actual_stats$mean - expected$mean) < tolerance * expected$mean,
              abs(actual_stats$std - expected$std) < tolerance * expected$std
            )
            
            if (!all(checks)) {
              status <- "❌ FAIL"
              overall_pass <- FALSE
            }
          } else {
            status <- "⚠️ NO REF"
          }
          
          validation_results[[subj_str]][[param_disp]] <- list(
            actual = actual_stats,
            status = status
          )
          
          cat(sprintf(" %-12s %d %8.2f %9.2f %10.2f %10.7f   %s\n",
                      param_disp, actual_stats$n, actual_stats$min, actual_stats$max, 
                      actual_stats$mean, actual_stats$std, status))
        }
      }
    }
    
    # Calculate and display log-transformed statistics
    log_params <- c("Cmax", "AUC0t", "AUC0inf")
    log_display <- c("LNCMAX", "LNAUC0T", "LNAUC0I")
    
    for (i in seq_along(log_params)) {
      param <- log_params[i]
      param_disp <- log_display[i]
      
      if (param %in% colnames(subj_data)) {
        values <- subj_data[[param]]
        values <- values[!is.na(values) & values > 0]
        
        if (length(values) > 0) {
          log_values <- log(values)
          
          # Calculate actual statistics
          actual_stats <- list(
            n = length(log_values),
            min = min(log_values),
            max = max(log_values),
            mean = mean(log_values),
            std = if (length(log_values) > 1) sd(log_values) else 0
          )
          
          # Validate against expected values
          status <- "✅ PASS"
          if (subj_str %in% names(expected_stats) && param_disp %in% names(expected_stats[[subj_str]])) {
            expected <- expected_stats[[subj_str]][[param_disp]]
            
            # Check each statistic
            checks <- c(
              abs(actual_stats$min - expected$min) < tolerance,
              abs(actual_stats$max - expected$max) < tolerance,
              abs(actual_stats$mean - expected$mean) < tolerance,
              abs(actual_stats$std - expected$std) < tolerance
            )
            
            if (!all(checks)) {
              status <- "❌ FAIL"
              overall_pass <- FALSE
            }
          } else {
            status <- "⚠️ NO REF"
          }
          
          validation_results[[subj_str]][[param_disp]] <- list(
            actual = actual_stats,
            status = status
          )
          
          cat(sprintf(" %-12s %d %8.7f %9.7f %10.7f %10.7f   %s\n",
                      param_disp, actual_stats$n, actual_stats$min, actual_stats$max, 
                      actual_stats$mean, actual_stats$std, status))
        }
      }
    }
    
    cat(" ---------------------------------------------------------------------------\n")
  }
  
  return(list(results = validation_results, overall_pass = overall_pass))
}

# Calculate and validate subject-level statistics
validation_result <- calculate_and_validate_subject_stats(be_data, expected_subject_stats)

# =============================================================================
# BIOEQUIVALENCE ANALYSIS
# =============================================================================

cat("\n⚖️  Performing bioequivalence analysis...\n")
be_results <- perform_be_analysis(
  data = be_data,
  design = "2x2x2",
  regulatory_standard = "ICH",
  alpha = ALPHA,
  be_limits = BE_LIMITS,
  parameters = PRIMARY_PARAMS
)

cat("   Analysis completed successfully\n\n")

# =============================================================================
# RESULTS DISPLAY
# =============================================================================

cat("📋 BIOEQUIVALENCE ANALYSIS RESULTS\n")
cat("===================================\n\n")

# Basic study information
cat("🏛️  Study Information:\n")
cat(sprintf("   Design: %s\n", be_results$design))
cat(sprintf("   Number of subjects: %d\n", be_results$n_subjects))
cat(sprintf("   Regulatory standard: %s\n", REGULATORY_STANDARD))

# Confidence intervals
if (!is.null(be_results$confidence_intervals)) {
  cat("\n📊 Confidence Intervals (90% CI):\n")
  for (param in names(be_results$confidence_intervals)) {
    ci <- be_results$confidence_intervals[[param]]
    be_status <- if (ci$ci_lower >= 80 && ci$ci_upper <= 125) "✅ BIOEQUIVALENT" else "❌ NOT BIOEQUIVALENT"
    cat(sprintf("   %s: %.1f%% (%.3f%% - %.3f%%) - %s\n", 
                param, ci$point_estimate, ci$ci_lower, ci$ci_upper, be_status))
  }
}

# Expected confidence interval results validation
expected_ci_results <- list(
  Cmax = list(lower = 97.586, upper = 115.644),
  AUC0t = list(lower = 94.340, upper = 120.059),
  AUC0inf = list(lower = 94.686, upper = 119.967)
)

cat("\n🔍 CONFIDENCE INTERVAL VALIDATION:\n")
ci_validation_passed <- TRUE

for (param in names(expected_ci_results)) {
  if (param %in% names(be_results$confidence_intervals)) {
    actual_ci <- be_results$confidence_intervals[[param]]
    expected_ci <- expected_ci_results[[param]]
    
    lower_match <- abs(actual_ci$ci_lower - expected_ci$lower) < 0.1
    upper_match <- abs(actual_ci$ci_upper - expected_ci$upper) < 0.1
    
    if (lower_match && upper_match) {
      cat(sprintf("   %s: ✅ PASS (%.3f - %.3f matches expected %.3f - %.3f)\n",
                  param, actual_ci$ci_lower, actual_ci$ci_upper, 
                  expected_ci$lower, expected_ci$upper))
    } else {
      cat(sprintf("   %s: ❌ FAIL (%.3f - %.3f, expected %.3f - %.3f)\n",
                  param, actual_ci$ci_lower, actual_ci$ci_upper,
                  expected_ci$lower, expected_ci$upper))
      ci_validation_passed <- FALSE
    }
  }
}

# Bioequivalence conclusions
if (!is.null(be_results$be_conclusions)) {
  cat("\n✅ Bioequivalence Conclusions:\n")
  for (param in names(be_results$be_conclusions)) {
    conclusion <- if (be_results$be_conclusions[[param]]) "BIOEQUIVALENT" else "NOT BIOEQUIVALENT"
    cat(sprintf("   %s: %s\n", param, conclusion))
  }
}

# Overall conclusion
all_be <- all(unlist(be_results$be_conclusions), na.rm = TRUE)
overall_conclusion <- if (all_be) "✅ OVERALL BIOEQUIVALENT" else "❌ NOT BIOEQUIVALENT"

cat(sprintf("\n🎯 Overall Conclusion: %s\n", overall_conclusion))

# =============================================================================
# FINAL TEST SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🧪 COMPREHENSIVE TEST SUMMARY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\n📊 Subject Statistics Validation:\n")
if (validation_result$overall_pass) {
  cat("   ✅ ALL SUBJECT STATISTICS MATCH EXPECTED VALUES\n")
  cat("   ✅ NCA calculations are accurate\n")
  cat("   ✅ Statistical summaries are correct\n")
} else {
  cat("   ❌ SOME SUBJECT STATISTICS DO NOT MATCH\n")
  cat("   ⚠️  NCA calculations may need review\n")
}

cat("\n📊 Confidence Interval Validation:\n")
if (ci_validation_passed) {
  cat("   ✅ ALL CONFIDENCE INTERVALS MATCH EXPECTED VALUES\n")
  cat("   ✅ BE statistical analysis is correct\n")
} else {
  cat("   ❌ CONFIDENCE INTERVALS DO NOT MATCH EXPECTED\n")
  cat("   ⚠️  BE statistical calculations need review\n")
}

overall_test_pass <- validation_result$overall_pass && ci_validation_passed

cat(sprintf("\n🎯 OVERALL TEST RESULT: %s\n", 
            if (overall_test_pass) "✅ ALL VALIDATIONS PASSED" else "❌ VALIDATION FAILURES DETECTED"))

if (overall_test_pass) {
  cat("   🎉 BioEQ calculations are validated and accurate!\n")
  cat("   📊 Ready for production use\n")
} else {
  cat("   ⚠️  Review calculations before production use\n")
  cat("   🔧 Check NCA and BE analysis implementations\n")
}

cat("\n📊 Test completed!\n")
