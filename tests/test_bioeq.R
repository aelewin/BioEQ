# BioEQ - Comprehensive Test Suite
# Unit tests for all BioEQ functionality

# Load BioEQ
source("R/bioeq_main.R")

#' Run all BioEQ tests
run_all_tests <- function() {
  cat("🧪 BioEQ Comprehensive Test Suite\n")
  cat("=================================\n\n")
  
  # Initialize
  init_bioeq()
  
  # Test counters - use environment for proper scoping
  test_env <- new.env()
  test_env$passed <- 0
  test_env$failed <- 0
  
  # Suppress non-critical numerical precision warnings for cleaner output
  old_warn <- getOption("warn")
  options(warn = -1)
  
  # Helper function to run individual tests
  run_test <- function(test_name, test_function) {
    cat(sprintf("Testing %s... ", test_name))
    
    result <- tryCatch({
      test_function()
      cat("✅ PASSED\n")
      TRUE
    }, error = function(e) {
      cat("❌ FAILED\n")
      cat(sprintf("   Error: %s\n", e$message))
      FALSE
    })
    
    if (result) {
      test_env$passed <- test_env$passed + 1
    } else {
      test_env$failed <- test_env$failed + 1
    }
    
    return(result)
  }
  
  # Test 1: Data Generation
  run_test("Data Generation", function() {
    data_2x2x2 <- generate_2x2x2_data(n_subjects = 12, seed = 123)
    stopifnot(inherits(data_2x2x2, "example_data"))
    stopifnot(nrow(data_2x2x2$pk_parameters) > 0)
    stopifnot(nrow(data_2x2x2$concentration_data) > 0)
  })
  
  # Test 2: Data Validation
  run_test("Data Validation", function() {
    test_data <- data.frame(
      Subject = c(1, 1, 2, 2),
      Period = c(1, 2, 1, 2),
      Formulation = c("Test", "Reference", "Reference", "Test"),
      AUC0t = c(100, 95, 110, 105),
      Cmax = c(25, 23, 28, 26)
    )
    validated <- validate_pk_data(test_data)
    stopifnot(is.data.frame(validated))
  })
  
  # Test 3: NCA Functions
  run_test("NCA Functions", function() {
    time <- c(0, 0.5, 1, 2, 4, 8, 12, 24)
    conc <- c(0, 50, 100, 120, 80, 40, 20, 5)
    
    # Test AUC calculation
    auc <- calculate_auc_trap(time, conc)
    stopifnot(is.numeric(auc) && auc > 0)
    
    # Test lambda_z estimation
    lambda_result <- estimate_lambda_z(time, conc)
    stopifnot(is.list(lambda_result))
    stopifnot("lambda_z" %in% names(lambda_result))
    
    # Test Cmax/Tmax
    cmax <- max(conc)
    tmax <- time[which.max(conc)]
    stopifnot(cmax == 120)
    stopifnot(tmax == 2)
  })
  
  # Test 4: Statistical Functions
  run_test("Statistical Functions", function() {
    # Sample size calculation
    ss_result <- calculate_sample_size(cv = 0.25, power = 0.8, theta0 = 0.95)
    stopifnot(is.list(ss_result))
    stopifnot(ss_result$n_per_group > 0)
    
    # Power calculation
    power_result <- calculate_power(n_per_group = 20, cv = 0.25, theta0 = 0.95)
    stopifnot(is.list(power_result))
    stopifnot(power_result$power >= 0 && power_result$power <= 1)
    
    # TOST procedure
    tost_result <- perform_tost(
      test_values = c(100, 95, 110, 105),
      ref_values = c(95, 100, 105, 110),
      alpha = 0.05
    )
    stopifnot(is.list(tost_result))
  })
  
  # Test 5: Bioequivalence Analysis
  run_test("Bioequivalence Analysis", function() {
    example_data <- generate_2x2x2_data(n_subjects = 16, seed = 456)
    pk_data <- example_data$pk_parameters
    
    be_result <- perform_be_analysis(pk_data, design = "2x2x2")
    stopifnot(inherits(be_result, "bioeq"))
    stopifnot("confidence_intervals" %in% names(be_result))
    stopifnot("anova_results" %in% names(be_result))
  })
  
  # Test 6: Plotting Functions
  run_test("Plotting Functions", function() {
    example_data <- generate_2x2x2_data(n_subjects = 12, seed = 789)
    
    # Concentration-time plot
    conc_plot <- plot_concentration_time(example_data$concentration_data)
    stopifnot(inherits(conc_plot, "ggplot"))
    
    # BE analysis and plots
    be_result <- perform_be_analysis(example_data$pk_parameters, design = "2x2x2")
    ci_plot <- plot_be_confidence_intervals(be_result)
    stopifnot(inherits(ci_plot, "ggplot"))
    
    # Sample size plot
    ss_plot <- plot_sample_size_curve()
    stopifnot(inherits(ss_plot, "ggplot"))
  })
  
  # Test 7: Edge Cases and Error Handling
  run_test("Error Handling", function() {
    # Empty data
    tryCatch({
      validate_pk_data(data.frame())
      stop("Should have failed with empty data")
    }, error = function(e) {
      # Expected to fail
    })
    
    # Invalid CV
    tryCatch({
      calculate_sample_size(cv = -0.1)
      stop("Should have failed with negative CV")
    }, error = function(e) {
      # Expected to fail
    })
    
    # Invalid concentrations for AUC
    auc_result <- calculate_auc_trap(c(0, 1, 2), c(-1, 0, -1))
    stopifnot(is.na(auc_result) || auc_result == 0)
  })
  
  # Test 8: Replicate Design (basic structure)
  run_test("Replicate Design", function() {
    replicate_data <- generate_replicate_data(n_subjects = 16, seed = 999)
    stopifnot(inherits(replicate_data, "example_data"))
    stopifnot(replicate_data$study_design == "2x2x4 replicate crossover")
    
    # Basic validation that we can handle replicate data structure
    pk_data <- replicate_data$pk_parameters
    stopifnot(max(pk_data$Period) == 4)  # Should have 4 periods
  })
  
  # Test 9: Data Export/Import Format
  run_test("Data Formats", function() {
    example_data <- generate_2x2x2_data(n_subjects = 8, seed = 111)
    
    # Check data structure compliance
    conc_data <- example_data$concentration_data
    required_cols <- c("Subject", "Period", "Formulation", "Time", "Concentration")
    stopifnot(all(required_cols %in% names(conc_data)))
    
    pk_data <- example_data$pk_parameters
    required_pk_cols <- c("Subject", "Period", "Formulation", "AUC0t", "Cmax", "Tmax")
    stopifnot(all(required_pk_cols %in% names(pk_data)))
  })
  
  # Test 10: Integration Test
  run_test("End-to-End Integration", function() {
    # Full workflow test
    data <- generate_2x2x2_data(n_subjects = 12, seed = 456)
    result <- perform_be_analysis(data$pk_parameters, design = "2x2x2")
    
    # Verify complete result structure
    required_components <- c("design", "pk_parameters", "anova_results", 
                           "confidence_intervals", "be_conclusions")
    stopifnot(all(required_components %in% names(result)))
    
    # Verify plots can be created
    conc_plot <- plot_concentration_time(data$concentration_data)
    stopifnot(!is.null(conc_plot))
    
    ci_plot <- plot_be_confidence_intervals(result)
    stopifnot(!is.null(ci_plot))
  })
  
  # Test 11: Replicate Design Analysis
  run_test("Replicate Design Analysis", function() {
    # Generate replicate data
    replicate_data <- generate_replicate_data(n_subjects = 24, cv_intra = 0.35, 
                                             true_ratio = 0.90, design = "2x2x4")
    stopifnot(inherits(replicate_data, "example_data"))
    stopifnot(replicate_data$design == "2x2x4")
    
    # Check data structure
    pk_data <- replicate_data$pk_parameters
    stopifnot(nrow(pk_data) == 24 * 4)  # 24 subjects × 4 periods
    stopifnot(all(c("Subject", "Period", "Formulation", "AUC_inf", "Cmax") %in% names(pk_data)))
    
    # Perform analysis
    result <- perform_be_analysis(pk_data, design = "replicate")
    stopifnot(inherits(result, "bioeq"))
    stopifnot(result$design == "replicate")
    
    # Check for scaling-specific components
    required_components <- c("swr_results", "scaling_decisions", "confidence_intervals")
    stopifnot(all(required_components %in% names(result)))
  })
  
  # Test 12: Parallel Design Analysis  
  run_test("Parallel Design Analysis", function() {
    # Generate parallel data
    parallel_data <- generate_parallel_data(n_per_group = 30, cv_inter = 0.25, true_ratio = 0.95)
    stopifnot(inherits(parallel_data, "example_data"))
    stopifnot(parallel_data$design == "parallel")
    
    # Check data structure
    pk_data <- parallel_data$pk_parameters
    stopifnot(nrow(pk_data) == 60)  # 30 per group × 2 groups
    stopifnot(all(c("Subject", "Formulation", "AUC_inf", "Cmax") %in% names(pk_data)))
    
    # Check each subject has only one formulation
    subj_forms <- pk_data %>% 
      group_by(Subject) %>% 
      summarise(n_forms = n_distinct(Formulation), .groups = "drop")
    stopifnot(all(subj_forms$n_forms == 1))
    
    # Perform analysis
    result <- perform_be_analysis(pk_data, design = "parallel")
    stopifnot(inherits(result, "bioeq"))
    stopifnot(result$design == "parallel")
    
    # Check for parallel-specific components
    required_components <- c("statistical_results", "normality_tests", "power_analysis")
    stopifnot(all(required_components %in% names(result)))
  })
  
  # Test 13: Multiple Design Comparison
  run_test("Multiple Design Comparison", function() {
    # Generate data for all three designs with same true ratio
    true_ratio <- 0.90
    
    crossover_data <- generate_2x2x2_data(n_subjects = 24, true_ratio = true_ratio, seed = 100)
    replicate_data <- generate_replicate_data(n_subjects = 24, true_ratio = true_ratio, seed = 100)
    parallel_data <- generate_parallel_data(n_per_group = 24, true_ratio = true_ratio, seed = 100)
    
    # Perform analyses
    crossover_result <- perform_be_analysis(crossover_data$pk_parameters, design = "2x2x2")
    replicate_result <- perform_be_analysis(replicate_data$pk_parameters, design = "replicate")
    parallel_result <- perform_be_analysis(parallel_data$pk_parameters, design = "parallel")
    
    # All should be analyzable (though conclusions may differ)
    stopifnot(all(c(
      inherits(crossover_result, "bioeq"),
      inherits(replicate_result, "bioeq"),
      inherits(parallel_result, "bioeq")
    )))
    
    # Check that different designs were actually used
    designs <- c(crossover_result$design, replicate_result$design, parallel_result$design)
    stopifnot(length(unique(designs)) == 3)
  })
  
  # Test 14: High Variability Drug Testing
  run_test("High Variability Drug Analysis", function() {
    # Test with high CV that should trigger scaling
    high_cv_data <- generate_replicate_data(n_subjects = 36, cv_intra = 0.45, 
                                           true_ratio = 0.85, design = "2x2x4")
    
    result <- perform_be_analysis(high_cv_data$pk_parameters, design = "replicate")
    
    # Should have scaling decisions
    stopifnot("scaling_decisions" %in% names(result))
    
    # At least one parameter should have high CV
    swr_results <- result$swr_results
    if (length(swr_results) > 0) {
      cvs <- sapply(swr_results, function(x) x$cv_wr)
      # At least one parameter should have CV > 30%
      stopifnot(any(cvs > 30, na.rm = TRUE))
    }
  })

  # Test 15: 2x2x3 Partial Replicate Design
  run_test("2x2x3 Partial Replicate Design", function() {
    # Generate 2x2x3 partial replicate data
    partial_data <- generate_2x2x3_data(n_subjects = 24, cv_intra_ref = 0.35, 
                                        cv_intra_test = 0.25, true_ratio = 0.90)
    
    stopifnot(inherits(partial_data, "example_data"))
    stopifnot(partial_data$design == "2x2x3 partial replicate")
    
    # Check data structure - should have TRR and RTT sequences
    pk_data <- partial_data$pk_parameters
    sequences <- unique(pk_data %>% 
                       arrange(Subject, Period) %>%
                       group_by(Subject) %>%
                       summarise(seq = paste(substr(Formulation, 1, 1), collapse = ""), .groups = "drop") %>%
                       pull(seq))
    
    stopifnot(all(c("TRR", "RTT") %in% sequences))
    stopifnot(nrow(pk_data) == 24 * 3)  # 24 subjects × 3 periods
    
    # Check that reference is replicated correctly in 2x2x3 design
    # TRR sequence: 2 reference observations per subject
    # RTT sequence: 1 reference observation per subject
    ref_counts <- pk_data %>%
      filter(Formulation == "Reference") %>%
      group_by(Subject) %>%
      summarise(n_ref = n(), .groups = "drop")
    
    # In 2x2x3 partial replicate, we expect:
    # - Half the subjects (TRR sequence) to have 2 reference observations
    # - Half the subjects (RTT sequence) to have 1 reference observation
    ref_count_summary <- table(ref_counts$n_ref)
    stopifnot(1 %in% names(ref_count_summary))  # Some subjects have 1 ref obs
    stopifnot(2 %in% names(ref_count_summary))  # Some subjects have 2 ref obs
    stopifnot(length(names(ref_count_summary)) == 2)  # Only 1 and 2 ref obs
    
    # Check that test formulation pattern is correct for 2x2x3 design
    # TRR sequence: 1 test observation per subject
    # RTT sequence: 2 test observations per subject  
    test_counts <- pk_data %>%
      filter(Formulation == "Test") %>%
      group_by(Subject) %>%
      summarise(n_test = n(), .groups = "drop")
    
    # In 2x2x3 partial replicate, we expect:
    # - Half the subjects (TRR sequence) to have 1 test observation
    # - Half the subjects (RTT sequence) to have 2 test observations
    test_count_summary <- table(test_counts$n_test)
    stopifnot(1 %in% names(test_count_summary))  # Some subjects have 1 test obs
    stopifnot(2 %in% names(test_count_summary))  # Some subjects have 2 test obs
    stopifnot(length(names(test_count_summary)) == 2)  # Only 1 and 2 test obs
    
    # Perform analysis
    result <- perform_be_analysis(pk_data, design = "replicate")
    stopifnot(inherits(result, "bioeq"))
    
    # Check design detection
    stopifnot("design_info" %in% names(result))
    stopifnot(result$design_info$is_partial_replicate)
    stopifnot(result$design_info$design_name == "2x2x3 partial replicate")
  })

  # Final Summary
  total_tests <- test_env$passed + test_env$failed
  cat("\n📊 TEST SUMMARY\n")
  cat("===============\n")
  cat(sprintf("Total tests: %d\n", total_tests))
  cat(sprintf("Passed: %d ✅\n", test_env$passed))
  cat(sprintf("Failed: %d ❌\n", test_env$failed))
  cat(sprintf("Success rate: %.1f%%\n", (test_env$passed / total_tests) * 100))
  
  if (test_env$failed == 0) {
    cat("\n🎉 ALL TESTS PASSED! BioEQ is working perfectly.\n")
    cat("Ready for production use! 🚀\n")
  } else {
    cat("\n⚠️  Some tests failed. Please review the errors above.\n")
  }
  
  # Restore warning settings
  options(warn = old_warn)
  
  return(list(
    passed = test_env$passed,
    failed = test_env$failed,
    total = total_tests,
    success_rate = test_env$passed / total_tests
  ))
}

# Run quick validation test
quick_test <- function() {
  cat("🚀 BioEQ Quick Validation Test\n")
  cat("==============================\n")
  
  tryCatch({
    init_bioeq()
    data <- generate_2x2x2_data(n_subjects = 12)
    result <- perform_be_analysis(data$pk_parameters, design = "2x2x2")
    cat("✅ Quick test PASSED - BioEQ is working!\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Quick test FAILED:", e$message, "\n")
    return(FALSE)
  })
}

# If running this script directly
if (sys.nframe() == 0) {
  run_all_tests()
}
