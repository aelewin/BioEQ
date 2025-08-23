# BioEQ - Main Application Entry Point
# Modern Bioequivalence Analysis Application


#' BioEQ: Modern Bioequivalence Analysis
#'
#' A comprehensive application for bioequivalence and bioavailability analysis
#' @docType package
#' @name BioEQ
NULL

# =============================================================================
# CONFIGURATION MANAGEMENT
# =============================================================================

#' Default BioEQ Configuration
#' @keywords internal
.bioeq_config <- list(
  # Analysis defaults
  default_alpha = 0.05,
  default_be_limits = c(0.8, 1.25),
  default_power = 0.8,
  default_design = "2x2x2",
  
  # Logging settings
  log_level = "INFO",  # DEBUG, INFO, WARNING, ERROR
  show_progress = TRUE,
  use_emoji = TRUE,
  
  # Version info
  bioeq_version = "1.0.0",
  
  # Module settings
  required_modules = c("utils.R", "nca_functions.R", "be_analysis.R", "statistics.R", "plotting.R"),
  optional_modules = c(),
  
  # Demo settings
  demo_cache_enabled = TRUE,
  demo_cleanup_enabled = TRUE
)

#' Get BioEQ Configuration
#' @param key Configuration key (optional)
#' @export
get_bioeq_config <- function(key = NULL) {
  if (is.null(key)) {
    return(.bioeq_config)
  }
  return(.bioeq_config[[key]])
}

#' Set BioEQ Configuration
#' @param key Configuration key
#' @param value Configuration value
#' @export
set_bioeq_config <- function(key, value) {
  if (!key %in% names(.bioeq_config)) {
    warning("Unknown configuration key: ", key)
  }
  .bioeq_config[[key]] <<- value
  invisible(TRUE)
}

# =============================================================================
# LOGGING SYSTEM
# =============================================================================

#' Log levels
#' @keywords internal
.log_levels <- list(
  DEBUG = 1,
  INFO = 2,
  WARNING = 3,
  ERROR = 4
)

#' BioEQ Logger
#' @param message Log message
#' @param level Log level (DEBUG, INFO, WARNING, ERROR)
#' @param use_emoji Whether to use emoji (default from config)
#' @keywords internal
bioeq_log <- function(message, level = "INFO", use_emoji = NULL) {
  if (is.null(use_emoji)) {
    use_emoji <- get_bioeq_config("use_emoji")
  }
  
  current_level <- .log_levels[[get_bioeq_config("log_level")]]
  message_level <- .log_levels[[level]]
  
  if (message_level < current_level) {
    return(invisible(NULL))
  }
  
  # Emoji mapping
  emoji_map <- list(
    DEBUG = "🔍",
    INFO = "ℹ️",
    WARNING = "⚠️",
    ERROR = "❌"
  )
  
  prefix <- if (use_emoji && level %in% names(emoji_map)) {
    paste0(emoji_map[[level]], " ")
  } else {
    paste0("[", level, "] ")
  }
  
  cat(prefix, message, "\n", sep = "")
}

#' Progress Bar Helper
#' @param total Total number of steps
#' @param title Progress bar title
#' @keywords internal
create_progress_bar <- function(total, title = "Progress") {
  if (!get_bioeq_config("show_progress")) {
    return(NULL)
  }
  
  if (requireNamespace("progress", quietly = TRUE)) {
    return(progress::progress_bar$new(
      format = paste0("  ", title, " [:bar] :percent :etas"),
      total = total,
      clear = FALSE,
      width = 60
    ))
  } else {
    bioeq_log("Install 'progress' package for progress bars", "DEBUG")
    return(NULL)
  }
}

# =============================================================================
# DEPENDENCY MANAGEMENT
# =============================================================================

#' Load Required Packages with Progress Indicator
#' @keywords internal
load_bioeq_dependencies <- function() {
  required_packages <- c(
    "nlme", "ggplot2", "dplyr", "reshape2", 
    "plotrix", "ICSNP", "coin", "gdata", "png"
  )
  
  optional_packages <- c("progress", "plotly")
  
  bioeq_log("Loading required packages...", "INFO")
  pb <- create_progress_bar(length(required_packages), "Loading packages")
  
  failed_packages <- character(0)
  
  for (pkg in required_packages) {
    if (!is.null(pb)) pb$tick()
    
    if (!requireNamespace(pkg, quietly = TRUE)) {
      failed_packages <- c(failed_packages, pkg)
      bioeq_log(paste("Missing required package:", pkg), "ERROR")
    } else {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      bioeq_log(paste("Loaded:", pkg), "DEBUG")
    }
  }
  
  # Load optional packages silently
  for (pkg in optional_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      bioeq_log(paste("Optional package loaded:", pkg), "DEBUG")
    }
  }
  
  if (length(failed_packages) > 0) {
    stop("Missing required packages: ", paste(failed_packages, collapse = ", "), 
         "\nRun: install.packages(c('", paste(failed_packages, collapse = "', '"), "'))")
  }
  
  bioeq_log("All required packages loaded successfully", "INFO")
  return(TRUE)
}

# Source all BioEQ modules
source_bioeq_modules <- function() {
  module_files <- get_bioeq_config("required_modules")
  
  bioeq_log("Loading BioEQ modules...", "INFO")
  pb <- create_progress_bar(length(module_files), "Loading modules")
  
  failed_modules <- character(0)
  loaded_modules <- character(0)
  
  for (file in module_files) {
    if (!is.null(pb)) pb$tick()
    
    file_path <- file.path("R", file)
    if (file.exists(file_path)) {
      tryCatch({
        source(file_path)
        loaded_modules <- c(loaded_modules, file)
        bioeq_log(paste("Loaded:", file), "DEBUG")
      }, error = function(e) {
        failed_modules <- c(failed_modules, file)
        bioeq_log(paste("Failed to load", file, ":", e$message), "ERROR")
      })
    } else {
      failed_modules <- c(failed_modules, file)
      bioeq_log(paste("Missing module file:", file), "ERROR")
    }
  }
  
  # Source example datasets
  data_file <- file.path("data", "example_datasets.R")
  if (file.exists(data_file)) {
    tryCatch({
      source(data_file)
      bioeq_log("Loaded: example_datasets.R", "DEBUG")
    }, error = function(e) {
      bioeq_log(paste("Failed to load example_datasets.R:", e$message), "WARNING")
    })
  }
  
  if (length(failed_modules) > 0) {
    stop("Failed to load modules: ", paste(failed_modules, collapse = ", "))
  }
  
  bioeq_log(paste("Successfully loaded", length(loaded_modules), "modules"), "INFO")
  return(loaded_modules)
}

#' Initialize BioEQ Application
#'
#' @export
init_bioeq <- function() {
  cat("🧬 BioEQ - Modern Bioequivalence Analysis Application\n")
  cat("Based on  algorithms, modernized for R 4.4+\n\n")
  
  # Load dependencies first
  load_bioeq_dependencies()
  
  # Load all modules
  source_bioeq_modules()
  
  cat("\n✅ BioEQ initialized successfully!\n")
  cat("📖 Use help_bioeq() for available functions\n")
}

#' Show BioEQ Help
#'
#' @export
help_bioeq <- function() {
  cat("🧬 BioEQ - Available Functions:\n\n")
  cat("📊 NON-COMPARTMENTAL ANALYSIS:\n")
  cat("  • calculate_nca()     - Perform NCA analysis\n")
  cat("  • estimate_lambda_z() - Lambda_z estimation\n")
  cat("  • calculate_auc()     - AUC calculations\n\n")
  
  cat("🔬 BIOEQUIVALENCE ANALYSIS:\n")
  cat("  • perform_be_analysis() - Main BE analysis function\n")
  cat("  • analyze_be_study()   - Quick BE analysis wrapper\n")
  cat("  • be_crossover_2x2x2() - 2x2x2 crossover analysis\n")
  cat("  • be_replicate()       - Replicate design analysis\n")
  cat("  • be_parallel()        - Parallel design analysis\n\n")
  
  cat("📈 STATISTICAL FUNCTIONS:\n")
  cat("  • calculate_power()   - Power analysis\n")
  cat("  • sample_size_be()    - Sample size calculation\n")
  cat("  • confidence_intervals() - 90% CI calculation\n\n")
  
  cat("📋 DATA UTILITIES:\n")
  cat("  • validate_be_data()  - Data validation (requires design)\n")
  cat("  • format_be_data()    - Data formatting\n")
  cat("  • load_example_data() - Load example datasets\n\n")
  
  cat("📊 PLOTTING & REPORTING:\n")
  cat("  • plot_concentration_time() - Concentration-time plots\n")
  cat("  • plot_individual_profiles() - Individual profiles\n")
  cat("  • generate_be_report() - Comprehensive report\n\n")
}

#' Quick Start - Analyze BE Study
#'
#' @param data Data frame with BE study data
#' @param design Study design ("2x2x2", "replicate", "parallel")
#' @param alpha Significance level (default 0.05)
#' @param be_limits Bioequivalence limits (default c(0.8, 1.25))
#' @export
analyze_be_study <- function(data, design = "2x2x2", alpha = 0.05, be_limits = c(0.8, 1.25)) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!design %in% c("2x2x2", "replicate", "parallel")) {
    stop("design must be one of: 2x2x2, replicate, parallel")
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a numeric value between 0 and 1")
  }
  if (!is.numeric(be_limits) || length(be_limits) != 2 || be_limits[1] >= be_limits[2]) {
    stop("be_limits must be a numeric vector of length 2 with lower < upper")
  }
  
  bioeq_log("Starting Bioequivalence Analysis...", "INFO")
  bioeq_log(paste("Design:", design), "INFO")
  bioeq_log(paste("BE Limits:", paste(be_limits, collapse = " - ")), "INFO")
  
  # Use the new main analysis function
  result <- perform_be_analysis(
    data = data,
    design = design,
    alpha = alpha,
    be_limits = be_limits
  )
  
  # Add metadata
  result$metadata <- list(
    design = design,
    alpha = alpha,
    be_limits = be_limits,
    analysis_date = Sys.time(),
    bioeq_version = get_bioeq_config("bioeq_version")
  )
  
  bioeq_log("Bioequivalence analysis completed!", "INFO")
  return(result)
}

# =============================================================================
# DEMO FUNCTIONS
# =============================================================================

#' Run Basic Bioequivalence Demo
#' @keywords internal
run_basic_demo <- function() {
  bioeq_log("Running basic bioequivalence analysis demo", "INFO")
  
  cat("📊 BASIC BIOEQUIVALENCE ANALYSIS DEMO\n")
  cat("-------------------------------------\n")
  
  # Generate example data
  demo_data <- load_example_data("standard_2x2x2")
  cat("✓ Generated 2x2x2 crossover study data\n")
  cat("  - Subjects:", demo_data$n_subjects, "\n")
  cat("  - True ratio:", demo_data$true_ratio, "\n")
  cat("  - CV:", demo_data$cv_intra, "\n\n")
  
  # Perform BE analysis
  be_results <- perform_be_analysis(demo_data$pk_parameters, design = "2x2x2")
  cat("✓ Bioequivalence analysis completed\n\n")
  
  # Show results summary
  print(be_results)
  
  # Create plots
  cat("\n📈 Creating visualization plots...\n")
  conc_plot <- plot_concentration_time(demo_data$concentration_data, log_scale = TRUE)
  ci_plot <- plot_be_confidence_intervals(be_results)
  
  cat("✓ Plots created successfully\n\n")
  
  # Cleanup if enabled
  if (get_bioeq_config("demo_cleanup_enabled")) {
    rm(demo_data, be_results, conc_plot, ci_plot, envir = parent.frame())
    bioeq_log("Demo objects cleaned up", "DEBUG")
  }
}

#' Run Sample Size Calculation Demo
#' @keywords internal
run_sample_size_demo <- function() {
  bioeq_log("Running sample size calculation demo", "INFO")
  
  cat("🧮 SAMPLE SIZE CALCULATION DEMO\n")
  cat("-------------------------------\n")
  
  cv_values <- c(0.15, 0.20, 0.25, 0.30)
  cat("Sample size requirements for 80% power:\n")
  
  pb <- create_progress_bar(length(cv_values), "Sample size calculations")
  
  for (cv in cv_values) {
    if (!is.null(pb)) pb$tick()
    ss_result <- calculate_sample_size(cv = cv, power = 0.8, theta0 = 0.95)
    # Handle different return value structures
    n_value <- if ("n_per_group" %in% names(ss_result)) {
      ss_result$n_per_group
    } else if ("n_subjects" %in% names(ss_result)) {
      ss_result$n_subjects
    } else {
      "Unknown"
    }
    cat(sprintf("  CV = %02.0f%%: n = %s per group\n", cv * 100, n_value))
  }
  cat("\n")
}

#' Run Advanced Analysis Demo
#' @keywords internal
run_advanced_demo <- function() {
  bioeq_log("Running advanced analysis demo", "INFO")
  
  cat("🔬 ADVANCED ANALYSIS DEMO\n")
  cat("-------------------------\n")
  
  # Replicate design demo
  cat("📋 Replicate Design (2x2x4) Analysis:\n")
  replicate_data <- generate_replicate_data(n_subjects = 24, cv_intra = 0.35, true_ratio = 0.90)
  cat("  Generated", nrow(replicate_data$pk_parameters), "observations\n")
  
  replicate_results <- perform_be_analysis(replicate_data$pk_parameters, design = "replicate")
  cat("  ✓ Replicate analysis completed (may use scaled BE limits)\n\n")
  
  # Parallel design demo
  cat("📋 Parallel Design Analysis:\n")
  parallel_data <- generate_parallel_data(n_per_group = 30, cv_inter = 0.25, true_ratio = 0.95)
  cat("  Generated", nrow(parallel_data$pk_parameters), "subjects\n")
  
  parallel_results <- perform_be_analysis(parallel_data$pk_parameters, design = "parallel")
  cat("  ✓ Parallel analysis completed\n\n")
  
  # Special scenarios
  cat("📋 Special Scenarios:\n")
  high_cv_data <- load_example_data("high_cv")
  cat("✓ Generated high CV study data (CV =", high_cv_data$cv_intra, ")\n")
  
  failed_data <- load_example_data("failed_be")
  cat("✓ Generated failed BE scenario (ratio =", failed_data$true_ratio, ")\n")
  
  cat("✓ Advanced scenarios ready for analysis\n\n")
  
  # Cleanup if enabled
  if (get_bioeq_config("demo_cleanup_enabled")) {
    rm(replicate_data, replicate_results, parallel_data, parallel_results, 
       high_cv_data, failed_data, envir = parent.frame())
    bioeq_log("Advanced demo objects cleaned up", "DEBUG")
  }
}

#' Run BioEQ Demo
#'
#' @param demo_type Type of demo ("basic", "advanced", "sample_size", "all")
#' @export
run_bioeq_demo <- function(demo_type = "basic") {
  # Input validation
  valid_types <- c("basic", "advanced", "sample_size", "all")
  if (!demo_type %in% valid_types) {
    stop("demo_type must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  bioeq_log(paste("Starting BioEQ demo:", demo_type), "INFO")
  
  cat("🎬 BioEQ Demo:", demo_type, "\n")
  cat("========================\n\n")
  
  if (demo_type %in% c("basic", "all")) {
    run_basic_demo()
  }
  
  if (demo_type %in% c("sample_size", "all")) {
    run_sample_size_demo()
  }
  
  if (demo_type %in% c("advanced", "all")) {
    run_advanced_demo()
  }
  
  cat("🎯 Demo completed! Use the generated data for further analysis.\n")
  bioeq_log("Demo completed successfully", "INFO")
}

#' Test BioEQ Installation
#'
#' @export
test_bioeq <- function() {
  cat("🧪 Testing BioEQ Installation\n")
  cat("=============================\n\n")
  
  tests_passed <- 0
  total_tests <- 0
  
  # Test 1: Load modules
  total_tests <- total_tests + 1
  tryCatch({
    source_bioeq_modules()
    cat("✅ Test 1: Module loading - PASSED\n")
    tests_passed <- tests_passed + 1
  }, error = function(e) {
    cat("❌ Test 1: Module loading - FAILED\n")
    cat("   Error:", e$message, "\n")
  })
  
  # Test 2: Generate example data
  total_tests <- total_tests + 1
  tryCatch({
    test_data <- generate_2x2x2_data(n_subjects = 12)
    cat("✅ Test 2: Data generation - PASSED\n")
    tests_passed <- tests_passed + 1
  }, error = function(e) {
    cat("❌ Test 2: Data generation - FAILED\n")
    cat("   Error:", e$message, "\n")
  })
  
  # Test 3: NCA calculations
  total_tests <- total_tests + 1
  tryCatch({
    time <- c(0, 1, 2, 4, 8, 12, 24)
    conc <- c(0, 100, 150, 120, 80, 40, 10)
    auc <- calculate_auc_trap(time, conc)
    if (is.numeric(auc) && auc > 0) {
      cat("✅ Test 3: NCA calculations - PASSED\n")
      tests_passed <- tests_passed + 1
    } else {
      cat("❌ Test 3: NCA calculations - FAILED (invalid result)\n")
    }
  }, error = function(e) {
    cat("❌ Test 3: NCA calculations - FAILED\n")
    cat("   Error:", e$message, "\n")
  })
  
  # Test 4: BE analysis
  total_tests <- total_tests + 1
  tryCatch({
    if (exists("test_data")) {
      be_result <- perform_be_analysis(test_data$pk_parameters, design = "2x2x2")
      if (inherits(be_result, "bioeq")) {
        cat("✅ Test 4: BE analysis - PASSED\n")
        tests_passed <- tests_passed + 1
      } else {
        cat("❌ Test 4: BE analysis - FAILED (invalid result)\n")
      }
    } else {
      cat("❌ Test 4: BE analysis - SKIPPED (no test data)\n")
    }
  }, error = function(e) {
    cat("❌ Test 4: BE analysis - FAILED\n")
    cat("   Error:", e$message, "\n")
  })
  
  # Test 5: Statistical functions
  total_tests <- total_tests + 1
  tryCatch({
    ss_result <- calculate_sample_size(cv = 0.25, power = 0.8)
    if (is.list(ss_result) && "n_subjects" %in% names(ss_result)) {
      cat("✅ Test 5: Statistical functions - PASSED\n")
      tests_passed <- tests_passed + 1
    } else {
      cat("❌ Test 5: Statistical functions - FAILED (invalid result)\n")
    }
  }, error = function(e) {
    cat("❌ Test 5: Statistical functions - FAILED\n")
    cat("   Error:", e$message, "\n")
  })
  
  # Summary
  cat("\n📊 Test Summary\n")
  cat("---------------\n")
  cat("Tests passed:", tests_passed, "/", total_tests, "\n")
  
  if (tests_passed == total_tests) {
    cat("🎉 All tests PASSED! BioEQ is ready to use.\n")
  } else {
    cat("⚠️  Some tests FAILED. Check the errors above.\n")
  }
  
  return(list(
    tests_passed = tests_passed,
    total_tests = total_tests,
    success_rate = tests_passed / total_tests
  ))
}

# Auto-initialize when sourced
if (interactive()) {
  init_bioeq()
}
