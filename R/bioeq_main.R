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
  required_modules = c("utils.R", "nca_functions.R", "nca_pknca.R", "be_analysis.R", "statistics.R", "plotting.R"),
  optional_modules = c()
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
  # Trimmed to what R/ and shiny/ actually call (verified via repo-wide grep,
  # 2026-08 dead code audit — see bioeq-dead-code-audit-2026-08 memory).
  # plotrix/ICSNP/coin/gdata/reshape2/png were a stale hard requirement from
  # an earlier version of the analysis code and are unused today; keeping
  # them here made init_bioeq() fail outright on a machine that only has the
  # Shiny app's own dependencies installed.
  required_packages <- c("nlme", "ggplot2", "dplyr")

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
  cat("📊 NON-COMPARTMENTAL ANALYSIS (via the PKNCA package):\n")
  cat("  • perform_nca_analysis() - AUC, Cmax/Tmax, lambda_z/half-life per profile\n\n")
  
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
  cat("  • validate_be_data()  - Data validation (requires design)\n\n")
  
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

# Auto-initialize when sourced
if (interactive()) {
  init_bioeq()
}
