#!/usr/bin/env Rscript
# install_dependencies.R
# Install all required packages for BioEQ

cat("Installing BioEQ dependencies...\n")

# Set CRAN repository
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# ---------------------------------------------------------------------------
# REQUIRED packages — app will not function without these
# ---------------------------------------------------------------------------
required_packages <- c(
  # Shiny UI
  "shiny",
  "shinydashboard",
  "bslib",
  "shinyjs",
  "DT",
  "shinycssloaders",
  # Data I/O
  "readr",
  "readxl",
  "writexl",
  # Data manipulation
  "dplyr",
  "tidyr",
  "reshape2",
  # Plotting
  "ggplot2",
  "plotly",
  "htmlwidgets",
  "gridExtra",
  "scales",
  # Statistics / BE analysis
  "nlme",
  "lme4",
  "lmerTest",
  "emmeans",
  "replicateBE",
  "PowerTOST",
  # Utilities
  "digest",
  "zip"
)

# ---------------------------------------------------------------------------
# REPORT packages — needed for PDF/Word/HTML report export only.
# The app runs fully without these; a warning is shown in-app when absent.
# ---------------------------------------------------------------------------
report_packages <- c(
  "rmarkdown",
  "knitr",
  "officer",    # Word (.docx) reports
  "flextable"   # Word table formatting
)

# ---------------------------------------------------------------------------
# OPTIONAL analysis enhancements — app falls back gracefully if missing.
#   - dtw       : DTW-based pairwise profile comparison in Anomaly Detection
#                 (falls back to RMSE if absent)
#   - pbkrtest  : Kenward-Roger denominator df for mixed-effects ANOVA
#                 (falls back to Satterthwaite if absent)
# ---------------------------------------------------------------------------
analysis_optional_packages <- c(
  "dtw",
  "pbkrtest"
)

# ---------------------------------------------------------------------------
# DEVELOPMENT / TESTING packages — only needed when running tests
# ---------------------------------------------------------------------------
dev_packages <- c(
  "testthat"
)

# ---------------------------------------------------------------------------
# Function to install packages if not already installed
# ---------------------------------------------------------------------------
install_if_missing <- function(packages, package_type = "required") {
  cat(sprintf("\nChecking %s packages...\n", package_type))

  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]

  if (length(missing_packages) > 0) {
    cat(sprintf("Installing %d %s package(s): %s\n",
                length(missing_packages),
                package_type,
                paste(missing_packages, collapse = ", ")))

    tryCatch({
      install.packages(missing_packages, dependencies = TRUE)
      cat(sprintf("\u2713 Successfully installed %s packages\n", package_type))
    }, error = function(e) {
      cat(sprintf("\u2717 Error installing %s packages: %s\n", package_type, e$message))
      if (package_type == "required") {
        stop("Failed to install required packages. Please install manually.")
      }
    })
  } else {
    cat(sprintf("\u2713 All %s packages already installed\n", package_type))
  }
}

# Install all groups
install_if_missing(required_packages,           "required")
install_if_missing(report_packages,             "report (optional)")
install_if_missing(analysis_optional_packages,  "analysis enhancements (optional)")
install_if_missing(dev_packages,                "development/testing (optional)")

cat(paste0("\n", strrep("=", 52), "\n"))
cat("BioEQ dependency installation complete!\n")
cat("\nTo launch the Shiny app:\n")
cat("  Rscript launch_app.R\n")
cat(paste0(strrep("=", 52), "\n"))

# Verify all required packages can be loaded
cat("\nVerifying required packages...\n")
failed <- character(0)
for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE, quietly = TRUE)
    cat(sprintf("\u2713 %s\n", pkg))
  }, error = function(e) {
    cat(sprintf("\u2717 %s  -- %s\n", pkg, e$message))
    failed <<- c(failed, pkg)
  })
}

if (length(failed) > 0) {
  cat(sprintf("\n\u26A0  %d required package(s) failed to load: %s\n",
              length(failed), paste(failed, collapse = ", ")))
  cat("Please install them manually and re-run this script.\n")
} else {
  cat("\n\u2713 All required packages verified.\n")
}
