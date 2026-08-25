#!/usr/bin/env Rscript
# install_dependencies.R
# Install all packages required to run BioEQ.
#
# Usage (from the BioEQ project root):
#   Rscript install_dependencies.R
#
# This installs the 31 packages BioEQ loads directly. Their own dependencies
# (~103 further packages) are pulled in automatically by install.packages()
# with dependencies = TRUE. See docs/user_guide.md section 2 for the complete
# categorized package inventory.
#
# To regenerate the categorized inventory after changing dependencies, run:
#   Rscript scripts/generate_dependency_manifest.R

cat("Installing BioEQ dependencies...\n")

# ---------------------------------------------------------------------------
# R version check
#
# The floor is set by the current CRAN builds of MASS and Matrix, both of
# which require R >= 4.4.0. On an older R, install.packages() will fail to
# build them, so we stop up front with a clear message rather than letting the
# user hit a confusing compilation error part-way through.
# ---------------------------------------------------------------------------
min_r <- "4.4.0"
if (getRversion() < min_r) {
  stop(sprintf(
    paste0(
      "BioEQ requires R version %s or newer. You are running R %s.\n",
      "  Please update R (https://cran.r-project.org/) and run this script again."
    ),
    min_r, as.character(getRversion())
  ), call. = FALSE)
}
cat(sprintf("✓ R version %s (requires >= %s)\n",
            as.character(getRversion()), min_r))

# Set CRAN repository
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# ---------------------------------------------------------------------------
# System library check (macOS / Linux)
#
# officer/flextable build on ragg -> textshaping, which compile against C
# libraries that are not part of R. When they are missing the install dies with
# a raw compiler error like "fatal error: 'fribidi.h' file not found", which is
# hard to interpret. Check up front and print the exact fix instead.
#
# Windows is skipped: CRAN ships pre-built binaries there.
# ---------------------------------------------------------------------------
check_system_libs <- function() {
  if (.Platform$OS.type == "windows") return(invisible(NULL))
  if (nzchar(Sys.which("pkg-config")) == FALSE) return(invisible(NULL))

  needed <- c("fribidi", "harfbuzz", "freetype2", "libpng", "libtiff-4", "libwebp")
  missing <- needed[vapply(needed, function(lib) {
    system2("pkg-config", c("--exists", lib), stdout = FALSE, stderr = FALSE) != 0
  }, logical(1))]

  if (length(missing)) {
    cat("\n")
    cat(strrep("-", 62), "\n")
    cat("NOTE: some system libraries appear to be missing:\n  ",
        paste(missing, collapse = ", "), "\n\n")
    cat("These are C libraries (not R packages) needed to build the Word\n")
    cat("report packages. If installation fails with an error such as\n")
    cat("\"fatal error: 'fribidi.h' file not found\", install them first:\n\n")
    if (Sys.info()[["sysname"]] == "Darwin") {
      cat("  brew install fribidi harfbuzz freetype libpng jpeg-turbo libtiff webp\n\n")
      cat("  Caution: if R itself was installed via Homebrew, this can also\n")
      cat("  upgrade R and reset your package library. If that happens, just\n")
      cat("  re-run this script.\n")
    } else {
      cat("  sudo apt-get install libfribidi-dev libharfbuzz-dev libfreetype6-dev \\\n")
      cat("       libpng-dev libjpeg-dev libtiff5-dev libwebp-dev\n")
    }
    cat(strrep("-", 62), "\n")
  }
  invisible(NULL)
}
check_system_libs()

# ---------------------------------------------------------------------------
# REQUIRED packages — the packages BioEQ loads directly.
# All 31 are needed; the app has no optional tier. (nlme is included here even
# though it ships with R as a Recommended package, because BioEQ calls it
# directly; installing it is a harmless no-op if already present.)
# ---------------------------------------------------------------------------
required_packages <- c(
  # Shiny UI
  "shiny",
  "shinydashboard",
  "bslib",
  "shinyjs",
  "DT",
  "shinycssloaders",
  "htmltools",
  # Data I/O
  "readr",
  "readxl",
  "writexl",
  "zip",
  # Data manipulation
  "dplyr",
  "tidyr",
  "rlang",
  # Plotting
  "ggplot2",
  "plotly",
  "htmlwidgets",
  # Statistics / BE analysis
  "nlme",
  "lme4",
  "lmerTest",
  "emmeans",
  "replicateBE",
  "PowerTOST",
  # Non-compartmental analysis
  "PKNCA",
  # Anomaly detection
  "dtw",
  # Reporting (HTML / PDF / Word)
  "rmarkdown",
  "knitr",
  "officer",
  "flextable",
  # Utilities
  "digest",
  "progress"
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
      cat(sprintf("✓ Successfully installed %s packages\n", package_type))
    }, error = function(e) {
      cat(sprintf("✗ Error installing %s packages: %s\n", package_type, e$message))
      if (package_type == "required") {
        stop("Failed to install required packages. Please install manually.")
      }
    })
  } else {
    cat(sprintf("✓ All %s packages already installed\n", package_type))
  }
}

install_if_missing(required_packages, "required")

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
    cat(sprintf("✓ %s\n", pkg))
  }, error = function(e) {
    cat(sprintf("✗ %s  -- %s\n", pkg, e$message))
    failed <<- c(failed, pkg)
  })
}

if (length(failed) > 0) {
  cat(sprintf("\n⚠  %d required package(s) failed to load: %s\n",
              length(failed), paste(failed, collapse = ", ")))
  cat("Please install them manually and re-run this script.\n")
} else {
  cat(sprintf("\n✓ All %d required packages verified.\n", length(required_packages)))
}
