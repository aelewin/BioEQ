#!/usr/bin/env Rscript
# install_dependencies.R
# Install all required packages for BioEQ

cat("Installing BioEQ dependencies...\n")

# Set CRAN repository
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Required packages (core functionality)
required_packages <- c(
  "shiny",
  "shinydashboard", 
  "DT",
  "readr",
  "dplyr", 
  "readxl",
  "bslib",
  "ggplot2",
  "gridExtra"
)

# Optional packages (enhanced functionality)
optional_packages <- c(
  "shinyjs",
  "digest",
  "shinycssloaders", 
  "rmarkdown",
  "knitr",
  "officer",
  "flextable",
  "zip",
  "plotly",
  "htmlwidgets",
  "testthat"
)

# Function to install packages if not already installed
install_if_missing <- function(packages, package_type = "required") {
  cat(sprintf("\nChecking %s packages...\n", package_type))
  
  missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    cat(sprintf("Installing %s %s packages: %s\n", 
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

# Install required packages
install_if_missing(required_packages, "required")

# Install optional packages (don't fail if these don't work)
install_if_missing(optional_packages, "optional")

cat("\n" %+% "=" %+% rep("=", 50) %+% "\n")
cat("BioEQ dependency installation complete!\n")
cat("\nTo launch the Shiny app:\n")
cat("  cd shiny\n")
cat("  R -e \"shiny::runApp(host='0.0.0.0', port=4000, launch.browser=TRUE)\"\n")
cat("\nOr run: Rscript launch_app.R\n")
cat("=" %+% rep("=", 50) %+% "\n")

# Verify critical packages can be loaded
cat("\nVerifying installation...\n")
critical_packages <- c("shiny", "shinydashboard", "DT", "dplyr")
for (pkg in critical_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat(sprintf("✓ %s loaded successfully\n", pkg))
  }, error = function(e) {
    cat(sprintf("✗ %s failed to load: %s\n", pkg, e$message))
  })
}
