#!/usr/bin/env Rscript
# launch_app.R
# Simple script to launch the BioEQ Shiny application

# Set CRAN repository
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Check if we're in the right directory
if (!file.exists("shiny/app.R")) {
  stop("Error: Please run this script from the BioEQ project root directory.\n",
       "Expected to find: shiny/app.R")
}

cat("Launching BioEQ Shiny Application...\n")
cat("If packages are missing, they will be installed automatically.\n")
cat("Access the app at: http://localhost:4000\n")
cat("Press Ctrl+C to stop the application.\n\n")

# Change to shiny directory and launch app
setwd("shiny")
# SECURITY: Default to localhost; override with BIOEQ_HOST env var for proxy deployments
host <- Sys.getenv("BIOEQ_HOST", unset = "127.0.0.1")
port <- as.integer(Sys.getenv("BIOEQ_PORT", unset = "4000"))
shiny::runApp(host = host, port = port, launch.browser = TRUE)
