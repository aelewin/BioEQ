#!/usr/bin/env Rscript
# launch_app.R
# Simple script to launch the BioEQ Shiny application

# Set CRAN repository
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Resolve the BioEQ project root from this script's location, regardless
# of the working directory the user happens to be in. This makes the
# launcher work whether invoked via `Rscript launch_app.R` from the
# project root, from elsewhere, or `source()`'d inside RStudio.
.find_launch_root <- function() {
  # Try this script's own path first
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
  candidates <- character(0)
  if (length(file_arg) && nzchar(file_arg)) {
    candidates <- c(candidates, dirname(normalizePath(file_arg, mustWork = FALSE)))
  }
  # Try sourced location
  this_file <- tryCatch({
    fr <- sys.frames()
    of <- vapply(fr, function(f) {
      x <- f$ofile; if (is.null(x)) NA_character_ else as.character(x)
    }, character(1))
    of <- of[!is.na(of) & nzchar(of)]
    if (length(of)) normalizePath(of[length(of)], mustWork = FALSE) else NA_character_
  }, error = function(e) NA_character_)
  if (!is.na(this_file) && nzchar(this_file)) {
    candidates <- c(candidates, dirname(this_file))
  }
  candidates <- c(candidates, getwd())
  for (start in candidates) {
    d <- start
    for (i in 1:8) {
      if (file.exists(file.path(d, "shiny", "app.R"))) {
        return(normalizePath(d, mustWork = TRUE))
      }
      parent <- dirname(d); if (parent == d) break; d <- parent
    }
  }
  stop("Could not locate the BioEQ project root (looking for shiny/app.R).")
}

bioeq_root <- .find_launch_root()
setwd(bioeq_root)

cat("Launching BioEQ Shiny Application...\n")
cat("Project root: ", bioeq_root, "\n", sep = "")
cat("Access the app at: http://127.0.0.1:4000\n")
cat("Press Ctrl+C (or Esc in RStudio) to stop the application.\n\n")

shiny::runApp("shiny", host = "127.0.0.1", port = 4000, launch.browser = TRUE)
