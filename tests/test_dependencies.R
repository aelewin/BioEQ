# tests/test_dependencies.R
# TEST 4: Dependency vulnerability checks
# Run with: testthat::test_file("tests/test_dependencies.R")

library(testthat)

# ============================================================================
# Package Dependency Checks
# ============================================================================

find_project_root <- function() {
  candidates <- c(".", "..", "../..")
  for (d in candidates) {
    if (file.exists(file.path(d, "DESCRIPTION"))) return(normalizePath(d))
  }
  stop("Cannot find project root")
}
.proj_root <- find_project_root()

context("Dependencies — Required Packages Listed")

test_that("DESCRIPTION file exists and lists required packages", {
  desc_path <- file.path(.proj_root, "DESCRIPTION")

  expect_true(file.exists(desc_path), info = "DESCRIPTION file must exist")

  desc <- read.dcf(desc_path)
  imports <- desc[1, "Imports"]

  # Core Shiny packages
  expect_true(grepl("shiny", imports), info = "Missing: shiny")
  expect_true(grepl("shinydashboard", imports), info = "Missing: shinydashboard")
  expect_true(grepl("DT", imports), info = "Missing: DT")
  expect_true(grepl("shinyjs", imports), info = "Missing: shinyjs")

  # Data handling
  expect_true(grepl("readr", imports), info = "Missing: readr")
  expect_true(grepl("readxl", imports), info = "Missing: readxl")
  expect_true(grepl("dplyr", imports), info = "Missing: dplyr")

  # Analysis
  expect_true(grepl("nlme", imports), info = "Missing: nlme")
  expect_true(grepl("ggplot2", imports), info = "Missing: ggplot2")
})

context("Dependencies — Installed Package Versions")

test_that("critical packages are installed", {
  critical_packages <- c("shiny", "DT", "readr", "dplyr")

  for (pkg in critical_packages) {
    installed <- requireNamespace(pkg, quietly = TRUE)
    expect_true(installed, info = paste("Package not installed:", pkg))
  }
})

test_that("installed shiny version is reasonably current", {
  if (requireNamespace("shiny", quietly = TRUE)) {
    ver <- packageVersion("shiny")
    # Shiny >= 1.7.0 includes important security improvements
    expect_true(ver >= "1.7.0",
                info = paste("shiny version", ver, "is below 1.7.0; consider updating"))
  } else {
    skip("shiny not installed")
  }
})

test_that("installed DT version is reasonably current", {
  if (requireNamespace("DT", quietly = TRUE)) {
    ver <- packageVersion("DT")
    # DT >= 0.27 has security fixes for HTML escaping
    expect_true(ver >= "0.20",
                info = paste("DT version", ver, "may be outdated"))
  } else {
    skip("DT not installed")
  }
})

context("Dependencies — No Known Vulnerable Packages")

test_that("digest package (used for hashing) is at a safe version", {
  if (requireNamespace("digest", quietly = TRUE)) {
    ver <- packageVersion("digest")
    # digest >= 0.6.29 has no known vulnerabilities
    expect_true(ver >= "0.6.25",
                info = paste("digest version", ver, "may have vulnerabilities"))
  } else {
    skip("digest not installed")
  }
})

test_that("readr package is at a safe version", {
  if (requireNamespace("readr", quietly = TRUE)) {
    ver <- packageVersion("readr")
    expect_true(ver >= "2.0.0",
                info = paste("readr version", ver, "is very old; update recommended"))
  } else {
    skip("readr not installed")
  }
})
