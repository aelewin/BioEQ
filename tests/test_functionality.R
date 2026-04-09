# tests/test_functionality.R
# TEST 7: Functional regression tests — verify original app features still work
# after security changes.
# Run with: testthat::test_file("tests/test_functionality.R")

library(testthat)

# ============================================================================
# App Structure Integrity
# ============================================================================

find_project_root <- function() {
  candidates <- c(".", "..", "../..")
  for (d in candidates) {
    if (file.exists(file.path(d, "DESCRIPTION"))) return(normalizePath(d))
  }
  stop("Cannot find project root")
}
.proj_root <- find_project_root()

context("Functionality — App File Structure")

test_that("all required application files exist", {
  root <- .proj_root
  required_files <- c(
    "shiny/app.R",
    "shiny/ui/main_ui.R",
    "shiny/server/main_server.R",
    "shiny/server/data_upload_server.R",
    "shiny/server/analysis_setup_server.R",
    "shiny/server/plots_server.R",
    "shiny/server/results_dashboard_server.R",
    "shiny/server/sample_size_server.R",
    "R/bioeq_main.R",
    "R/nca_functions.R",
    "R/be_analysis.R",
    "R/statistics.R",
    "R/utils.R",
    "R/plotting.R"
  )

  for (f in required_files) {
    path <- file.path(root, f)
    expect_true(file.exists(path), info = paste("Missing required file:", f))
  }
})

test_that("security modules are present", {
  root <- .proj_root
  security_files <- c(
    "R/security_validation.R",
    "R/security_database.R",
    "R/security_output.R"
  )

  for (f in security_files) {
    path <- file.path(root, f)
    expect_true(file.exists(path), info = paste("Missing security module:", f))
  }
})

test_that("CSS and static assets exist", {
  root <- .proj_root
  expect_true(file.exists(file.path(root, "shiny", "www", "custom.css")),
              info = "Missing custom.css")
})

# ============================================================================
# Core R Function Integrity
# ============================================================================

context("Functionality — Core R Functions Load")

test_that("security_validation.R loads without errors", {
  root <- .proj_root
  expect_no_error(source(file.path(root, "R", "security_validation.R")))
})

test_that("security_database.R loads without errors", {
  root <- .proj_root
  expect_no_error(source(file.path(root, "R", "security_database.R")))
})

test_that("security_output.R loads without errors", {
  root <- .proj_root
  expect_no_error(source(file.path(root, "R", "security_output.R")))
})

# ============================================================================
# app.R Integrity After Security Patches
# ============================================================================

context("Functionality — app.R Integrity")

test_that("app.R still contains essential Shiny components", {
  root <- .proj_root
  app_file <- file.path(root, "shiny", "app.R")
  content <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  # Essential library loads
  expect_true(grepl("library\\(shiny\\)", content), info = "Missing library(shiny)")
  expect_true(grepl("library\\(shinydashboard\\)", content), info = "Missing library(shinydashboard)")
  expect_true(grepl("library\\(DT\\)", content), info = "Missing library(DT)")

  # Essential source statements
  expect_true(grepl("source.*bioeq_main", content), info = "Missing bioeq_main.R source")
  expect_true(grepl("source.*nca_functions", content), info = "Missing nca_functions.R source")

  # UI definition
  expect_true(grepl("dashboardPage|fluidPage|ui\\s*<-", content),
              info = "Missing UI definition")

  # Server definition
  expect_true(grepl("server\\s*<-|function.*input.*output.*session", content),
              info = "Missing server definition")

  # shinyApp call
  expect_true(grepl("shinyApp", content), info = "Missing shinyApp() call")
})

test_that("app.R does NOT contain the removed eval() handler", {
  root <- .proj_root
  app_file <- file.path(root, "shiny", "app.R")
  content <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_false(grepl("eval\\(data\\.script\\)", content),
               info = "eval(data.script) should have been removed")
})

test_that("app.R includes security headers", {
  root <- .proj_root
  app_file <- file.path(root, "shiny", "app.R")
  content <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_true(grepl("X-Frame-Options", content, fixed = TRUE))
  expect_true(grepl("X-Content-Type-Options", content, fixed = TRUE))
  expect_true(grepl("X-XSS-Protection", content, fixed = TRUE))
})

# ============================================================================
# launch_app.R Integrity
# ============================================================================

context("Functionality — launch_app.R Integrity")

test_that("launch_app.R still launches the app with correct parameters", {
  root <- .proj_root
  launch_file <- file.path(root, "launch_app.R")
  content <- paste(readLines(launch_file, warn = FALSE), collapse = "\n")

  expect_true(grepl("shiny::runApp", content), info = "Missing runApp call")
  expect_true(grepl("launch.browser\\s*=\\s*TRUE", content),
              info = "Missing launch.browser = TRUE")
})

# ============================================================================
# DESCRIPTION File Integrity
# ============================================================================

context("Functionality — DESCRIPTION Integrity")

test_that("DESCRIPTION has correct package metadata", {
  root <- .proj_root
  desc <- read.dcf(file.path(root, "DESCRIPTION"))

  expect_equal(unname(desc[1, "Package"]), "BioEQ")
  expect_equal(unname(desc[1, "Type"]), "Package")
  expect_true(nchar(desc[1, "Imports"]) > 0)
})

# ============================================================================
# Data Processing Functions
# ============================================================================

context("Functionality — Data Processing")

test_that("Treatment value validation works for standard values", {
  source(file.path(.proj_root, "R", "security_validation.R"))

  # Valid treatments
  expect_true(validate_treatment_values(c("R", "T"))$valid)
  expect_true(validate_treatment_values(c("Reference", "Test"))$valid)

  # Invalid treatments
  expect_false(validate_treatment_values(c("X", "Y"))$valid)
})

test_that("Numeric validation works for BE limits", {
  source(file.path(.proj_root, "R", "security_validation.R"))

  # Standard BE limits
  expect_true(validate_numeric_input(80, min_val = 0, max_val = 200)$valid)
  expect_true(validate_numeric_input(125, min_val = 0, max_val = 200)$valid)
  expect_true(validate_numeric_input(90, min_val = 80, max_val = 125)$valid)

  # Out of range
  expect_false(validate_numeric_input(-1, min_val = 0, max_val = 200)$valid)
  expect_false(validate_numeric_input(300, min_val = 0, max_val = 200)$valid)
})
