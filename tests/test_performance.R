# tests/test_performance.R
# TEST 6: Performance regression tests — ensure security functions don't
# introduce unacceptable slowdowns.
# Run with: testthat::test_file("tests/test_performance.R")

library(testthat)

# Source the security modules
find_project_root <- function() {
  candidates <- c(".", "..", "../..")
  for (d in candidates) {
    if (file.exists(file.path(d, "R", "security_validation.R"))) return(normalizePath(d))
  }
  stop("Cannot find project root")
}
.proj_root <- find_project_root()

source(file.path(.proj_root, "R", "security_validation.R"))
source(file.path(.proj_root, "R", "security_database.R"))
source(file.path(.proj_root, "R", "security_output.R"))

# ============================================================================
# Performance Benchmarks
# ============================================================================

context("Performance — Input Validation Speed")

test_that("validate_text_input handles 10,000 calls in < 5 seconds", {
  inputs <- paste0("Sample text input #", seq_len(10000))
  start <- proc.time()["elapsed"]
  for (inp in inputs) {
    validate_text_input(inp)
  }
  elapsed <- proc.time()["elapsed"] - start
  expect_lt(elapsed, 5)
})

test_that("validate_numeric_input handles 10,000 calls in < 2 seconds", {
  values <- runif(10000, 0, 1000)
  start <- proc.time()["elapsed"]
  for (v in values) {
    validate_numeric_input(v, min_val = 0, max_val = 1000)
  }
  elapsed <- proc.time()["elapsed"] - start
  expect_lt(elapsed, 2)
})

context("Performance — Output Escaping Speed")

test_that("escape_html handles 10,000 strings in < 2 seconds", {
  strings <- paste0("String with <special> & 'chars' #", seq_len(10000))
  start <- proc.time()["elapsed"]
  for (s in strings) {
    escape_html(s)
  }
  elapsed <- proc.time()["elapsed"] - start
  expect_lt(elapsed, 2)
})

test_that("detect_xss handles 10,000 strings in < 5 seconds", {
  strings <- paste0("Normal text #", seq_len(10000))
  start <- proc.time()["elapsed"]
  for (s in strings) {
    detect_xss(s)
  }
  elapsed <- proc.time()["elapsed"] - start
  expect_lt(elapsed, 5)
})

context("Performance — SQL Detection Speed")

test_that("detect_sql_injection handles 10,000 calls in < 5 seconds", {
  inputs <- paste0("Normal input #", seq_len(10000))
  start <- proc.time()["elapsed"]
  for (inp in inputs) {
    detect_sql_injection(inp)
  }
  elapsed <- proc.time()["elapsed"] - start
  expect_lt(elapsed, 5)
})

context("Performance — File Validation Speed")

test_that("validate_file_path handles 1,000 calls in < 2 seconds", {
  paths <- file.path(tempdir(), paste0("file_", seq_len(1000), ".csv"))
  start <- proc.time()["elapsed"]
  for (p in paths) {
    validate_file_path(p, allowed_dir = tempdir())
  }
  elapsed <- proc.time()["elapsed"] - start
  expect_lt(elapsed, 2)
})

context("Performance — Data Frame Sanitization")

test_that("sanitize_column_names on large data frame is fast", {
  # Create a data frame with 100 columns
  col_names <- paste0("<b>Col_", seq_len(100), "</b>")
  df <- as.data.frame(matrix(1, nrow = 10, ncol = 100))
  names(df) <- col_names

  start <- proc.time()["elapsed"]
  for (i in seq_len(100)) {
    sanitize_column_names(df)
  }
  elapsed <- proc.time()["elapsed"] - start
  expect_lt(elapsed, 2)
})
