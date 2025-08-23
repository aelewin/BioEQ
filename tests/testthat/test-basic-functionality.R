library(testthat)
library(shiny)

# Test the data validation function
test_that("Data validation works correctly", {
  
  # Valid data
  valid_data <- data.frame(
    Subject = c(1, 1, 2, 2),
    Sequence = c("RT", "RT", "TR", "TR"),
    Period = c(1, 2, 1, 2),
    Treatment = c("R", "T", "T", "R"),
    Time = c(0, 0, 0, 0),
    Concentration = c(0, 10, 0, 12)
  )
  
  # Source the validation function
  source("../../shiny/server/main_server.R")
  
  result <- validate_bioeq_data(valid_data)
  expect_true(result$is_valid)
  expect_length(result$errors, 0)
  
  # Invalid data - missing columns
  invalid_data <- data.frame(
    Subject = c(1, 2),
    Time = c(0, 1)
  )
  
  result_invalid <- validate_bioeq_data(invalid_data)
  expect_false(result_invalid$is_valid)
  expect_gt(length(result_invalid$errors), 0)
})

test_that("Data summary creation works", {
  test_data <- data.frame(
    Subject = c(1, 1, 2, 2),
    Sequence = c("RT", "RT", "TR", "TR"),
    Period = c(1, 2, 1, 2),
    Treatment = c("R", "T", "T", "R"),
    Time = c(0, 1, 0, 1),
    Concentration = c(0, 10, 0, 12)
  )
  
  # Source the function
  source("../../shiny/server/main_server.R")
  
  summary <- create_data_summary(test_data)
  
  expect_equal(summary$n_subjects, 2)
  expect_equal(summary$n_periods, 2)
  expect_equal(summary$n_treatments, 2)
  expect_equal(summary$total_observations, 4)
})
