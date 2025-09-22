library(testthat)
library(shiny)

# Test that the Shiny app can be loaded without errors
test_that("Shiny app loads without errors", {
  
  # Test that main UI components exist
  expect_true(file.exists("../../shiny/app.R"))
  expect_true(file.exists("../../shiny/ui/main_ui.R"))
  expect_true(file.exists("../../shiny/server/main_server.R"))
  
  # Test that CSS file exists
  expect_true(file.exists("../../shiny/www/custom.css"))
  
  # Test that required R files exist
  expect_true(file.exists("../../R/bioeq_main.R"))
  expect_true(file.exists("../../R/nca_functions.R"))
  expect_true(file.exists("../../R/plotting.R"))
  expect_true(file.exists("../../R/statistics.R"))
  expect_true(file.exists("../../R/utils.R"))
})

test_that("Analysis templates exist", {
  
  # Test that template files exist
  expect_true(file.exists("../../inst/templates/fda_standard_template.R"))
  expect_true(file.exists("../../inst/templates/ema_standard_template.R"))
  expect_true(file.exists("../../inst/templates/ich_m13a_template.R"))
  
})

test_that("Required packages are specified in DESCRIPTION", {
  
  # Read DESCRIPTION file
  desc <- read.dcf("../../DESCRIPTION")
  imports <- desc[1, "Imports"]
  
  # Check that key Shiny packages are listed
  expect_true(grepl("shiny", imports))
  expect_true(grepl("shinydashboard", imports))
  expect_true(grepl("DT", imports))
  expect_true(grepl("plotly", imports))
  expect_true(grepl("readr", imports))
  expect_true(grepl("dplyr", imports))
})
