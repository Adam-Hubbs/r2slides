# Helper function to clean up test environment
cleanup_test_env <- function() {
  if (exists("google_spreadsheet", envir = .GlobalEnv)) {
    rm(google_spreadsheet, envir = .GlobalEnv)
  }
}


test_that("create_spreadsheet_env_in_global creates environment with correct parent", {
  cleanup_test_env()
  
  # Test the helper function directly
  create_spreadsheet_env_in_global()
  
  expect_true(exists("google_spreadsheet", envir = .GlobalEnv))
  expect_type(google_spreadsheet, "environment")
  expect_identical(parent.env(google_spreadsheet), emptyenv())
  
  cleanup_test_env()
})

test_that("create_spreadsheet_env_in_global does not overwrite existing environment", {
  cleanup_test_env()
  
  # Create environment and add a variable
  create_spreadsheet_env_in_global()
  google_spreadsheet$test_var <- "preserved"
  
  # Call function again
  create_spreadsheet_env_in_global()
  
  # Check that the variable is still there
  expect_equal(google_spreadsheet$test_var, "preserved")
  
  cleanup_test_env()
})