# Install and load the 'testthat' package if not already installed
#if (!requireNamespace("testthat", quietly = TRUE)) {
  #install.packages("testthat")}
#library(testthat)
library(jsonlite)
# Load your getapidata function
#source("~/R/koladaAPI/R/getapidata.R")


context_start_file("getapidata")

# Test suite for getapidata function
test_that("Test getapidata function", {
  endpoint <- "http://api.kolada.se/v2"
  
  # Test with valid entity
  result <- getapidata(endpoint, entity = "municipality", params = list())
  expect_type(result, "list")
  
  
  # Test with invalid endpoint
  expect_error(getapidata("http://example.com", entity = "municipality", params = list()), "Invalid endpoint")
  
  # Test with invalid entity
  expect_error(getapidata(endpoint, entity = "invalid_entity", params = list()), "Invalid entity")
  
  # Test with valid entity from the list
  for (entity in c("kpi", "kpi_groups", "municipality", "municipality_groups", "ou")) {
    result <- getapidata(endpoint, entity = entity, params = list())
    expect_type(result, "list")
  }
  
  # Test with invalid entity not from the list
  expect_error(getapidata(endpoint, entity = "invalid_entity", params = list()), "Invalid entity")
  
  #Test with 'per_page' within the limit (should not throw an error)
  result <- getapidata(endpoint, entity = "ou", params = list(title = "skola", per_page = 500))
  expect_type(result, "list")
  
  # Test with 'per_page' exceeding the limit (should throw an error)
  expect_error(getapidata(endpoint, entity = "ou", params = list(title = "skola", per_page = 6000)), "API Limit exceeded.")
})


