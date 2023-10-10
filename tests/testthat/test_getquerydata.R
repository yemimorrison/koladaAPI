context_start_file("getquerydata")

library(jsonlite)

# Load your functions and the necessary data
source("~/R/koladaAPI/R/getquerydata.R")

# Mock data for testing
mock_api_data <- '{"data": [{"id": 1, "name": "Item 1"}, {"id": 2, "name": "Item 2"}]}'
mock_query_data <- '{"data": [{"Count": 10, "KPI": "Test KPI", "MunicipalityID": 123, "Year": 2020, "UnemploymentRate": 5.0}]}'

# Define test cases
test_that("Test getquerydata function", {
  endpoint <- "http://api.kolada.se/v2/data/kpi/N03932"
  municipalities <- data.frame(ID = 123, Municipality = "Test Municipality")
  
  # Test with valid parameters
  result <- getquerydata(endpoint, entity = "municipality", municipality = "Stockholm", year = 2020)
  expect_type(result, "list")
  
  # Test with invalid entity
  expect_error(getquerydata(endpoint, entity = "invalid_entity", municipality = "Test Municipality", year = 2020), "Invalid entity")
  
  # Test with invalid municipality
  expect_error(getquerydata(endpoint, entity = "municipality", municipality = "Invalid Municipality", year = 2020), "Invalid municipality")
  
  # Test with invalid year
  expect_error(getquerydata(endpoint, entity = "municipality", municipality = "Linkoping", year = 2023), "Invalid year")
  
  # Test with invalid endpoint
  expect_error(getquerydata("http://example.com", entity = "municipality", municipality = "Norrkoping", year = 2020), "Invalid endpoint")
})


