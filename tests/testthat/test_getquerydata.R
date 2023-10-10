context_start_file("getquerydata")

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


