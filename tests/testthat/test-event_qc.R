# Load test data
loadPine(data_path = test_path("testData"), dictionary_dir = test_path("testData", "dictionary"))

# Declare Visit column names as global variables to reduce warning in R CMD Check
globalVariables(colnames(loadPine(data_path = test_path("testData"), dictionary_dir = test_path("testData", "dictionary"))$data$Visit))


test_that("Test that noSeedlingDataQC() works", {

  # Compare number of rows returned
  actual_rows <- nrow(noSeedlingDataQC())
  expect_equal(actual_rows, 5)

  # Compare expected and actual column names
  actual_cols <- colnames(noSeedlingDataQC())
  expected_cols <- c("locationID", "eventID", "eventDate", "network", "park")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- noSeedlingDataQC()
  expect_s3_class(returnType, "data.frame")

  returned_data <- noSeedlingDataQC() %>% dplyr::arrange(locationID)
  # check that first row has expected values
  expect_equal(returned_data[[1,1]], "GRBA_N_010")
  # Check that eventDate column has correct type
  expect_s3_class(returned_data$eventDate, c("Date", "POSIXt"))
})


test_that("Test that noTreeDataQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(noSeedlingDataQC())
  expect_equal(actual_rows, 5)

  # Compare expected and actual column names
  actual_cols <- colnames(noTreeDataQC())
  expected_cols <- c("locationID", "eventID", "eventDate", "network", "park")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- noTreeDataQC()
  expect_s3_class(returnType, "data.frame")

  returned_data <- noTreeDataQC() %>% dplyr::arrange(locationID)
  # check that first row has expected values
  expect_equal(returned_data[[1,1]], "GRBA_N_201")
  # Check that eventDate column has correct type
  expect_s3_class(returned_data$eventDate, c("Date", "POSIXt"))
})

