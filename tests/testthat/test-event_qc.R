test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Load test data
loadPine("C:/Users/ifoster/Documents/R/mojn-pine-rpackage/tests/testthat/testData", dictionary_dir = "testData/dictionary")

test_that("Test that noSeedlingDataQC() works", {

  # Compare number of rows returned
  actual_rows <- nrow(noSeedlingDataQC())
  expect_equal(actual_rows, 5)

  # Compare expected and actual column names
  actual_cols <- colnames(noSeedlingDataQC())
  expected_cols <- c('locationID', 'eventID', 'eventDate', 'network', 'park')
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- noSeedlingDataQC()
  expect_s3_class(returnType, "data.frame")

  returned_data <- noSeedlingDataQC() %>% arrange(locationID)
  # check that first row has expected values
  expect_equal(returned_data[[1,1]], "GRBA_N_010")
  # Check that eventDate column has correct type
  expect_s3_class(returned_data$eventDate, "POSIXt")
})


test_that("Test that noTreeDataQC() works", {

  # Compare expected and actual column names
  actual_cols <- colnames(noTreeDataQC())
  expected_cols <- c("locationID", "eventID", "treeCount")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- noTreeDataQC()
  expect_s3_class(returnType, "data.frame")
})

