# Load test data
loadPine(data_path = test_path("testData"), dictionary_dir = test_path("testData", "dictionary"))

# # Declare Seedling column names as global variables to reduce warning in R CMD Check
# globalVariables(colnames(loadPine(data_path = test_path("testData"), dictionary_dir = test_path("testData", "dictionary"))$data$Seedling))

test_that("Test that numberOfSubplotsQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(numberOfSubplotsQC())
  expect_equal(actual_rows, 55)

  # Compare expected and actual column names
  actual_cols <- colnames(numberOfSubplotsQC())
  expected_cols <- c("eventID", "locationID", "eventDate", "numSubplots")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- numberOfSubplotsQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that subplotQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(subplotQC())
  expect_equal(actual_rows, 10)

  # Compare expected and actual column names
  actual_cols <- colnames(subplotQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- subplotQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that missingTagQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(missingTagQC())
  expect_equal(actual_rows, 4)

  # Compare expected and actual column names
  actual_cols <- colnames(missingTagQC())
  expected_cols <- c("eventID", "locationID", "eventDate", "subplot", "tag", "scientificName", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- missingTagQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that duplicateSeedlingTagQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(duplicateSeedlingTagQC())
  expect_equal(actual_rows, 2)

  # Compare expected and actual column names
  actual_cols <- colnames(duplicateSeedlingTagQC())
  expected_cols <- c("locationID", "subplot", "tag", "year", "scientificName", "countTotal")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- duplicateSeedlingTagQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that causeOfDeathQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(causeOfDeathQC())
  expect_equal(actual_rows, 4)

  # Compare expected and actual column names
  actual_cols <- colnames(causeOfDeathQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag", "vitality", "causeOfDeath")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- causeOfDeathQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that vitalityQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(vitalityQC())
  expect_equal(actual_rows, 3)

  # Compare expected and actual column names
  actual_cols <- colnames(vitalityQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- vitalityQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that seedlingSpeciesQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(seedlingSpeciesQC())
  expect_equal(actual_rows, 6)

  # Compare expected and actual column names
  actual_cols <- colnames(seedlingSpeciesQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- seedlingSpeciesQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that heightClassQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(heightClassQC())
  expect_equal(actual_rows, 2)

  # Compare expected and actual column names
  actual_cols <- colnames(heightClassQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag", "vitality", "heightClass")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- heightClassQC()
  expect_s3_class(returnType, "data.frame")
})

test_that("Test that recentlyDeadSeedlingQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(recentlyDeadSeedlingQC())
  expect_equal(actual_rows, 1)

  # Compare expected and actual column names
  actual_cols <- colnames(recentlyDeadSeedlingQC())
  expected_cols <- c("locationID", "subplot", "tag", "vitality", "uniqueID", "count")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- recentlyDeadSeedlingQC()
  expect_s3_class(returnType, "data.frame")
})
