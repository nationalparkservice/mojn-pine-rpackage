

test_that("Test that numberOfSubplotsQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(numberOfSubplotsQC())
  expected_cols <- c("eventID", "locationID", "eventDate", "numSubplots")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- numberOfSubplotsQC()
  expect_s3_class(returnType, "data.frame")

  # This function returns two rows rn
})


test_that("Test that subplotQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(subplotQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- subplotQC()
  expect_s3_class(returnType, "data.frame")
})


# TODO: this currently fails devtools::test() but not when you run the file
test_that("Test that missingTagQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(missingTagQC())
  expected_cols <- c("eventID", "locationID", "eventDate", "subplot", "tag", "scientificName")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- missingTagQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that duplicateSeedlingTagQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(duplicateSeedlingTagQC())
  expected_cols <- c("eventID", "locationID", "eventDate", "subplot", "tag", "scientificName", "countTotal")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- duplicateSeedlingTagQC()
  expect_s3_class(returnType, "data.frame")
})


# TODO: this currently fails devtools::test() but not when you run the file
test_that("Test that causeOfDeathQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(causeOfDeathQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag", "vitality", "causeOfDeath")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- causeOfDeathQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that vitalityQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(vitalityQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- vitalityQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that seedlingSpeciesQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(seedlingSpeciesQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- seedlingSpeciesQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that heightClassQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(heightClassQC())
  expected_cols <- c("eventID", "scientificName", "locationID", "eventDate", "subplot", "tag", "vitality", "heightClass")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- heightClassQC()
  expect_s3_class(returnType, "data.frame")
})
