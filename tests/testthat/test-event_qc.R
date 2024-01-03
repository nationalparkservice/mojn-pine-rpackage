test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("Test that noSeedlingDataQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(noSeedlingDataQC())
  expected_cols <- c("locationID", "eventID", "seedlingCount")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- noSeedlingDataQC()
  expect_type(returnType, "list")
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that noTreeDataQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(noTreeDataQC())
  expected_cols <- c("locationID", "eventID", "treeCount")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- noTreeDataQC()
  expect_type(returnType, "list")
  expect_s3_class(returnType, "data.frame")
})

