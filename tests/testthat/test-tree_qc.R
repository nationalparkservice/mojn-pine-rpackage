# Load test data
loadPine(data_path = test_path("testData"), dictionary_dir = test_path("testData", "dictionary"))

# Declare Tree column names as global variables to reduce warning in R CMD Check
globalVariables(colnames(loadPine(data_path = test_path("testData"), dictionary_dir = test_path("testData", "dictionary"))$data$Tree))

test_that("Test that treeDuplicateTagQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(treeDuplicateTagQC())
  expect_equal(actual_rows, 2)

  # Compare expected and actual column names
  actual_cols <- colnames(treeDuplicateTagQC())
  expected_cols <- c("park", "locationID", "year", "subplot", "tag", "scientificName", "countTotal")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeDuplicateTagQC()
  expect_s3_class(returnType, "data.frame")

  # This function returns two rows rn
})


test_that("Test that treeMissingTagQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(treeDuplicateTagQC())
  expect_equal(actual_rows, 2)

  # Compare expected and actual column names
  actual_cols <- colnames(treeMissingTagQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "scientificName", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeMissingTagQC()
  expect_s3_class(returnType, "data.frame")

  # This function returns two rows rn
})


test_that("Test that stemLetterQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(treeDuplicateTagQC())
  expect_equal(actual_rows, 2)

  # Compare expected and actual column names
  actual_cols <- colnames(stemLetterQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "stemLetter")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- stemLetterQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeCauseOfDeathQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(treeDuplicateTagQC())
  expect_equal(actual_rows, 2)

  # Compare expected and actual column names
  actual_cols <- colnames(treeCauseOfDeathQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "causeOfDeath", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeCauseOfDeathQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeHeightQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(treeHeightQC())
  expect_equal(actual_rows, 3)

  # Compare expected and actual column names
  actual_cols <- colnames(treeHeightQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality", "treeHeight_m", "scientificName", "threeStandardDeviations")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeHeightQC()
  expect_s3_class(returnType, "data.frame")
})

test_that("Test that coneCountQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(coneCountQC())
  expect_equal(actual_rows, 20)

  # Compare expected and actual column names
  actual_cols <- colnames(coneCountQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "femaleCones", "coneCount", "scientificName")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- coneCountQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that dbhQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(dbhQC())
  expect_equal(actual_rows, 5)

  # Compare expected and actual column names
  actual_cols <- colnames(dbhQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality", "treeDBH_cm", "scientificName", "threeStandardDeviations")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- dbhQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeSubplotQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(treeSubplotQC())
  expect_equal(actual_rows, 10)

  # Compare expected and actual column names
  actual_cols <- colnames(treeSubplotQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeSubplotQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeVitalityQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(treeVitalityQC())
  expect_equal(actual_rows, 2)

  # Compare expected and actual column names
  actual_cols <- colnames(treeVitalityQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeVitalityQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeSpeciesQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(treeSpeciesQC())
  expect_equal(actual_rows, 6)

  # Compare expected and actual column names
  actual_cols <- colnames(treeSpeciesQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "scientificName", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeSpeciesQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that recentlyDeadTreeQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(recentlyDeadTreeQC())
  expect_equal(actual_rows, 1)

  # Compare expected and actual column names
  actual_cols <- colnames(recentlyDeadTreeQC())
  expected_cols <- c("locationID", "subplot", "tag", "vitality", "uniqueID", "count")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- recentlyDeadTreeQC()
  expect_s3_class(returnType, "data.frame")
})




# The fake data has not been modified to fail the tests below

test_that("Test that boleCankersILowerQC() works", {

  # Compare expected and actual column names
  actual_cols <- colnames(boleCankersILowerQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "boleCankers_I_Lower", "boleCanks_ITypes_Lower")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- boleCankersILowerQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that boleCankersIMiddleQC() works", {

  # Compare expected and actual column names
  actual_cols <- colnames(boleCankersIMiddleQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "boleCankers_I_Mid", "boleCankers_ITypes_Mid")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- boleCankersIMiddleQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that boleCankersIUpperQC() works", {

  # Compare expected and actual column names
  actual_cols <- colnames(boleCankersIUpperQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "boleCankers_I_Upper", "boleCankers_ITypes_Upper")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- boleCankersIUpperQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that branchCankersILowerQC() works", {

  # Compare expected and actual column names
  actual_cols <- colnames(branchCankersILowerQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "branchCanks_I_Lower", "branchCanks_ITypes_Lower")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- branchCankersILowerQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that branchCankersIMiddleQC() works", {

  # Compare expected and actual column names
  actual_cols <- colnames(branchCankersIMiddleQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "branchCanks_I_Mid", "branchCanks_ITypes_Mid")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- branchCankersIMiddleQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that branchCankersIUpperQC() works", {

  # Compare expected and actual column names
  actual_cols <- colnames(branchCankersIUpperQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "branchCanks_I_Upper", "branchCanks_ITypes_Upper")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- branchCankersIUpperQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that crownHealthQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(crownHealthQC())
  expect_equal(actual_rows, 4)

  # Compare expected and actual column names
  actual_cols <- colnames(crownHealthQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "crownHealth", "vitality", "scientificName")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- crownHealthQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that crownKillLowerQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(crownKillLowerQC())
  expect_equal(actual_rows, 4)

  # Compare expected and actual column names
  actual_cols <- colnames(crownKillLowerQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality", "scientificName", "crownKill_Lower_percent", "crownKill_Mid_percent", "crownKill_Upper_percent")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- crownKillLowerQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that crownKillMiddleQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(crownKillMiddleQC())
  expect_equal(actual_rows, 3)

  # Compare expected and actual column names
  actual_cols <- colnames(crownKillMiddleQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality", "scientificName", "crownKill_Lower_percent", "crownKill_Mid_percent", "crownKill_Upper_percent")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- crownKillMiddleQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that crownKillUpperQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(crownKillUpperQC())
  expect_equal(actual_rows, 4)

  # Compare expected and actual column names
  actual_cols <- colnames(crownKillUpperQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality", "scientificName", "crownKill_Lower_percent", "crownKill_Mid_percent", "crownKill_Upper_percent")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- crownKillUpperQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that mortalityYearQC() works", {
  # Compare number of rows returned
  actual_rows <- nrow(mortalityYearQC())
  expect_equal(actual_rows, 1)

  # Compare expected and actual column names
  actual_cols <- colnames(mortalityYearQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "estimatedMortalityYear", "scientificName", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- mortalityYearQC()
  expect_s3_class(returnType, "data.frame")
})

