

test_that("Test that treeDuplicateTagQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(treeDuplicateTagQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "scientificName", "countTotal")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeDuplicateTagQC()
  expect_s3_class(returnType, "data.frame")

  # This function returns two rows rn
})


test_that("Test that treeMissingTagQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(treeMissingTagQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "scientificName")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeMissingTagQC()
  expect_s3_class(returnType, "data.frame")

  # This function returns two rows rn
})


test_that("Test that stemLetterQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(stemLetterQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "stemLetter")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- stemLetterQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeCauseOfDeathQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(treeCauseOfDeathQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "causeOfDeath", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeCauseOfDeathQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeHeightQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(treeHeightQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "treeHeight_m", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeHeightQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that mortalityYearQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(mortalityYearQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "estimatedMortalityYear", "scientificName", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- mortalityYearQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that coneCountQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(coneCountQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "femaleCones", "coneCount", "scientificName")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- coneCountQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that crownHealthQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(crownHealthQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "crownHealth", "vitality", "scientificName")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- crownHealthQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that crownKillLowerQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(crownKillLowerQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality", "scientificName", "crownKill_Lower_percent", "crownKill_Mid_percent", "crownKill_Upper_percent")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- crownKillLowerQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that crownKillMiddleQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(crownKillMiddleQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality", "scientificName", "crownKill_Lower_percent", "crownKill_Mid_percent", "crownKill_Upper_percent")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- crownKillMiddleQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that crownKillUpperQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(crownKillUpperQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality", "scientificName", "crownKill_Lower_percent", "crownKill_Mid_percent", "crownKill_Upper_percent")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- crownKillUpperQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that dbhQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(dbhQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "treeDBH_cm")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- dbhQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeSubplotQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(treeSubplotQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeSubplotQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeVitalityQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(treeVitalityQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeVitalityQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that boleCankersILowerQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(boleCankersILowerQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "boleCankers_I_Lower", "boleCanks_ITypes_Lower")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- boleCankersILowerQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that boleCankersIMiddleQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(boleCankersIMiddleQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "boleCankers_I_Mid", "boleCankers_ITypes_Mid")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- boleCankersIMiddleQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that boleCankersIUpperQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(boleCankersIUpperQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "boleCankers_I_Upper", "boleCankers_ITypes_Upper")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- boleCankersIUpperQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that branchCankersILowerQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(branchCankersILowerQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "branchCanks_I_Lower", "branchCanks_ITypes_Lower")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- branchCankersILowerQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that branchCankersIMiddleQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(branchCankersIMiddleQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "branchCanks_I_Mid", "branchCanks_ITypes_Mid")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- branchCankersIMiddleQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that branchCankersIUpperQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(branchCankersIUpperQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "branchCanks_I_Upper", "branchCanks_ITypes_Upper")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- branchCankersIUpperQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that treeSpeciesQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(treeSpeciesQC())
  expected_cols <- c("eventID", "park", "locationID", "eventDate", "subplot", "tag", "scientificName", "vitality")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- treeSpeciesQC()
  expect_s3_class(returnType, "data.frame")
})


test_that("Test that recentlyDeadTreeQC() works", {
  data_dir = "data/final"
  fiveneedlepine::loadPine("M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb")

  # Compare expected and actual column names
  actual_cols <- colnames(recentlyDeadTreeQC())
  expected_cols <- c("locationID", "subplot", "tag", "vitality", "uniqueID", "count")
  expect_equal(actual_cols, expected_cols)

  # Check that function returns a database
  returnType <- recentlyDeadTreeQC()
  expect_s3_class(returnType, "data.frame")
})
