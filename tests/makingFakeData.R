# for QC unit tests use load pine with location of fake data

library(tidyverse)
data_dir = "data/final"
database_dir = "data/FNP_MOJN_Primary.accdb"
pine <- fiveneedlepine::loadPine(database_dir)


set.seed(2023)

# Make fake event Data
visitFakeData <- pine$data$Visit %>%
  # Select some sites for sample
  dplyr::filter(locationID == "GRBA_N_201" | locationID == "GRBA_N_408" | locationID == "GRBA_N_010")


# Make fake seeding data
seedlingFakeData <- pine$data$Seedling %>%
  # Select random sample
  dplyr::sample_n(300) %>%
  # Filter out some sites to fail noSeedlingDataQC() in event QC
  dplyr::filter(locationID != "GRBA_N_201" & locationID != "GRBA_N_408")

seedlingFakeData2 <- seedlingFakeData

# To fail subplotQC: make some entries have missing subplot numbers or subplot numbers that are not between 1 and 9
seedlingFakeData2[sample(1:284, 5, replace=FALSE), 'subplot'] =  sample(9:20, 5)
seedlingFakeData2[sample(1:284, 5, replace=FALSE), 'subplot'] =  NA

# To fail missingTagQC: make some of the live tree tags NA
# Arrange data frame so the plots with no seedlings are not selected
seedlingFakeData2 <- seedlingFakeData2 %>% dplyr::arrange(scientificName)
seedlingFakeData2[sample(233:284, 5, replace=FALSE), 'tag'] =  NA

# To fail duplicateSeedlingTagQC: make some of the tags be duplicates of other tags
seedlingFakeData2 <- seedlingFakeData2 %>%
  dplyr::mutate(tag = dplyr::case_when(
    # Renumber one tree tag so it is a duplicate of another tree on the same plot and subplot
    (locationID == "GRBA_N_405" & subplot == 7 & tag == 485) ~ 489,
    .default = tag))

# To fail causeOfDeathQC: make some seedlings D/RD and have NA or non-valid causes of death
seedlingFakeData2 <- seedlingFakeData2 %>%
  dplyr::mutate(
    causeOfDeath = dplyr::case_when(
      # Make some trees have invalid or missing causes of death
      (locationID == "GRBA_N_010" & subplot == 2 & tag == 1101) ~ "beetle",
      (locationID == "GRBA_N_004" & subplot == 7 & tag == 305) ~ NA,
      .default = causeOfDeath),
    vitality = dplyr::case_when(
      # Change the vitality of the trees that now have missing/non-valid causes of death
      (locationID == "GRBA_N_010" & subplot == 2 & tag == 1101) ~ "D",
      (locationID == "GRBA_N_004" & subplot == 7 & tag == 305) ~ "RD",
      .default = vitality))


# To fail vitalityQC:make some seedlings NA or not L,D,RD
seedlingFakeData2 <- seedlingFakeData2 %>%
  dplyr::mutate(vitality = dplyr::case_when(
    # Change the vitality of the trees so they have missing or non-valid vitality
    (locationID == "GRBA_N_401" & subplot == 5 & tag == 140) ~ "P",
    (locationID == "GRBA_N_209" & subplot == 8 & tag == 1322) ~ NA,
    .default = vitality))

# To fail seedlingSpeciesQC: make some species names NA or blank
seedlingFakeData2[sample(1:284, 3, replace=FALSE), 'scientificName'] =  ""
seedlingFakeData2[sample(1:284, 3, replace=FALSE), 'scientificName'] =  NA

# To fail heightClassQC: make some have non-valid height classes
seedlingFakeData2 <- seedlingFakeData2 %>%
  dplyr::mutate(heightClass = dplyr::case_when(
    # Change the vitality of the trees so they have missing or non-valid vitality
    (locationID == "GRBA_N_405" & subplot == 8 & tag == 208) ~ "90 - <137 cm",
    (locationID == "GRBA_N_401" & subplot == 6 & tag == 1297) ~ NA,
    .default = heightClass))

# To fail recentlyDeadSeedlingQC: make some seedlings be RD for multiple entries
seedlingFakeData2 <- seedlingFakeData2 %>%
  dplyr::mutate(vitality = dplyr::case_when(
    (locationID == "GRBA_N_401" & subplot == 6 & tag == 135) ~ "RD",
    .default = vitality))




set.seed(2024)

# Make fake tree data
treeFakeData  <- pine$data$Tree %>%
  # Select random sample
  dplyr::sample_n(500) %>%
  # Filter out some sites to fail noTreeDataQC in event QC
  dplyr::filter(locationID != "GRBA_N_201" & locationID != "GRBA_N_408")

treeFakeData2 <- treeFakeData

# To fail treeDuplicateTagQC: make some trees have duplicate tags of other tags
treeFakeData2 <- treeFakeData2 %>%
  dplyr::mutate(tag = dplyr::case_when(
    # Renumber one tree tag so it is a duplicate of another tree on the same plot and subplot
    (locationID == "GRBA_N_003" & subplot == 4 & tag == 130) ~ 125,
    .default = tag))

# To fail treeMissingTagQC: make some of the live trees have missing tags
# Arrange data frame so only live trees are not selected
treeFakeData2 <- treeFakeData2 %>% dplyr::arrange(vitality)
treeFakeData2[sample(90:470, 5, replace=FALSE), 'tag'] =  NA

# To fail stemLetterQC: make some of the stem letter entries be not a letter
treeFakeData2 <- treeFakeData2 %>%
  dplyr::mutate(stemLetter = dplyr::case_when(
    # Renumber one tree tag so it is a duplicate of another tree on the same plot and subplot
    (locationID == "GRBA_N_209" & subplot == 1 & tag == 1340 & clumpNumber == 2 & stemLetter == "a") ~ as.factor(1),
    .default = stemLetter))

# To fail treeCauseOfDeathQC: make some trees D/RD and have NA or non-valid causes of death
treeFakeData2 <- treeFakeData2 %>%
  dplyr::mutate(
    causeOfDeath = dplyr::case_when(
      # Make some trees have invalid or missing causes of death
      (locationID == "GRBA_N_208" & subplot == 3 & tag == 1690) ~ "beetle",
      (locationID == "GRBA_N_209" & subplot == 5 & tag == 1567) ~ NA,
      .default = causeOfDeath),
    vitality = dplyr::case_when(
      # Change the vitality of the trees that now have missing/non-valid causes of death
      (locationID == "GRBA_N_208" & subplot == 3 & tag == 1690) ~ "Dead",
      (locationID == "GRBA_N_209" & subplot == 5 & tag == 1567) ~ "Recently Dead",
      .default = vitality))

# To fail coneCountQC: make some trees have cones but no cone count and some have no cones but cone count is populated
treeFakeData2 <- treeFakeData2 %>%
  dplyr::mutate(
  femaleCones = dplyr::case_when(
    (locationID == "GRBA_N_204" & subplot == 5 & tag == 454) ~ "Y",
    (locationID == "GRBA_N_008" & subplot == 3 & tag == 1286) ~ "N",
    .default = femaleCones),
  coneCount = dplyr::case_when(
    (locationID == "GRBA_N_204" & subplot == 5 & tag == 454) ~ 0,
    (locationID == "GRBA_N_008" & subplot == 3 & tag == 1286) ~ 48,
    .default = coneCount))

# To fail treeSubplotQC: make some trees have missing subplot numbers or subplot numbers not between 1 and 5
treeFakeData2[sample(1:480, 5, replace=FALSE), 'subplot'] =  sample(10:20, 5)
treeFakeData2[sample(1:480, 5, replace=FALSE), 'subplot'] =  NA

# To fail treeVitalityQC: make some trees NA or not Live, Dead, or Recently Dead
treeFakeData2 <- treeFakeData2 %>%
  dplyr::mutate(vitality = dplyr::case_when(
    # Change the vitality of the trees so they have missing or non-valid vitality
    (locationID == "GRBA_N_212" & subplot == 3 & tag == 3172) ~ "Pulled",
    (locationID == "GRBA_N_209" & subplot == 2 & tag == 1450) ~ NA,
    .default = vitality))

# To fail boleCankersILowerQC: make some of the trees have non-valid entries in the lower bole canker columns

# TODO: FIX THIS
# treeFakeData2 <- treeFakeData2 %>%
#   dplyr::mutate(
#     boleCankers_I_Lower = dplyr::case_when(
#       # Make one entry yes for bole cankers but NA for type
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ "Y",
#       # Make one entry no for bole cankers but have valid infestation type
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ "N",
#       # Make one entry yes for bole cankers but have a non-valid infestation type
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ "Y",
#       .default = boleCankers_I_Lower)
#     ,
#   boleCanks_ITypes_Lower = dplyr::case_when(
#     (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ NA,
#     (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ as.character("C"),
#     (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ as.character("Q"),
#     .default = boleCanks_ITypes_Lower)
#   )

# To fail boleCankersIMiddleQC: make some of the trees have non-valid entries in the middle bole canker columns

# To fail boleCankersIUpperQC: make some of the trees have non-valid entries in the upper bole canker columns

# To fail branchCankersILowerQC: make some of the trees have a non-valid response in the lower branch canker columns

# To fail branchCankersIMiddleQC: make some of the trees have a non-valid response in the middle branch canker columns

# To fail branchCankersIUpperQC: make some of the trees have non-valid entries in the upper branch canker columns


# To fail treeSpeciesQC: make some scientific names NA or blank
treeFakeData2[sample(1:480, 3, replace=FALSE), 'scientificName'] =  ""
treeFakeData2[sample(1:480, 3, replace=FALSE), 'scientificName'] =  NA


# To fail recentlyDeadTreeQC: make some of the trees be RD for multiple entries
treeFakeData2 <- treeFakeData2 %>%
  dplyr::mutate(vitality = dplyr::case_when(
    (locationID == "GRBA_N_403" & subplot == 3 & tag == 340) ~ "Recently Dead",
    .default = vitality))


# To fail mortalityYearQC: make some of the recently dead PIAL trees have a null mortality year
# treeFakeData2 <- treeFakeData2 %>%
#   # Make one tree PIAL
#   dplyr::mutate(
#     scientificName = dplyr::case_when(
#       () ~ 'Pinus albicaulis',
#       .default = scientificName
#     )
#   )
  # Make that tree be RD but have a null mortality year

# To fail crownHealthQC: make some trees have crown health that isn't a domain value or live PIAL trees with missing crown health

# To fail crownKillLowerQC: make some of the trees have lower crown kill that adds to more than 100% and make some live PIAL trees have a null lower crown kill but not null in both upper and middle crown kill percents

# To fail crownKillMiddleQC: make some of the trees have middle crown kill more than 100% and live PIAL trees have a null middle crown kill but not null in both upper and lower crown kill percents

# To fail crownKillUpperQC: make some of the trees have upper crown kill more than 100% and live PIAL trees have a null upper crown kill but not null in both middle and lower crown kill percents


# Write fake data to CSVs

# Location where the data should be saved
dataPath <- here::here("tests", "testthat", "testData")
# Location where the dictionary should be saved
dictionaryPath <- here::here("tests", "testthat", "testData", "dictionary")

# Run writePine() to save dictionaries to testData folder
fiveneedlepine::writePine(data_dir = dataPath, dictionary_dir = dictionaryPath)

# Save fake data to testData folder
readr::write_csv(visitFakeData, file = paste0(dataPath, "/Visit.csv"))
readr::write_csv(seedlingFakeData2, file = paste0(dataPath, "/Seedling.csv"))
readr::write_csv(treeFakeData2, file = paste0(dataPath, "/Tree.csv"))


