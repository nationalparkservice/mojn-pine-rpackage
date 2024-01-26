# for QC unit tests use load pine with location of fake data

library(tidyverse)
data_dir = "data/final"

# Extract the path of the first Access database in the data folder of the project directory
database_dir <- common::file.find(here::here("data"), "*.accdb", up = 0)[1]

# The database path can also be input manually
# database_dir <- "M:/MONITORING/Pine/Data/Database/Backend/FNP_MOJN_Primary.accdb"

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

# To fail subplotQC: make some entries have missing subplot numbers or subplot numbers that are not between 1 and 9
seedlingFakeData[sample(1:284, 5, replace=FALSE), 'subplot'] =  sample(9:20, 5)
seedlingFakeData[sample(1:284, 5, replace=FALSE), 'subplot'] =  NA

# To fail missingTagQC: make some of the live tree tags NA
# Arrange data frame so the plots with no seedlings are not selected
seedlingFakeData <- seedlingFakeData %>% dplyr::arrange(scientificName)
seedlingFakeData[sample(233:284, 5, replace=FALSE), 'tag'] =  NA

# To fail duplicateSeedlingTagQC: make some of the tags be duplicates of other tags
seedlingFakeData <- seedlingFakeData %>%
  dplyr::mutate(tag = dplyr::case_when(
    # Renumber one tree tag so it is a duplicate of another tree on the same plot and subplot
    (locationID == "GRBA_N_405" & subplot == 7 & tag == 485) ~ 489,
    .default = tag))

# To fail causeOfDeathQC: make some seedlings D/RD and have NA or non-valid causes of death
seedlingFakeData <- seedlingFakeData %>%
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
seedlingFakeData <- seedlingFakeData %>%
  dplyr::mutate(vitality = dplyr::case_when(
    # Change the vitality of the trees so they have missing or non-valid vitality
    (locationID == "GRBA_N_401" & subplot == 5 & tag == 140) ~ "P",
    (locationID == "GRBA_N_209" & subplot == 8 & tag == 1322) ~ NA,
    .default = vitality))

# To fail seedlingSpeciesQC: make some species names NA or blank
seedlingFakeData[sample(1:284, 3, replace=FALSE), 'scientificName'] =  ""
seedlingFakeData[sample(1:284, 3, replace=FALSE), 'scientificName'] =  NA

# To fail heightClassQC: make some have non-valid height classes
seedlingFakeData <- seedlingFakeData %>%
  dplyr::mutate(heightClass = dplyr::case_when(
    # Change the vitality of the trees so they have missing or non-valid vitality
    (locationID == "GRBA_N_405" & subplot == 8 & tag == 208) ~ "90 - <137 cm",
    (locationID == "GRBA_N_401" & subplot == 6 & tag == 1297) ~ NA,
    .default = heightClass))

# To fail recentlyDeadSeedlingQC: make some seedlings be RD for multiple entries
seedlingFakeData <- seedlingFakeData %>%
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

# To fail treeDuplicateTagQC: make some trees have duplicate tags of other tags
treeFakeData <- treeFakeData %>%
  dplyr::mutate(tag = dplyr::case_when(
    # Renumber one tree tag so it is a duplicate of another tree on the same plot and subplot
    (locationID == "GRBA_N_003" & subplot == 4 & tag == 130) ~ 125,
    .default = tag))

# To fail treeMissingTagQC: make some of the live trees have missing tags
# Arrange data frame so only live trees are not selected
treeFakeData <- treeFakeData %>% dplyr::arrange(vitality)
treeFakeData[sample(90:470, 5, replace=FALSE), 'tag'] =  NA

# To fail stemLetterQC: make some of the stem letter entries be not a letter
treeFakeData <- treeFakeData %>%
  dplyr::mutate(stemLetter = dplyr::case_when(
    # Renumber one tree tag so it is a duplicate of another tree on the same plot and subplot
    (locationID == "GRBA_N_209" & subplot == 1 & tag == 1340 & clumpNumber == 2 & stemLetter == "a") ~ as.factor(1),
    .default = stemLetter))

# To fail treeCauseOfDeathQC: make some trees D/RD and have NA or non-valid causes of death
treeFakeData <- treeFakeData %>%
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
treeFakeData <- treeFakeData %>%
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
treeFakeData[sample(1:480, 5, replace=FALSE), 'subplot'] =  sample(10:20, 5)
treeFakeData[sample(1:480, 5, replace=FALSE), 'subplot'] =  NA

# To fail treeVitalityQC: make some trees NA or not Live, Dead, or Recently Dead
treeFakeData <- treeFakeData %>%
  dplyr::mutate(vitality = dplyr::case_when(
    # Change the vitality of the trees so they have missing or non-valid vitality
    (locationID == "GRBA_N_212" & subplot == 3 & tag == 3172) ~ "Pulled",
    (locationID == "GRBA_N_209" & subplot == 2 & tag == 1450) ~ NA,
    .default = vitality))



# TODO: FIX bole and branch canker code, the error might be from the columns not being parsed in correctly
#
# # To fail boleCankersILowerQC: make some of the trees have non-valid entries in the lower bole canker columns
# treeFakeData <- treeFakeData %>%
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
#
# # To fail boleCankersIMiddleQC: make some of the trees have non-valid entries in the middle bole canker columns
# treeFakeData <- treeFakeData %>%
#   dplyr::mutate(
#     boleCankers_I_Mid = dplyr::case_when(
#       # Make one entry yes for bole cankers but NA for type
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ "Y",
#       # Make one entry no for bole cankers but have valid infestation type
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ "N",
#       # Make one entry yes for bole cankers but have a non-valid infestation type
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ "Y",
#       .default = boleCankers_I_Mid)
#     ,
#     boleCankers_ITypes_Mid = dplyr::case_when(
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ NA,
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ as.character("C"),
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ as.character("Q"),
#       .default = boleCankers_ITypes_Mid)
#   )
#
#
# # To fail boleCankersIUpperQC: make some of the trees have non-valid entries in the upper bole canker columns
# treeFakeData <- treeFakeData %>%
#   dplyr::mutate(
#     boleCankers_I_Upper = dplyr::case_when(
#       # Make one entry yes for bole cankers but NA for type
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ "Y",
#       # Make one entry no for bole cankers but have valid infestation type
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ "N",
#       # Make one entry yes for bole cankers but have a non-valid infestation type
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ "Y",
#       .default = boleCankers_I_Upper)
#     ,
#     boleCankers_ITypes_Upper = dplyr::case_when(
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ NA,
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ as.character("C"),
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ as.character("Q"),
#       .default = boleCankers_ITypes_Upper)
#   )
#
#
# # To fail branchCankersILowerQC: make some of the trees have a non-valid response in the lower branch canker columns
# treeFakeData <- treeFakeData %>%
#   dplyr::mutate(
#     branchCanks_I_Lower = dplyr::case_when(
#       # Make one entry yes for branch cankers but NA for type
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ "Y",
#       # Make one entry no for branch cankers but have valid infestation type
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ "N",
#       # Make one entry yes for branch cankers but have a non-valid infestation type
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ "Y",
#       .default = branchCanks_I_Lower)
#     ,
#     branchCanks_ITypes_Lower = dplyr::case_when(
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ NA,
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ as.character("C"),
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ as.character("Q"),
#       .default = branchCanks_ITypes_Lower)
#   )
#
#
# # To fail branchCankersIMiddleQC: make some of the trees have a non-valid response in the middle branch canker columns
# treeFakeData <- treeFakeData %>%
#   dplyr::mutate(
#     branchCanks_I_Mid = dplyr::case_when(
#       # Make one entry yes for branch cankers but NA for type
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ "Y",
#       # Make one entry no for branch cankers but have valid infestation type
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ "N",
#       # Make one entry yes for branch cankers but have a non-valid infestation type
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ "Y",
#       .default = branchCanks_I_Mid)
#     ,
#     branchCanks_ITypes_Mid = dplyr::case_when(
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ NA,
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ as.character("C"),
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ as.character("Q"),
#       .default = branchCanks_ITypes_Mid)
#   )
#
#
# # To fail branchCankersIUpperQC: make some of the trees have non-valid entries in the upper branch canker columns
# treeFakeData <- treeFakeData %>%
#   dplyr::mutate(
#     branchCanks_I_Upper = dplyr::case_when(
#       # Make one entry yes for branch cankers but NA for type
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ "Y",
#       # Make one entry no for branch cankers but have valid infestation type
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ "N",
#       # Make one entry yes for branch cankers but have a non-valid infestation type
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ "Y",
#       .default = branchCanks_I_Upper)
#     ,
#     branchCanks_ITypes_Upper = dplyr::case_when(
#       (locationID == "GRBA_N_204" & subplot == 5 & tag == 452) ~ NA,
#       (locationID == "GRBA_N_403" & subplot == 1 & tag == 170) ~ as.character("C"),
#       (locationID == "GRBA_N_206" & subplot == 1 & tag == 561) ~ as.character("Q"),
#       .default = branchCanks_ITypes_Upper)
#   )

# To fail treeSpeciesQC: make some scientific names NA or blank
treeFakeData[sample(1:480, 3, replace=FALSE), 'scientificName'] =  ""
treeFakeData[sample(1:480, 3, replace=FALSE), 'scientificName'] =  NA


# To fail recentlyDeadTreeQC: make some of the trees be RD for multiple entries
treeFakeData <- treeFakeData %>%
  dplyr::mutate(vitality = dplyr::case_when(
    (locationID == "GRBA_N_403" & subplot == 3 & tag == 340) ~ "Recently Dead",
    .default = vitality))


# To fail mortalityYearQC: make a tree be PIAL and RD and have a null mortality year
treeFakeData <- treeFakeData %>%
  # Make one tree PIAL
  dplyr::mutate(
    # Make some trees PIAL
    scientificName = dplyr::case_when(
      (locationID == "GRBA_N_211" & subplot == 5 & tag == 2237) ~ 'Pinus albicaulis',
      (locationID == "GRBA_N_407" & subplot == 5 & tag == 1833) ~ 'Pinus albicaulis',
      (locationID == "GRBA_N_210" & subplot == 4 & tag == 1510) ~ 'Pinus albicaulis',
      (locationID == "GRBA_N_212" & subplot == 4 & tag == 3203) ~ 'Pinus albicaulis',
      (locationID == "GRBA_N_403" & subplot == 5 & tag == 1132) ~ 'Pinus albicaulis',
      .default = scientificName),
    # Make that tree recently dead
    vitality = dplyr::case_when(
      (locationID == "GRBA_N_211" & subplot == 5 & tag == 2237) ~ 'Recently Dead',
      .default = vitality),
    # Give the same tree a null mortality year
    estimatedMortalityYear = dplyr::case_when(
      (locationID == "GRBA_N_211" & subplot == 5 & tag == 2237) ~ NA,
      .default = estimatedMortalityYear))

# To fail crownHealthQC: make some trees have crown health that isn't a domain value or live PIAL trees with missing crown health
treeFakeData <- treeFakeData %>%
  dplyr::mutate(
    crownHealth = dplyr::case_when(
      (locationID == "GRBA_N_403" & subplot == 5 & tag == 1142) ~ 'Risk',
      (locationID == "GRBA_N_407" & subplot == 5 & tag == 1833) ~ NA,
      .default = crownHealth),
  )


treeFakeData <- treeFakeData %>%
  dplyr::mutate(
    crownKill_Lower_percent = dplyr::case_when(
      # To fail crownKillMiddleQC: make live PIAL tree have null middle crown kill but not null in both upper and lower crown kill percents
      (locationID == "GRBA_N_210" & subplot == 4 & tag == 1510) ~ 75,
      # To fail crownKillLowerQC: make one tree have lower crown kill more than 100%
      (locationID == 'GRBA_N_002' & subplot == 1 & tag == 400) ~ 110,
      .default = crownKill_Lower_percent),
    crownKill_Mid_percent = dplyr::case_when(
      # To fail crownKillUpperQC: make live PIAL tree have null upper crown kill but not null in both middle and lower crown kill percents
      (locationID == "GRBA_N_212" & subplot == 4 & tag == 3203) ~ 75,
      # To fail crownKillMiddleQC: make one tree have middle crown kill more than 100%
      (locationID == 'GRBA_N_002' & subplot == 1 & tag == 400) ~ 110,
      .default = crownKill_Mid_percent),
    crownKill_Upper_percent = dplyr::case_when(
      # To fail crownKillLowerQC: make live PIAL tree have null lower crown kill but not null in both upper and middle crown kill percents
      (locationID == "GRBA_N_403" & subplot == 5 & tag == 1132) ~ 75,
      # To fail crownKillLowerQC: make one tree have upper crown kill more than 100%
      (locationID == 'GRBA_N_002'& subplot == 1 & tag == 400) ~ 110,
      .default = crownKill_Upper_percent))



# Write fake data to CSVs

# Location where the data should be saved
dataPath <- here::here("tests", "testthat", "testData")
# Location where the dictionary should be saved
dictionaryPath <- here::here("tests", "testthat", "testData", "dictionary")

# Run writePine() to save dictionaries to testData folder
fiveneedlepine::writePine(data_dir = dataPath, dictionary_dir = dictionaryPath)

# Save fake data to testData folder
readr::write_csv(visitFakeData, file = paste0(dataPath, "/Visit.csv"))
readr::write_csv(seedlingFakeData, file = paste0(dataPath, "/Seedling.csv"))
readr::write_csv(treeFakeData, file = paste0(dataPath, "/Tree.csv"))


