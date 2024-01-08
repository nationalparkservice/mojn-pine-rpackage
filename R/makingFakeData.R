# for QC unit tests use load pine with location of fake data

set.seed(2023)
# Fake Event Data
# for fake event test data pick a couple visits and join it with seedling/tree data that does not contain the corresponding visits
# (which join is the correct one for this???)
# fake event data
visitFakeData <- pine$data$Visit %>%
  filter(locationID == "GRBA_N_201" | locationID == "GRBA_N_408" | locationID == "GRBA_N_010")

# fake seedling data
seedlingFakeData <- pine$data$Seedling %>%
  sample_n(300) %>%
  filter(locationID != "GRBA_N_201" & locationID != "GRBA_N_408")

# join visit and seedling data
# and then flag any visits that did not join
noSeedlingFlag <- visitFakeData %>%
  dplyr::select(locationID, eventID, eventDate) %>%
  # Join with seedling data
  dplyr::full_join(seedlingFakeData, by = c('eventID', 'locationID', 'eventDate')) %>%
  dplyr::select(locationID, eventID, eventDate, network, park) %>%
  filter(is.na(network))

treeDataSmall  <- pine$data$Tree %>%
  sample_n(300) %>%
  filter(locationID != "GRBA_N_201" & locationID != "GRBA_N_408")

# join visit and seedlingd data
# and then flag any visits that did not join
noTreeDataFlag <- visitDataSmall %>%
  dplyr::select(locationID, eventID, eventDate) %>%
  # Join with seedling data
  dplyr::full_join(treeDataSmall, by = c('eventID', 'locationID', 'eventDate')) %>%
  dplyr::select(locationID, eventID, eventDate, network, park) %>%
  filter(is.na(network))

# Fake Seedling Data

# Use fake seedling data generated above
seedlingDataSmall3 <- seedlingDataSmall2 %>%
  dplyr::mutate()
# To fail numberOfSubplotsQC: either delete some entries or add fake entries (since data is a sample it'll probs fail this automatically)
# To fail subplotQC: make some entries of subplot have missing subplot numbers or numbers that are not between 1 and 9
# To fail missingTagQC: make some of the tags NA
# To fail duplicateSeedlingTagQC: make some of the tags be duplicates of other tags
# To fail causeOfDeathQC: make some seedlings D/RD and then make them have NA or non-valid causes of death
# To fail vitalityQC:make some seedlings NA or not L,D,RD
# To fail seedlingSpeciesQC: make some species names NA or blank
# To fail heightClassQC: make some have non-valid height classes
# To fail recentlyDeadSeedlingQC: make some seedlings be RD for multiple entries




