

#' Return a list of events that do not have 9 subplots
numberOfSubplotsQC <- function() {

  numOfSubplots <- get_data("Seedling")$data$Seedling %>%
    dplyr::group_by(eventID, locationID, eventDate) %>%
    dplyr::summarize(numSubplots = dplyr::n_distinct(subplot)) %>%
    dplyr::filter(numSubplots != 9)

  return(numOfSubplots)
}

#' Return a list of subplots that do not have a value between 1 and 9 or have a missing subplot number
subplotQC <- function() {
  subplotNumber <- get_data("Seedling")$data$Seedling %>%
    dplyr::select(eventID, scientificName, locationID, eventDate, subplot, tag) %>%
    dplyr::filter(subplot < 1 | subplot > 9 | is.na(subplot))

  return(subplotNumber)
}


#' Return a list of seedlings with missing tags
missingTagQC <- function(){
  seedlingDuplicateTag <- get_data("Seedling")$data$Seedling %>%
    dplyr::select(eventID, locationID, eventDate, subplot, tag, scientificName) %>%
    dplyr::filter(scientificName != 'No seedlings' & is.na(tag))

  return(seedlingDuplicateTag)
}

#' Return a list of seedling records with duplicate tags
duplicateSeedlingTagQC <- function() {
  seedlingDuplicateTag <- get_data("Seedling")$data$Seedling %>%
    dplyr::group_by(locationID, eventDate, subplot, tag) %>%
    dplyr::summarize(countTotal = dplyr::n()) %>%
    dplyr::filter((!is.na(tag) & countTotal > 1))

  return(seedlingDuplicateTag)
}


#' Return a list of seedlings that have a missing or non-valid cause of death
causeOfDeathQC <- function() {
  # Create vector of allowed cause of death categories
  causesOfDeath <- get_data("categories")$metadata$categories %>%
    dplyr::filter(attributeName == "causeOfDeath")

  causesOfDeath <- causesOfDeath[['code']]

  causeOfDeathFlag <- get_data("Seedling")$data$Seedling %>%
    dplyr::select(eventID, scientificName, locationID, eventDate, subplot, tag, vitality, causeOfDeath) %>%
    # flag data if a seedling is marked as recently dead or dead and the
    dplyr::filter((is.na(causeOfDeath) & (vitality == "RD" | vitality == "D")) | (!(causeOfDeath %in% causesOfDeath) & (vitality == "RD" | vitality == "D")))

  return(causeOfDeathFlag)
}

#' Return a list of seedlings whose vitality is missing or does not match a domain value (L, RD, D)
vitalityQC <- function(){
  vitalityOptions = c('L', 'D', 'RD')

  vitalityFlag <- get_data("Seedling")$data$Seedling %>%
    dplyr::select(eventID, scientificName, locationID, eventDate, subplot, tag, vitality) %>%
    dplyr::filter((scientificName != "No seedlings" & is.na(vitality)) | (!(vitality %in% vitalityOptions) & scientificName != "No seedlings"))

  return(vitalityFlag)
}

#' Return list of seedlings that do not have a species name that matches domain options
seedlingSpeciesQC <- function(){

  speciesFlag <- get_data("Seedling")$data$Seedling %>%
    dplyr::select(eventID, scientificName, locationID, eventDate, subplot, tag, vitality) %>%
    dplyr::filter(is.na(scientificName) | scientificName == "")

  return(speciesFlag)
}

#' Return a list of seedlings that are alive but the height class does not match domain values
heightClassQC <- function() {
  heightClassOptions <- c("20 - <50 cm", "50 - <100 cm", "100 - <137 cm")

  heightClassFlag <- get_data("Seedling")$data$Seedling %>%
    dplyr::select(eventID, scientificName, locationID, eventDate, subplot, tag, vitality, heightClass) %>%
    dplyr::filter(scientificName != "No seedlings" & vitality == 'L' & !(heightClass %in% heightClassOptions))

  return(heightClassFlag)
}


#' Returns a list of seedlings which are RD for multiple entries
recentlyDeadSeedlingQC <- function() {

  recentlyDeadFlag <- get_data("Seedling")$data$Seedling %>%
    dplyr::select(eventID, locationID, eventDate, subplot, tag, vitality) %>%
    dplyr::mutate(uniqueID = paste0(locationID, "_", subplot, "_", tag)) %>%
    # Filter for only recently dead entries
    dplyr::filter(vitality == 'RD') %>%
    dplyr::group_by(locationID, subplot, tag, vitality, uniqueID) %>%
    # Count number of entries for each seedling
    dplyr::summarise(count = dplyr::n()) %>%
    # Filter for only seedlings that have multiple recently dead entries
    dplyr::filter(count >1)

  return(recentlyDeadFlag)
}

