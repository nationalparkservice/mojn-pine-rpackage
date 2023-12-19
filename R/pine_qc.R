

# TODO: do i need to do this too?
# rm(list=ls()) # start with a clean slate

# TODO: PlotID_Number, SeedlingCount_ID, and PlotPhoto_ID are missing from query
# TODO: boleCanks_ITypes_Lower is named differently from mid and upper
# TODO: we can get rid of summmary table and might be able to get rid of exporting to excel
# TODO: should we restructure into more smaller functions??
# TODO: if something is commented out from FNP_Validation_20211110.R I didn't include it


#' Returns any events with no photo, seedling, and/or tree data
#' @returns table containing all of the pine event data quality flags
#' @export
eventDataQC <- function(){

  eventFlags <- get_data("Visit")$data$Visit %>%
    dplyr::select(locationID, eventID) %>%
    # Join with photo tables
    dplyr::full_join(get_data("Photo")$data$Photo, by = c('eventID', 'locationID')) %>%
    dplyr::group_by(eventID) %>%
    dplyr::summarise(photoCount = dplyr::n()) %>%
    # Flag any events that have no corresponding photos
    dplyr::mutate(flagNoPhotos = dplyr::case_when(
      (photoCount == 0 | photoCount == NA) ~ 'E',
      TRUE ~ NA)) %>%
    # Join with seedling data
    dplyr::full_join(get_data("Seedling")$data$Seedling, by = c('eventID')) %>%
    dplyr::group_by(eventID, photoCount, flagNoPhotos) %>%
    dplyr::summarise(seedlingCount = dplyr::n()) %>%
    # Flag any events with no corresponding seedling data
    dplyr::mutate(flagNoSeedlings = dplyr::case_when(
      (seedlingCount == 0 | seedlingCount == NA) ~ 'E',
      TRUE ~ NA)) %>%
    # Join with tree data
    dplyr::full_join(get_data("Tree")$data$Tree, by = c('eventID')) %>%
    dplyr::group_by(eventID, photoCount, flagNoPhotos, seedlingCount, flagNoSeedlings, locationID, eventDate) %>%
    dplyr::summarise(treeCount = dplyr::n()) %>%
    # Flag any events with no corresponding tree data
    dplyr::mutate(flagNoTrees = dplyr::case_when(
      (treeCount == 0 | treeCount == NA) ~ 'E',
      TRUE ~ NA)) %>%
    # Only return rows that are not NA in one of the flag columns
    dplyr::filter(dplyr::if_any(contains("flag"), ~ !is.na(.x)))
    # dplyr::filter(dplyr::if_any(grep("(?i)flag", names(.)), ~ !is.na(.x)))

  print(eventFlags)
  return(eventFlags)
}

#' Creates an excel spreadsheet with any data quality flags present in the pine photo table
#' @returns table containing all of the pine photo data quality flags
#' @export
photoDataQC <- function() {

  # here::here()
  # source("R/utils.R")

  # Returns events where there are less than 4 photos
  countPhotos <- pine$data$Photo %>%
    dplyr::group_by(locationID, eventDate) %>%
    dplyr::summarize(countPhotos = dplyr::n())

  photoFlags <- get_data("Photo")$data$Photo %>%
    dplyr::select(locationID, eventDate, park, bearing, location, cameraImageNumber, eventID) %>%
    # QC checks photo data and adds flags where necessary
    dplyr::mutate(
      # If bearing not between 0 and 360, add 'E' to the bearing flagging field, if missing add 'M
      flagBearing = dplyr::case_when(
        bearing < 0 | bearing > 360 ~ "E",
        is.na(bearing) & !is.na(eventID) ~ "M",
        TRUE ~ NA)) %>%
    # Join photos table with table that has fewer than 4 photos
    dplyr::full_join(countPhotos, by = c('locationID', 'eventDate')) %>%
    # If a event has less than 4 photos add 'E' to photo count flag
    dplyr::mutate(flagPhotoCount = dplyr::case_when(
      countPhotos < 4 ~ 'E',
      TRUE ~ NA)) %>%
    # If location reference does not match a domain value, add 'E' to the location reference flagging field, if missing add 'M'
    # TODO: might be a smoother way to do this by referencing location categories instead of hardcoding them
    dplyr::mutate(flagPhotoLocation = dplyr::case_when(
      !(location %in% c('NE_Corner', 'NW_Corner', 'SE_Corner', 'SW_Corner', 'Landmark')) ~ "E",
      is.na(location) & !is.na(eventID) ~ "M",
      TRUE ~ NA)) %>%
    # If Camera Image Number is missing, add 'M' to the Camera Image Number flagging field
    dplyr::mutate(flagImageNum = dplyr::case_when(
      is.na(cameraImageNumber) & !is.na(eventID) ~ "M",
      TRUE ~ NA)) %>%
    # Checks if all photos are missing
    dplyr::mutate(flagAllPhotosMissing = dplyr::case_when(
      is.na(eventID) ~ "M",
      TRUE ~ NA)) %>%
    # Only return rows that are not NA in one of the flag columns
    dplyr::filter(dplyr::if_any(grep("(?i)flag", names(.)), ~ !is.na(.x)))

  # Creates a table with how many flags there are for each category
  # TODO: could also just return only rows that have an error (AKA filter out all rows that have zero errors)
  photoFlagSummary <- photoFlags %>%
    dplyr::select(grep("(?i)flag", names(.))) %>%
    tidyr::pivot_longer(cols = grep("(?i)flag", names(.)) , names_to = 'flagType', values_to = "errorType") %>%
    dplyr::mutate(error = dplyr::case_when(
        (errorType == 'E') ~ 1,
        TRUE ~ 0),
      missing = dplyr::case_when(
        (errorType == 'M') ~ 1,
        TRUE ~ 0),
      sample = dplyr::case_when(
        (errorType == 'S') ~ 1,
        TRUE ~ 0)) %>%
    dplyr::group_by(flagType) %>%
    dplyr::summarise(
      totalError = sum(error),
      totalMissing = sum(missing),
      totalSample = sum(sample))

  # Data Export to Excel
  here::here()
  folder <- "dataFlags"
  # Write csv of tree data flags to folder
  if(file.exists(folder)) {
    readr::write_csv(photoFlags, paste0(folder, "/photoDataFlags_", format(lubridate::now(), "%Y%m%d_%H%M%S"),".csv"), na = "")
  } else {
    # if folder doesn't exist create folder then write csv of tree data flags
    dir.create("dataFlags")
    readr::write_csv(photoFlags, paste0(folder, "/photoDataFlags_", format(lubridate::now(), "%Y%m%d_%H%M%S"),".csv"), na = "")
  }

    print(photoFlags)
    return(photoFlags)

}

#' Creates an excel spreadsheet with any data quality flags present in the pine seedling table
#' @returns table containing all the pinr seedling data quality flags
#' @export
seedlingDataQC <- function(){
  # Returns seedling records with duplicate tags
  seedlingDuplicateTag <- get_data("Seedling")$data$Seedling %>%
    #select(park, locationID, eventDate, tag) %>%
    dplyr::group_by(locationID, eventDate, tag) %>%
    dplyr::summarize(countTotal = dplyr::n()) %>%
    dplyr::filter(!is.na(tag) & countTotal > 1)

  # TODO: figure out what to do with this check
  # since there's a group by when it gets joined to the normal table all the rows that were grouped get an error
  numOfSubplots <- seedling %>%
    group_by(locationID, eventDate) %>%
    summarize(numSubplots = dplyr::n()) %>%
    filter(numSubplots != 9)


  seedlingFlags <- get_data("Seedling")$data$Seedling %>%
    dplyr::select(locationID, eventDate, subplot, speciesCode, heightClass, tag, vitality, causeOfDeath, eventID, ) %>%
    # Join seedling data with duplicate tags
    dplyr::full_join(seedlingDuplicateTag, by = c('locationID', 'eventDate', 'tag')) %>%
    # If a tag number is used more than once, add S to tag flagging field, if missing add 'M'
    dplyr::mutate(flagTag = dplyr::case_when(
      !is.na(countTotal) ~ 'S',
      (speciesCode != '_NONE' & speciesCode != '_NotSampled' & is.na(tag)) ~ 'M',
      TRUE ~ NA)) %>%
    # If cause of death doesn't match a domain value, add 'E' to the death cause flagging field, if missing add 'M'
    dplyr::mutate(flagCauseOfDeath = dplyr::case_when(
      # TODO: once categories are importing correctly fix code below
      # SeedlingFlags$Flag_Death_Cause <- if_else(is.na(DataSeedlings$DeadTreeCause_Code) & !is.na(DataSeedlings$Death_Cause),"E", SeedlingFlags$Flag_Death_Cause)
      # ~ 'E',
      is.na(causeOfDeath) & vitality == "D" ~ 'M',
      TRUE ~ NA)) %>%
    # #If the seedling is not dead, the species is not NONE and height class does not match domain values, add 'E' to the height class flagging field, if missing add 'M'
    # TODO: once categories are imported correctly fix code below
    # SeedlingFlags$Flag_Height_Class <- if_else((SeedlingFlags$Height_Class != '20 - <50 cm' & SeedlingFlags$Height_Class != '50 - <100 cm' & SeedlingFlags$Height_Class != '100 - <137 cm' & !is.na(SeedlingFlags$Height_Class)),"E", SeedlingFlags$Flag_Height_Class)
    dplyr::mutate(flagHeightClass = dplyr::case_when(
      # ~ 'E',
      vitality != "D" & is.na(heightClass) & speciesCode != "_NONE" ~ 'M',
      TRUE ~ NA)) %>%
    # If subplot number doesn't match a domain value (not between 1 and -9), add 'E' to the status flagging field, if missing add 'M'
    dplyr::mutate(flagSubplot = dplyr::case_when(
      (subplot < 1 | subplot > 9) ~ 'E',
      is.na(subplot) ~ 'M',
      TRUE ~ NA)) %>%
    # If Species_Code doesn't match a domain value, add 'E' to the species code flagging field, if missing add 'M
    dplyr::mutate(flagSpeciesCode = dplyr::case_when(
      # TODO: once categories are imported correctly fix code below
      # SeedlingFlags$Flag_SpCode <- if_else(((DataSeedlings$Unit_Code == "CRLA" & DataSeedlings$Species_Code != "_NONE" & DataSeedlings$Species_Code != "_NotSampled" & DataSeedlings$Species_Code != "ABAM" & DataSeedlings$Species_Code != "ABCO" & DataSeedlings$Species_Code != "ABLA" & DataSeedlings$Species_Code != "ABMA" & DataSeedlings$Species_Code != "ABSH" & DataSeedlings$Species_Code != "CADE27" & DataSeedlings$Species_Code != "CANO9" & DataSeedlings$Species_Code != "DEAD" & DataSeedlings$Species_Code != "JUOCO" & DataSeedlings$Species_Code != "PIAL" & DataSeedlings$Species_Code != "PICOM" & DataSeedlings$Species_Code != "PIEN" & DataSeedlings$Species_Code != "PIJE" & DataSeedlings$Species_Code != "PILA" & DataSeedlings$Species_Code != "PIMO3" & DataSeedlings$Species_Code != "PIPOW2" & DataSeedlings$Species_Code != "PSMEM" & DataSeedlings$Species_Code != "TSHE" & DataSeedlings$Species_Code != "TSME" & DataSeedlings$Species_Code != "UNKNOWN") | (DataSeedlings$Unit_Code == "LAVO" & DataSeedlings$Species_Code != "_NONE" & DataSeedlings$Species_Code != "_NotSampled" & DataSeedlings$Species_Code != "ABAM" & DataSeedlings$Species_Code != "ABCO" & DataSeedlings$Species_Code != "ABLA" & DataSeedlings$Species_Code != "ABMA" & DataSeedlings$Species_Code != "ABSH"  & DataSeedlings$Species_Code != "DEAD" & DataSeedlings$Species_Code != "PIAL" & DataSeedlings$Species_Code != "PICOM" & DataSeedlings$Species_Code != "PIJE" & DataSeedlings$Species_Code != "PILA" & DataSeedlings$Species_Code != "PIMO3" & DataSeedlings$Species_Code != "PIPOW2" & DataSeedlings$Species_Code != "TSME" & DataSeedlings$Species_Code != "UNKNOWN")),"E", SeedlingFlags$Flag_SpCode)
      # ~ 'E',
      is.na(speciesCode) ~ 'M',
      TRUE ~ NA)) %>%
    # If status doesn't match a domain value (L, RD, D), add 'E' to the status flagging field, if missing add 'M'
    dplyr::mutate(flagVitality = dplyr::case_when(
      # TODO: once categories are imported correctly fix code below
      # SeedlingFlags$Flag_Status <- ifelse((SeedlingFlags$Status != 'L') & (SeedlingFlags$Status != 'D') & (SeedlingFlags$Status != 'RD') & !is.na(SeedlingFlags$Status),"E", SeedlingFlags$Flag_Status)
      # ~ 'E',
      (speciesCode != '_NONE' & speciesCode != '_NotSamples' & is.na(vitality)) ~ 'M',
      TRUE ~ NA)) %>%
    # Join seedling data with visits that don't have 9 subplots data
    dplyr::full_join(numOfSubplots, by = c('locationID', 'eventDate')) %>%
    # If there are not 9 seedling subplots for an event add 'E' to the subplot number flag
    dplyr::mutate(flagSubplotNumber = dplyr::case_when(
      (numSubplots != 9) ~ 'E',
      TRUE ~ NA)) %>%
    # Only return rows that have are not NA in one of the flag columns
    dplyr::filter(dplyr::if_any(grep("(?i)flag", names(.)), ~ !is.na(.x)))


  # Creates a summary table of how many flags there are for each category
  seedlingFlagSummary <- seedlingFlags %>%
    dplyr::select(grep("(?i)flag", names(.))) %>%
    tidyr::pivot_longer(cols = grep("(?i)flag", names(.)), names_to = 'flagType', values_to = "errorType") %>%
    dplyr::mutate(error = dplyr::case_when(
      (errorType == 'E') ~ 1,
      TRUE ~ 0),
      missing = dplyr::case_when(
        (errorType == 'M') ~ 1,
        TRUE ~ 0),
      sample = dplyr::case_when(
        (errorType == 'S') ~ 1,
        TRUE ~ 0)) %>%
    dplyr::group_by(flagType) %>%
    dplyr::summarise(
      totalError = sum(error),
      totalMissing = sum(missing),
      totalSample = sum(sample))

  # Data export to Excel
  here::here()
  folder <- "dataFlags"
  # Write csv of seedling data flags to folder
  if(file.exists(folder)) {
    readr::write_csv(seedlingFlags, paste0(folder, "/seedlingDataFlags_", format(lubridate::now(), "%Y%m%d_%H%M%S"),".csv"), na = "")
  } else {
    # if folder doesn't exist create folder then write csv of seedling data flags
    dir.create("dataFlags")
    readr::write_csv(seedlingFlags, paste0(folder, "/seedlingDataFlags_", format(lubridate::now(), "%Y%m%d_%H%M%S"),".csv"), na = "")
  }

  print(seedlingFlags)
  return(seedlingFlags)
}

#' Creates an excel spreadsheet with any data quality flags present in the pine tree table
#' @returns table containing all of the pine tree data quality flags
#' @export
treeDataQC <- function(){
  # Returns tree records with duplicate tags
  # TODO: in the original QC script it groups by eventData but in the validation script it doesn't
  treeDuplicateTag <- get_data("Tree")$data$Tree %>%
    #dplyr::select(park, locationID, eventDate, tag) %>%
    dplyr::group_by(locationID, eventDate, tag)%>%
    dplyr::summarize(countTotal = dplyr::n()) %>%
    dplyr::filter(!is.na(tag) & countTotal > 1)


  treeFlags <- get_data("Tree")$data$Tree %>%
    dplyr::select(park, locationID:treeDBH_cm, vitality, causeOfDeath, estimatedMortalityYear:crownKill_Lower_percent,
                  branchCanks_I_Upper, branchCanks_ITypes_Upper, branchCanks_I_Mid, branchCanks_ITypes_Mid, branchCanks_I_Lower, branchCanks_ITypes_Lower,
                  boleCankers_I_Lower, boleCanks_ITypes_Lower, boleCankers_I_Mid, boleCankers_ITypes_Mid, boleCankers_I_Upper, boleCankers_ITypes_Upper,
                  femaleCones:treeNotes) %>%
    # Join tree data with the duplicate tag table
    dplyr::full_join(treeDuplicateTag, by = c('locationID', 'eventDate', 'tag')) %>%
    # If a tag number is used more than once, add S to tag flagging field, if missing add 'M'
    dplyr::mutate(flagTag = dplyr::case_when(
      !is.na(countTotal) ~ 'S',
      is.na(tag) ~ 'M',
      TRUE ~ NA)) %>%
    #If stem letter is not a letter, add 'E' to stem letter flagging field
    dplyr::mutate(flagStemLetter = dplyr::case_when(
      ((!grepl("^[[:alpha:]]+$", stemLetter, FALSE)) & !is.na(stemLetter)) ~ 'E',
      TRUE ~ NA)) %>%
    #If death cause does not match domain values (tlu_DeadTree_ProbCauses), add 'E' to the death cause flagging field, if missing add 'M'
    dplyr::mutate(flagCauseOfDeath = dplyr::case_when(
      # TODO: once categories are imported correctly fix code below
      # TreeFlags$Flag_Death_Cause <- if_else((TreeFlags$StatusDead_Cause != "ANMLDMG" & TreeFlags$StatusDead_Cause != "BB_notMPB" & TreeFlags$StatusDead_Cause != "BROKSTEM" & TreeFlags$StatusDead_Cause != "CRUSH" & TreeFlags$StatusDead_Cause != "DEFOLIATE" & TreeFlags$StatusDead_Cause != "DIS_notWPBR" & TreeFlags$StatusDead_Cause != "FIRE" & TreeFlags$StatusDead_Cause != "LGHTNIN" & TreeFlags$StatusDead_Cause != "MPB" & TreeFlags$StatusDead_Cause != "MSTLTOE" & TreeFlags$StatusDead_Cause != "SUPPRESS" & TreeFlags$StatusDead_Cause != "UNKNOWN" & TreeFlags$StatusDead_Cause != "UPROOT" & TreeFlags$StatusDead_Cause != "WPBR"),"E", TreeFlags$Flag_Death_Cause)
      #~ 'E',
      (is.na(causeOfDeath) & vitality == 'RD') ~ 'M',
      TRUE ~ NA)) %>%
    # If the tree is not dead and greater than 50 m tall add an 'E' to the height flagging field
    # If tree height is 999 or -999, make the field null and add "M" to height flagging field. Also add M to any fields that were already null.
    dplyr::mutate(
      flagTreeHeight = dplyr::case_when(
      (vitality != 'D' & (treeHeight_m > 50 & treeHeight_m != 999)) ~ 'E',
      (is.na(treeHeight_m) | treeHeight_m == 999 | treeHeight_m == -999) ~ 'M',
      TRUE ~ NA),
      # If tree height is 999 or -999 make the height null
      treeHeight_m = dplyr::case_when(
        (treeHeight_m == 999 | treeHeight_m == -999) ~ NA_real_,
        .default = treeHeight_m)) %>%
    #If tree is recently dead, species is PIAL, and mortality year is null, add "M" to status flagging column
    dplyr::mutate(flagMortalityYear = dplyr::case_when(
      (vitality == 'RD' & speciesCode == 'PIAL' & is.na(estimatedMortalityYear)) ~ 'M',
      TRUE ~ NA)) %>%
    # If cones exist but cone count is not populated, add M to cone count flag field
    # ALTHOUGH THE ERROR COULD BE ON THE FemaleCones_YN field
    # If cones count = No but cone count is populated, add 'S' to cone count flag field
    dplyr::mutate(
      flagConeCount = dplyr::case_when(
        (femaleCones == 'Y' & is.na(coneCount)) ~ 'M',
        (femaleCones == 'N' & coneCount > 0) ~ 'S',
        TRUE ~ NA),
      # TODO: this is some data cleaning that could probably go in a separate file
      femaleCones = dplyr::case_when(
        (femaleCones == 'ND') ~ NA,
        .default = femaleCones),
      # TODO: this is some data cleaning that could probably go in a separate file
      coneCount = dplyr::case_when(
        (coneCount == -999 | coneCount == 999) ~ NA,
        .default = coneCount)) %>%
    # If crown health doesn't equal domain values (1-5) add E to crown health flagging field, if missing add 'M'
    dplyr::mutate(flagCrownHealth = dplyr::case_when(
      (crownHealth < 1 | crownHealth > 5) ~ 'E',
      (vitality == 'L' & speciesCode == 'PIAL' & is.na(crownHealth)) ~ 'M',
      TRUE ~ NA)) %>%
    # If crown kill lower is greater than 100, add E to crown kill lower flag field
    # If tree status is L, species is PIAL, the lower crown kill percent is null, but the upper or middle crownkill percents are not null, add S to crown kill lower percent flag field
    dplyr::mutate(flagCrownKillLower = dplyr::case_when(
      (crownKill_Lower_percent > 100) ~ "E",
      (vitality == "L" & speciesCode == "PIAL" & is.na(crownKill_Lower_percent) & (!is.na(crownKill_Upper_percent) | !is.na(crownKill_Mid_percent))) ~"S",
      TRUE ~ NA)) %>%
    # If crown kill mid is greater than 100, add E to crown kill mid flag field
    # If tree status is L, species is PIAL, the middle crown kill percent is null, but the upper or lower crownkill percents are not null, add S to crown kill middle percent flag field
    dplyr::mutate(flagCrownKillMid = dplyr::case_when(
      (crownKill_Mid_percent > 100) ~ "E",
      (vitality == "L" & speciesCode == "PIAL") & is.na(crownKill_Mid_percent) & (!is.na(crownKill_Upper_percent) | !is.na(crownKill_Lower_percent)) ~ "S",
      TRUE ~ NA)) %>%
    # If crown kill upper percent is greater than 100, add E to crown kill upper flag field
    # If tree status is L, species is PIAL, the upper crown kill percent is null, but the mid or lower crownkill percents are not null, add S to crown kill upper percent flag field
    dplyr::mutate(flagCrownKillUpper = dplyr::case_when(
      (crownKill_Upper_percent > 100) ~ "E",
      (vitality == "L" & speciesCode == "PIAL") & is.na(crownKill_Upper_percent) & (!is.na(crownKill_Mid_percent) | !is.na(crownKill_Lower_percent)) ~ "S",
      TRUE ~ NA)) %>%
    # If DBH is greater than 200cm, add an S to the Tree DBH flag field, if missing add 'M'
    dplyr::mutate(
      flagTreeDBH = dplyr::case_when(
        (treeDBH_cm > 200 & treeDBH_cm != 999) ~ "S",
        #If tree DBH is 999, -999, or missing add 'M' to the DBH flagging field
        (is.na(treeDBH_cm) | treeDBH_cm == 999 | treeDBH_cm == -999) ~ 'M',
        TRUE ~ NA),
        #If tree DBH is 999 or -999 make the field null
      treeDBH_cm = dplyr::case_when(
        (treeDBH_cm == 999 | treeDBH_cm == -999) ~ NA_real_,
        .default = treeDBH_cm
      )) %>%
    # If subplotID not within range (1-5), add an M to the subplot flag field, if missing add 'M'
    dplyr::mutate(flagSubplot = dplyr::case_when(
      (subplot < 1 | subplot > 5) ~ 'E',
      is.na(subplot) ~ 'M',
      TRUE ~ NA)) %>%
    # If status doesn't match a domain value (L, D, RD), add an E to the status flag field, if missing add 'M'
    dplyr::mutate(flagVitality = dplyr::case_when(
      # TODO: once categories table works use that instead of hardcoding it
      (vitality != 'Live' & vitality != 'Dead' & vitality != 'Recently Dead') ~ 'E',
      is.na(vitality) ~ 'M',
      TRUE ~ NA)) %>%
    # TODO: could combine into one column: ex: flag 'I' for one type and 'T' for the other type
    # Flags for lower bole cankers
    dplyr::mutate(
      # If lower bole canks infestation checkbox = Yes but infestation type is null, add S to BoleCanks_ITypes_Lower flagging field
      flagBoleCanks_ITypes_Lower = dplyr::case_when(
        (boleCankers_I_Lower == 'Y' & is.na(boleCanks_ITypes_Lower)) ~ 'S',
        # If bole canks lower doesn't match domain values, add E to bole canks lower flagging field
        (!grepl("A|C|F|O|R|S", boleCanks_ITypes_Lower, FALSE)) & !is.na(boleCanks_ITypes_Lower) ~ 'E',
        TRUE ~ NA),
      # If lower bole canks infestation checkbox = No but infestation type is not null, add S to BoleCankers_I_Lower_YN flagging field
      flagBoleCanks_I_Lower = dplyr::case_when(
        (boleCankers_I_Lower == 'N' & !is.na(boleCanks_ITypes_Lower)) ~ 'S',
        TRUE ~ NA)) %>%
    # Flags for middle bowl cankers
    dplyr::mutate(
      # If middle bole canks infestation checkbox = Yes but infestation type is null, add S to BoleCanks_ITypes_Mid flagging field
      flagBoleCanks_ITypes_Mid = dplyr::case_when(
        (boleCankers_I_Mid == 'Y' & is.na(boleCankers_ITypes_Mid)) ~ 'S',
        # If bole canks mid doesn't match domain values, add E to bole canks mid flagging field
        (!grepl("A|C|F|O|R|S", boleCankers_ITypes_Mid, FALSE)) & !is.na(boleCankers_ITypes_Mid) ~ 'E',
        TRUE ~ NA),
      # If middle bole canks infestation checkbox = No but infestation type is not null, add S to BoleCankers_I_Mid_YN flagging field
      flagBoleCanks_I_Mid = dplyr::case_when(
        (boleCankers_I_Mid == 'N' & !is.na(boleCankers_ITypes_Mid)) ~ 'S',
        TRUE ~ NA)) %>%
    # Flags for upper bowl cankers
    dplyr::mutate(
      # If upper bole canks infestation checkbox = Yes but infestation type is null, add S to BoleCanks_ITypes_Upper flagging field
      flagBoleCanks_ITypes_Upper = dplyr::case_when(
        (boleCankers_I_Upper == 'Y' & is.na(boleCankers_ITypes_Upper)) ~ 'S',
        # If bole canks lower doesn't match domain values, add E to bole canks lower flagging field
        (!grepl("A|C|F|O|R|S", boleCankers_ITypes_Upper, FALSE)) & !is.na(boleCankers_ITypes_Upper) ~ 'E',
        TRUE ~ NA),
      # If upper bole canks infestation checkbox = No but infestation type is not null, add S to BoleCankers_I_Upper_YN flagging field
      flagBoleCanks_I_Upper = dplyr::case_when(
        (boleCankers_I_Upper == 'N' & !is.na(boleCankers_ITypes_Upper)) ~ 'S',
        TRUE ~ NA)) %>%
    # Flags for lower branch cankers
    dplyr::mutate(
      # If lower branch canks infestation checkbox = Yes but infestation type is null, add S to BranchCanks_ITypes_Lower flagging field
      flagBranchCanks_ITypes_Lower = dplyr::case_when(
        (branchCanks_I_Lower == 'Y' & is.na(branchCanks_ITypes_Lower)) ~ 'S',
        # If branch canks lower doesn't match domain values, add E to branch canks lower flagging field
        (!grepl("A|C|F|O|R|S", branchCanks_ITypes_Lower, FALSE)) & !is.na(branchCanks_ITypes_Lower) ~ 'E',
        TRUE ~ NA),
      # If lower branch canks infestation checkbox = No but infestation type is not null, add S to BranchCanks_I_Lower_YN flagging field
      flagBranchCanks_I_Lower = dplyr::case_when(
        (branchCanks_I_Lower == 'N' & !is.na(branchCanks_ITypes_Lower)) ~ 'S',
        TRUE ~ NA)) %>%
    # Flags for middle branch cankers
    dplyr::mutate(
      # If middle branch canks infestation checkbox = Yes but infestation type is null, add S to BranchCanks_ITypes_Mid flagging field
      flagBranchCanks_ITypes_Mid = dplyr::case_when(
        (branchCanks_I_Mid == 'Y' & is.na(branchCanks_ITypes_Mid)) ~ 'S',
        # If branch canks mid doesn't match domain values, add E to branch canks mid flagging field
        (!grepl("A|C|F|O|R|S", branchCanks_ITypes_Mid, FALSE)) & !is.na(branchCanks_ITypes_Mid) ~ 'E',
        TRUE ~ NA),
      # If middle branch canks infestation checkbox = No but infestation type is not null, add S to BranchCanks_I_Mid_YN flagging field
      flagBranchCanks_I_Mid = dplyr::case_when(
        (branchCanks_I_Mid == 'N' & !is.na(branchCanks_ITypes_Mid)) ~ 'S',
        TRUE ~ NA)) %>%
    # Flags for upper branch cankers
    dplyr::mutate(
      # If upper branch canks infestation checkbox = Yes but infestation type is null, add S to BranchCanks_ITypes_Upper flagging field
      flagBranchCanks_ITypes_Upper = dplyr::case_when(
        (branchCanks_I_Upper == 'Y' & is.na(branchCanks_ITypes_Upper)) ~ 'S',
        # If branch canks upper doesn't match domain values, add E to branch canks upper flagging field
        (!grepl("A|C|F|O|R|S", branchCanks_ITypes_Upper, FALSE)) & !is.na(branchCanks_ITypes_Upper) ~ 'E',
        TRUE ~ NA),
      # If upper branch canks infestation checkbox = No but infestation type is not null, add S to BranchCanks_I_Upper_YN flagging field
      flagBranchCanks_I_Upper = dplyr::case_when(
        (branchCanks_I_Upper == 'N' & !is.na(branchCanks_ITypes_Upper)) ~ 'S',
        TRUE ~ NA)) %>%
    # If Species_Code doesn't match a domain value, add 'E' to the species code flagging field, if missing add 'M'
    dplyr::mutate(flagSpeciesCode = dplyr::case_when(
      # TODO: once categories metadata is imported correctly fix the code below
      # TreeFlags$Flag_Sp_Code <- if_else(((DataTrees$Unit_Code == "CRLA" & DataTrees$Species_Code != "_NONE" & DataTrees$Species_Code != "_NotSampled" & DataTrees$Species_Code != "ABAM" & DataTrees$Species_Code != "ABCO" & DataTrees$Species_Code != "ABLA" & DataTrees$Species_Code != "ABMA" & DataTrees$Species_Code != "ABSH" & DataTrees$Species_Code != "CADE27" & DataTrees$Species_Code != "CANO9" & DataTrees$Species_Code != "DEAD" & DataTrees$Species_Code != "JUOCO" & DataTrees$Species_Code != "PIAL" & DataTrees$Species_Code != "PICOM" & DataTrees$Species_Code != "PIEN" & DataTrees$Species_Code != "PIJE" & DataTrees$Species_Code != "PILA" & DataTrees$Species_Code != "PIMO3" & DataTrees$Species_Code != "PIPOW2" & DataTrees$Species_Code != "PSMEM" & DataTrees$Species_Code != "TSHE" & DataTrees$Species_Code != "TSME" & DataTrees$Species_Code != "UNKNOWN") | (DataTrees$Unit_Code == "LAVO" & DataTrees$Species_Code != "_NONE" & DataTrees$Species_Code != "_NotSampled" & DataTrees$Species_Code != "ABAM" & DataTrees$Species_Code != "ABCO" & DataTrees$Species_Code != "ABLA" & DataTrees$Species_Code != "ABMA" & DataTrees$Species_Code != "ABSH"  & DataTrees$Species_Code != "DEAD" & DataTrees$Species_Code != "PIAL" & DataTrees$Species_Code != "PICOM" & DataTrees$Species_Code != "PIJE" & DataTrees$Species_Code != "PILA" & DataTrees$Species_Code != "PIMO3" & DataTrees$Species_Code != "PIPOW2" & DataTrees$Species_Code != "TSME" & DataTrees$Species_Code != "UNKNOWN")),"E", TreeFlags$Flag_Sp_Code)
      #~ 'E',
      is.na(speciesCode) ~ 'M',
      TRUE ~ NA)) %>%
    # Only return rows that have are not NA in one of the flag columns
    dplyr::filter(dplyr::if_any(grep("(?i)flag", names(.)), ~ !is.na(.x)))

  # Creates a summary table of all the pine tree data quality flags
  treeFlagSummary <- treeFlags %>%
    dplyr::select(grep("(?i)flag", names(.))) %>%
    tidyr::pivot_longer(cols = grep("(?i)flag", names(.)), names_to = 'flagType', values_to = "errorType") %>%
    dplyr::mutate(error = dplyr::case_when(
      (errorType == 'E') ~ 1,
      TRUE ~ 0),
      missing = dplyr::case_when(
        (errorType == 'M') ~ 1,
        TRUE ~ 0),
      sample = dplyr::case_when(
        (errorType == 'S') ~ 1,
        TRUE ~ 0)) %>%
    dplyr::group_by(flagType) %>%
    dplyr::summarise(
      totalError = sum(error),
      totalMissing = sum(missing),
      totalSample = sum(sample))

  # Data export to Excel
  here::here()
  folder <- "dataFlags"
  # Write csv of tree data flags to folder
  if(file.exists(folder)) {
    readr::write_csv(treeFlags, paste0(folder, "/treeDataFlags_", format(lubridate::now(), "%Y%m%d_%H%M%S"),".csv"), na = "")
  } else {
    # if folder doesn't exist create folder then write csv of tree data flags
    dir.create("dataFlags")
    readr::write_csv(treeFlags, paste0(folder, "/treeDataFlags_", format(lubridate::now(), "%Y%m%d_%H%M%S"),".csv"), na = "")
  }

  print(treeFlags)
  return(treeFlags)
}

