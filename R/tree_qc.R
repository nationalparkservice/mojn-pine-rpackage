# TODO: maybe add QC functions that check things like if dbh or height
#is more than two standard deviations away from the mean/median

#' @importFrom magrittr %>% %<>%

#' Return a list of trees that have duplicate tags within a plot/tract
treeDuplicateTagQC <- function(){
  # Returns tree records with duplicate tags
  treeDuplicateTag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, scientificName) %>%
    dplyr::group_by(locationID, eventDate, subplot, tag)%>%
    dplyr::mutate(countTotal = dplyr::n()) %>%
    dplyr::filter(!is.na(tag) & countTotal > 1)

  return(treeDuplicateTag)
}

#' Return list of alive trees with no tag
missingTagQC <- function(){
  missingTag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, scientificName) %>%
    dplyr::filter(is.na(tag))

  return(missingTag)
}

#' Return list of trees that have a stem letter that is not a letter
stemLetterQC <- function(){
  treeDuplicateTag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, stemLetter) %>%
    # Filter for trees whose stem letter is not NA and not a letter
    dplyr::filter(!grepl("^[[:alpha:]]+$", stemLetter, FALSE) & !is.na(stemLetter))

  return(treeDuplicateTag)
}

#' Return a list of dead trees that have a missing cause of death or one that does not match domain values
causeOfDeathQC <- function(){
  causeOfDeathList <- get_data("metadata")$metadata$categories %>% filter(attributeName == "causeOfDeath")
  causeOfDeathList <- causeOfDeathList[['definition']]

  causeOfDeathFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, causeOfDeath, vitality) %>%
    # Filter for dead trees with a missing cause of death or one that does not match domain values
    dplyr::filter((vitality != 'Live' & is.na(causeOfDeath)) | (vitality != 'Live'  & !(causeOfDeath %in% causeOfDeathList) & !is.na(causeOfDeath)))

  return(causeOfDeathFlag)
}

#' Return list of trees with heights greater than 50 m or missing
treeHeightQC <- function(){

  treeHeightFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, treeHeight_m, vitality) %>%
    # Filter for trees with heights greater than 50 m or missing
    dplyr::filter((treeHeight_m > 50 & treeHeight_m != 999) | is.na(treeHeight_m) | treeHeight_m == 999 | treeHeight_m == -999)

  return(treeHeightFlag)
}

#' Return list of recently dead PIAL trees with a null mortality year
mortalityYearQC <- function() {

  mortalityYearFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, estimatedMortalityYear, scientificName, vitality) %>%
    # Filter for trees that are recently dead, species is PIAL/Pinus albicaulis, and mortality year is null
    dplyr::filter(vitality == 'RD' & scientificName == 'Pinus albicaulis' & is.na(estimatedMortalityYear))

  return(mortalityYearFlag)
}

#' Return list of trees where cones exist but there is no cone count or where cones do not exist but cone count is populated
#' TODO: check this, cone count might not be recorded for non target species
coneCountQC <- function(){

  coneCountFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, femaleCones, coneCount, scientificName) %>%
    dplyr::filter((femaleCones == 'Y' & is.na(coneCount)) | (femaleCones == 'N' & coneCount > 0))

  return(coneCountFlag)
}

#' Return list of trees who's crown health isn't a domain value or live PIAL trees with missing crown health
crownHealthQC <- function(){
  crownHealthList <- get_data("metadata")$metadata$categories %>% filter(attributeName == "crownHealth")
  crownHealthList <- crownHealthList[['code']]

  crownHealthFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, crownHealth, vitality, scientificName) %>%
    dplyr::filter((!(crownHealth %in% crownHealthList) & !is.na(crownHealth)) | (vitality == 'Live' & scientificName == 'Pinus albicaulis' & is.na(crownHealth)))

  return(crownHealthFlag)
}

#' Return a list of trees whose lower crown kill is more than 100% and alive PIAL trees that have a null lower crown kill but are not null in both upper and middle crown kill percents
crownKillLowerQC <- function() {

  crownKillLowerFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, vitality, scientificName, crownKill_Lower_percent, crownKill_Mid_percent, crownKill_Upper_percent) %>%
    dplyr::filter(crownKill_Lower_percent > 100 | (vitality == "L" & scientificName == 'Pinus albicaulis'
                                                   & is.na(crownKill_Lower_percent) & (!is.na(crownKill_Upper_percent) | !is.na(crownKill_Mid_percent))))

  return(crownKillLowerFlag)
}

#' Return a list of trees whose middle crown kill is more than 100% and alive PIAL trees that have a null middle crown kill but are not null in both upper and lower crown kill percents
crownKillMiddleQC <- function() {

  crownKillLowerFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, vitality, scientificName, crownKill_Lower_percent, crownKill_Mid_percent, crownKill_Upper_percent) %>%
    dplyr::filter(crownKill_Mid_percent > 100 | (vitality == "L" & scientificName == 'Pinus albicaulis'
                                                 & is.na(crownKill_Mid_percent) & (!is.na(crownKill_Upper_percent) | !is.na(crownKill_Lower_percent))))

  return(crownKillLowerFlag)
}

#' Return a list of trees whose upper crown kill is more than 100% and alive PIAL trees that have a null upper crown kill but are not null in both middle and lower crown kill percents
crownKillUpperQC <- function() {

  crownKillLowerFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, vitality, scientificName, crownKill_Lower_percent,
                  crownKill_Mid_percent, crownKill_Upper_percent) %>%
    dplyr::filter(crownKill_Upper_percent > 100 | (vitality == "L" & scientificName == 'Pinus albicaulis'
                                                   & is.na(crownKill_Upper_percent) & (!is.na(crownKill_Mid_percent) | !is.na(crownKill_Lower_percent))))

  return(crownKillLowerFlag)
}

#' Return a list of trees with either missing DBH or a DBH greater than 200 cm
dbhQC <- function(){

  dbhFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, treeDBH_cm) %>%
    dplyr::filter((treeDBH_cm > 200 & treeDBH_cm != 999) | is.na(treeDBH_cm) | treeDBH_cm == 999 | treeDBH_cm == -999)

  return(dbhFlag)
}

#' Return a list of trees either with a missing subplot number or a subplot number not within 1-5
treeSubplotQC <- function(){

  subplotFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, treeDBH_cm) %>%
    dplyr::filter(subplot < 1 | subplot > 5 | is.na(subplot))

  return(subplotFlag)

}

#' Return a list of trees with a missing vitality or a vitality that don't match a domain value
treeVitalityQC <- function(){
  vitalityList <- C('Live', 'Dead', 'RecentlyDead')

  vitalityFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, vitality) %>%
    dplyr::filter(is.na(vitality) | (!is.na(vitality) & !(vitality %in% vitalityList)))

  return(vitalityFlag)
}

#' Return a list of trees with non-valid responses in the lower bole canker columns
boleCankersILowerQC <- function(){

  boleCakers_I_LowerFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, boleCankers_I_Lower, boleCanks_ITypes_Lower) %>%
    # Filter for entries where lower bole canker infestation checkbox = Yes but infestation type is null
    dplyr::filter((boleCankers_I_Lower == 'Y' & is.na(boleCanks_ITypes_Lower))
                  # Filter for entries where the bole canks lower value doesn't match domain values
                  | (!grepl("A|C|F|O|R|S", boleCanks_ITypes_Lower, FALSE)) & !is.na(boleCanks_ITypes_Lower)
                  # Filter for entries where lower bole canker infestation checkbox = No but infestation type is not null
                  | (boleCankers_I_Lower == 'N' & !is.na(boleCanks_ITypes_Lower)))

  return(boleCakers_I_LowerFlag)
}

#' Return a list of trees with non-valid responses in the middle bole canker columns
boleCankersIMiddleQC <- function(){

  boleCakers_I_MiddleFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, boleCankers_I_Mid, boleCankers_ITypes_Mid) %>%
    # Filter for entries where middle bole canker infestation checkbox = Yes but infestation type is null
    dplyr::filter((boleCankers_I_Mid == 'Y' & is.na(boleCankers_ITypes_Mid))
                  # Filter for entries where the bole canker middle value doesn't match domain values
                  | (!grepl("A|C|F|O|R|S", boleCankers_ITypes_Mid, FALSE)) & !is.na(boleCankers_ITypes_Mid)
                  # Filter for entries where middle bole canker infestation checkbox = No but infestation type is not null
                  | (boleCankers_I_Mid == 'N' & !is.na(boleCankers_ITypes_Mid)))

  return(boleCakers_I_MiddleFlag)
}

#' Return a list of trees with non-valid responses in the upper bole canker columns
boleCankersIUpperQC <- function(){

  boleCakers_I_UpperFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, boleCankers_I_Upper, boleCankers_ITypes_Upper) %>%
    # Filter for entries where upper bole canker infestation checkbox = Yes but infestation type is null
    dplyr::filter((boleCankers_I_Upper == 'Y' & is.na(boleCankers_ITypes_Upper))
                  # Filter for entries where the bole canker upper value doesn't match domain values
                  | (!grepl("A|C|F|O|R|S", boleCankers_ITypes_Upper, FALSE)) & !is.na(boleCankers_ITypes_Upper)
                  # Filter for entries where upper bole canker infestation checkbox = No but infestation type is not null
                  | (boleCankers_I_Upper == 'N' & !is.na(boleCankers_ITypes_Upper)))

  return(boleCakers_I_UpperFlag)
}

#' Return a list of trees with non-valid responses in the lower branch canker columns
branchCankersILowerQC <- function(){

  branchCankers_I_LowerFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, branchCanks_I_Lower, branchCanks_ITypes_Lower) %>%
    # Filter for entries where lower branch canker infestation checkbox = Yes but infestation type is null
    dplyr::filter((branchCanks_I_Lower == 'Y' & is.na(branchCanks_ITypes_Lower))
                  # Filter for entries where the branch canker lower value doesn't match domain values
                  | (!grepl("A|C|F|O|R|S", branchCanks_ITypes_Lower, FALSE)) & !is.na(branchCanks_ITypes_Lower)
                  # Filter for entries where lower branch canker infestation checkbox = No but infestation type is not null
                  | (branchCanks_I_Lower == 'N' & !is.na(branchCanks_ITypes_Lower)))

  return(branchCankers_I_LowerFlag)
}

#' Return a list of trees with non-valid responses in the middle branch canker columns
branchCankersIMiddleQC <- function(){

  branchCankers_I_MiddleFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, branchCanks_I_Mid, branchCanks_ITypes_Mid) %>%
    # Filter for entries where middle branch canker infestation checkbox = Yes but infestation type is null
    dplyr::filter((branchCanks_I_Mid == 'Y' & is.na(branchCanks_ITypes_Mid))
                  # Filter for entries where the branch canker middle value doesn't match domain values
                  | (!grepl("A|C|F|O|R|S", branchCanks_ITypes_Mid, FALSE)) & !is.na(branchCanks_ITypes_Mid)
                  # Filter for entries where middle branch canker infestation checkbox = No but infestation type is not null
                  | (branchCanks_I_Mid == 'N' & !is.na(branchCanks_ITypes_Mid)))

  return(branchCankers_I_MiddleFlag)
}

#' Return a list of trees with non-valid responses in the upper branch canker columns
branchCankersIUpperQC <- function(){

  branchCankers_I_UpperFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, branchCanks_I_Upper, branchCanks_ITypes_Upper) %>%
    # Filter for entries where upper branch canker infestation checkbox = Yes but infestation type is null
    dplyr::filter((branchCanks_I_Upper == 'Y' & is.na(branchCanks_ITypes_Upper))
                  # Filter for entries where the branch canker upper value doesn't match domain values
                  | (!grepl("A|C|F|O|R|S", branchCanks_ITypes_Upper, FALSE)) & !is.na(branchCanks_ITypes_Upper)
                  # Filter for entries where upper branch canker infestation checkbox = No but infestation type is not null
                  | (branchCanks_I_Upper == 'N' & !is.na(branchCanks_ITypes_Upper)))

  return(branchCankers_I_UpperFlag)
}

#' Return list of trees with missing or empty scientific names or (scientific names not in look up table will import as a blank)
treeSpeciesQC <- function(){

  speciesFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, park, locationID, eventDate, subplot, tag, scientificName, vitality) %>%
    dplyr::filter(is.na(scientificName) | scientificName == "")

  return(speciesFlag)
}

#' Returns a list of trees which are RD for multiple entries
recentlyDeadQC <- function() {

  recentlyDeadFlag <- get_data("Tree")$data$Tree %>%
    dplyr::select(eventID, locationID, eventDate, subplot, tag, vitality) %>%
    dplyr::mutate(uniqueID = paste0(locationID, "_", subplot, "_", tag)) %>%
    # Filter for only recently dead entries
    dplyr::filter(vitality == 'Recently Dead') %>%
    dplyr::group_by(locationID, subplot, tag, vitality, uniqueID) %>%
    # Count number of entries for each tree
    dplyr::summarise(count = n()) %>%
    # Filter for only trees that have multiple recently dead entries
    dplyr::filter(count >1)

  return(recentlyDeadFlag)
}
