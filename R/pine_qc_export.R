#' @importFrom magrittr %>%

# should this be a function??
# i'm going to make a function to QC each data table
# i'm just adding tidyverse to the imports section and using :: to call it, should I add it to the depends

# do i need to do this too?
# rm(list=ls()) # start with a clean slate

# these are the packages the original use
# library("RODBC")
# library("lubridate")
# library("tidyr")
# library("openxlsx")
# library(tidyverse)
# library("stringr")
# library("distr")
# library("dplyr")


# Before Running any of the QC functions make sure you have run loadPine()


# the original made the files easier to read, do i  need to do this too? like this
# Locations$Coord_Units[Locations$Coord_Units == "m"] <- "meters"
# Locations$PlotCorner_Primary_Ref[Locations$PlotCorner_Primary_Ref == "SW"] <- "southwest"
# Locations$PlotCorner_Primary_Ref[Locations$PlotCorner_Primary_Ref == "NE"] <- "northeast"
# Locations$Coord_System[Locations$Coord_System == "UTM"] <- "Universal Transverse Mercator"
#source("R/utils.R")


#' Creates an excel spreadsheet with any data quality flags present in the pine photo table
#' @returns description
#' @export
photoDataQC <- function() {
  # TODO: figure out how to load in data correctly using get_data()
  photoFlags <- get_data("Photo")

  # TODO: PlotID_Number and PlotPhoto_ID seem to be misssing from query??
  # TODO: questoion: why are we checking if eventID is NA??
  # TODO: tbh should double check the logic (esp last one) on all the bc i just copied it from the original script
  photoFlags <- pine$data$Photo %>%
    dplyr::select(eventDate, park, bearing, location, cameraImageNumber, eventID) %>%
    # If bearing not between 0 and 360, add 'E' to the bearing flagging field, if missing add 'M
    dplyr::mutate(flagBearing = dplyr::case_when(
      bearing < 0 | bearing > 360 ~ "E",
      is.na(bearing) & !is.na(eventID) ~ "M",
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
    # Only return rows that have a flag
    dplyr::filter(
      !is.na(flagAllPhotosMissing) | !is.na(flagImageNum) | !is.na(flagPhotoLocation) | !is.na(flagBearing)
    )

    # TODO: should i still make an excel spreadsheet with results?
    print(photoFlags)
    return(photoFlags)

}

#' Creates an excel spreadsheet with any data quality flags present in the pine seedling table
#' @returns description
#' @export
seedlingDataQC <- function(){

  # TODO: PlotID_Number and SeedlingCount_ID are missing from query
  # TODO: there's gotta be a more clear word than "domain value"
  # TODO: should i just have them all in one mutate?
  seedlingFlags <- pine$data$Seedling %>%
    dplyr::select(locationID, eventDate, subplot, speciesCode, heightClass, tag, vitality, causeOfDeath, eventID, ) %>%
    # If cause of death doesn't match a domain value, add 'E' to the death cause flagging field, if missing add 'M'
    dplyr::mutate(flagCauseOfDeath = dplyr::case_when(
      # TODO: once categories are importing correctly fix code below
      # ~ 'E',
      # originally it was:
      # SeedlingFlags$Flag_Death_Cause <- if_else(is.na(DataSeedlings$DeadTreeCause_Code) & !is.na(DataSeedlings$Death_Cause),"E", SeedlingFlags$Flag_Death_Cause)
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
    # If status doesn't match a domain value (L, RD, D), add 'E' to the status flagging field, if missning add 'M'
    dplyr::mutate(flagVitality = dplyr::case_when(
      # TODO: once categories are imported correctly fix code below
      # SeedlingFlags$Flag_Status <- ifelse((SeedlingFlags$Status != 'L') & (SeedlingFlags$Status != 'D') & (SeedlingFlags$Status != 'RD') & !is.na(SeedlingFlags$Status),"E", SeedlingFlags$Flag_Status)
      # ~ 'E',
      (speciesCode != '_NONE' & speciesCode != '_NotSamples' & is.na(vitality)) ~ 'M',
      TRUE ~ NA)) %>%
    #If tag is missing, add 'M' to the seedling tag flagging field
    dplyr::mutate(flagTag = dplyr::case_when(
      (speciesCode != '_NONE' & speciesCode != '_NotSampled' & is.na(tag)) ~ 'M',
      TRUE ~ NA))


  # TODO: check logic on this PlotID_Number no longer exists
  # also why are all the tags with count > 1 NA???
  duplicateTag <- pine$data$Seedling %>%
    group_by(park, eventDate, tag) %>%
    summarize(count = dplyr::n()) %>%
    filter(!is.na(tag) & count > 1)

    # TODO: finish updating this
          # #Returns seedling records with duplicate tags
          #
          # SeedDupTag <-DataSeedlings%>%
          #   select(Unit_Code, PlotID_Number, Start_Date, SeedlingTag)%>%
          #   group_by(PlotID_Number, Start_Date, SeedlingTag)%>%
          #   summarize(CountTot = dplyr::n())
          #
          # SeedDupTag2 <- subset(SeedDupTag, (!is.na(SeedDupTag$SeedlingTag) & (SeedDupTag$CountTot > 1)))
          #
          # #MERGE/JOIN Then remove the duplicate columns
          # SeedlingFlags$SeedlingTag <- as.character(SeedlingFlags$SeedlingTag)
          # SeedDupTag2$SeedlingTag <- as.character(SeedDupTag2$SeedlingTag)
          #
          #
          # SeedlingFlags <- full_join(x=SeedlingFlags,y=SeedDupTag2, by.x=c("PlotID_Number","SeedlingTag", "Start_Date"), by.y=c("PlotID_Number","SeedlingTag", "Start_Date"))
          #
          # #WHERE Count_Tot is populated, update Flag_Tag to "S"
          # SeedlingFlags$Flag_Tag <- ifelse(!is.na(SeedlingFlags$CountTot),"S", SeedlingFlags$Flag_Tag)


  print(seedlingFlags)
  return(seedlingFlags)
}

#' Creates an excel spreadsheet with any data quality flags present in the pine tree table
#' @returns description
#' @export
treeDataQC <- function(){

  treeFlags <- pine$data$Tree %>%
    dplyr::select()




  TreeFlags <-DataTrees%>%
    select(Unit_Code, PlotID_Number, Start_Date, TreeData_ID, TreeData_SubPlot_StripID, Flag_SubplotID, TreeID_Number, Flag_TreeNumber, Species_Code, Flag_Sp_Code, Clump_Number, Stem_Letter, Tag_Moved, Tag_Replaced, Tree_Status, Flag_Status, StatusDead_Cause, Flag_Death_Cause, TreeHeight_m, Flag_Tree_Ht, Mort_Year, Flag_Mort_Year, FemaleCones_YN, Cone_Count, Flag_Cone_Count, Crown_Health, Flag_Crown_Health, CrownKill_Lower_perc, Flag_Crown_Kill_Lower, CrownKill_Mid_perc, Flag_Crown_Kill_Mid, CrownKill_Upper_perc, Flag_Crown_Kill_Upper, TreeDBH_cm, Flag_TreeDBH, Stem_Letter, Flag_Stem_Letter, BoleCankers_I_Lower_YN, Flag_BoleCankers_I_Lower_YN, BoleCanks_ITypes_Lower, Flag_BoleCanks_ITypes_Lower, BoleCankers_I_Mid_YN, Flag_BoleCankers_I_Mid_YN, BoleCanks_ITypes_Mid, Flag_BoleCanks_ITypes_Mid, BoleCankers_I_Upper_YN, Flag_BoleCankers_I_Upper_YN, BoleCanks_ITypes_Upper, Flag_BoleCanks_ITypes_Upper, BranchCanks_I_Upper_YN, Flag_BranchCankers_I_Upper_YN, BranchCanks_ITypes_Upper, Flag_BranchCanks_ITypes_Upper, BranchCanks_I_Mid_YN, Flag_BranchCankers_I_Mid_YN, BranchCanks_ITypes_Mid, Flag_BranchCanks_ITypes_Mid, BranchCanks_I_Lower_YN, Flag_BranchCankers_I_Lower_YN, BranchCanks_ITypes_Lower, Flag_BranchCanks_ITypes_Lower, TreeData_Notes)



  #If stem letter is not a letter, add E to stem letter flagging field
  TreeFlags$Flag_Stem_Letter <- if_else((!grepl("^[[:alpha:]]+$", TreeFlags$Stem_Letter, FALSE)) & !is.na(TreeFlags$Stem_Letter),"E", TreeFlags$Flag_Stem_Letter)


  #If death cause does not match domain values (tlu_DeadTree_ProbCauses), add 'E' to the death cause flagging field
  TreeFlags$Flag_Death_Cause <- if_else((TreeFlags$StatusDead_Cause != "ANMLDMG" & TreeFlags$StatusDead_Cause != "BB_notMPB" & TreeFlags$StatusDead_Cause != "BROKSTEM" & TreeFlags$StatusDead_Cause != "CRUSH" & TreeFlags$StatusDead_Cause != "DEFOLIATE" & TreeFlags$StatusDead_Cause != "DIS_notWPBR" & TreeFlags$StatusDead_Cause != "FIRE" & TreeFlags$StatusDead_Cause != "LGHTNIN" & TreeFlags$StatusDead_Cause != "MPB" & TreeFlags$StatusDead_Cause != "MSTLTOE" & TreeFlags$StatusDead_Cause != "SUPPRESS" & TreeFlags$StatusDead_Cause != "UNKNOWN" & TreeFlags$StatusDead_Cause != "UPROOT" & TreeFlags$StatusDead_Cause != "WPBR"),"E", TreeFlags$Flag_Death_Cause)

  #If death cause is missing for Dead tree records, add 'M' to the death cause flagging field
  TreeFlags$Flag_Death_Cause <- if_else((is.na(TreeFlags$StatusDead_Cause) & TreeFlags$Tree_Status == "RD"),"M", TreeFlags$Flag_Death_Cause)

  #If tree height is 999, make the field null and add "M" to height flagging field. Also add M to any fields that were already null.
  TreeFlags$Flag_Tree_Ht <- if_else(is.na(TreeFlags$TreeHeight_m) | TreeFlags$TreeHeight_m == '999',"M", TreeFlags$Flag_Tree_Ht)
  TreeFlags$TreeHeight_m <- if_else(TreeFlags$TreeHeight_m == '999', NA_real_,  TreeFlags$TreeHeight_m)



  #If tree is recently dead, species is PIAL, and mortality year is null, add "M" to status flagging column
  TreeFlags$Flag_Mort_Year <- if_else((TreeFlags$Tree_Status == "RD") & (TreeFlags$Species_Code == "PIAL") & is.na(TreeFlags$Mort_Year),"M", TreeFlags$Flag_Mort_Year)


  #If cones exist but cone count is not populated, add M to cone count flag field
  #ALTHOUGH THE ERROR COULD BE ON THE FemaleCones_YN field
  TreeFlags$Flag_Cone_Count <- if_else((TreeFlags$FemaleCones_YN == "Y") & is.na(TreeFlags$Cone_Count),"M", TreeFlags$Flag_Cone_Count)


  #If cones count = No but cone count is populated, add S to cone count flag field
  #ALTHOUGH THE ERROR COULD BE ON THE FemaleCones_YN field
  TreeFlags$Flag_Cone_Count <- if_else((TreeFlags$FemaleCones_YN == "N") & (TreeFlags$Cone_Count > 0),"S", TreeFlags$Flag_Cone_Count)


  #If crown health doesn't equal domain values (1-5), add E to crown health flagging field
  TreeFlags$Flag_Crown_Health <- if_else((TreeFlags$Crown_Health < 1) | (TreeFlags$Crown_Health) > 5,"E", TreeFlags$Flag_Crown_Health)

  #If crown health is null for live PIAL, add M to crown health flagging field
  TreeFlags$Flag_Crown_Health <- if_else(DataTrees$Tree_Status == "L" & DataTrees$Species_Code == "PIAL" & is.na(DataTrees$Crown_Health),"M", TreeFlags$Flag_Crown_Health)


  #If crown kill lower is greater than 100, add E to crown kill lower flag field
  TreeFlags$Flag_Crown_Kill_Lower <- if_else((TreeFlags$CrownKill_Lower_perc > 100),"E", TreeFlags$Flag_Crown_Kill_Lower)


  #If crown kill mid is greater than 100, add E to crown kill mid flag field
  TreeFlags$Flag_Crown_Kill_Mid <- if_else((TreeFlags$CrownKill_Mid_perc > 100),"E", TreeFlags$Flag_Crown_Kill_Mid)

  #If tree status is L, species is PIAL, the lower crown kill percent is null, but the upper and middle crownkill percents are not null, add S to crown kill lower percent flag field
  TreeFlags$Flag_Crown_Kill_Lower <- if_else((DataTrees$Tree_Status == "L" & DataTrees$Species_Code == "PIAL") & is.na(DataTrees$CrownKill_Lower_perc) & ( !is.na(DataTrees$CrownKill_Upper_perc) | !is.na(DataTrees$CrownKill_Mid_perc)),"S", TreeFlags$Flag_Crown_Kill_Lower)


  #If tree status is L, species is PIAL, the middle crown kill percent is null, but the upper and lower crownkill percents are not null, add S to crown kill middle percent flag field
  TreeFlags$Flag_Crown_Kill_Mid <- if_else((DataTrees$Tree_Status == "L" & DataTrees$Species_Code == "PIAL") & is.na(DataTrees$CrownKill_Mid_perc) & ( !is.na(DataTrees$CrownKill_Upper_perc) | !is.na(DataTrees$CrownKill_Lower_perc)),"S", TreeFlags$Flag_Crown_Kill_Mid)


  #If crown kill upper percent is greater than 100, add E to crown kill upper flag field
  TreeFlags$Flag_Crown_Kill_Upper <- if_else((TreeFlags$CrownKill_Upper_perc > 100),"E", TreeFlags$Flag_Crown_Kill_Upper)

  #If tree status is L, species is PIAL, the upper crown kill percent is null, but the mid and lower crownkill percents are not null, add S to crown kill upper percent flag field
  TreeFlags$Flag_Crown_Kill_Upper <- if_else((DataTrees$Tree_Status == "L" & DataTrees$Species_Code == "PIAL") & is.na(DataTrees$CrownKill_Upper_perc) & ( !is.na(DataTrees$CrownKill_Mid_perc) | !is.na(DataTrees$CrownKill_Lower_perc)),"S", TreeFlags$Flag_Crown_Kill_Upper)


  #If DBH is greater than 200cm, add an S to the Tree DBH flag field
  TreeFlags$Flag_TreeDBH <- if_else((TreeFlags$TreeDBH_cm > 200 & TreeFlags$TreeDBH_cm != 999),"S", TreeFlags$Flag_TreeDBH)

  #If DBH is missing, add an M to the Tree DBH flag field
  #TreeFlags$Flag_TreeDBH <- if_else(is.na(TreeFlags$TreeDBH_cm),"M", TreeFlags$Flag_TreeDBH)


  #If tree DBH is 999, make the field null and add "M" to DBH flagging field. Also add M to any fields that were already null.
  TreeFlags$Flag_TreeDBH <- if_else(is.na(TreeFlags$TreeDBH_cm) | TreeFlags$TreeDBH_cm == '999',"M", TreeFlags$Flag_TreeDBH)
  TreeFlags$TreeDBH_cm <- if_else(TreeFlags$TreeDBH_cm == '999', NA_real_,  TreeFlags$TreeDBH_cm)


  #If tree number is missing, add an M to the tree id flag field
  TreeFlags$Flag_TreeNumber <- if_else(is.na(TreeFlags$TreeID_Number),"M", TreeFlags$Flag_TreeNumber)

  #If subplotID not within range (1-5), add an M to the subplot flag field
  TreeFlags$Flag_SubplotID <- if_else((TreeFlags$TreeData_SubPlot_StripID <1 | TreeFlags$TreeData_SubPlot_StripID >5),"E", TreeFlags$Flag_SubplotID)


  #If subplotID is missing, add an M to the subplot flag field
  TreeFlags$Flag_SubplotID <- if_else(is.na(TreeFlags$TreeData_SubPlot_StripID),"M", TreeFlags$Flag_SubplotID)




  #If status doesn't match a domain value (L, D, RD), add an E to the status flag field
  TreeFlags$Flag_Status <- if_else((TreeFlags$Tree_Status != "L" & DataTrees$Tree_Status != "D" & DataTrees$Tree_Status != "RD"),"E", TreeFlags$Flag_Status)

  #If status is missing, add an M to the status flag field
  TreeFlags$Flag_Status <- if_else(is.na(TreeFlags$Tree_Status),"M", TreeFlags$Flag_Status)








  #If lower bole canks infestation checkbox = Yes but infestation type is null, add S to BoleCanks_ITypes_Lower flagging field
  TreeFlags$Flag_BoleCanks_ITypes_Lower <- if_else((TreeFlags$BoleCankers_I_Lower_YN == "Y" & is.na(TreeFlags$BoleCanks_ITypes_Lower)),"S", TreeFlags$Flag_BoleCanks_ITypes_Lower)


  #If lower bole canks infestation checkbox = No but infestation type is not null, add S to BoleCankers_I_Lower_YN flagging field
  TreeFlags$Flag_BoleCankers_I_Lower_YN <- if_else((TreeFlags$BoleCankers_I_Lower_YN == "N" & !is.na(TreeFlags$BoleCanks_ITypes_Lower)),"S", TreeFlags$Flag_BoleCankers_I_Lower_YN)


  #If middle bole canks infestation checkbox = Yes but infestation type is null, add S to BoleCanks_ITypes_Mid flagging field
  TreeFlags$Flag_BoleCanks_ITypes_Mid <- if_else((TreeFlags$BoleCankers_I_Mid_YN == "Y" & is.na(TreeFlags$BoleCanks_ITypes_Mid)),"S", TreeFlags$Flag_BoleCanks_ITypes_Mid)


  #If middle bole canks infestation checkbox = No but infestation type is not null, add S to BoleCankers_I_Mid_YN flagging field
  TreeFlags$Flag_BoleCankers_I_Mid_YN <- if_else((TreeFlags$BoleCankers_I_Mid_YN == "N" & !is.na(TreeFlags$BoleCanks_ITypes_Mid)),"S", TreeFlags$Flag_BoleCankers_I_Mid_YN)


  #If upper bole canks infestation checkbox = Yes but infestation type is null, add S to BoleCanks_ITypes_Upper flagging field
  TreeFlags$Flag_BoleCanks_ITypes_Upper <- if_else((TreeFlags$BoleCankers_I_Upper_YN == "Y" & is.na(TreeFlags$BoleCanks_ITypes_Upper)),"S", TreeFlags$Flag_BoleCanks_ITypes_Upper)


  #If upper bole canks infestation checkbox = No but infestation type is not null, add S to BoleCankers_I_Upper_YN flagging field
  TreeFlags$Flag_BoleCankers_I_Upper_YN <- if_else((TreeFlags$BoleCankers_I_Upper_YN == "N" & !is.na(TreeFlags$BoleCanks_ITypes_Upper)),"S", TreeFlags$Flag_BoleCankers_I_Upper_YN)


  #If lower branch canks infestation checkbox = Yes but infestation type is null, add S to BranchCanks_ITypes_Lower flagging field
  TreeFlags$Flag_BranchCanks_ITypes_Lower <- if_else((TreeFlags$BranchCanks_I_Lower_YN == "Y" & is.na(TreeFlags$BranchCanks_ITypes_Lower)),"S", TreeFlags$Flag_BranchCanks_ITypes_Lower)


  #If lower branch canks infestation checkbox = No but infestation type is not null, add S to BranchCanks_I_Lower_YN flagging field
  TreeFlags$Flag_BranchCankers_I_Lower_YN <- if_else((TreeFlags$BranchCanks_I_Lower_YN == "N" & !is.na(TreeFlags$BranchCanks_ITypes_Lower)),"S", TreeFlags$Flag_BranchCankers_I_Lower_YN)


  #If middle branch canks infestation checkbox = Yes but infestation type is null, add S to BranchCanks_ITypes_Mid flagging field
  TreeFlags$Flag_BranchCanks_ITypes_Mid <- if_else((TreeFlags$BranchCanks_I_Mid_YN == "Y" & is.na(TreeFlags$BranchCanks_ITypes_Mid)),"S", TreeFlags$Flag_BranchCanks_ITypes_Mid)


  #If middle branch canks infestation checkbox = No but infestation type is not null, add S to BranchCanks_I_Mid_YN flagging field
  TreeFlags$Flag_BranchCankers_I_Mid_YN <- if_else((TreeFlags$BranchCanks_I_Mid_YN == "N" & !is.na(TreeFlags$BranchCanks_ITypes_Mid)),"S", TreeFlags$Flag_BranchCankers_I_Mid_YN)


  #If upper branch canks infestation checkbox = Yes but infestation type is null, add S to BranchCanks_ITypes_Upper flagging field
  TreeFlags$Flag_BranchCanks_ITypes_Upper <- if_else((TreeFlags$BranchCanks_I_Upper_YN == "Y" & is.na(TreeFlags$BranchCanks_ITypes_Upper)),"S", TreeFlags$Flag_BranchCanks_ITypes_Upper)


  #If upper branch canks infestation checkbox = No but infestation type is not null, add S to BranchCanks_I_Upper_YN flagging field
  TreeFlags$Flag_BranchCankers_I_Upper_YN <- if_else((TreeFlags$BranchCanks_I_Upper_YN == "N" & !is.na(TreeFlags$BranchCanks_ITypes_Upper)),"S", TreeFlags$Flag_BranchCankers_I_Upper_YN)


  #If branch canks upper doesn't match domain values, add E to branch canks upper flagging field
  TreeFlags$Flag_BranchCanks_ITypes_Upper <- if_else((!grepl("A|C|F|O|R|S", TreeFlags$BranchCanks_ITypes_Upper, FALSE)) & !is.na(TreeFlags$BranchCanks_ITypes_Upper),"E", TreeFlags$Flag_BranchCanks_ITypes_Upper)


  #If branch canks mid doesn't match domain values, add E to branch canks mid flagging field
  TreeFlags$Flag_BranchCanks_ITypes_Mid <- if_else((!grepl("A|C|F|O|R|S", TreeFlags$BranchCanks_ITypes_Mid, FALSE)) & !is.na(TreeFlags$BranchCanks_ITypes_Mid),"E", TreeFlags$Flag_BranchCanks_ITypes_Mid)


  #If branch canks lower doesn't match domain values, add E to branch canks lower flagging field
  TreeFlags$Flag_BranchCanks_ITypes_Lower <- if_else((!grepl("A|C|F|O|R|S", TreeFlags$BranchCanks_ITypes_Lower, FALSE)) & !is.na(TreeFlags$BranchCanks_ITypes_Lower),"E", TreeFlags$Flag_BranchCanks_ITypes_Lower)


  #If bole canks lower doesn't match domain values, add E to bole canks lower flagging field
  TreeFlags$Flag_BoleCanks_ITypes_Upper <- if_else((!grepl("A|C|F|O|R|S", TreeFlags$BoleCanks_ITypes_Upper, FALSE)) & !is.na(TreeFlags$BoleCanks_ITypes_Upper),"E", TreeFlags$Flag_BoleCanks_ITypes_Upper)


  #If bole canks mid doesn't match domain values, add E to bole canks mid flagging field
  TreeFlags$Flag_BoleCanks_ITypes_Mid <- if_else((!grepl("A|C|F|O|R|S", TreeFlags$BoleCanks_ITypes_Mid, FALSE)) & !is.na(TreeFlags$BoleCanks_ITypes_Mid),"E", TreeFlags$Flag_BoleCanks_ITypes_Mid)


  #If bole canks lower doesn't match domain values, add E to bole canks lower flagging field
  TreeFlags$Flag_BoleCanks_ITypes_Lower <- if_else((!grepl("A|C|F|O|R|S", TreeFlags$BoleCanks_ITypes_Lower, FALSE)) & !is.na(TreeFlags$BoleCanks_ITypes_Lower),"E", TreeFlags$Flag_BoleCanks_ITypes_Lower)


  #If treeID number is used more than once, add S to TreeNumber flagging field
  #Returns tree records with duplicate tags

  TreeDupTag <-DataTrees%>%
    select(Unit_Code, PlotID_Number, Start_Date, TreeID_Number)%>%
    group_by(PlotID_Number, Start_Date, TreeID_Number)%>%
    summarize(CountTot = dplyr::n())

  TreeDupTag2 <- subset(TreeDupTag, (!is.na(TreeDupTag$TreeID_Number) & (TreeDupTag$CountTot > 1)))

  #MERGE/JOIN Then remove the duplicate columns
  TreeFlags$TreeID_Number <- as.character(TreeFlags$TreeID_Number)
  TreeDupTag2$TreeID_Number <- as.character(TreeDupTag2$TreeID_Number)


  TreeFlags <- full_join(x=TreeFlags,y=TreeDupTag2, by.x=c("PlotID_Number","TreeID_Number", "Start_Date"), by.y=c("PlotID_Number","TreeID_Number", "Start_Date"))

  #WHERE Count_Tot is populated, update Flag_TreeNumber to "S"
  TreeFlags$Flag_TreeNumber <- ifelse(!is.na(TreeFlags$CountTot),"S", TreeFlags$Flag_TreeNumber)

  #Remove Species_Code from final table
  TreeFlags <-TreeFlags%>%
    select(Unit_Code, PlotID_Number, Start_Date, TreeData_ID, TreeData_SubPlot_StripID, Flag_SubplotID, TreeID_Number, Flag_TreeNumber, Species_Code, Flag_Sp_Code, Clump_Number, Stem_Letter, Tag_Moved, Tag_Replaced, Tree_Status, Flag_Status, StatusDead_Cause, Flag_Death_Cause, TreeHeight_m, Flag_Tree_Ht, Mort_Year, Flag_Mort_Year, FemaleCones_YN, Cone_Count, Flag_Cone_Count, Crown_Health, Flag_Crown_Health, CrownKill_Lower_perc, Flag_Crown_Kill_Lower, CrownKill_Mid_perc, Flag_Crown_Kill_Mid, CrownKill_Upper_perc, Flag_Crown_Kill_Upper, TreeDBH_cm, Flag_TreeDBH, Stem_Letter, Flag_Stem_Letter, BoleCankers_I_Lower_YN, Flag_BoleCankers_I_Lower_YN, BoleCanks_ITypes_Lower, Flag_BoleCanks_ITypes_Lower, BoleCankers_I_Mid_YN, Flag_BoleCankers_I_Mid_YN, BoleCanks_ITypes_Mid, Flag_BoleCanks_ITypes_Mid, BoleCankers_I_Upper_YN, Flag_BoleCankers_I_Upper_YN, BoleCanks_ITypes_Upper, Flag_BoleCanks_ITypes_Upper, BranchCanks_I_Upper_YN, Flag_BranchCankers_I_Upper_YN, BranchCanks_ITypes_Upper, Flag_BranchCanks_ITypes_Upper, BranchCanks_I_Mid_YN, Flag_BranchCankers_I_Mid_YN, BranchCanks_ITypes_Mid, Flag_BranchCanks_ITypes_Mid, BranchCanks_I_Lower_YN, Flag_BranchCankers_I_Lower_YN, BranchCanks_ITypes_Lower, Flag_BranchCanks_ITypes_Lower, TreeData_Notes, CountTot)


  #If Species_Code doesn't match a domain value, add 'E' to the species code flagging field
  TreeFlags$Flag_Sp_Code <- if_else(((DataTrees$Unit_Code == "CRLA" & DataTrees$Species_Code != "_NONE" & DataTrees$Species_Code != "_NotSampled" & DataTrees$Species_Code != "ABAM" & DataTrees$Species_Code != "ABCO" & DataTrees$Species_Code != "ABLA" & DataTrees$Species_Code != "ABMA" & DataTrees$Species_Code != "ABSH" & DataTrees$Species_Code != "CADE27" & DataTrees$Species_Code != "CANO9" & DataTrees$Species_Code != "DEAD" & DataTrees$Species_Code != "JUOCO" & DataTrees$Species_Code != "PIAL" & DataTrees$Species_Code != "PICOM" & DataTrees$Species_Code != "PIEN" & DataTrees$Species_Code != "PIJE" & DataTrees$Species_Code != "PILA" & DataTrees$Species_Code != "PIMO3" & DataTrees$Species_Code != "PIPOW2" & DataTrees$Species_Code != "PSMEM" & DataTrees$Species_Code != "TSHE" & DataTrees$Species_Code != "TSME" & DataTrees$Species_Code != "UNKNOWN") | (DataTrees$Unit_Code == "LAVO" & DataTrees$Species_Code != "_NONE" & DataTrees$Species_Code != "_NotSampled" & DataTrees$Species_Code != "ABAM" & DataTrees$Species_Code != "ABCO" & DataTrees$Species_Code != "ABLA" & DataTrees$Species_Code != "ABMA" & DataTrees$Species_Code != "ABSH"  & DataTrees$Species_Code != "DEAD" & DataTrees$Species_Code != "PIAL" & DataTrees$Species_Code != "PICOM" & DataTrees$Species_Code != "PIJE" & DataTrees$Species_Code != "PILA" & DataTrees$Species_Code != "PIMO3" & DataTrees$Species_Code != "PIPOW2" & DataTrees$Species_Code != "TSME" & DataTrees$Species_Code != "UNKNOWN")),"E", TreeFlags$Flag_Sp_Code)


  #If species code is missing, add an M to the species code flag field
  TreeFlags$Flag_Sp_Code <- if_else(is.na(TreeFlags$Species_Code),"M", TreeFlags$Flag_Sp_Code)

  TreeFlags$Tree_Status[TreeFlags$Tree_Status == "L"] <- "Live"
  TreeFlags$Tree_Status[TreeFlags$Tree_Status == "D"] <- "Dead"
  TreeFlags$Tree_Status[TreeFlags$Tree_Status == "RD"] <- "Recently Dead"

  TreeFlags$Crown_Health[TreeFlags$Crown_Health == "1"] <- "Low Risk"
  TreeFlags$Crown_Health[TreeFlags$Crown_Health == "2"] <- "Moderate Risk"
  TreeFlags$Crown_Health[TreeFlags$Crown_Health == "3"] <- "High Risk"
  TreeFlags$Crown_Health[TreeFlags$Crown_Health == "4"] <- "Very High Risk"
  TreeFlags$Crown_Health[TreeFlags$Crown_Health == "5"] <- "Dead"

  TreeFlags$Tag_Moved[TreeFlags$Tag_Moved == "0"] <- "No"
  TreeFlags$Tag_Moved[TreeFlags$Tag_Moved == "1"] <- "Yes"

  TreeFlags$Tag_Replaced[TreeFlags$Tag_Replaced == "0"] <- "No"
  TreeFlags$Tag_Replaced[TreeFlags$Tag_Replaced == "1"] <- "Yes"

}

