#' @importFrom magrittr %>% %<>%


#' TODO:  do i still have to include getting rid of pulled trees and seedlings if they were removed from the database
cleaningPine <- function(pine){
  # Calculate the visit number for each visit to a site
  visitNum <- pine$data$Visit %>%
    dplyr::filter(repeatSample != 1) %>%
    dplyr::mutate(year = year(as_date(eventDate))) %>%
    # TODO: make more robust, would a visit to a panel ever be one year off
    # so something like make visit year the year the majority of visits are
    dplyr::select(panel, year)%>%
    dplyr::group_by(panel, year) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::group_by(panel) %>%
    dplyr::mutate(visitNumber = seq_along(year))
  # arrange year descending within panel descending


  # Seedling Data Wrangling

  pine$data$Seedling <- pine$data$Seedling %>%
    # Remove seedling plots with no seedlings
    dplyr::filter(speciesCode != "_NONE") %>%
    # Make a unique identifier for all seedlings
    dplyr::mutate(uniqueSeedlingID = paste0(locationID, "_", tag))

  # Make a table with all of the seedlings that were pulled or missing
  pulledSeedlings <- pine$data$Seedling %>%
    dplyr::filter(vitality == 'P' | vitality == 'M')

  # Remove all instances of pulled or missing seedlings based on unique seedling ID
  pine$data$Seedling <- pine$data$Seedling %>%
    dplyr::filter(!(uniqueSeedlingID %in% pulledSeedlings$uniqueSeedlingID)) %>%
    dplyr::select(-uniqueSeedlingID) %>%
    dplyr::mutate(year = year(as_date(eventDate))) %>%
    # Join with table containing visit number
    dplyr::left_join(visitNum) %>%
    dplyr::filter(!is.na(visitNumber))


  # Tree Data Wrangling

  # Make unique tree ID
  pine$data$Tree <- pine$data$Tree %>%
    dplyr::mutate(uniqueTreeID = paste(locationID, "_", subplot, "_", tag),
           year = year(as_date(eventDate)))

  # Make a table with all of the trees that were pulled or missing
  pulledTrees <- pine$data$Tree %>%
    dplyr::filter(vitality == 'Pulled' | vitality == 'Missed')

  # Remove pulled and missing trees
  pine$data$Tree <- pine$data$Tree %>%
    dplyr::filter(!(uniqueTreeID %in% pulledTrees$uniqueTreeID)) %>%
    # Join with table containing visit number
    dplyr::left_join(visitNum) %>%
    # TODO update
    dplyr::filter(!is.na(visitNumber)) %>%
    # Remove rows where DBH is NA so calculations can be performed
    dplyr::filter(!is.na(treeDBH_cm) & treeDBH_cm != -999 & treeDBH_cm != 999)

  # Live Tree Data Wrangling
  pine$data$LiveTrees <- pine$data$Tree %>%
    # Remove all dead trees
    dplyr::filter(vitality == "Live") %>%
    # Remove rows where height is NA so calculations can be performed
    dplyr::filter(!is.na(treeHeight_m) & treeHeight_m != -999 & treeHeight_m != 999)

  # Dead Tree Data Wrangling
  pine$data$DeadTrees <- pine$data$Tree %>%
    dplyr::filter(vitality == "Dead" | vitality == "Recently Dead")

  return(pine)
  # # Cleaning live tree data for wrangling and visualization
  # pine$data$LiveTrees <- pine$data$Tree %>%
  #   # Remove all dead trees
  #   filter(vitality == "Live") %>%
  #   # Remove rows where height or DBH that are NA so calculations can be performed
  #   filter(!is.na(treeHeight_m) & treeHeight_m != -999 & treeHeight_m != 999) %>%
  #   filter(!is.na(treeDBH_cm) & treeDBH_cm != -999 & treeDBH_cm != 999) %>%
  #   mutate(uniqueTreeID = paste(locationID, "_", subplot, "_", tag))
  #
  # pineDead <- fiveneedlepine::loadPine("C:/Users/ifoster/Documents/R/mojn-pine-rpackage/data/FNP_MOJN_Primary_Copy.accdb")
  #
  # pineDead <- pineDead$data$Tree %>%
  #   filter(vitality == "Dead" | vitality == "Recently Dead") %>%
  #   mutate(uniqueTreeID = paste(locationID, "_", subplot, "_", tag),
  #          year = year(as_date(eventDate))) %>%
  #   # Join with table containing visit number
  #   left_join(visitNum) %>%
  #   # TODO update
  #   filter(!is.na(visitNumber)) %>%
  #   filter(!is.na(treeDBH_cm) & treeDBH_cm != -999 & treeDBH_cm != 999)
  #
  # pineAliveAndDead <- fiveneedlepine::loadPine("C:/Users/ifoster/Documents/R/mojn-pine-rpackage/data/FNP_MOJN_Primary_Copy.accdb")
  #
  #
  # # TODO: move this
  # # Calculates the visit number for each visit to a site
  # visitNum <- pineAliveAndDead$data$Visit %>%
  #   dplyr::filter(repeatSample != 1) %>%
  #   dplyr::mutate(year = year(as_date(eventDate))) %>%
  #   # TODO: make more robust, would a visit to a panel ever be one year off
  #   # so something like make visit year the year the majority of visits are
  #   dplyr::select(panel, year)%>%
  #   dplyr::group_by(panel, year) %>%
  #   dplyr::summarize(n = n()) %>%
  #   dplyr::group_by(panel) %>%
  #   dplyr::mutate(visitNumber = seq_along(year))
  # # arrange year descending within panel descending
  #
  # # TODO: ONLY INCLUDES LIVE TREES
  # # Cleaning live tree data for wrangling and visualization
  # pineAliveAndDead$data$Tree <- pineAliveAndDead$data$Tree %>%
  #   # Remove rows where height or DBH that are NA
  #   filter(!is.na(treeHeight_m) & treeHeight_m != -999 & treeHeight_m != 999) %>%
  #   filter(!is.na(treeDBH_cm) & treeDBH_cm != -999 & treeDBH_cm != 999) %>%
  #   mutate(uniqueTreeID = paste(locationID, "_", subplot, "_", tag),
  #          year = year(as_date(eventDate))) %>%
  #   # Join with table containing visit number
  #   left_join(visitNum) %>%
  #   # TODO update
  #   filter(!is.na(visitNumber))
}
