

#' Return list of events with no corresponding seedling data
noSeedlingDataQC <- function() {

  noSeedlingFlag <- get_data("Visit")$data$Visit %>%
    dplyr::select(locationID, eventID, eventDate) %>%
    # Join with seedling data
    dplyr::full_join(get_data("Seedling")$data$Seedling, by = c('eventID', 'locationID', 'eventDate')) %>%
    dplyr::select(locationID, eventID, eventDate, network, park) %>%
    # Filter for events with no corresponding seedling data
    filter(is.na(network))

  return(noSeedlingFlag)
}

#' Return list of events with no corresponding tree data
noTreeDataQC <- function() {

  noTreeFlag <- get_data("Visit")$data$Visit %>%
    dplyr::select(locationID, eventDate, eventID) %>%
  # Join with tree data
  dplyr::full_join(get_data("Tree")$data$Tree, by = c('eventID', 'locationID')) %>%
    dplyr::group_by(locationID, eventID) %>%
    dplyr::summarise(treeCount = dplyr::n()) %>%
    # Filter for events with no corresponding tree data
    dplyr::filter(treeCount == 0 | is.na(treeCount))

  return(noTreeFlag)
}
