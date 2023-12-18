#' basal area calculated for each tree in a plot
#' @returns
#' @export
#' TODO: decide if data should be filtered before going into the function (probably) and how data should be imported access vs csv
#' TODO: create a unique identifier in tree table (combine locationID, transect, and tag)
#' TODO: do i need to store units? if so in column name or in a separate column
getBasalArea <- function(filteredData, siteCode){
  # group by tree and year
  # basalArea = pi*r*^2
  # tree tags are unique within transects (within plots) = to really be a unique identifier you need to use tag,subplot, and locationID
  basalAreaData <- filteredData %>%
    dplyr::group_by(treeID, year) %>%
    dplyr::mutate(basalArea = pi*((treeDBH_cm/2)^2))

  return(basalAreaData)
}


# # TODO: move this
# # Calculates the visit number for each visit to a site
# visitNum <- pine$data$Visit %>%
#   dplyr::mutate(year = year(as_date(eventDate))) %>%
#   # TODO: make more robust, would a visit to a panel ever be one year off
#   # so something like make visit year the year the majority of visits are
#   dplyr::select(panel, year)%>%
#   dplyr::group_by(panel, year) %>%
#   dplyr::summarize(n = n()) %>%
#   dplyr::group_by(panel) %>%
#   dplyr::mutate(visitNumber = seq_along(year))
#   # arange year descending within panel descending
