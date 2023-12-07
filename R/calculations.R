#' Calculates basal area for every tree
#' @returns
#' TODO: decide if data should be filtered before going into the function (probably) and how data should be imported access vs csv
#' TODO: create a unique identifier in tree table (combine locationID, transect, and tag)
#' TODO: do i need to store units? if so in column name or in a separate column
getBasalArea <- function(treeData){
  # basalArea = pi*r*^2
  basalAreaData <- treeData %>%
    dplyr::group_by(uniqueTreeID, year) %>%
    # Convert the DBH from cm to m
    dplyr::mutate(basalArea = pi*(((treeDBH_cm/100)/2)^2))

  return(basalAreaData)
}

#' Calculate dominance of species in plots
#' @returns
#' Input area of meters sampled as meters squared, function converts to hectacres
# TODO: you have to enter cleaned data into this function that has been joined with visit number table
getDominance <- function(treeData, areaSampled = 2500){

  domincanceData <- treeData %>%
    getBasalArea() %>%
    group_by(speciesCode, locationID, year, sampleFrame, visitNumber) %>%
    # dominance = Total basal area of a species / Total area sampled
    # divide by 10000 to convert m^2 to hectacres
    summarize(dominance = sum(basalArea)/(areaSampled/10000))

  return(domincanceData)

}


#' Calculate avg density of species in 5 cm DBH bins
#' @returns
#' Input area of meters sampled as meters squared, function converts to hectacres
# TODO: you have to enter cleaned data into this function that has been joined with visit number table
# TODO: can probably group by fewer things
getAvgDensity <- function(treeData, areaSampled = 2500){

  densityData <- treeData %>%
    mutate(DBHGroup = cut(x = treeDBH_cm, breaks = 5*(0:(max(treeDBH_cm)/5)))) %>%
    group_by(speciesCode, locationID, visitNumber, DBHGroup) %>%
    # Find the density for each species, plot, DBHGroup and year combo
    summarise(density = n()/(areaSampled/10000)) %>%
    # Average density for each species, year, DBHGroup
    group_by(speciesCode, DBHGroup, visitNumber) %>%
    summarise(avgDensity = mean(density))

  return(densityData)
}


# TODO: should there be a getDensity, getAvgDensity, and getRelativeDensity functions???
#' Finds the relative density of species within a specified variable
#' @param areaSampled the area of the area sampled, enter in m^2
#' @param grouping the variable you want to find the relative density of (eg. locationID or park)
getRelativeDensity <- function(treeData, areaSampled = 2500, grouping = locationID){

  relativeDensity <- treeData %>%
    group_by({{grouping}}, visitNumber) %>%
    mutate(plotDensity = n()/(areaSampled/10000)) %>%
    ungroup() %>%
    group_by({{grouping}}, visitNumber, speciesCode) %>%
    mutate(relativeDensity = n()/(areaSampled/10000)) %>%
    group_by({{grouping}}, visitNumber, speciesCode, relativeDensity, plotDensity) %>%
    summarise(n = n()) %>%
    summarise(relativeDensity = relativeDensity/plotDensity*100)


    return(relativeDensity)
}

# Count the number of each species present within a specified variable
# TODO: decide if we want summarise or mutate (do we want all of the rows?)
#' @param grouping the variable you want to find the count of (eg. locationID or park)
getTreeCount <- function(treedata, grouping = locationID){
  treeCount <- treedata %>%
    group_by({{grouping}}, speciesCode, visitNumber, sampleFrame) %>%
    summarise(count = n())

  return(treeCount)
}

# Calculate the frequency and relative frequency of each species
#' @param transectArea the area of each transect, default is 500
#' @param totalArea the total area of all transects in a plot, default is 2500
getFrequency <- function(treeData, transectArea = 500, totalArea = 2500){
  frequency <- treeData %>%
    group_by(locationID, visitNumber) %>%
    mutate(totalFrequency = n_distinct(subplot)*transectArea/totalArea*100) %>%
    ungroup() %>%
    group_by(locationID, speciesCode, visitNumber, totalFrequency) %>%
    #group_by(speciesCode) %>%
    summarise(frequency = (n_distinct(subplot)*transectArea)/totalArea*100,
              relativeFrequency = frequency/totalFrequency, .groups = "drop_last")

  return(frequency)
}



# TODO: move this
# Calculates the visit number for each visit to a site
visitNum <- pine$data$Visit %>%
  filter(repeatSample != 1) %>%
  dplyr::mutate(year = year(as_date(eventDate))) %>%
  # TODO: make more robust, would a visit to a panel ever be one year off
  # so something like make visit year the year the majority of visits are
  dplyr::select(panel, year)%>%
  dplyr::group_by(panel, year) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::group_by(panel) %>%
  dplyr::mutate(visitNumber = seq_along(year))
  # arrange year descending within panel descending
