#' Calculates basal area for every tree
#' @returns
getBasalArea <- function(treeData){
  # basalArea = pi*r*^2
  basalAreaData <- treeData %>%
    dplyr::group_by(uniqueTreeID, year) %>%
    # Convert the DBH from cm to m
    dplyr::mutate(basalArea = pi*(((treeDBH_cm/100)/2)^2))

  return(basalAreaData)
}

#' Calculate dominance and relative dominance of species in plots
#' @returns
#' @param areaSampled area sampled as meters squared, function converts to hectacres
#' @param grouping the variable you want to find the relative density of (eg. locationID or park)
#' NOTE: if you change the grouping be aware that the area sampled changes, but the calculations cancel this out so its not necessary to change it
getDominance <- function(treeData, areaSampled = 2500, grouping = locationID){
  # dominance = Total basal area of a species / Total area sampled
  # relative dominance = (Dominance of a species / Dominance of all species) * 100

  domincanceData <- treeData %>%
    getBasalArea() %>%
    group_by({{grouping}}, visitNumber) %>%
    # Find the total dominance of each plot
    mutate(totalDominance = sum(basalArea)/(areaSampled/10000)) %>%
    group_by(speciesCode, {{grouping}}, visitNumber, totalDominance) %>%
    # Find the dominance for each species in each plot
    # divide by 10000 to convert m^2 to hectares
    summarize(dominance = sum(basalArea)/(areaSampled/10000)) %>%
    # Find relative density
    mutate(relativeDominance = dominance/totalDominance*100)

  return(domincanceData)

}


#' Calculate avg density of species in 5 cm DBH bins
#' @returns
#' @param areaSampled the area of a plot in meters^2, default is 2500
#' Input area of meters sampled as meters squared, function converts to hectares
#' TODO: make sure this can  be scaled up
#' TODO: could make height groups an input into the function
getDensityDBH <- function(treeData, areaSampled = 2500){

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
#' TODO: does it still work calculation wise?? - FIX
#' TODO: add just density to this function
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
#' @param grouping the variable you want to find the count of (eg. locationID or park)
getTreeCount <- function(treedata, grouping = locationID){
  treeCount <- treedata %>%
    group_by({{grouping}}, speciesCode, visitNumber) %>%
    summarise(count = n())

  return(treeCount)
}

# Calculate the frequency and relative frequency of each species within a specified variable
#' @param transectArea the area of each transect, default is 500
#' @param totalArea the total area of all transects in a plot, default is 2500
#' NOTE: if you change the grouping don't change totalArea, its automatically calculated
#' @param grouping the variable you want to find the relative density of (eg. locationID or park)
getFrequency <- function(treeData, transectArea = 500, totalArea = 2500, grouping = locationID){

  frequency <- treeData %>%
    # create a ID so all subplots across
    mutate(uniqueSubPlotNUm = paste0(locationID, "_", subplot)) %>%
    # Calculate total frequency for each section
    # number of subplots * transect area / number of plots * plot area
    mutate(totalFrequency = n_distinct(uniqueSubPlotNUm)*transectArea/(n_distinct(locationID)*totalArea)*100, .by = c({{grouping}}, visitNumber)) %>%
    ungroup() %>%
    group_by({{grouping}}, speciesCode, visitNumber, totalFrequency) %>%
    summarise(frequency = (n_distinct(uniqueSubPlotNUm)*transectArea)/(n_distinct(locationID)*totalArea)*100) %>%
    # Calculate relative frequency for each species in a plot
    mutate(relativeFrequency = frequency/totalFrequency*100)

  return(frequency)
}

# Calculate the importance value of each species within a specified variable
# You can pass in any params which are valid for the inside functions
getImportanceValue <- function(treeData, ...){

  # Calculate needed fields
  relativeDensity <- getRelativeDensity(treeData, ...)
  relativeFrequency <- getFrequency(treeData, ...)
  relativeDominance <- getDominance(treeData, ...)

  #Join tables and add fields together to get importance value
  importanceValue <- plyr::join_all(list(relativeDensity, relativeFrequency, relativeDominance)) %>%
    # importance value = Relative density + relative frequency + relative dominance
    mutate(importanceValue = relativeDensity + relativeFrequency + relativeDominance)

  return(importanceValue)
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
