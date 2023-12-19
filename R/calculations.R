#' Calculates basal area for every tree
#' @returns
getBasalArea <- function(treeData){
  # basalArea = pi*r*^2
  basalAreaData <- treeData %>%
    # Convert the DBH from cm to m
    dplyr::mutate(basalArea = pi*(((treeDBH_cm/100)/2)^2)) %>%

  return(basalAreaData)
}

#' Calculate dominance and relative dominance of species in plots
#' @returns
#' @param areaSampled area of plots as meters squared, function converts to hectacres
#' @param grouping the variable you want to find the relative density of (eg. locationID or park)
#' NOTE the total sample area is calculated from 'areaSampled' so if you change the grouping don't update 'areaSampled'
getDominance <- function(treeData, plotArea = 2500, grouping = locationID){
  # dominance = Total basal area of a species / Total area sampled
  # relative dominance = (Dominance of a species / Dominance of all species) * 100


  dominanceData <- treeData %>%
    getBasalArea() %>%
    dplyr::group_by({{grouping}}, visitNumber) %>%
    # Find the number of plots in the grouping variable
    dplyr::mutate(numberOfPlots = n_distinct(locationID)) %>%
    # Find the total dominance of each grouping
    dplyr::mutate(totalDominance = sum(basalArea)/((numberOfPlots*plotArea)/10000)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{grouping}}, visitNumber, speciesCode, totalDominance) %>%
    # Find the dominance for each species in each grouping
    # divide by 10000 to convert m^2 to hectares
    # Use first() so data is aggregated by the summarization groups, all numberOfPlots should be the same for the groups
    dplyr::summarise(dominance = round(sum(basalArea)/((first(numberOfPlots)*plotArea)/10000), 2)) %>%
    # Find relative density
    mutate(relativeDominance = round(dominance/totalDominance*100, 2))

  # # switch from area sampled to actually finding the number of plots in group and using that number
  # domincanceData <- treeData %>%
  #   getBasalArea() %>%
  #   group_by({{grouping}}, visitNumber) %>%
  #   # Find the number of plots in the grouping variable
  #   mutate(numberOfPlots = n_distinct(locationID)) %>%
  #   group_by({{grouping}}, visitNumber, numberOfPlots) %>%
  #   # Find the total dominance of each grouping
  #   mutate(totalDominance = sum(basalArea)/((numberOfPlots*areaSampled)/10000)) %>%
  #   group_by(speciesCode, {{grouping}}, visitNumber, totalDominance, numberOfPlots) %>%
  #   # Find the dominance for each species in each grouping
  #   # divide by 10000 to convert m^2 to hectares
  #   summarize(dominance = sum(basalArea)/((numberOfPlots*areaSampled)/10000)) %>%
  #   # Find relative density
  #   mutate(relativeDominance = round(dominance/totalDominance*100, 2))

  return(dominanceData)

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

#' Finds the density and relative density of species within a specified variable per hectare
#' @param areaSampled the area of the area sampled in m^2, default is 2500
#' @param grouping the variable you want to find the relative density of (eg. locationID or park)
getRelativeDensity <- function(treeData, areaSampled = 2500, grouping = locationID){

  relativeDensity <- treeData %>%
    group_by({{grouping}}, visitNumber) %>%
    mutate(plotDensity = n()/(areaSampled/10000)) %>%
    ungroup() %>%
    group_by({{grouping}}, visitNumber, speciesCode) %>%
    mutate(density = n()/(areaSampled/10000)) %>%
    group_by({{grouping}}, visitNumber, speciesCode, density, plotDensity) %>%
    summarise(n = n()) %>%
    summarise(relativeDensity = round(density/plotDensity*100, 2))

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
getFrequency <- function(treeData, transectArea = 500, plotArea = 2500, grouping = locationID){

  frequency <- treeData %>%
    # create a ID so all subplots across the sections (locationID, park, etc) are unique
    mutate(uniqueSubPlotNum = paste0(locationID, "_", subplot)) %>%
    group_by({{grouping}}, visitNumber) %>%
    # Find out how many plots are in the grouping variable
    mutate(numberOfPlots = n_distinct(locationID)) %>%
    group_by({{grouping}}, speciesCode, visitNumber, numberOfPlots) %>%
    # Calculate frequency for each species in a section
    summarise(frequency = (n_distinct(uniqueSubPlotNum)*transectArea)/(n_distinct(locationID)*plotArea)*100) %>%
    ungroup() %>%
    group_by({{grouping}}, visitNumber, numberOfPlots) %>%
    # Calculate total frequency for each section which = sum of all the frequencies in the section
    mutate(totalFrequency = sum(frequency)) %>%
    ungroup() %>%
    group_by({{grouping}}, speciesCode, visitNumber, numberOfPlots) %>%
    # Calculate relative frequency for each species in a section
    mutate(relativeFrequency = round(frequency/totalFrequency*100, 2))

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
