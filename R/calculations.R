
#' Some basic cleaning of the pine data to be able to graph it
#' @param pine the data to input into the function (entire pine dataset)
#' TODO:  do i still have to include getting rid of pulled trees and seedlings if they were removed from the database
cleaningPine <- function(pine){
  # Calculate the visit number for each visit to a site
  visitNum <- pine$data$Visit %>%
    dplyr::filter(repeatSample != 1) %>%
    dplyr::mutate(year = lubridate::year(lubridate::as_date(eventDate))) %>%
    # TODO: make more robust, would a visit to a panel ever be one year off
    # so something like make visit year the year the majority of visits are
    dplyr::select(panel, year, park)%>%
    dplyr::group_by(panel, year, park) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::group_by(panel, park) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(visitNumber = seq_along(year))
  # arrange year descending within panel descending


  # visitNum <- pine$data$Visit %>%
  #   dplyr::filter(repeatSample != 1) %>%
  #   dplyr::mutate(year = lubridate::year(lubridate::as_date(eventDate))) %>%
  #   # TODO: make more robust, would a visit to a panel ever be one year off
  #   # so something like make visit year the year the majority of visits are
  #   dplyr::select(panel, year)%>%
  #   dplyr::group_by(panel, year) %>%
  #   dplyr::summarize(n = dplyr::n()) %>%
  #   dplyr::group_by(panel) %>%
  #   dplyr::arrange(year, panel) %>%
  #   dplyr::mutate(visitNumber = seq_along(year))


  # Seedling Data Wrangling

  pine$data$Seedling <- pine$data$Seedling %>%
    # Remove seedling plots with no seedlings
    # dplyr::filter(speciesCode != "_NONE") %>%
    # Make a unique identifier for all seedlings
    dplyr::mutate(uniqueSeedlingID = paste0(locationID, "_", tag))

  # Make a table with all of the seedlings that were pulled or missing
  pulledSeedlings <- pine$data$Seedling %>%
    dplyr::filter(vitality == 'P' | vitality == 'M')

  # Remove all instances of pulled or missing seedlings based on unique seedling ID
  pine$data$Seedling <- pine$data$Seedling %>%
    dplyr::filter(!(uniqueSeedlingID %in% pulledSeedlings$uniqueSeedlingID)) %>%
    dplyr::select(-uniqueSeedlingID) %>%
    dplyr::mutate(year = lubridate::year(lubridate::as_date(eventDate))) %>%
    # Join with table containing visit number
    dplyr::left_join(visitNum, by = dplyr::join_by(panel, year, park)) %>%
    dplyr::filter(!is.na(visitNumber))

  # Order the height classes in the correct order
  pine$data$Seedling$heightClass <- factor(pine$data$Seedling$heightClass,
                                           levels =c("20 - <50 cm", "50 - <100 cm", "100 - <137 cm"))

  # Tree Data Wrangling

  # Make unique tree ID
  pine$data$Tree <- pine$data$Tree %>%
    dplyr::mutate(uniqueTreeID = paste(locationID, "_", subplot, "_", tag),
                  year = lubridate::year(lubridate::as_date(eventDate)))

  # Make a table with all of the trees that were pulled or missing
  pulledTrees <- pine$data$Tree %>%
    dplyr::filter(vitality == 'Pulled' | vitality == 'Missed')

  # Remove pulled and missing trees
  pine$data$Tree <- pine$data$Tree %>%
    dplyr::filter(!(uniqueTreeID %in% pulledTrees$uniqueTreeID)) %>%
    # Join with table containing visit number
    dplyr::left_join(visitNum, by = dplyr::join_by(panel, year, park)) %>%
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
}


#' Calculates basal area for every tree
#' @param treeData the data to input into the function (tree table of pine data)
#' @returns basal area calculated for every entry
getBasalArea <- function(treeData){
  # basalArea = pi*r*^2
  basalAreaData <- treeData %>%
    # Convert the DBH from cm to m
    dplyr::mutate(basalArea = pi*(((treeDBH_cm/100)/2)^2)) %>%

  return(basalAreaData)
}

#' Calculate dominance and relative dominance of species in plots
#' @param treeData the data to input into the function (tree table of pine data)
#' @param plotArea area of plots as meters squared, function converts to hectacres
#' @param grouping the variable you want to find the relative density of (eg. locationID or park)
#' NOTE the total sample area is calculated from 'plotArea' so if you change the grouping don't update 'plotArea'
getDominance <- function(treeData, plotArea = 2500, grouping = locationID){
  # dominance = Total basal area of a species / Total area sampled
  # relative dominance = (Dominance of a species / Dominance of all species) * 100

  dominanceData <- treeData %>%
    getBasalArea() %>%
    dplyr::group_by({{grouping}}, visitNumber) %>%
    # Find the number of plots in the grouping variable
    dplyr::mutate(numberOfPlots = dplyr::n_distinct(locationID)) %>%
    # Find the total dominance of each grouping
    dplyr::mutate(totalDominance = sum(basalArea)/((numberOfPlots*plotArea)/10000)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{grouping}}, visitNumber, scientificName, totalDominance) %>%
    # Find the dominance for each species in each grouping
    # divide by 10000 to convert m^2 to hectares
    # Use first() so data is aggregated by the summarization groups, all numberOfPlots should be the same for the groups
    dplyr::summarise(dominance = round(sum(basalArea)/((dplyr::first(numberOfPlots)*plotArea)/10000), 2)) %>%
    # Find relative density
    dplyr::mutate(relativeDominance = round(dominance/totalDominance*100, 2))

  return(dominanceData)

}

#' Calculate avg density of species in 5 cm DBH bins
#' @param treeData the data to input into the function (tree table of pine data)
#' @param areaSampled the area of a plot in meters^2, default is 2500
#' Input area of meters sampled as meters squared, function converts to hectares
#' TODO: make sure this can  be scaled up
#' TODO: could make height groups an input into the function
getDensityDBH <- function(treeData, areaSampled = 2500){

  densityData <- treeData %>%
    dplyr::mutate(DBHGroup = cut(x = treeDBH_cm, breaks = 5*(0:(max(treeDBH_cm)/5)))) %>%
    dplyr::group_by(locationID, visitNumber) %>%
    # Calculate how many plots are in the grouping
    dplyr::mutate(numberOfPlots = dplyr::n_distinct(locationID)) %>%
    dplyr::group_by(scientificName, locationID, visitNumber, DBHGroup) %>%
    # Find the density for each species, plot, DBHGroup and year combo
    dplyr::summarise(density = dplyr::n()/(dplyr::first(numberOfPlots)*0.25)) %>%
    dplyr::group_by(visitNumber) %>%
    # Calculate how many plots in each visit
    dplyr::mutate(totalPlots = dplyr::n_distinct(locationID)) %>%
    # Average density for each species, year, DBHGroup
    dplyr::group_by(scientificName, DBHGroup, visitNumber) %>%
    # Include all plots in average, not just ones with that scientificName/DBHGroup
    dplyr::summarise(avgDensity = sum(density)/dplyr::first(totalPlots))

  return(densityData)
}

#' Finds the density and relative density of species within a specified variable per hectare
#' @param treeData the data to input into the function (tree table of pine data)
#' @param plotArea the area of the plots sampled in m^2, default is 2500
#' @param grouping the variable you want to find the relative density of (eg. locationID or park)
getRelativeDensity <- function(treeData, plotArea = 2500, grouping = locationID){

  relativeDensity <- treeData %>%
    dplyr::group_by({{grouping}}, visitNumber) %>%
    dplyr::mutate(numberOfPlots = dplyr::n_distinct(locationID)) %>%
    dplyr::mutate(plotDensity = dplyr::n()/(numberOfPlots*plotArea/10000)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{grouping}}, visitNumber, scientificName) %>%
    dplyr::mutate(density = dplyr::n()/(numberOfPlots*plotArea/10000)) %>%
    dplyr::group_by({{grouping}}, visitNumber, scientificName, density, plotDensity) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::summarise(relativeDensity = round(density/plotDensity*100, 2))

    return(relativeDensity)
}

#' Count the number of each species present within a specified variable
#' @param treeData the data to input into the function (tree table of pine data)
#' @param grouping the variable you want to find the count of (eg. locationID or park)
getTreeCount <- function(treeData, grouping = locationID){

  treeCount <- treeData %>%
    dplyr::group_by({{grouping}}, scientificName, visitNumber) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup()

  return(treeCount)
}

#' Calculate the frequency and relative frequency of each species within a specified variable
#' @param treeData the data to input into the function (tree table of pine data)
#' @param transectArea the area of each transect, default is 500
#' @param plotArea the total area of all transects in a plot, default is 2500
#' NOTE: if you change the grouping don't change plotArea, its automatically calculated
#' @param grouping the variable you want to find the relative density of (eg. locationID or park)
getFrequency <- function(treeData, transectArea = 500, plotArea = 2500, grouping = locationID){

  frequency <- treeData %>%
    # create a ID so all subplots across the sections (locationID, park, etc) are unique
    dplyr::mutate(uniqueSubPlotNum = paste0(locationID, "_", subplot)) %>%
    dplyr::group_by({{grouping}}, visitNumber) %>%
    # Find out how many plots are in the grouping variable
    dplyr::mutate(numberOfPlots = dplyr::n_distinct(locationID)) %>%
    dplyr::group_by({{grouping}}, scientificName, visitNumber, numberOfPlots) %>%
    # Calculate frequency for each species in a section
    dplyr::summarise(frequency = (dplyr::n_distinct(uniqueSubPlotNum)*transectArea)/(dplyr::first(numberOfPlots)*plotArea)*100) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{grouping}}, visitNumber, numberOfPlots) %>%
    # Calculate total frequency for each section which = sum of all the frequencies in the section
    dplyr::mutate(totalFrequency = sum(frequency)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{grouping}}, scientificName, visitNumber, numberOfPlots) %>%
    # Calculate relative frequency for each species in a section
    dplyr::mutate(relativeFrequency = round(frequency/totalFrequency*100, 2))

  return(frequency)
}

#' Calculate the importance value of each species within a specified variable
#' @param treeData the data to input into the function (tree table of pine data)
#' @param ... You can pass in any params which are valid for the inside functions
getImportanceValue <- function(treeData, ...){

  # Calculate needed fields
  relativeDensity <- getRelativeDensity(treeData, ...)
  relativeFrequency <- getFrequency(treeData, ...)
  relativeDominance <- getDominance(treeData, ...)

  #Join tables and add fields together to get importance value
  importanceValue <- plyr::join_all(list(relativeDensity, relativeFrequency, relativeDominance)) %>%
    # importance value = Relative density + relative frequency + relative dominance
    dplyr::mutate(importanceValue = relativeDensity + relativeFrequency + relativeDominance)

  return(importanceValue)
}
