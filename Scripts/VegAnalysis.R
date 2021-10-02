# Analysis script for FRE CompSites 2021


library(tidyverse)
library(BiodiversityR)
source("./Scripts/VegFunctions.R")

datapath <- "./FieldData/2015/csv/"
resultspath <- "./Results/2015/"
year <- "2015"

files <- list.files(datapath)

ProjectResults <- data.frame()

for(i in 1:length(files)){
  # import each csv and transform to wide format
  # only community 1 (marsh) is considered
  vegLong <- VegImport(paste(datapath, files[i], sep = ""), year = year)
  vegWide <- VegLongToWide(vegLong)
  
  # unique species list for each origin, used in other calculations.
  plants <- VegUniqueSpecies(vegLong)
  
  # veg percent cover summary stats
  pcStats <- VegPercentCoverStats(vegWide)
  write.csv(pcStats, paste(resultspath, "PC_Stats/", "PC_Stats_", files[i], sep = ""))
  
  # site veg community statistics
  siteStats <- VegStats(vegLong, vegWide, plants)
  relativeAbundance <- VegRelativeAbundance(vegWide, plants)
  siteStats <- c(siteStats, relativeAbundance)
  
  # add new row for each site
  ProjectResults <- rbind(ProjectResults, siteStats)
}

ProjectResults$Site_ID <- str_remove(files, ".csv") # add site id column to results
ProjectResults$RA_Sum <- rowSums(select(ProjectResults, c("n_ra", "e_ra", "i_ra", "u_ra"))) # this should be 1 for every site.
write.csv(ProjectResults, paste(resultspath, "VegDataResults_", year, ".csv", sep = ""))
