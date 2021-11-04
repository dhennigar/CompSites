# veg analysis to generate plot-level summary statistics.


# libraries ---------------------------------------------------------------

library(tidyverse)
library(vegan)


# global variables --------------------------------------------------------

year = "2015" # SET THIS VARIABLE MANUALLY

if (year == "2021"){
  datapath <- "./FieldData/2021/"
}

if (year == "2015"){
  datapath <- "./FieldData/2015/csv/"
}

files <- list.files(datapath)

resultpath <-  paste("./Results/", year, "/", sep = "")

project_results <- data.frame()


# define functions --------------------------------------------------------

veg_import <- function(filepath, year){
  
  df <- read.csv(filepath, fileEncoding = "UTF-8-BOM") 
  
  # fix bugs in 2015 data import
  if (year == "2015") {
    df <- df[1:8] %>% 
      mutate(Site_Number = SITE_ID) %>% 
      select(-SITE_ID)
  }
  
  # create PLOT_CODE column for unique plot identifiers. Filter out Riparian.
  df <- df %>% 
    mutate(PERCENT_COVER = as.numeric(PERCENT_COVER),
           PLOT_CODE = paste(Site_Number, COMMUNITY, PLOT, sep = "-")) %>% 
    filter(COMMUNITY != "RIP")
  
  return(df)
}


veg_long_to_wide <- function(data, remove_codes = T){

  wideData <- data %>%
    select(c(PLOT_CODE, SPECIES_CODE, PERCENT_COVER)) %>%
    spread(SPECIES_CODE, PERCENT_COVER)
  
  wideData[is.na(wideData)] <- 0
  
  if (remove_codes == T){
    return(wideData %>% select(-PLOT_CODE))
  } else {
    return(wideData)
  }
}


veg_diversity <- function(longData, plantList){

  if (length(plantList$N) == 0){
    native_richness <- 0
    shannon_index <- 0
    simpson_index <- 0
    
  } else {
    
    native_richness <- longData %>% 
    veg_long_to_wide() %>% 
    select(plantList$N) %>% 
    specnumber()
  
    shannon_index <- longData %>% 
      veg_long_to_wide() %>%
      select(plantList$N) %>% 
      diversity(index = "shannon")
    
    simpson_index <- longData %>% 
      veg_long_to_wide() %>%
      select(plantList$N) %>% 
      diversity(index = "simpson")
  }
  
  total_richness <- longData %>% 
  veg_long_to_wide() %>%
  select(!plantList$S) %>%
  specnumber()
  
  
  return(data.frame(total_richness,
                    native_richness,
                    shannon_index,
                    simpson_index))
}

 
veg_rel_abundance <- function(longData, plantList){
  
  native_ra <- longData %>% 
    veg_long_to_wide() %>% 
    select(plantList$N) %>% 
    rowSums() / longData %>%
    veg_long_to_wide() %>%
    select(-plantList$S) %>% 
    rowSums()
  
  exotic_ra <- longData %>% 
    veg_long_to_wide() %>% 
    select(plantList$E) %>% 
    rowSums() / longData %>%
    veg_long_to_wide() %>%
    select(-plantList$S) %>% 
    rowSums()
  
  invasive_ra <- longData %>% 
    veg_long_to_wide() %>% 
    select(plantList$I) %>% 
    rowSums() / longData %>% 
    veg_long_to_wide() %>%
    select(-plantList$S) %>% 
    rowSums()
  
  unknown_ra <- longData %>% 
    veg_long_to_wide() %>% 
    select(plantList$U) %>% 
    rowSums() / longData %>%
    veg_long_to_wide() %>%
    select(-plantList$S) %>% 
    rowSums()
  
  return(data.frame(native_ra,
                    exotic_ra,
                    invasive_ra,
                    unknown_ra))
}


veg_plant_list <- function(longData){
  plants <- tapply(longData$SPECIES_CODE, longData$ORIGIN, FUN = unique)
  return(as.list(plants))
}


# main loop ---------------------------------------------------------------

for(i in 1:length(files)){
  
  filepath = paste(datapath, files[i], sep = "")
  site_long <- veg_import(filepath, year)
  
  site_codes <- site_long %>% 
    veg_long_to_wide(remove_codes = FALSE) %>% 
    select(PLOT_CODE)
  
  plants <- veg_plant_list(site_long)
  
  if (length(plants) == 0) {
    project_results <- rbind(project_results, rep(NA, times = 8))
    
  } else {
    
    site_diversity <- veg_diversity(site_long, plants)
  
    site_rel_abund <- veg_rel_abundance(site_long, plants)
  
    site_results <- c(site_codes, site_diversity, site_rel_abund)
  
    project_results <- rbind(project_results, site_results)
    
  }
}


# clean up and export -----------------------------------------------------

# fix some inconsistent plot naming and issue where plot codes were converted to dates
if (year == 2015) {
  project_results$PLOT_CODE <- project_results$PLOT_CODE %>% 
    str_remove_all("-Mar") %>% 
    str_remove_all("-Feb") %>% 
    str_remove_all("-Jan") %>% 
    str_replace("-2-2-", "-2-") %>% 
    str_replace("-1-1-", "-1-")
}

write.csv(project_results,
          paste(resultpath, "plot_results", year, ".csv", sep = ""))
