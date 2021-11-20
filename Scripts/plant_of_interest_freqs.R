# calculate frequency for select plants


# libraries ---------------------------------------------------------------

library(tidyverse)
source("./Scripts/VegFunctions.R")

# global variables --------------------------------------------------------

year <- "2021"

if(year == "2021"){
  filepath <- "./FieldData/2021/"
  plants_of_interest <- c("CARELYN", "IRISPSE", "LYTHSAL",
                        "PHALARU", "TYPHANG", "TYPHGLA")
}

if(year == "2015"){
  filepath <- "./FieldData/2015/csv/"
  plants_of_interest <- c("Lyngbye's sedge", "yellow iris", "purple loosestrife",
                          "reed canarygrass","lesser cattail", "blue cattail")
}

files <- list.files(filepath)

freq_results <- data.frame()

# main loop ---------------------------------------------------------------

for(i in 1:length(files)){
  site <- VegImport(paste(filepath, files[i], sep = ""), year) %>%
    filter(COMMUNITY == 1) %>% 
    VegLongToWide()
  
  freqs <- plantFreq(site, plants_of_interest = plants_of_interest)
  
  freq_results <- rbind(freq_results, freqs)
}

names(freq_results) <- plants_of_interest

freq_results$SITE <- str_remove(files, ".csv")

