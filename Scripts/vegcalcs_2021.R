# Revision 2 of veg community analysis
# 29/June/2021

# libraries
library(tidyverse)
library(BiodiversityR)

# DATA IMPORT & PREP

# import (select your file path by uncommenting the correct line)
veg <- read.csv("C://Users/User/Desktop/Github/CompSites/FieldData/2021/09-006.csv", fileEncoding = "UTF-8-BOM") # Robyn
# veg <- read.csv("C://Users/Owner/OneDrive/Documents/GitHub/CompSites/FieldData/2021/", fileEncoding="UTF-8-BOM") # Daniel


veg$PERCENT_COVER <- as.numeric(veg$PERCENT_COVER) # ensure numeric cover data
veg <- subset(veg, COMMUNITY == 1) # select only community 1
veg <- subset(veg, SPECIES_CODE != "MUD" & SPECIES_CODE != "WOOD" & SPECIES_CODE != "ROCK" & SPECIES_CODE != "LITTER" & SPECIES_CODE != "LOG")

# transform data for richness calculations
veg.wide <- subset(veg, select = c(-COMMENTS, -COMMUNITY, -Site_Number, -MAX_LH_CM, -ORIGIN)) %>%
  spread(SPECIES_CODE, PERCENT_COVER) # long to wide format
veg.wide[is.na(veg.wide)] <- 0 # empty cells are zero
veg.wide <- veg.wide[-1] # remove plot id column

# generate species lists
species <- subset(veg$SPECIES_CODE, veg$ORIGIN == "N" | veg$ORIGIN == "E" | veg$ORIGIN == "I") %>% # unique species list
  unique()  
species.nat <- subset(veg$SPECIES_CODE, veg$ORIGIN == "N") %>% # unique native species
  unique()
species.inv <- subset(veg$SPECIES_CODE, veg$ORIGIN == "I") %>% # unique invasive species
  unique()

# native and invasive subsets
veg.wide.nat <- veg.wide %>% select(any_of(species.nat)) # select only native species
veg.wide.inv <- veg.wide %>% select(any_of(species.inv)) # select only invasive species
veg.wide.exo <- veg.wide %>% select(-any_of(species.nat)) # select any non-native species

  
# CALCULATIONS

# mean height of tallest Carex lyngbyei
lyngbyHeight <- mean(veg$MAX_LH_CM, na.rm=TRUE)

# richness (native and total)
richness <- specnumber(veg.wide)
richness.nat <- specnumber(veg.wide.nat)

# shannon-weiner diversity index (native)
shannon <- diversity(veg.wide.nat, index = "shannon")

# simpson's diversity index (native)
simpson <- diversity(veg.wide.nat, index = "simpson")

# relative abundance
natives <- mean(rowSums(veg.wide.nat)/rowSums(veg.wide))
invasives <- mean(rowSums(veg.wide.inv)/rowSums(veg.wide))
exotics <- mean(rowSums(veg.wide.exo)/rowSums(veg.wide))


# RESULTS (modify filepath)

result <- data.frame(lyngbyHeight,
                     mean(richness),
                     mean(richness.nat),
                     mean(simpson),
                     mean(shannon),
                     natives,
                     exotics,
                     invasives)

write.csv(result, "C://Users/User/Desktop/Github/CompSites/Results/2021/09-006-results.csv") # veg analysis results

write.csv(species, "C://Users/User/Desktop/Github/CompSites/Results/2021/09-006-species.csv") # unique species lists
