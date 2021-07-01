# Revision 2 of veg community analysis
# 29/June/2021

# libraries
library(tidyverse)
library(BiodiversityR)

# DATA IMPORT & PREP

# import (modify filepath)
veg <- read.csv("C://Users/Owner/OneDrive/Documents/GitHub/CompSites/FieldData/09-006.csv",
                fileEncoding = "UTF-8-BOM")
veg$PERCENT_COVER <- as.numeric(veg$PERCENT_COVER) # ensure numeric cover data
veg <- subset(veg, COMMUNITY == 1) # select only community 1

# transform data for richness calculations
veg.wide <- subset(veg, select = c(-COMMENTS, -COMMUNITY, -Site_Number, -MAX_LH_CM, -ORIGIN)) %>%
  spread(SPECIES_CODE, PERCENT_COVER) # long to wide format
veg.wide[is.na(veg.wide)] <- 0 # empty cells are zero
veg.wide <- veg.wide[-1] # remove plot id column

# generate species lists
## WIP ###

# native and invasive subsets
veg.wide.nat <- veg.wide %>% select(any_of(species.nat)) # select only native species
veg.wide.inv <- veg.wide %>% select(any_of(species.inv)) # select only invasive species
veg.wide.exo <- veg.wide %>% select(-any_of(species.nat)) # select any non-native species

  
# CALCULATIONS

# mean height of tallest Carex lyngbyei per plot
lyngbyHeight <- mean(veg$MAX_LH_CM)
lyngbyHeight

# richness (native and total)
richness <- specnumber(veg.wide)
richness.nat <- specnumber(veg.nat.wide)

# shannon-weiner diversity index (native)
shannon <- diversity(veg.nat.wide, index = "shannon")

# simpson's diversity index (native)
simpson <- diversity(veg.nat.wide, index = "simpson")

# relative abundance
natives <- mean(rowSums(veg.wide.nat)/rowSums(veg.wide))
invasives <- mean(rowSums(veg.wide.inv)/rowSums(veg.wide))
