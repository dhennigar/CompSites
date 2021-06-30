# Revision 2 of veg community analysis
# 29/June/2021

# libraries
library(dplyr)
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

veg.nat.wide <- subset(veg, ORIGIN == "N", select = c(-COMMENTS, -COMMUNITY, # select natives only
                                                      -Site_Number, -MAX_LH_CM, -ORIGIN))  %>%
  spread(SPECIES_CODE, PERCENT_COVER) # long to wide format
veg.nat.wide[is.na(veg.nat.wide)] <- 0 # empty cells are zero
veg.nat.wide <- veg.nat.wide[-1] # remove plot id column

veg.inv.wide <- subset(veg, ORIGIN == "I", select = c(-COMMENTS, -COMMUNITY, # select invasives only
                                                      -Site_Number, -MAX_LH_CM, -ORIGIN)) %>%
  spread(SPECIES_CODE, PERCENT_COVER) # long to wide format
veg.inv.wide[is.na(veg.inv.wide)] <- 0 # empty cells are zero
veg.inv.wide <- veg.inv.wide[-1] # remove plot id column

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

# data summary for reporting relative abundance
veg.summary <- as.data.frame(rowSums(veg.wide), row.names = "TOTAL")
veg.summary$NATIVE <- rowSums(veg.nat.wide)
veg.summary$INV <- rowSums(veg.inv.wide)

