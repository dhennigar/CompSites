# Revision 2 of veg community analysis
# 29/June/2021

# libraries
library(tidyverse)
library(BiodiversityR)

# DATA IMPORT & PREP

# import (select your file path by uncommenting the correct line) 
# Note that 2015 spreadsheets must have rows 1 and 3 removed prior to import.
veg <- read.csv("C://Users/Owner/OneDrive/Documents/GitHub/CompSites/FieldData/2015/REF-02-001.csv", fileEncoding = "UTF-8-BOM")# Daniel
# veg <- read.csv("INSERT ROBYN's FILE PATH", fileEncoding="UTF-8-BOM") # Robyn

# Prep the data
veg$X..COVER <- as.numeric(veg$X..Cover) # ensure numeric cover data
veg <- subset(veg, COMMUNITY == 1 | is.na(COMMUNITY)) # select only community 1. Include "NA" for single community sites.

# transform data for richness calculations
veg.wide <- subset(veg, select = c(-Comments, -COMMUNITY, -SITE.ID, -Max.LH..cm., -N.E.I.T.U,
                                   -ROBEL..cm., -X, -X.1, -X.2, -X.3, -Scientific, -FWR.SEED)) %>%
  spread(Common, X..Cover) # long to wide format
veg.wide[is.na(veg.wide)] <- 0 # empty cells are zero
veg.wide <- veg.wide[-1] # remove extra columns
veg.wide <- veg.wide[-1]
veg.wide <- veg.wide %>% select(-any_of(c("WOOD", "MUD", "LITTER", "ROCK",
                                          "Ground", "ground", "Ground ", "ground ",
                                          "log", "log ", "Log", "Log "))) # exclude non-veg entries

# generate species lists
species <- subset(veg$Common, veg$N.E.I.T.U == "N" | veg$N.E.I.T.U == "E" | veg$N.E.I.T.U == "I") %>% # unique species list
  unique() 
species.nat <- subset(veg$Common, veg$N.E.I.T.U == "N") %>% # unique native species
  unique()
species.inv <- subset(veg$Common, veg$N.E.I.T.U == "I") %>% # unique invasive species
  unique()

# native and invasive subsets
veg.wide.nat <- veg.wide %>% select(any_of(species.nat)) # select only native species
veg.wide.inv <- veg.wide %>% select(any_of(species.inv)) # select only invasive species
veg.wide.exo <- veg.wide %>% select(-any_of(species.nat)) # select any non-native species

  
# CALCULATIONS

# mean height of tallest Carex lyngbyei
lyngbyeHeight <- mean(veg$Max.LH..cm.)

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

result <- data.frame(lyngbyeHeight,
                     mean(richness),
                     mean(richness.nat),
                     mean(simpson),
                     mean(shannon),
                     natives,
                     exotics,
                     invasives)

write.csv(result, "C://Users/Owner/OneDrive/Documents/GitHub/CompSites/Results/2015/REF-02-001.csv") # veg analysis results
write.csv(species, "C://Users/Owner/OneDrive/Documents/GitHub/Compsites/Results/2015/REF-02-001.csv") # unique species list