# Revision 2 of veg community analysis
# 29/June/2021

# libraries
library(tidyverse)
library(BiodiversityR)
library(mosaic)
# DATA IMPORT & PREP

# Path relative to working directory. Run getwd() to see your current working directory. It should print your path to "CompSites".
# If not, ensure you are working with the CompSites R project provided in the CompSites folder: "CompSites.Rproj".
# Note that "." here represents the current working directory.

veg <- read.csv("C:/Users/User/Desktop/Github/CompSites/FieldData/2021/02-011.csv", fileEncoding="UTF-8-BOM") # Modify filepath per site.


veg$PERCENT_COVER <- as.numeric(veg$PERCENT_COVER) # ensure numeric cover data
veg <- subset(veg, COMMUNITY == 1) # select only community 1
veg <- subset(veg, SPECIES_CODE != "SAND" & SPECIES_CODE != "ALGAE" & SPECIES_CODE != "MUD" & SPECIES_CODE != "WOOD" & SPECIES_CODE != "ROCK" & SPECIES_CODE != "LITTER" & SPECIES_CODE != "LOG")


# transform data for richness calculations
veg.wide <- subset(veg, select = c(-COMMENTS, -COMMUNITY, -Site_Number, -MAX_LH_CM, -ORIGIN)) %>%
  spread(SPECIES_CODE, PERCENT_COVER) # long to wide format
veg.wide[is.na(veg.wide)] <- 0 # empty cells are zero
veg.wide <- veg.wide[-1] # remove plot id column

# generate species lists
species <- unique(veg$SPECIES_CODE) # unique species list

species.nat <- subset(veg$SPECIES_CODE, veg$ORIGIN == "N") %>% # unique native species
  unique()
species.inv <- subset(veg$SPECIES_CODE, veg$ORIGIN == "I") %>% # unique invasive species
  unique()
species.exo <- subset(veg$SPECIES_CODE, veg$ORIGIN == "E") %>% # unique exotic species
  unique()
species.unk <- subset(veg$SPECIES_CODE, veg$ORIGIN == "U") %>% # unique unknown species
  unique()

# native and invasive subsets
veg.wide.nat <- veg.wide %>% select(any_of(species.nat)) # select only native species
veg.wide.inv <- veg.wide %>% select(any_of(species.inv)) # select only invasive species
veg.wide.exo <- veg.wide %>% select(any_of(species.exo)) # select any non-native species
veg.wide.unk <- veg.wide %>% select(any_of(species.unk)) # select any non-native species
  
# CALCULATIONS

#Percent cover summary
PC_mean <- sapply(veg.wide, mean, na.rm=TRUE)
PC_sd <- sapply(veg.wide, sd, na.rm=TRUE)
data.frame(PC_mean, PC_sd)

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
unknowns <- mean(rowSums(veg.wide.unk)/rowSums(veg.wide))

# RESULTS (modify filepath)

result <- data.frame(lyngbyHeight,
                     mean(richness),
                     mean(richness.nat),
                     mean(simpson),
                     mean(shannon),
                     natives,
                     exotics,
                     invasives,
                     unknowns)

PC_result <- data.frame (PC_mean, PC_sd)

write.csv(result, "./Results/2021/14-001-results.csv") # veg analysis results

write.csv(species, "./Results/2021/14-001-species.csv") # unique species list

write.csv(PC_result, "C:/Users/User/Desktop/Github/CompSites/Results/2021/02-011-percentcover.csv")
#
#
#
