#R code for shifting data from long to wide, calculating richness/plot, and simpson's and shannon's diversity/plot.

library("dplyr")
library("tidyverse")
library("BiodiversityR")


#create and prep Comp dataset and subsets
Comp <- read.csv("~/Documents/GitHub/CompSites/FieldData/09-006.csv")
Comp$PERCENT_COVER = as.numeric(Comp$PERCENT_COVER)
Comp <- subset(Comp, select = c(-COMMENTS, -Site_Number, -MAX_LH_CM), Comp$COMMUNITY == 1) # Community 1, all species subset
Comp.N <- subset(Comp, Comp$ORIGIN == "N", select = c(-ORIGIN, -COMMUNITY)) # native species subset
Comp <- subset(Comp, select = c(-ORIGIN, -COMMUNITY)) # remove origin column


#create and prep reference site dataset
Ref <- read.csv("~/Documents/GitHub/FieldData/REFERENCE_DIVERSITY.csv")
Ref$PERCENT_COVER = as.numeric(Ref$PERCENT_COVER)
Ref <- subset(Ref, select = c(-COMMENTS, -Site_Number, -MAX_LH_CM), Ref$COMMUNITY == 1)
Ref.N <- subset(Ref, Ref$ORIGIN == "N", select = c(-ORIGIN, -COMMUNITY))
Ref <- subset(Ref, select = c(-ORIGIN, -COMMUNITY)) 

#transforming datasets into wide format for richness calcs
Comp.Wide <- Comp %>%
  spread(SPECIES_CODE, PERCENT_COVER)
Comp.Wide[is.na(Comp.Wide)] <- 0
Comp.Wide = Comp.Wide[-1]

Comp.N.Wide <- Comp.N %>%
  spread(SPECIES_CODE, PERCENT_COVER)
Comp.N.Wide[is.na(Comp.N.Wide)] <- 0
Comp.N.Wide = Comp.N.Wide[-1]

Ref.Wide <- Ref %>%
  spread(SPECIES_CODE, PERCENT_COVER)
Ref.Wide[is.na(Ref.Wide)] <- 0
Ref.Wide = Ref.Wide[-1]

Ref.N.Wide <- Ref.N %>%
  spread(SPECIES_CODE, PERCENT_COVER)
Ref.N.Wide[is.na(Ref.N.Wide)] <- 0
Ref.N.Wide = Ref.N.Wide[-1]

#RICHNESS CALCS
CompRichness <- specnumber(Comp.Wide)
CompRichness.N <- specnumber(Comp.N.Wide)
RefRichness <- specnumber(Ref.Wide)
RefRichness.N <- specnumber(Ref.N.Wide)

#DIVERSITY CALCS
#SIMPSONS
CompSimp <- diversity(Comp.Wide, "simpson")
RefSimp <- diversity(Ref.Wide, "simpson")
#SHANNONS
CompShan <- diversity(Comp.Wide, "shannon")
RefShan <- diversity(Ref.Wide, "shannon")

#calculating mean diversity of each site
mean(CompSimp)
mean(RefSimp)

mean(CompShan)
mean(RefShan)


#export diversity tables
write.csv(CompSimp, file ="FILEPATH" )