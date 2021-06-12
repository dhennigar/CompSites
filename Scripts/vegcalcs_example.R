#R code for shifting data from long to wide, calculating richness/plot, and simpson's and shannon's diversity/plot. For this example I have two datasets (Frenchies and Reference) for example. 
#Loading Libraries
library("dplyr")
library("tidyverse")
library("BiodiversityR")


#CREATION OF FRENCHIES DATASET
#note that the test.cv was now absorbed into the 2nd sheet of REFERENCEMASTER.xls
Frenchies <- read.csv("~/Git/TyphaData/FieldData/JULY2019_DIVERSITY.csv")
Frenchies$PERCENT_COVER = as.numeric(Frenchies$PERCENT_COVER)
Frenchies$PERCENT_COVER = Frenchies$PERCENT_COVER*10

Reference<- read.csv("~/Git/TyphaData/FieldData/REFERENCE_DIVERSITY.csv") %>%
  na.omit(JULY2020)
Reference$PERCENT_COVER = as.numeric(Reference$PERCENT_COVER)
Reference$PERCENT_COVER = Reference$PERCENT_COVER*10


#transforming dataset into wide format for richness calcs
FrenchiesWIDE <- Frenchies %>%
  spread(SPECIES, PERCENT_COVER)
FrenchiesWIDE[is.na(FrenchiesWIDE)] <- 0
FrenchiesWIDE = FrenchiesWIDE[-1]

ReferenceWIDE <- Reference %>%
  spread(SPECIES, PERCENT_COVER)
ReferenceWIDE[is.na(ReferenceWIDE)] <- 0
ReferenceWIDE = ReferenceWIDE[-1]

#RICHNESS CALCS
Frenchiesrichness <- specnumber(FrenchiesWIDE)
Referencerichness <- specnumber(ReferenceWIDE)

#DIVERSITY CALCS
#SIMPSONS
Frenchiessimp <- diversity(FrenchiesWIDE, "simpson")
Referencesimp <- diversity(ReferenceWIDE, "simpson")
#SHANNONS
Frenchiesshann <- diversity(ReferenceWIDE, "shannon")
Referenceshann <- diversity(ReferenceWIDE, "shannon")

#calculating mean simpsons diversity of each site
mean(Frenchiessimp)
mean(Referencesimp)
mean(F)

#export diversity tables (Example)
write.csv(Frenchiessimp, file ="FILEPATH" )














