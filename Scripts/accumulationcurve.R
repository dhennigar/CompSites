# Species accumulation curves for a given comp site and the two nearest reference sites.

library("BiodiversityR")
library("dplyr")
library("tidyverse")
library("ggplot2")


### IMPORT DATA ###
# edit file paths as necessary

# Compensation Site
Comp <- read.csv("C://Users/Owner/OneDrive/Documents/GitHub/CompSites/FieldData/09-006.csv", fileEncoding = "UTF-8-BOM")
#ensuring data are numeric
Comp$PERCENT_COVER = as.numeric(Comp$PERCENT_COVER)
# Comp$PERCENT_COVER = Comp$PERCENT_COVER*10 # unsure as to the purpose of this operation.
Comp <- subset(Comp, ORIGIN == "N", select = c(-ORIGIN, -Site_Number, -COMMENTS, -MAX_LH_CM, -COMMUNITY)) # select native species only and remove extra columns

# Reference Site 1
Ref1 <- read.csv("C://Users/Owner/OneDrive/Documents/GitHub/CompSites/FieldData/FAKE_REF1.csv", fileEncoding = "UTF-8-BOM")
Ref1$PERCENT_COVER - as.numeric(Ref1$PERCENT_COVER)
Ref1 <- subset(Ref1, ORIGIN == "N", select = c(-ORIGIN, -Site_Number, -COMMENTS, -MAX_LH_CM, -COMMUNITY))

# Reference Site 2
Ref2 <- read.csv("C://Users/Owner/OneDrive/Documents/GitHub/CompSites/FieldData/FAKE_REF2.csv", fileEncoding = "UTF-8-BOM")
Ref2$PERCENT_COVER - as.numeric(Ref2$PERCENT_COVER)
Ref2 <- subset(Ref2, ORIGIN == "N", select = c(-ORIGIN, -Site_Number, -COMMENTS, -MAX_LH_CM, -COMMUNITY))


### TRANSFROM DATA FROM LONG TO WIDE FORMAT ###

CompWIDE <- Comp %>%
  spread(SPECIES_CODE, PERCENT_COVER)
CompWIDE[is.na(CompWIDE)] <- 0
CompWIDE = CompWIDE[-1]

Ref1WIDE <- Ref1 %>%
  spread(SPECIES_CODE, PERCENT_COVER)
Ref1WIDE[is.na(Ref1WIDE)] <- 0
Ref1WIDE = Ref1WIDE[-1]

Ref2WIDE <- Ref2 %>%
  spread(SPECIES_CODE, PERCENT_COVER)
Ref2WIDE[is.na(Ref2WIDE)] <- 0
Ref2WIDE = Ref2WIDE[-1]

### CREATE SPECIES ACCUM CURVES
CompRichness <- specaccum(CompWIDE)
Ref1Richness <- specaccum(Ref1WIDE)
Ref2Richness <- specaccum(Ref2WIDE)


### PLOT SAC's
plot(CompRichness, ci.type = "poly", col="tomato4", lwd = 2, ci.lty = 0, ci.col = rgb(1,0,0,0.4), 
        main = "", xlab = "# of plots sampled", ylab = "# of native species", ylim = c(0,40))

lines(Ref1Richness, ci.type = "poly", ci.col = "cyan", lwd = 2, ci.lty = 0)
lines(Ref2Richness, ci.type = "poly", ci.col = "green", lwd = 2, ci.lty = 0)





