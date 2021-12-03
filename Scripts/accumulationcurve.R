# Species accumulation curves for a given comp site and the two nearest reference sites.


# import libraries
library("BiodiversityR")
library("tidyverse")
source("./Scripts/VegFunctions.R")

### IMPORT DATA ###

Comp <- VegImport("./FieldData/2021/12-003.csv", year = "2021") %>%
  filter(ORIGIN != "S" & COMMUNITY == "1") %>% 
  VegLongToWide()

Ref1 <- VegImport("./FieldData/2021/REF-07.csv", year = "2021") %>% 
  filter(ORIGIN != "S" & COMMUNITY == "1") %>% 
  VegLongToWide()

Ref2 <- VegImport("./FieldData/2015/csv/REF-11-2015.csv", year = "2015") %>% 
  filter(ORIGIN != "S" & COMMUNITY == "1") %>% 
  VegLongToWide()

### CREATE SPECIES ACCUMULATION CURVES (SAC)

CompRichness <- specaccum(Comp)
Ref1Richness <- specaccum(Ref1)
Ref2Richness <- specaccum(Ref2)


### PLOT SAC

# plot comp site SAC
plot(CompRichness, ci.type = "poly", lwd = 2, ci.lty = 0, ci.col = rgb(1,0,0,0.1), col = "red", 
     xlab = "# of plots sampled", ylab = "native species richness", ylim = c(0,40))

# plot reference site SAC
lines(Ref1Richness, ci.type = "poly", ci.col = rgb(0.3,1,0.3,0.1), col = rgb(0,0.7,0), lwd = 2, ci.lty = 0, lty = 2)
lines(Ref2Richness, ci.type = "poly", ci.col = rgb(0.3,0.3,1,0.1), col = "blue", lwd = 2, ci.lty = 0, lty = 2)

# legend
legend(1, 38, legend = c("12-003", "REF-7", "REF-2015-11"),
       col = c("red", rgb(0,0.7,0), "blue"),
       lty = c(1,2,2))




