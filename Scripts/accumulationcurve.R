#This code will produce species accumulation curves for plots. I think in an ideal world we would plot the curve of (1) our given compensation site, and (2) the nearest two reference sites for our site reports. This wasn't done in 2016 but I think it would be a useful addition. 

#To use the code,it should be as simple as changing the .csv filepath to wherever our files are.


#Loading Libraries
library("BiodiversityR")
library("dplyr")
library("tidyverse")


#IMPORTING A .CSV
SITEDATA <- read.csv("INSERT FILEPATH, IDEALLY A GITHUB FOLDER")
#ensuring data are numeric
SITEDATA$PERCENT_COVER = as.numeric(SITEDATA$PERCENT_COVER)
SITEDATA$PERCENT_COVER = SITEDATA$PERCENT_COVER*10

#transforming dataset into wide format for richness calcs
SITEDATAWIDE <- SITEDATA %>%
  spread(SPECIES_CODE, PERCENT_COVER)
SITEDATAWIDE[is.na(SITEDATAWIDE)] <- 0
SITEDATAWIDE = SITEDATAWIDE[-1]

#species accumulation curve
SITECURVE <- specaccum(SITEDATAWIDE)

#plotting the curve
PLOTSITECURVE <- plot(SITECURVE, ci.type="poly", col="tomato4", lwd=2, ci.lty=0, ci.col=rgb(1,0,0,0.4), 
              main = "", xlab="# of plots sampled", ylab= "# of native species", ylim =c(0,40))







