library("ggplot2")
library("tidyverse")
library("car")
library("visreg")
library("robustHD")
library("sjPlot")


#library("lme4")
#dormant packages that may or may not be used
#library("GGally")
#library("knitr")
#library("piecewiseSEM")
#library("cowplot")
#library("MuMIn")
#library("LMERConvenienceFunctions")
#library("reshape2")
#library("rstatix")

#library("lmerTest")
#library("fitdistrplus")
#library("ggpubr")
#library("emmeans")
#Loading Libraries


#NOTE: THIS IS A WORK IN PROGRESS!!!


#LOADING MASTER DATA .CSV 
MASTERDATA <- read.csv("~/Documents/R/CompSites/FieldData/SiteData_Master.csv") 

#Ensuring Sample Year, Reference, and Project Type are factors, and ordering dummy variables as preferred
MASTERDATA$SAMPLE_YEAR <- as.factor(MASTERDATA$SAMPLE_YEAR)
MASTERDATA$REFERENCE <- as.factor(MASTERDATA$REFERENCE)
MASTERDATA$TYPE <- factor(MASTERDATA$TYPE, levels = c("Other", "Basin", "Embayment", "Inline", "Protruding"))


##Creating Subset Objects for Later Models 
#All Fraser Only 
FRESITES <- MASTERDATA %>%
  filter(RIVER == "Fraser") 
#Fraser Comp Site Subset (No REF Sites) 
FRECOMPSITES <- FRESITES %>%
  filter(REFERENCE == "NO") 
#Fraser Ref Site Subset (No Comp Sites)
FREREFSITES <- FRESITES %>%
  filter(REFERENCE == "YES") 
#Cattail-free Comp sites
FRECOMPSITESNOCATTAIL <- FRECOMPSITES %>%
  filter(TYPHA_PRES == "N")


#standardize continuous variables to be centered on the mean (mean becomes 0) using the standardize function from robustHD  
FRECOMPSITES$ELEV_MEANs <-standardize(FRECOMPSITES$ELEV_MEAN, centerFun = mean, scaleFun = sd)
FRECOMPSITES$DIST_UPRIVERs <-standardize(FRECOMPSITES$DIST_UPRIVER, centerFun = mean, scaleFun = sd)
FRECOMPSITES$PRCNT_EDGEs <-standardize(FRECOMPSITES$PRCNT_EDGE, centerFun = mean, scaleFun = sd)
FRECOMPSITES$AREA_MAPPEDs <-standardize(FRECOMPSITES$AREA_MAPPED, centerFun = mean, scaleFun = sd)
FRECOMPSITES$AGEs <-standardize(FRECOMPSITES$AGE, centerFun = mean, scaleFun = sd)

###RESEARCH QUESTION #1: What factors affect marshes being vegetated?

#MODEL 1A: Percent Marsh
#I did not used a mixed effects model for MODEL 1, with the rationale that the random effects used in Models 2 and 3 (site, year) are not revelant
#One could argue that "Year" could be used as a random effect, but much of the data used in these site-based models was acquired in 2021 using remote sensing
#Note that the only interaction included to date is %edge*elevation, as edge effect is likely more pronounced with lower marshes than high
#Experimenting with all covariates (MODEL1A), and only covariates that had simple linear regression p values of <.20 (MODEL1B)
#included an interaction of percent edge and mean elevation as I believe the effect of edge habitat on % vegetated is dependent on elevation
MODEL1A_ALL <- lm(PRCNT_MARSH ~ (TYPE + LOG_FENCE + SHEAR_BOOM + OFFSHORE_STRUCTURE + AGEs + AREA_MAPPEDs + DIST_UPRIVERs + ARM + PRCNT_EDGEs*ELEV_MEANs), data = FRECOMPSITES,na.action = na.exclude)
MODEL1A_SMALL <- lm(PRCNT_MARSH ~ (LOG_FENCE + OFFSHORE_STRUCTURE + ARM + PRCNT_EDGEs*ELEV_MEANs), data = FRECOMPSITES,na.action = na.exclude)

#comparing model performance using AIC
AIC(MODEL1A_ALL)
AIC(MODEL1A_SMALL)
anova(MODEL1A,MODEL1B)

#comparing model outputs
summary(MODEL1A_ALL)
summary(MODEL1A_SMALL)

#the same factors are statistically significant in both models, but the predictive power of the model containing all covariates is higher (Model 1A)
#we will stick with MODEL 1A for the time being 

#VISUALIZING DIAGNOSTICS
#plotting the interaction effect
plot_model(MODEL1A, type = "int", terms = c("PRCNT_EDGE", "ELEV_MEAN"))
#plotting model
plot(MODEL1A)
#plotting how the expected value of the outcome (% marsh) changes as a function of x, with all other variables in the model held fixed.
visreg(MODEL1A, points.par = list(pch = 16, cex = 1.2, col = "red"))

#OTHER DIAGNOSTICS
#Variance inflation factor (measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model) 
#none above 5, so no concerns (James et al. 2014)
vif(MODEL1A_ALL)

#MODEL 1B: Percent Mudflat
#Note that the only interaction included to date is %edge*elevation, as edge effect is likely more pronounced with lower marshes than high
#Mudflat is not the inverse of vegetated marsh (log debris is the third category)
#I followed the same protocol as Model 1A and found the entire model to be more powerful than the small model
MODEL1B_ALL <- lm(PRCNT_MUDFLAT ~ (TYPE + LOG_FENCE + SHEAR_BOOM + OFFSHORE_STRUCTURE + AGEs + AREA_MAPPEDs + DIST_UPRIVERs + ARM + PRCNT_EDGEs*ELEV_MEANs), data = FRECOMPSITES,na.action = na.exclude)
MODEL1B_SMALL <- lm(PRCNT_MUDFLAT ~ (ARM + PRCNT_EDGEs*ELEV_MEANs), data = FRECOMPSITES,na.action = na.exclude)

#comparing model performance using AIC
AIC(MODEL1B_ALL)
AIC(MODEL1B_SMALL)

#comparing model outputs
summary(MODEL1B_ALL)
summary(MODEL1B_SMALL)

#VISUALIZING DIAGNOSTICS
#plotting the interaction effect
plot_model(MODEL1B_ALL, type = "int", terms = c("PRCNT_EDGE", "ELEV_MEAN"))
#plotting model
plot(MODEL1B_ALL)
#plotting how the expected value of the outcome (% marsh) changes as a function of x, with all other variables in the model held fixed.
visreg(MODEL1B_ALL, points.par = list(pch = 16, cex = 1.2, col = "red"))

#OTHER DIAGNOSTICS
#Variance inflation factor (measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model) 
#none above 5, so no concerns (James et al. 2014)
vif(MODEL1B_ALL)


###PLOTTING RESULTS FOR REPORT###
avPlots(MODEL1A_ALL)
