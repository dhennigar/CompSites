library("ggplot2")
library("tidyverse")
library("car")
library("visreg") #for model visualisation
library("robustHD") #for model visualisation
library("sjPlot") #for model visualisation
library("cowplot") #for panel plots

#dormant packages that may or may not be used
#library("lme4")
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

#Ensuring Sample Year is a factor
MASTERDATA$SAMPLE_YEAR <- as.factor(MASTERDATA$SAMPLE_YEAR)

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

#standardize continuous variables to be centered on the mean (mean becomes 0) using the standardize function from robustHD  
FRECOMPSITES$ELEV_MEANs <-standardize(FRECOMPSITES$ELEV_MEAN, centerFun = mean, scaleFun = sd)
FRECOMPSITES$DIST_UPRIVERs <-standardize(FRECOMPSITES$DIST_UPRIVER, centerFun = mean, scaleFun = sd)
FRECOMPSITES$PRCNT_EDGEs <-standardize(FRECOMPSITES$PRCNT_EDGE, centerFun = mean, scaleFun = sd)
FRECOMPSITES$AREA_MAPPEDs <-standardize(FRECOMPSITES$AREA_MAPPED, centerFun = mean, scaleFun = sd)
FRECOMPSITES$AGEs <-standardize(FRECOMPSITES$AGE, centerFun = mean, scaleFun = sd)
FRECOMPSITES$SAMPLING_AGEs <-standardize(FRECOMPSITES$SAMPLING_AGE, centerFun = mean, scaleFun = sd)

###RESEARCH QUESTION #1: What factors affect marshes being vegetated?
##VISUALISING EFFECT OF EACH VARIABLE

#distance upriver
M1.1 <- ggplot(FRECOMPSITES, aes(x=DIST_UPRIVER_KM,y=PRCNT_MARSH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x ="Distance Upriver (km)", y = "% Vegetated") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#project age
M1.2 <- ggplot(FRECOMPSITES, aes(x=AGE,y=PRCNT_MARSH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x ="Age (years)", y = "% Vegetated") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#project size
M1.3 <- ggplot(FRECOMPSITES, aes(x=AREA_MAPPED,y=PRCNT_MARSH)) +
  geom_point() +
  ylim(0,100) +
  geom_smooth(method = 'lm')+
  labs(x ="Project Size (m^2)", y = "% Vegetated") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#presence of shear boom
M1.4 <- ggplot(FRECOMPSITES, aes(x=SHEAR_BOOM,y=PRCNT_MARSH)) +
  geom_boxplot() +
  geom_jitter() +
  labs(x ="Foreshore Shear Boom", y = "% Vegetated") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#presence of adjacent offshore structure
M1.5 <-ggplot(FRECOMPSITES, aes(x=OFFSHORE_STRUCTURE,y=PRCNT_MARSH)) +
  geom_boxplot() +
  geom_jitter()+
  labs(x ="Offshore Structure", y = "% Vegetated") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#log fence
M1.6 <- ggplot(FRECOMPSITES, aes(x=LOG_FENCE,y=PRCNT_MARSH)) +
  geom_boxplot() +
  geom_jitter()+
  labs(x ="Log Fence", y = "% Vegetated") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#river arm 
M1.7 <- ggplot(FRECOMPSITES, aes(x=ARM,y=PRCNT_MARSH)) +
  geom_boxplot() +
  geom_jitter()+
  labs(x ="River Arm", y = "% Vegetated") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#interaction between elevation and % edge 
#first have to calculate mean, and mean +/- sd for visualisation
FRECOMPSITES$ELEV_MEAN_2tile <- ntile(FRECOMPSITES$ELEV_MEAN, 2)
FRECOMPSITES$ELEV_MEAN_3tile <- ntile(FRECOMPSITES$ELEV_MEAN, 3)
x <- FRECOMPSITES$ELEV_MEAN

FRECOMPSITES$ELEV_MEAN3group <-
  case_when(x > mean(x)+sd(x) ~ "high",
            x < mean(x)+sd(x) & x > mean(x)-sd(x) ~ "average",
            x < mean(x)-sd(x) ~ "low")

count(FRECOMPSITES,FRECOMPSITES$ELEV_MEAN3group)
FRECOMPSITES$ELEV_MEAN3group <- factor(FRECOMPSITES$ELEV_MEAN3group, levels = c("high", "average", "setosa"))

#plot
M1.8 <- FRECOMPSITES %>%
ggplot() +
  aes(x = PRCNT_EDGE, y = PRCNT_MARSH, group = ELEV_MEAN3group, color = ELEV_MEAN3group) +
  geom_point(alpha = .7) +
  ylim(0,100) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none") +
  labs(x ="% Edge Habitat", y = "% Vegetated", color = "Elevation") 

#dummy plot for legend 
M1.8L <- FRECOMPSITES %>%
  ggplot() +
  aes(x = PRCNT_EDGE, y = PRCNT_MARSH, group = ELEV_MEAN3group, color = ELEV_MEAN3group) +
  geom_point(alpha = .7) +
  ylim(0,100) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "left",
        legend.title = element_text(size=8),legend.text = element_text(size=7)) +
  labs(x ="% Edge Habitat", y = "% Vegetated", color = "Elevation") 

#legend: note that margin order is "top", "right", "bottom", "left"
M1.8Legend <- get_legend(M1.8L +
                           theme(legend.box.margin = margin(-50,0,0,-100))) 

#creation of panel figure for paper! 
M1TopRow <- plot_grid(M1.1, M1.2, M1.3, align = "h", axis = "l", ncol =3)
M1MidRow <- plot_grid(M1.4, M1.5, M1.6, align = "h", axis = "l", ncol =3)
M1BotRow <- plot_grid(M1.7,M1.8,M1.8Legend, align = "h", axis = "l", ncol =3)

plot_grid(M1TopRow , M1MidRow, M1BotRow, ncol = 1, align = "h")


#MODEL 1A: Percent Marsh
#I did not use a mixed effects model for MODEL 1, with the rationale that the random effects used in Models 2 and 3 (site, year) are not relevant
#One could argue that "Year" could be used as a random effect, but much of the data used in these site-based models was acquired in 2021 using remote sensing
#Note that the only interaction included to date is %edge*elevation, as edge effect is likely more pronounced with lower marshes than high

#Experimenting with all covariates (MODEL1A), and only covariates that had simple linear regression p values of <.20 (MODEL1B)
MODEL1A_ALL <- lm(PRCNT_MARSH ~ (LOG_FENCE + SHEAR_BOOM + OFFSHORE_STRUCTURE + AGEs + AREA_MAPPEDs + DIST_UPRIVERs + ARM + PRCNT_EDGEs*ELEV_MEANs), data = FRECOMPSITES)
MODEL1A_SMALL <- lm(PRCNT_MARSH ~ (LOG_FENCE + OFFSHORE_STRUCTURE+ PRCNT_EDGEs*ELEV_MEANs), data = FRECOMPSITES)

#comparing model performance using AIC: full model is the better fit 
AIC(MODEL1A_ALL)
AIC(MODEL1A_SMALL)
anova(MODEL1A,MODEL1B)

#comparing model outputs
summary(MODEL1A_ALL)
summary(MODEL1A_SMALL)

#the predictive power of the model containing all covariates is higher (MODEL1A_ALL), and results frankly make a lot more sense 
#we will stick with MODEL 1A for the time being 

#MODEL DIAGNOSTICS
#plotting model fit
plot(MODEL1A)
#Variance inflation factor (measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model) 
#none above 5, so no concerns (James et al. 2014)
vif(MODEL1A_ALL)

#MODEL VISUALISATIONS: LIKELY FOR SUPPLEMENTAL MATERIAL 
#plotting how the expected value of the outcome (% marsh) changes as a function of x, with all other variables in the model held fixed.
visreg(MODEL1A_ALL, points.par = list(pch = 16, cex = 1.2, col = "red"),type="contrast")
#plotting interaction effect
visreg(MODEL1A_ALL,"PRCNT_EDGEs", by = "ELEV_MEANs", overlay=TRUE,partial = FALSE, gg=TRUE) + 
  theme_bw()+
  xlab("% Edge Habitat") + ylab("% Vegetated Marsh") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#COEFFICIENT PLOT
set_theme(base = theme_classic()) #To remove the background color and the grids
#ploting model coefficients
names(MODEL1A_ALL$coefficients) <- c('Intercept','Log Fence Present','Shear Boom','Offshore Structure', 'Age', 'Size','Distance Upriver', 'North Arm', '% Edge', 'Mean Elevation', '% Edge:Mean Elevation')
plot_model(MODEL1A_ALL, show.values = TRUE, value.offset = .3, title = "% Vegetated Marsh", ci.lvl = .95,sort.est = TRUE)
#plotting the interaction effect
MODEL1A2 <- plot_model(MODEL1A, type = "int", terms = c("PRCNT_EDGEs", "ELEV_MEANs"))




#the following code is for an identical model that models % mudflat instead of % marsh. At this  point it is unlikely to be included in the paper






#MODEL 1B: Percent Mudflat
#Note that the only interaction included to date is %edge*elevation, as edge effect is likely more pronounced with lower marshes than high
#Mudflat is not the inverse of vegetated marsh (log debris is the third category)
#I followed the same protocol as Model 1A and found the entire model to be more powerful than the small model
MODEL1B_ALL <- lm(PRCNT_MUDFLAT ~ (LOG_FENCE + SHEAR_BOOM + OFFSHORE_STRUCTURE + AGEs + AREA_MAPPEDs + DIST_UPRIVERs + ARM + PRCNT_EDGEs*ELEV_MEANs), data = FRECOMPSITES,na.action = na.exclude)
MODEL1B_SMALL <- lm(PRCNT_MUDFLAT ~ (ARM + PRCNT_EDGEs*ELEV_MEANs), data = FRECOMPSITES,na.action = na.exclude)

#comparing model performance using AIC
AIC(MODEL1B_ALL)
AIC(MODEL1B_SMALL)

#comparing model outputs
summary(MODEL1B_ALL)
summary(MODEL1B_SMALL)

#MODEL DIAGNOSTICS
#plotting model fit
plot(MODEL1B)
#Variance inflation factor (measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model) 
#none above 5, so no concerns (James et al. 2014)
vif(MODEL1B_ALL)

#MODEL VISUALISATIONS

#VISREG PACKAGE
#plotting how the expected value of the outcome (% marsh) changes as a function of x, with all other variables in the model held fixed.
visreg(MODEL1B_ALL, points.par = list(pch = 16, cex = 1.2, col = "red"))
#plotting interaction effect
visreg(MODEL1B_ALL,"PRCNT_EDGEs", by = "ELEV_MEANs", overlay=TRUE,partial = FALSE, gg=TRUE) + 
  theme_bw()+
  xlab("% Edge Habitat") + ylab("% Mudflat") +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#COEFFICIENT PLOT
set_theme(base = theme_classic()) #To remove the background color and the grids
#ploting model coefficients
plot_model(MODEL1B_ALL, show.values = TRUE, value.offset = .3)
#plotting the interaction effect
MODEL1B2 <- plot_model(MODEL1B, type = "int", terms = c("PRCNT_EDGEs", "ELEV_MEANs"))

