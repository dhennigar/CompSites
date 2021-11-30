library("ggplot2")
library("tidyverse")
library("car")
library("visreg") #for model visualisation
library("robustHD") #for model visualisation
library("sjPlot") #for model visualisation
library("cowplot") #for panel plots

#LOADING MASTER DATA .CSV 
MASTERDATA <- read.csv("~/Documents/R/CompSites/FieldData/SiteData_Master.csv") 

#Ensuring Sample Year and Area Mapped are in correct format
MASTERDATA$SAMPLE_YEAR <- as.factor(MASTERDATA$SAMPLE_YEAR)
MASTERDATA$AREA_MAPPED_K <- as.numeric(MASTERDATA$AREA_MAPPED_K)

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

###RESEARCH QUESTION #1: What factors lead to marsh recession?

##Exploratory Plots for Review
#distance upriver
M1.1 <- ggplot(FRECOMPSITES, aes(x=DIST_UPRIVER_KM,y=PRCNT_MUDFLAT)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Distance Upriver (km)", y = "% Recessed Marsh") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#project age
M1.2 <- ggplot(FRECOMPSITES, aes(x=AGE,y=PRCNT_MUDFLAT)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Age (years)", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#project size
M1.3 <- ggplot(FRECOMPSITES, aes(x=AREA_MAPPED_K,y=PRCNT_MUDFLAT)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  ylim(0,100) +
  labs(x ="Project Size (1000 m^2)", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#mean elevation
M1.4 <- ggplot(FRECOMPSITES, aes(x=ELEV_MEAN,y=PRCNT_MUDFLAT)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  ylim(0,100) +
  geom_smooth(method = 'lm')+
  labs(x ="Mean Elevation (m)", y = "% Recessed Marsh") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#mean elevation
M1.5 <- ggplot(FRECOMPSITES, aes(x=PRCNT_EDGE,y=PRCNT_MUDFLAT)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  ylim(0,100) +
  geom_smooth(method = 'lm')+
  labs(x ="% Edge Habitat", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#presence of shear boom
M1.6 <- ggplot(FRECOMPSITES, aes(x=SHEAR_BOOM,y=PRCNT_MUDFLAT)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Foreshore Shear Boom", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#presence of adjacent offshore structure
M1.7 <-ggplot(FRECOMPSITES, aes(x=OFFSHORE_STRUCTURE,y=PRCNT_MUDFLAT)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Offshore Structure", y = "% Recessed Marsh") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#log fence
M1.8 <- ggplot(FRECOMPSITES, aes(x=LOG_FENCE,y=PRCNT_MUDFLAT)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Log Fence", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#river arm 
M1.9 <- ggplot(FRECOMPSITES, aes(x=ARM,y=PRCNT_MUDFLAT)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="River Arm", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#inland basin
M1.10 <- ggplot(FRECOMPSITES, aes(x=INLAND,y=PRCNT_MUDFLAT)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Inland Basin", y = "% Recessed Marsh") +
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
FRECOMPSITES$ELEV_MEAN3group <- factor(FRECOMPSITES$ELEV_MEAN3group, levels = c("high", "average", "low"))

#plot 
M1.11 <- FRECOMPSITES %>%
ggplot() +
  aes(x = PRCNT_EDGE, y = PRCNT_MUDFLAT, group = ELEV_MEAN3group, color = ELEV_MEAN3group) +
  geom_point(alpha = .3) +
  ylim(0,100) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none") +
  labs(x ="% Edge Habitat", y = "% Recessed Marsh", color = "Elevation") 

#dummy plot for legend 
M1.11L <- FRECOMPSITES %>%
  ggplot() +
  aes(x = PRCNT_EDGE, y = PRCNT_MARSH, group = ELEV_MEAN3group, color = ELEV_MEAN3group) +
  geom_point(alpha = .7) +
  ylim(0,100) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "left",
        legend.title = element_text(size=8),legend.text = element_text(size=9)) +
  labs(x ="% Edge Habitat", y = "% Recessed Marsh", color = "Elevation") 

#legend object
M1.11Legend <- get_legend(M1.11L + theme(legend.box.margin = margin(-50,0,0,-100))) #note that margin order is "top", "right", "bottom", "left"

#creation of panel figure for paper
M1TopRow <- cowplot::plot_grid(M1.1, M1.2, M1.3, align = "h", axis = "l", ncol =3) # top row
M1MidRow <- cowplot::plot_grid(M1.4, M1.5, M1.6, align = "h", axis = "l", ncol =3) # middle row
M1MidRow2 <- cowplot::plot_grid(M1.7,M1.8,M1.9, align = "h", axis = "l", ncol =3) # 2nd middle row
M1BotRow <- cowplot::plot_grid(M1.10,M1.11,M1.11Legend, align = "h", axis = "l", ncol =3) # bottom row
cowplot::plot_grid(M1TopRow , M1MidRow,M1MidRow2, M1BotRow, ncol = 1, align = "h")

#####MODEL#####
#MODEL 1: Percent Mudflat (recessed marsh)
#I did not use a mixed effects model for MODEL 1, with the rationale that the random effects used in our other models (site, year) are not relevant
#One could argue that "Year" could be used as a random effect, but much of the data used in these site-based models was acquired in 2021 using remote sensing
#Note that the only interaction included to date is %edge*elevation, as edge effect is likely more pronounced with lower marshes than high

#Experimenting with all covariates (MODEL1A), and only covariates that had simple linear regression p values of <.20 (MODEL1B)
MODEL1 <- lm(PRCNT_MUDFLAT ~ (INLAND + LOG_FENCE + SHEAR_BOOM + OFFSHORE_STRUCTURE + AGE + AREA_MAPPED + DIST_UPRIVER + ARM + PRCNT_EDGE*ELEV_MEAN), data = FRECOMPSITES)
AIC(MODEL1)

#model results 
summary(MODEL1) #summary table
plot(MODEL1) #plotting model fit
#Variance inflation factor (measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model) 
vif(MODEL1) #none above 5, so no concerns (James et al. 2014)

#MODEL VISUALISATIONS: LIKELY FOR SUPPLEMENTAL MATERIAL 
#VISREG PACAKAGE: plotting how the expected value of the outcome (% marsh) changes as a function of x, with all other variables in the model held fixed.
visreg(MODEL1, points.par = list(pch = 16, cex = 0.8, col = "red"),type="contrast", ylab = "% Recessed Marsh")


#plotting interaction effect
visreg(MODEL1,"PRCNT_EDGE", by = "ELEV_MEAN", overlay=TRUE,partial = FALSE, gg=TRUE) + 
  theme_bw()+
  xlab("% Edge Habitat") + ylab("% Recessed Marsh") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#COEFFICIENT PLOT
set_theme(base = theme_classic()) #To remove the background color and the grids
#ploting model coefficients
names(MODEL1$coefficients) <- c('Intercept','Inland Basin [Yes]','Log Fence [Present]','Shear Boom [Present]','Offshore Structure [Present]', 'Project Age', 'Size','Distance Upriver', 'Arm [North]', '% Edge', 'Mean Elevation', '% Edge:Mean Elevation')
plot_model(MODEL1, show.values = TRUE, value.offset = .3, title = "% Recessed Marsh", ci.lvl = .95,sort.est = TRUE)


#OLD CODE: Kept Just in Case
#standardize continuous variables to be centered on the mean (mean becomes 0) using the standardize function from robustHD  
#FRECOMPSITES$ELEV_MEANs <-standardize(FRECOMPSITES$ELEV_MEAN, centerFun = mean, scaleFun = sd)
#FRECOMPSITES$DIST_UPRIVERs <-standardize(FRECOMPSITES$DIST_UPRIVER, centerFun = mean, scaleFun = sd)
#FRECOMPSITES$PRCNT_EDGEs <-standardize(FRECOMPSITES$PRCNT_EDGE, centerFun = mean, scaleFun = sd)
#FRECOMPSITES$AREA_MAPPEDs <-standardize(FRECOMPSITES$AREA_MAPPED, centerFun = mean, scaleFun = sd)
#FRECOMPSITES$AGEs <-standardize(FRECOMPSITES$AGE, centerFun = mean, scaleFun = sd)
#FRECOMPSITES$SAMPLING_AGEs <-standardize(FRECOMPSITES$SAMPLING_AGE, centerFun = mean, scaleFun = sd)

