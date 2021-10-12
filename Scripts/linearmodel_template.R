#Loading Libraries
library("ggplot2")
library("tidyverse")
library("reshape2")
library("rstatix")
library("lme4")
library("lmerTest")
library("fitdistrplus")
library("ggpubr")
library("emmeans")
library("car")
library("LMERConvenienceFunctions")
library("visreg")
library("MuMIn")

##NOTE: THIS IS A WORK IN PROGRESS

#LOADING MASTER DATA .CSV 
MASTERDATA <- read.csv("~/Documents/R/CompSites/FieldData/SiteData_Master.csv") 

#Creating Compsite Subset (No REF Sites) 
COMPSITES <- MASTERDATA %>%
  filter(REFERENCE == "NO") 

REFSITES <- MASTERDATA %>%
  filter(REFERENCE == "YES") 

#Creating Fraser Subset (No Serp/Nico) 
FRECOMPSITES <- COMPSITES %>%
  filter(RIVER != "Serpentine") %>%
  filter(RIVER != "Nicomekl")

##Research Question #1: What factors affect marshes being vegetated?
#MODEL 1: Percent Marsh
MODEL1 <- lm(PRCNT_MARSH ~ (TYPE + LOG_FENCE + SHEAR_BOOM + AGE + AREA_MAPPED + OFFSHORE_STRUCTURE + DIST_UPRIVER + PRCNT_EDGE  + ELEV_MEAN)^2, data = FRECOMPSITES)

MODEL2 <- lm(RC_Native ~ (TYPE + LOG_FENCE + SHEAR_BOOM + AGE + AREA_MAPPED + OFFSHORE_STRUCTURE + DIST_UPRIVER + PRCNT_EDGE  + ELEV_MEAN), data = FRECOMPSITES)

summary(MODEL1)
anova(MODEL2)

#performing dredge for optimal models
options(na.action="na.fail")
DREDGE <- dredge(MODEL1, rank = "AICc")
DREDGE

#isolating best model
bestMODEL <- get.models(DREDGE,1)[[1]]
summary(bestMODEL)

JULYRAMETMODEL_FINAL <- glmer(bestMODEL, nAGQ = 1, data = JULYDATA,
                              family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(JULYRAMETMODEL_FINAL)

##DIAGNOSTICS##

#plot residuals - looks good
coef(MODEL)
resid(MODEL)
qqnorm(residuals(MODEL))
qqline(residuals(JULYRAMETMODEL_FINAL))
residualPlots(JULYRAMETMODEL_FINAL)
shapiro_test(residuals(JULYRAMETMODEL_FINAL))
sjPlot::plot_model(JULYRAMETMODEL_FINAL, type = "int")

# ANOVA AND TUKEY PAIRWISE - JULY DATA
JULYRAMETMODELANOVA <- Anova(JULYRAMETMODEL_FINAL, test = "Chisq")
summary(JULYRAMETMODELANOVA)
JULYRAMETMODELem <- emmeans(JULYRAMETMODEL_FINAL, ~ (TREATMENTCODE*YEAR) | ELEVATION) 
JULYRAMETMODELcon <-pairs(JULYRAMETMODELem)
JULYRAMETMODELcon = data.frame(JULYRAMETMODELcon)



####AUGUST ANALYSIS###

#AUGUST: one extreme outlier, but left because they are not likely to be consequential
outliers <- AUGUSTDATA %>%
  group_by(YEAR, TREATMENTCODE) %>%
  identify_outliers(RAMETS)
view(outliers)

#checking normality assumption
#JULY
ggqqplot(JULYDATA, "RAMETS", ggtheme = theme_bw()) +
  facet_grid(YEAR ~ TREATMENTCODE, labeller = "label_both")
fit.gamma <- fitdist(JULYDATA$RAMETS, distr = "gamma", method = "mme")
plot(fit.gamma)

#AUGUST
hist(AUGUSTDATA$RAMETS)
ggqqplot(AUGUSTDATA, "RAMETS", ggtheme = theme_bw()) +
  facet_grid(YEAR ~ TREATMENTCODE, labeller = "label_both")
fit.gamma <- fitdist(AUGUSTDATA$RAMETS, distr = "gamma", method = "mme")
plot(fit.gamma)

#SHAPIRO-WILK TEST FOR NORMAL DISTRIBUTION
AUGUSTDATA %>% 
  shapiro_test(RAMETS)
#had p values <.05, indicating non-normal distributions 
#we are dealing with a gamma distribution + count data so GLMER is our model of choice


#initial AUGUST model - includes 3-way interactions
AUGUSTRAMETMODEL <- glmer(1+RAMETS ~ (TREATMENTCODE+YEAR+ELEVATION)^3 + (1|PLOT), data = AUGUSTDATA,
                          family=poisson(link="log"))

#failed to converge: TROUBLESHOOTING
#singularity check
theta <- getME(AUGUSTRAMETMODEL,"theta")
lower <- getME(AUGUSTRAMETMODEL,"lower")
min(theta[lower==0])
#because none of the theta paramaters ==0 or are super close (<.001), we know singularity is not an issue
#experimented with increasing iterations to 25,000 (no avail)
AUGUSTRAMETMODEL <- glmer(1+RAMETS ~ (TREATMENTCODE+YEAR+ELEVATION)^2 + (1|PLOT), data = AUGUSTDATA,
                          family=poisson(link="log"),control=glmerControl(optCtrl=list(maxfun=2e5)))
#experimented with changing optimizer to "bobyqa" (success!)
AUGUSTRAMETMODEL <- glmer(1+RAMETS ~ (TREATMENTCODE+YEAR+ELEVATION)^2 + (1|PLOT), data = AUGUSTDATA,
                          family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#PROCEEDING WITH MODEL
options(na.action="na.fail")
AUGUSTRAMETMODEL <- glmer(1+RAMETS ~ (TREATMENTCODE+YEAR+ELEVATION)^3 + (1|PLOT), data = AUGUSTDATA,
                          family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#performing dredge for opitmal models
JRM_DREDGE <- dredge(AUGUSTRAMETMODEL, rank = "AICc")
JRM_DREDGE


#isolating best model
bestAUGUSTRAMETMODEL <- get.models(JRM_DREDGE,1)[[1]]
AUGUSTRAMETMODEL_FINAL <- glmer(bestAUGUSTRAMETMODEL, nAGQ = 1, data = AUGUSTDATA,
                                family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(AUGUSTRAMETMODEL_FINAL)

#plotting model interactions
plot_model(AUGUSTRAMETMODEL_FINAL, type = "int")

# ANOVA AND TUKEY PAIRWISE - JULY DATA
AUGUSTRAMETMODELANOVA <- Anova(AUGUSTRAMETMODEL_FINAL, test = "Chisq")


AUGUSTRAMETMODELem <- emmeans(AUGUSTRAMETMODEL_FINAL, ~ (TREATMENTCODE*YEAR) | ELEVATION) 
AUGUSTRAMETMODELcon <-contrast(AUGUSTRAMETMODELem, method = "pairwise")
AUGUSTRAMETMODELcon = data.frame(AUGUSTRAMETMODELcon)

#saving model outputs to .xls format
tab_df(JULYRAMETMODELANOVA, digits = 10,file="~/Git/TyphaData/FieldData/MODELOUTPUTS/JULYRAMETANOVA.xls" )
tab_df(JULYRAMETMODELcon,digits = 10, file="~/Git/TyphaData/FieldData/MODELOUTPUTS/JULYRAMETMODELcon.xls")
tab_df(AUGUSTRAMETMODELANOVA, digits = 10,file="~/Git/TyphaData/FieldData/MODELOUTPUTS/AUGUSTRAMETANOVA.xls" )
tab_df(AUGUSTRAMETMODELcon,digits = 10, file="~/Git/TyphaData/FieldData/MODELOUTPUTS/AUGUSTRAMETMODELcon.xls")



OLD CODE FROM MASTERS:
  
  #%>%
  # group_by(YEAR, TREATMENTCODE)
  #JULYDATA$YEAR = as.factor(JULYDATA$YEAR)
  #JULYDATA$TREATMENT1 = as.factor(JULYDATA$TREATMENT1)
  #JULYDATA = JULYDATA[complete.cases(JULYDATA),]
  
  ### STEP 2: DATA ANALYSIS (JULY) ### 
  
  #JULY: one extreme outlier, but left because they are not likely to be consequential
  outliers <- COMPSITES %>%
  identify_outliers(ELEV_MEAN)
view(outliers)

#checking normality assumption
#JULY
ggqqplot(JULYDATA, "ELEV_MEAN", ggtheme = theme_bw()) +
  facet_grid(YEAR ~ TREATMENTCODE, labeller = "label_both")
fit.gamma <- fitdist(JULYDATA$RAMETS, distr = "gamma", method = "mme")
plot(fit.gamma)


#SHAPIRO-WILK TEST FOR NORMAL DISTRIBUTION
JULYDATA %>% 
  shapiro_test(RAMETS)

#had p values <.05, indicating non-normal distributions 
#we are dealing with a gamma distribution + count data so GLMER is our model of choice

#initial July model - includes 2-way interactions
JULYRAMETMODEL <- glmer(RC_NATIVE ~ (TREATMENTCODE+YEAR+ELEVATION)^2 + (1|PLOT), data = JULYDATA,
                        family=poisson(link="log"))

#failed to converge: TROUBLESHOOTING
#singularity check
theta <- getME(JULYRAMETMODEL,"theta")
lower <- getME(JULYRAMETMODEL,"lower")
min(theta[lower==0])
#because none of the theta paramaters ==0 or are super close (<.001), we know singularity is not an issue

#experimented with increasing iterations to 25,000 (no avail)
JULYRAMETMODEL <- glmer(1+RAMETS ~ (TREATMENTCODE+YEAR+ELEVATION)^2 + (1|PLOT), data = JULYDATA,
                        family=poisson(link="log"),control=glmerControl(optCtrl=list(maxfun=2e5)))
#experimented with changing optimizer to "bobyqa" (success!)
JULYRAMETMODEL <- glmer(1+RAMETS ~ (TREATMENTCODE+YEAR+ELEVATION)^2 + (1|PLOT), data = JULYDATA,
                        family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#PROCEEDING WITH MODEL
JULYRAMETMODEL <- glmer(1+RAMETS ~ (TREATMENTCODE+YEAR+ELEVATION)^3 + (1|PLOT), data = JULYDATA,
                        family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(JULYRAMETMODEL)


###CONTINUED
















