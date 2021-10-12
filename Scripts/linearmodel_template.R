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
library("robustHD")
library("GGally")
library("knitr")
library("piecewiseSEM")
library("cowplot")


#NOTE: THIS IS A WORK IN PROGRESS
#remember to acknowledge authors of Chalifour et al. for coding 

#LOADING MASTER DATA .CSV 
MASTERDATA <- read.csv("~/Documents/R/CompSites/FieldData/SiteData_Master.csv") 

#ensuring sampling year is categorical
MASTERDATA$SAMPLE_YEAR <- as.factor(MASTERDATA$SAMPLE_YEAR)

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
MODEL1 <- lm(PRCNT_MARSH ~ (TYPE + SHEAR_BOOM + OFFSHORE_STRUCTURE + AGE + AREA_MAPPED + ARM_1 + DIST_UPRIVER + GRAZING + ELEV_MEAN), data = FRECOMPSITES)

###Evaluating multicolinearity using VIF
#All VIF values are <4, therefore colinearity does not appear to be an issue among variables
alias(MODEL1)
vif(MODEL1)

##Pearson Corr with all vars  
SAMPLE_YEAR<- FRECOMPSITES$SAMPLE_YEAR
TYPE<- FRECOMPSITES$TYPE
SHEAR_BOOM<- FRECOMPSITES$SHEAR_BOOM
OFFSHORE_STRUCTURE<- FRECOMPSITES$OFFSHORE_STRUCTURE
AGE<- FRECOMPSITES$AGE
AREA <- FRECOMPSITES$AREA_MAPPED
ARM <- FRECOMPSITES$ARM_1
DISTANCE_UPRIVER <- FRECOMPSITES$DIST_UPRIVER
GRAZING <- FRECOMPSITES$GRAZING
ELEVATION <- FRECOMPSITES$ELEV_MEAN
sitecovar<- cbind.data.frame(SAMPLE_YEAR,TYPE, SHEAR_BOOM, OFFSHORE_STRUCTURE, AGE, AREA, ARM, DISTANCE_UPRIVER, GRAZING, ELEVATION)
ggpairs(data = na.omit(sitecovar), title = "Pearson Correlation plot for all variables")


#model selection
MODEL1 <- lm(RC_Invasive~ (TYPE + SHEAR_BOOM + OFFSHORE_STRUCTURE + AGE + AREA_MAPPED + ARM_1 + DIST_UPRIVER + GRAZING + ELEV_MEAN), data = FRECOMPSITES,na.action = "na.fail")
summary(MODEL1) #AIC 746.4772
AIC(MODEL1)

#checking variance inflation factor (VIF)
#all GVIF values <2, proceeding with model as-is 
vif(MODEL1)

### Model selection using dredge function  
# Generate model set
model.set.1 <- dredge(MODEL1) 

# Create model selection table
model_table.1 <- model.sel(model.set.1)
options(scipen = 7)
names(model_table.1) <- c("TYPE", "SHEAR_BOOM", "OFFSHORE_STRUCTURE", "AGE", "AREA", "ARM","DISTANCE_UPRIVER","GRAZING","ELEVATION")
kable(head(model_table.1, n=100), digits = 3)  

# Model averaging Version 1: use all models with delta AIC score less than 4 
model.set.1.4 <- get.models(model.set.1, subset = delta <4)
avg_model.1.4 <- model.avg(model.set.1.4)  
summary(avg_model.1.4)
model.set.1.4ci<- data.frame(confint(avg_model.1.4, full = TRUE)) 
#Get pseudo R squared values for models up to delta < 4
model.set.1.4.Rsq<- rsquared(model.set.1.4)
##write tables to .csv for easy comparison and plugging into appendix table
avg_model_1.4df<- data.frame(avg_model.1.4$msTable)
avg_model_components1.4<- cbind(model.set.1.4.Rsq, avg_model_1.4df)
r = data.frame(Coeff=rownames(avg_model_1.4df, rep(NA, length(avg_model_components1.4))))
avg_model_components1.4<- cbind(avg_model_components1.4,r)
avg_model_components1.4<- avg_model_components1.4[, -c(6,7)] 
#write.csv(avg_model_components1.4, "~/Documents/R/CompSites/ModelOutputs/model1_componentsummary.csv")

importance(model.set.1.4)

## Parameter Plot  
#The results of model averaging including all top ranked models up to delta AICc 4   
model1.coef <- data.frame(summary(avg_model.1.4)[9])
model1.coef <- cbind(model1.coef, model.set.1.4ci)
names(model1.coef)[names(model1.coef) == "coefmat.full.Estimate"] <- "Estimate"
names(model1.coef)[names(model1.coef) == "X2.5.."] <- "LowerCI"
names(model1.coef)[names(model1.coef) == "X97.5.."] <- "UpperCI"
### Order of coefficients in data frame may change - check with FINAL data
model1.coef <- model1.coef [-1, ]
rownames(model1.coef )[1] <- "North Arm"
rownames(model1.coef )[2] <- "South Arm"
rownames(model1.coef )[3] <- "Distance Upriver"
rownames(model1.coef )[4] <- "Minor Grazing"
rownames(model1.coef )[5] <- "Moderate Grazing"
rownames(model1.coef )[6] <- "No Grazing"
rownames(model1.coef )[7] <- "Offshore Structure Present"
rownames(model1.coef )[8] <- "Shear Boom Present"
rownames(model1.coef )[9] <- "Marsh Area"
rownames(model1.coef )[10] <- "Mean Elevation"
rownames(model1.coef )[11] <- "Marsh Age"


model1.coef$Variable <- rownames(model1.coef )
model1.coef$Variable <- as.factor(model1.coef$Variable)
labels <- expression("North Arm", "South Arm", "Distance Upriver", "Minor Grazing", "Moderate Grazing","No Grazing", "Offshore Structure Present", "Shear Boom Present","Marsh Area", "Mean Elevation", "Marsh Age")
labels[[3]] <- bquote(bold(.(labels[[3]])))
labels[[4]] <- bquote(bold(.(labels[[4]])))
labels[[5]] <- bquote(bold(.(labels[[5]])))
labels[[6]] <- bquote(bold(.(labels[[6]])))
labels[[1]] <- bquote(bold(.(labels[[1]])))
labels[[2]] <- bquote(bold(.(labels[[2]])))
gg.model1 <- ggplot(model1.coef, aes(x = reorder(Variable, Estimate), y = Estimate)) + geom_hline(yintercept = 0, color = gray(1/2), lty = 2)
gg.model1 <- gg.model1 + geom_pointrange(aes(x = reorder(Variable, Estimate), y = Estimate, ymin = LowerCI, ymax = UpperCI), position = position_dodge(width = 1/2), shape = 21, fatten = 6, size = 1/2, fill = "black") +theme_cowplot() + ggtitle("Percent Marsh") +
  theme(axis.title = element_blank()) + scale_x_discrete(labels=labels) + scale_y_continuous(limits = c(-60,60)) +
  coord_flip()
gg.model1

summary()




#model selection
MODEL2 <- lm(RC_Native ~ (TYPE + LOG_FENCE + SHEAR_BOOM + AGE + AREA_MAPPED + OFFSHORE_STRUCTURE + DIST_UPRIVER + PRCNT_EDGE  + ELEV_MEAN), data = FRECOMPSITES)

summary(MODEL1)
anova(MODEL1)

#performing dredge for optimal models
options(na.action="na.fail")
DREDGE <- dredge(MODEL1, rank = "AICc")
DREDGE

#isolating best model
bestMODEL <- get.models(DREDGE,1)[[1]]
summary(bestMODEL)

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
















