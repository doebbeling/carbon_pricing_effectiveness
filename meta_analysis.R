library(metafor)
library(tidyverse)
library(BMS)

load(file="CMinc_final.RData")

##############################################
#### Estimating average treatment effects ####
##############################################

# Random Effects Model
eff_average<-CMinc_meta %>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy,
         method = "REML")

### Outlier detection ### 
cd <- cooks.distance(eff_average,cluster = idStudy)
plot(cd, type="o", pch=19, xlab="idStudy", ylab="Cook's Distance", xaxt="n")
axis(side=1, at=seq_along(cd), labels=names(cd))
text(cd,labels=names(cd))

dfb <- dfbetas(eff_average,cluster = idStudy, reestimate=TRUE)#, parallel="snow", ncpus=3)
plot(dfb$intrcpt, type="o", pch=19, xlab="idStudy", ylab="DFBETAS", xaxt="n")
axis(side=1, at=seq_along(dfb$intrcpt), labels=names(dfb$intrcpt))
text(x=c(1:101),y=dfb$intrcpt,labels=row.names(dfb))

## Both outlier dection methods show that effect sizes measuring the effect on emissions from burning of coal exert an undue influence on the average effect sizes. 
## As these effects likely result from fuel switching without capturing the overall emission effect, five studies with a focus on emissions from coal are excluded in the main assessment.
## We therefore exclude 13 effect sizes from 5 studies studying emissions from coal in our main assessment.
eff_average_with_outliers<-eff_average

# Random Effects Model (full sample)
eff_average<-CMinc_meta %>%
  filter(!(fuelType %in% c("Coal","coal, natural gas"))) %>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy,
         method = "REML")

# Random Effects Model (full sample, clustered standard errors)
eff_average_clustered <- eff_average %>%
     robust(cluster = idStudy, clubSandwich = TRUE)

## Clustering of standard errors only has a marginal impact on the standard errors.

# Random Effects Model (only low risk of bias studies)
eff_average_rob<-CMinc_meta %>%
  filter(!(fuelType %in% c("Coal","coal, natural gas"))) %>%
  filter(RoB=="low")%>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy,
         method = "REML")

### Bias assessment ###
# Precision effect test 
eff_average_PET<-CMinc_meta %>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy, 
         method = "REML",
         mods = SE_percent)

# Precision effect estimate with standard error
eff_average_PEESE<-CMinc_meta %>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy, 
         method = "REML",
         mods = SE_percent^2)

# Estimating statistical power of each effect size
CMinc_meta <- CMinc_meta %>%
  mutate(power_re= pnorm(qnorm(0.05/2, lower.tail = FALSE), mean = abs(eff_average$b[1] / SE_percent), sd=1, lower.tail = FALSE)) %>%
  mutate(power_re=ifelse(synthetic_control,NA,power_re))  #the power analysis is not possible for studies applying synthetic control

# Random Effects Model (only adequately powered effect sizes)
eff_average_waap <- CMinc_meta %>%
  filter(!(fuelType %in% c("Coal","coal, natural gas"))) %>%
  filter(power_re>=0.8)%>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy,
         method = "REML")

# Random Effects Model (only adequately powered effect sizes from low risk of bias studies)
eff_average_waap_rob<-CMinc_meta %>%
  filter(!(fuelType %in% c("Coal","coal, natural gas"))) %>%
  filter(RoB=="low")%>%
  filter(power_re>=0.8)%>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy,
         method = "REML")

# Fixed Effects Model (full sample, clustered standard errors)
eff_average_FE<-CMinc_meta %>%
  filter(!(fuelType %in% c("Coal","coal, natural gas"))) %>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         method = "FE")%>%
  robust(cluster = idStudy, clubSandwich = TRUE)

# Heterogeneity in the studies
I2<-((eff_average$QE-(eff_average$k-1))/eff_average$QE)


######################################
#### Estimating effects by scheme ####
######################################

# Mixed effects model (full sample)
meta_reg<-CMinc_meta %>%
  filter(!(fuelType %in% c("Coal","coal, natural gas"))) %>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy, 
         method = "REML",
         mods = ~ intervention -1)

# Mixed effects model (only low risk of bias studies)
meta_reg_rob<-CMinc_meta %>%
  filter(!(fuelType %in% c("Coal","coal, natural gas"))) %>%
  filter(RoB=="low")%>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy, 
         method = "REML",
         mods = ~ intervention -1)

# Mixed effects model (only adequately powered effect sizes)
meta_reg_waap<-CMinc_meta %>%
  filter(!(fuelType %in% c("Coal","coal, natural gas"))) %>%
  filter(power_re>=0.8)%>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy,
         method = "REML",
         mods = ~ intervention -1)

# Mixed effects model (only adequately powered effect sizes from low risk of bias studies)
meta_reg_waap_rob<-CMinc_meta %>%
  filter(!(fuelType %in% c("Coal","coal, natural gas"))) %>%
  filter(RoB=="low")%>%
  filter(power_re>=0.8)%>%
  rma.mv(data=.,
         yi = effect_size_percent,
         V = SE_percent^2,
         random = ~ idEffect | idStudy, 
         method = "REML",
         mods = ~ intervention -1)


###############################
#### Meta-regressions: BMA ####
###############################

# Preparation of variables
df <- mutate(CMinc_meta, 
             
             BC_carbon_tax = grepl("BC", intervention),
             Chinese_pilot_ETS  = grepl("Chinese", intervention),
             EU_ETS = grepl("EU", intervention),
             Finnish_carbon_tax  = grepl("Finnish", intervention),
             Quebec_ETS = grepl("Quebec", intervention),
             RGGI   = grepl("RGGI", intervention),
             Saitama_ETS  = grepl("Saitama", intervention),
             Swedish_carbon_tax   = grepl("Swedish ", intervention),
             Swiss_ETS   = grepl("Swiss", intervention),
             Tokyo_ETS    = grepl("Tokyo", intervention),
             UK   = grepl("UK", intervention),
             other_schemes = grepl("Australian", intervention) | grepl("California", intervention)|
               grepl("Korea", intervention) | grepl("cross", intervention),
             
             
             Coal = grepl("Coal", fuelType) | grepl("heavy", fuelType),
             AllFuels = grepl("all", fuelType),
             Gasoline = grepl("Gasoline", fuelType) | grepl("Petrol", fuelType),
             Gas = grepl("gas", fuelType),
             
             DVTotal = grepl("Total", dependentVariable.totalCapita),
             DVPC = grepl("Per", dependentVariable.totalCapita),
             
             TransLevelLevel = grepl("level-level", logTransformation),
             TransLogLevel = grepl("log", logTransformation),
             TransRelativePC = grepl("relative", logTransformation),
             
             Data_Month = grepl("Month", timeAggregationLevel) | grepl("month", timeAggregationLevel),
             Data_Year = grepl("Year", timeAggregationLevel),
             
             Data_Country = grepl("country", entityAggregation),
             Data_Firm = grepl("firm", entityAggregation),
             Data_Plant = grepl("plant", entityAggregation),
             Data_Sector = grepl("sector", entityAggregation),
             Data_Region = grepl("region", entityAggregation) | grepl("census", entityAggregation),
             Data_City = grepl("city", entityAggregation),
             Data_Airline = grepl("airline", entityAggregation),
             
             Less_Bias = grepl("low", RoB),
             log_carbon_price = log(carbonPriceMean.USD.2010)
             
) 

metregvar <- c(
  "effect_size_percent",           
  "SE_percent",
  "log_carbon_price",
  "industrial_sectors", #"consumer_sectors",
  "tax",#"cap_and_trade",
  "duration",
  
  "Finnish_carbon_tax", "BC_carbon_tax", "Quebec_ETS", "Chinese_pilot_ETS",
  "RGGI", "Saitama_ETS", "Swiss_ETS", "Tokyo_ETS",  "other_schemes", "Swedish_carbon_tax", #"EU","UK",
  
  "Less_Bias",
  "synthetic_control", 
  "Gasoline", "Gas", "Coal", #"AllFuels"
  "DVTotal", #"DVPC",
  "TransLevelLevel", # "TransLogLevel", #"TransRelativePC",
  "Data_Firm", "Data_Plant","Data_Sector", "Data_Region", "Data_City", "Data_Airline", #"Country",
  "Data_Month", "Data_Year"
  
)

# Bayesian model averaging

dfbms <- df %>%  select(all_of(metregvar)) %>% drop_na() 
dfbms <- mutate_if(dfbms, is.character, as.numeric)
dfbms <- mutate_if(dfbms, is.logical, as.numeric)
summary(dfbms)

percent = bms(dfbms, burn=1e5,iter=3e5, g="UIP", mprior="uniform", nmodel=50000, mcmc="bd", user.int=FALSE)
percent1 = bms(dfbms, burn=1e5,iter=3e5, g="UIP", mprior="dilut", nmodel=50000, mcmc="bd", user.int=FALSE)
percent2 = bms(dfbms, burn=1e5,iter=3e5, g="BRIC", mprior="random", nmodel=50000, mcmc="bd", user.int=FALSE)

coef <- as.data.frame(coef(percent, order.by.pip = T, exact=T, include.constant=T))
print(coef)
coef1 <- as.data.frame(coef(percent1, order.by.pip = T, exact=T, include.constant=T))
print(coef1)
coef2 <- as.data.frame(coef(percent2, order.by.pip = T, exact=T, include.constant=T))
print(coef2)

### Alternative BMA specification without scheme dummies

metregvar <- c(
  "effect_size_percent",           
  "SE_percent",
  "log_carbon_price",
  "industrial_sectors", #"consumer_sectors",
  "tax",#"cap_and_trade",
  "duration",
  
  "Less_Bias",
  "synthetic_control", 
  "Gasoline", "Gas", "Coal", #"AllFuels"
  "DVTotal", #"DVPC",
  "TransLevelLevel", # "TransLogLevel", #"TransRelativePC",
  "Data_Firm", "Data_Plant","Data_Sector", "Data_Region", "Data_City", "Data_Airline", #"Country",
  "Data_Month", "Data_Year"
  
)

dfbms <- df %>%  select(all_of(metregvar)) %>% drop_na() 
dfbms <- mutate_if(dfbms, is.character, as.numeric)
dfbms <- mutate_if(dfbms, is.logical, as.numeric)
summary(dfbms)

percent = bms(dfbms, burn=1e5,iter=3e5, g="UIP", mprior="uniform", nmodel=50000, mcmc="bd", user.int=FALSE)
percent1 = bms(dfbms, burn=1e5,iter=3e5, g="UIP", mprior="dilut", nmodel=50000, mcmc="bd", user.int=FALSE)
percent2 = bms(dfbms, burn=1e5,iter=3e5, g="BRIC", mprior="random", nmodel=50000, mcmc="bd", user.int=FALSE)

coef <- as.data.frame(coef(percent, order.by.pip = T, exact=T, include.constant=T))
print(coef)
coef1 <- as.data.frame(coef(percent1, order.by.pip = T, exact=T, include.constant=T))
print(coef1)
coef2 <- as.data.frame(coef(percent2, order.by.pip = T, exact=T, include.constant=T))
print(coef2)