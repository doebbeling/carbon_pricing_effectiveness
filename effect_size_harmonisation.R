library(tidyverse)

load("~/CMinc_raw.RData")

################################################
######## Preparation of harmonisation ##########
################################################

#Identification:
  #How is the effect size estimated?
  #Is there data missing for the harmonisation?
  #What uncertainty measure is provided?

CMinc<-CMinc %>%
  #Effect size transformations are done differently for log-level/level-level estimates and for treatment effects ("introduction effects")/price elasticities.
  #We create a variable capturing these.
  mutate(effect_size_transformation=paste(logTransformation, effectSize.type,sep=", ")) %>%
  
  #We identify missing data and other transformation issues.
  mutate(transformation_issues=ifelse(effectSize.type=="price elasticity" & is.na(carbonPriceMean) & is.na(carbonPriceMean.standardized),"missing carbon price mean",NA)) %>%
  mutate(transformation_issues=ifelse(logTransformation=="level-level" & is.na(meanEmissions),"level-level, missing mean emissions",transformation_issues)) %>%
    #We do not consider log-log regression coefficients. There are only two studies reporting effect sizes from such regressions. Both papers also include effect sizes from log-level regressions. Only the latter are considered here for simplicity and to avoid more transformation assumptions.
  mutate(transformation_issues=ifelse(logTransformation=="log-log","log-log",transformation_issues)) %>%
    #There is one effect size that cannot be correctly transformed. The study reports the mean emissions in log transformed format. Since the study offers multiple log-level estimates and only one level-level estimate, the study is not lost due to the exclusion of the one effect size.
  mutate(transformation_issues=ifelse(idNacsos==3598249 & logTransformation=="level-level","level-level, missing mean emissions",transformation_issues))%>%
    #Estimates using emission growth rates as dependent variables can also not be harmonised to treatment effects comparable to the remaining studies.
  mutate(transformation_issues=ifelse(dependentVariable.levelsGrowthrates=="Growth rates", "dependent variable: growth rate",transformation_issues))%>%
 
  #Possible measures of uncertainty: t statistic, standard error, pvalue, confidence interval, significance level (if more than one measure is provided, they are prioritised in this order)
  mutate(uncertainty_measure=ifelse(effectSize.informationComplete=="incomplete","incomplete information",NA))%>%
  mutate(uncertainty_measure=ifelse(!is.na(significanceLevel),"significance level",uncertainty_measure))%>%    
  mutate(uncertainty_measure=ifelse(!is.na(confidenceIntervall),"confidence interval",uncertainty_measure))%>%
  mutate(uncertainty_measure=ifelse(!is.na(pvalue),"p value",uncertainty_measure))%>%
  mutate(uncertainty_measure=ifelse(!is.na(standardError) | !is.na(standardError.standardized),"standard error",uncertainty_measure))%>%
  mutate(uncertainty_measure=ifelse(!is.na(tStatistic),"t statistic",uncertainty_measure))%>%
  mutate(uncertainty_measure=factor(uncertainty_measure,levels = c("t statistic","standard error","p value","confidence interval","significance level","incomplete information"))) %>%
  
  mutate(transformation_issues=ifelse(uncertainty_measure=="incomplete information", "missing uncertainty information",transformation_issues))

#Removing all effect sizes with incomplete information or other reasons that do not allow us to transform the effect size into a harmonised percentage treatment effect, leaves us with 80 out of 90 captured articles.
CMinc <- CMinc %>%
filter(is.na(transformation_issues))

#For completeness and transparency of the data collection, we have extracted all effect sizes provided by each study. Some of these are provided by the primary studies as extreme robustness tests (i.e. estimates without any control variables).
#These effect sizes should not be considered valuable estimates of the policy effect. They were marked as "largerRoBDummy" (larger risk of bias) in the data extraction.
#We exclude these values from any further assessment. 
CMinc <- CMinc %>%
  filter(is.na(largerRoBDummy) | largerRoBDummy=="No")    

  
##############################################
######## Effect size transformation ##########
##############################################  
  
## Introduction effects
#transform log-level estimates to percentage values
CMinc <- CMinc %>%
  mutate(effect_size_percent=
           ifelse(effect_size_transformation=="log-level, introduction effect",
                  (exp(effectSize.statisticalEstimate)-1)*100,
                  NA))

#transform level-level estimates to percentage values
CMinc <- CMinc %>%
  mutate(effect_size_percent=
           ifelse(effect_size_transformation=="level-level, introduction effect",
                  (ifelse(!is.na(effectSize.statisticalEstimate.standardized),effectSize.statisticalEstimate.standardized,effectSize.statisticalEstimate)/meanEmissions)*100,
                  effect_size_percent))

#effect sizes reported as percentage changes
CMinc <- CMinc %>%
  mutate(effect_size_percent=
           ifelse(effect_size_transformation=="relative change in percent/100, introduction effect",
                  (ifelse(!is.na(effectSize.statisticalEstimate.standardized),effectSize.statisticalEstimate.standardized,effectSize.statisticalEstimate))*100,
                  effect_size_percent))

## Price elasticities
CMinc <- CMinc %>%
  mutate(effect_size_percent=
           ifelse(effect_size_transformation=="log-level, price elasticity",
                  (exp(effectSize.statisticalEstimate*carbonPriceMean)-1)*100,
                  effect_size_percent),
         
         effect_size_percent=
           ifelse(effect_size_transformation=="relative change in percent/100, price elasticity",
                  effectSize.statisticalEstimate*carbonPriceMean*100,
                  effect_size_percent))


########################################
########  Transformation of  ###########
######## uncertainty measure ###########
########################################

#Transformations of the uncertainty measure are all done in a two step procedure. First the t-statistic is derived from the effect size information provided in the primary study.
#In a second step the standard error of the transformed (percentage) effect size is derived, by keeping the t-statistic stable. In this way all regression specifications are treated the same, independent e.g. of log transformations. 

#First step
#For the transformation of uncertainty measures to t-statistics the most precise uncertainty measure is used. I.e. only if no precise uncertainty measure is provided, the indicated significance level is used.
CMinc <- CMinc %>%
  mutate(
    
    #significance level -> p value
    pvalue=ifelse(uncertainty_measure=="significance level",
                  ifelse(significanceLevel=="insignificant", 0.1, 1-as.numeric(significanceLevel)),
                  pvalue),
    
    pvalue=ifelse(pvalue==0,0.0001,pvalue),
    
    #p value -> t statistic
    tStatistic=ifelse(uncertainty_measure %in% c("significance level", "p value"),
                      ifelse(pvalue<=0.5,
                             ifelse(!is.na(NumberOfObservations),
                                    qt(pvalue,NumberOfObservations),  #this assumes data was not clustered (conservative assumption) and ignores number of variables
                                    qnorm(pvalue)),                   #if no number of observations is reported, the conservative assumption of normal distribution is used
                                                                      #both go by the conservative (but plausible) assumption of a one sided test
                             ifelse(!is.na(NumberOfObservations),
                                    qt(pvalue/2,NumberOfObservations),  #the assumption of a one sided test is only removed, for p values above 0.5, as this would otherwise mean that the effect size is on the other side of the zero
                                    qnorm(pvalue/2))),                   
                             
                      tStatistic),
    
    #standard error -> t statistic
    tStatistic=ifelse(uncertainty_measure=="standard error" & !is.na(standardError.standardized) & !is.na(effectSize.statisticalEstimate.standardized),  
                      effectSize.statisticalEstimate.standardized/standardError.standardized,
                      tStatistic),
    
    tStatistic=ifelse(uncertainty_measure=="standard error" & !is.na(standardError) & !is.na(effectSize.statisticalEstimate),    #if both standardised and non-standardised effect size and standard error are available, the t-statistic of the non-standardised values overrides the standardised ones (removing additional assumptions or imprecision in the standardisation)
                      effectSize.statisticalEstimate/standardError,
                      tStatistic),
    
    #confidence interval
    standardError=ifelse(uncertainty_measure=="confidence interval",  
                      (effectSize.statisticalEstimate-confidenceIntervall.upperBound)/qt((1-confidenceIntervall.level)/2,NumberOfObservations),    #as above for the transformation of p values, it is assumed that data is not clustered and number of variables is ignored 
                      standardError),
    
    tStatistic=ifelse(uncertainty_measure=="confidence interval",  
                      effectSize.statisticalEstimate/standardError,
                      tStatistic)
                      
  )   #error message on NAs in significance level transformation does not cause any issues in the transformation and can be ignored


#Second step
CMinc <- CMinc %>%
  mutate(SE_percent= abs(effect_size_percent/tStatistic)) %>%  #When transforming p-values to t statistics, the sign is not adequately captured. To correct for this we use the absolute function.
  
  #There is one exception when this transformation does not work, this is when the reported effect size equals 0, which causes the denominator (tStatistic derived above) to be 0.
  #In our dataset this applies to 2 effect sizes, which are therefore transformed separately. As they are both coming from log-level regressions, we use a specific transformation, proposed by Rice as a propagation of error.
  mutate(SE_percent=ifelse(effect_size_percent==0 & effect_size_transformation=="log-level, introduction effect",standardError*exp(effectSize.statisticalEstimate)*100,SE_percent))%>%
  
  #There is one effect sizes which is reported with a zero standard error. Zero standard errors cause problems in the conducted meta-analyses. A marginal standard error is therefore added in this case.
  mutate(SE_percent=ifelse(SE_percent==0,0.00001,SE_percent)) %>%
  mutate(standardError=ifelse(standardError==0,0.00001,standardError))


################################################
########## Save the harmonised data ############
################################################

CMinc_meta <- CMinc  

save(CMinc_meta,file="~/CMinc_final.RData")
write.csv(CMinc_meta,file="~/CMinc_final.csv",row.names = FALSE)
