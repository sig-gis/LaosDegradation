library(survey)
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)
library(networkD3)
library(tidyverse)
library(dplyr)


setwd('C:\\Users\\karis\\Documents\\Laos\\LaosDegradation\\6ERPDprovinces')

dataMap <- read.csv('collection\\laos_samples_updated.csv')
head(dataMap)

#Map classes for ccdc are: 

dataCEO_05_10 <- read.csv('collection\\ceo-ERPD-6-provinces-2005---2010-(plots-501-999)-v2-sample-data-2021-08-24.csv')
dataCEO_10_15 <- read.csv('collection\\ceo-ERPD-6-provinces-2010---2015-(plots-1-500)--v2-sample-data-2021-08-24.csv')

dataCEO_05_10$random<-rnorm(length(dataCEO_05_10[,1]), mean = 0, sd = 1)
dataCEO_10_15$random<-rnorm(length(dataCEO_10_15[,1]), mean = 0, sd = 1)
write.csv(dataCEO_10_15, 'QAQCSmpl_10_15.csv', row.names = F)
write.csv(dataCEO_05_10, 'QAQCSmpl_05_10.csv', row.names = F)

colnames(dataCEO_05_10)[1]<-"CEO_plotID"
colnames(dataCEO_10_15)[1]<-"CEO_plotID"


dataMrgd_10 <- merge(dataCEO_10_15, dataMap, by.x = 'pl_plotid', by.y = 'PLOTID', all.x = T)
colnames(dataMrgd_10)
head(dataMrgd_10)

dataMrgd_10$CCDCFPC <- 0
#1 - Stable forest: The land cover was always forest and no disturbance occurred during the study period.
dataMrgd_10$CCDCFPC[dataMrgd_10$ccdc_2010_2015 == 1]<- 52292
#2 - Forest degradation: The land cover was forest at the start and the end, and disturbance occurred during the study period. Disturbance includes shifting cultivation, selective logging, natural forest to plantation, drought, pest damage, etc.
dataMrgd_10$CCDCFPC[dataMrgd_10$ccdc_2010_2015 == 2]<- 8498
#3 - Deforestation: The land cover was forest at the start and non-forest at the end.
dataMrgd_10$CCDCFPC[dataMrgd_10$ccdc_2010_2015 == 3]<- 4665
#4 - Reforestation: The land cover was non-forest at the start and forest at the end.
dataMrgd_10$CCDCFPC[dataMrgd_10$ccdc_2010_2015 == 4]<- 3578
#5 - Non-forest:    The land cover was non-forest at the start and non-forest at the end. 
dataMrgd_10$CCDCFPC[dataMrgd_10$ccdc_2010_2015 == 5]<- 12058

strat_design_10 <- svydesign(id = ~1, strata = ~ccdc_2010_2015, fpc = ~CCDCFPC, data = dataMrgd_10)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
activityData_10

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
activityData_10

#############################

dataMrgd_05 <- merge(dataCEO_05_10, dataMap, by.x = 'pl_plotid', by.y = 'PLOTID', all.x = T)
colnames(dataMrgd_05)
head(dataMrgd_05)

dataMrgd_05$CCDCFPC <- 0
dataMrgd_05$CCDCFPC[dataMrgd_05$ccdc_2005_2010 == 1]<- 52657
dataMrgd_05$CCDCFPC[dataMrgd_05$ccdc_2005_2010 == 2]<- 9400
dataMrgd_05$CCDCFPC[dataMrgd_05$ccdc_2005_2010 == 3]<- 4946
dataMrgd_05$CCDCFPC[dataMrgd_05$ccdc_2005_2010 == 4]<- 3395
dataMrgd_05$CCDCFPC[dataMrgd_05$ccdc_2005_2010 == 5]<- 10693

dataMrgd_05$FCDMFPC[dataMrgd_05$frel_2005_2010 == 1]<- 271972
dataMrgd_05$FCDMFPC[dataMrgd_05$frel_2005_2010 == 2]<- 97911
dataMrgd_05$FCDMFPC[dataMrgd_05$frel_2005_2010 == 3]<- 178200+45869
dataMrgd_05$FCDMFPC[dataMrgd_05$frel_2005_2010 == 4]<- 6933250
dataMrgd_05$FCDMFPC[dataMrgd_05$frel_2005_2010 == 5]<- 595948


strat_design_05 <- svydesign(id = ~1, strata = ~ccdc_2005_2010, fpc = ~CCDCFPC, 
                             data = dataMrgd_05)
strat_design_05

activityData_05 <- svytotal(~Change.type., strat_design)
activityData_05
