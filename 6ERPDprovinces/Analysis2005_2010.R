library(survey)
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)
library(networkD3)
library(tidyverse)
library(dplyr)


setwd('C:\\Users\\karis\\Documents\\Laos\\LaosDegradation\\6ERPDprovinces')

#sample map intersection
dataMap <- read.csv('collection\\laos_samples_updated_20210909.csv')
head(dataMap)

# CEO data
dataCEO_05_10 <- read.csv('collection\\ceo-ERPD-6-provinces-2005---2010-(plots-501-999)-v2-sample-data-2021-08-24.csv')

colnames(dataCEO_05_10)[1]<-"CEO_plotID"

dataMrgd_05 <- merge(dataCEO_05_10, dataMap, by.x = 'pl_plotid', by.y = 'PLOTID', all.x = T)
dataMrgd_05 <- dataMrgd_05[, c(1:2,4:7,15:22, 26, 32, 33, 34)]
colnames(dataMrgd_05)
head(dataMrgd_05)

##############################
## 2005 - 2010
##############################
mapAreas05 <- read.csv('collection\\lao-expanded-area-counts05-10.csv')
head(mapAreas05)

colnames(dataMrgd_05)
head(dataMrgd_05)

## FCP for CODED
unique(mapAreas05$map_name)
dataMrgd_05$coded0510fcp <- 0
dataMrgd_05$coded0510fcp[dataMrgd_05$coded_updated_2005_2010 == 0] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 0)]
dataMrgd_05$coded0510fcp[dataMrgd_05$coded_updated_2005_2010 == 1] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 1)]
dataMrgd_05$coded0510fcp[dataMrgd_05$coded_updated_2005_2010 == 2] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 2)]
dataMrgd_05$coded0510fcp[dataMrgd_05$coded_updated_2005_2010 == 3] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 3)]
dataMrgd_05$coded0510fcp[dataMrgd_05$coded_updated_2005_2010 == 4] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 4)]
dataMrgd_05$coded0510fcp

## FCP for CCDC SMA
colnames(dataMrgd_05)
dataMrgd_05$ccdcsma0510fcp <- 0
dataMrgd_05$ccdcsma0510fcp[dataMrgd_05$ccdc_2005_2010 == 0] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_ccdc_class_2005_2010_v11' & mapAreas05$map_value == 0)]
dataMrgd_05$ccdcsma0510fcp[dataMrgd_05$ccdc_2005_2010 == 1] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_ccdc_class_2005_2010_v11' & mapAreas05$map_value == 1)]
dataMrgd_05$ccdcsma0510fcp[dataMrgd_05$ccdc_2005_2010 == 2] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_ccdc_class_2005_2010_v11' & mapAreas05$map_value == 2)]
dataMrgd_05$ccdcsma0510fcp[dataMrgd_05$ccdc_2005_2010 == 3] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_ccdc_class_2005_2010_v11' & mapAreas05$map_value == 3)]
dataMrgd_05$ccdcsma0510fcp[dataMrgd_05$ccdc_2005_2010 == 4] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_ccdc_class_2005_2010_v11' & mapAreas05$map_value == 4)]
dataMrgd_05$ccdcsma0510fcp[dataMrgd_05$ccdc_2005_2010 == 5] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_ccdc_class_2005_2010_v11' & mapAreas05$map_value == 5)]
dataMrgd_05$ccdcsma0510fcp

###### Update with FCDM
dataMrgd_05$FCDM0510fcp <- 0
sort(unique(dataMrgd_05$fcdm))

dataMrgd_05$FCDM0510fcp[dataMrgd_05$fcdm >= 2005 & dataMrgd_05$fcdm < 2010] <- (
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2005)]+
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2006)]+
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2007)]+
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2008)]+
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2009)])
dataMrgd_05$FCDM0510fcp[dataMrgd_05$fcdm >= 2010 | dataMrgd_05$fcdm == 0] <- (
  mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 0)]+
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2010)]+
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2011)]+
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2012)]+
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2013)]+
    mapAreas05$count[(mapAreas05$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas05$map_value == 2014)])
dataMrgd_05$FCDM0510fcp

dataMrgd_05$fcdm0510[dataMrgd_05$fcdm >= 2005 & dataMrgd_05$fcdm < 2010]<- 1
dataMrgd_05$fcdm0510[dataMrgd_05$fcdm >= 2010 | dataMrgd_05$fcdm == 0]<- 2

## FCP for FREL
colnames(dataMrgd_05)
dataMrgd_05$FREL0510fcp <- 0

sort(unique(dataMrgd_05$frel_change0510))
sort(unique(mapAreas05$map_value[mapAreas05$map_name == 'a_frel_change0510']))

dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 11] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 11)]
dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 22] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 22)]
dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 24] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 24)]
dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 25] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 25)]
dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 33] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 33)]
dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 42] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 42)]
dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 44] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 44)]
dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 45] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 45)]
dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 54] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 54)]
dataMrgd_05$FREL0510fcp[dataMrgd_05$frel_change0510 == 55] <- 
  mapAreas05$count[(mapAreas05$map_name == 'a_frel_change0510' & mapAreas05$map_value == 55)]

##############################
## test assumptions

length(unique(dataMrgd_05$coded_updated_2005_2010)) ==
  length(unique(mapAreas05$map_value[mapAreas05$map_name == 'a_coded05_10']))

length(unique(dataMrgd_05$ccdc_2005_2010)) ==
  length(unique(mapAreas05$map_value[mapAreas05$map_name == 'a_ccdc_class_2005_2010_v11']))

length(unique(dataMrgd_05$frel_change0510)) ==
  length(unique(mapAreas05$map_value[mapAreas05$map_name == 'a_frel_change0510']))

##############################
## 2005 - 2010
##############################
colnames(dataMrgd_05[19:22])

## CODED
strat_design_10 <- svydesign(id = ~1, strata = ~coded_updated_2005_2010, fpc = ~coded0510fcp, data = dataMrgd_05)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
write.csv(activityData_10, file = 'results\\change05coded.csv')

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
write.csv(activityData_10, file = 'results\\driver05coded.csv')

## CCDC-SMA
colnames(dataMrgd_05)
colnames(dataMrgd_05[19:22])
strat_design_10 <- svydesign(id = ~1, strata = ~ccdc_2005_2010, fpc = ~ccdcsma0510fcp, data = dataMrgd_05)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
write.csv(activityData_10, file = 'results\\change05CCDC.csv')

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
write.csv(activityData_10, file = 'results\\driver05CCDC.csv')

## FCDM
colnames(dataMrgd_05)
colnames(dataMrgd_05[19:22])
strat_design_10 <- svydesign(id = ~1, strata = ~fcdm0510, fpc = ~FCDM0510fcp, 
                             data = dataMrgd_05)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
write.csv(activityData_10, file = 'results\\change05FCDM.csv')

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
write.csv(activityData_10, file = 'results\\driver05FCDM.csv')

## FREL
colnames(dataMrgd_05)
colnames(dataMrgd_05[19:22])
strat_design_10 <- svydesign(id = ~1, strata = ~frel_change0510, fpc = ~FREL0510fcp, 
                             data = dataMrgd_05)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
write.csv(activityData_10, file = 'results\\change05FREL.csv')

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
write.csv(activityData_10, file = 'results\\driver05FREL.csv')

############################################
## QA/QC subset
############################################

dataCEO_05_10$random<-rnorm(length(dataCEO_05_10[,1]), mean = 0, sd = 1)
dataCEO_10_15$random<-rnorm(length(dataCEO_10_15[,1]), mean = 0, sd = 1)
write.csv(dataCEO_10_15, 'QAQCSmpl_10_15.csv', row.names = F)
write.csv(dataCEO_05_10, 'QAQCSmpl_05_10.csv', row.names = F)


rawData <- mutate(dataMrgd_05, coded0510fcp = case_when(
  (coded_updated_2005_2010 == 0) ~ 
    mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 0)],
  (coded_updated_2005_2010 == 1) ~
    mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 1)],
  (coded_updated_2005_2010 == 2) ~
    mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 2)],
  (coded_updated_2005_2010 == 3) ~
    mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 3)],
  (coded_updated_2005_2010 == 4) ~ 
    mapAreas05$count[(mapAreas05$map_name == 'a_coded05_10' & mapAreas05$map_value == 4)],
  # All else
  TRUE ~ 999
)
)
