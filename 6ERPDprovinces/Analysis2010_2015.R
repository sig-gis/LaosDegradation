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
## dataMap <- read.csv('collection\\laos_samples_updated_20210909.csv')
dataMap <- read.csv('collection\\laos_samples_buffer_20210922.csv')
head(dataMap)

# CEO data
dataCEO_10_15 <- read.csv('collection\\ceo-ERPD-6-provinces-2010---2015-(plots-1-500)--v2-sample-data-2021-08-24.csv')

#QA/QC data
QAQC <- read.csv('collection\\laoJeremyDisagree.csv')

colnames(dataCEO_10_15)[1]<-"CEO_plotID"

dataMrgd_10 <- merge(dataCEO_10_15, dataMap, by.x = 'pl_plotid', by.y = 'PLOTID', all.x = T)
colnames(dataMrgd_10)
dataMrgd_10 <- dataMrgd_10[, c(1:2,4:7,15:22, 27, 31, 33, 35)]
colnames(dataMrgd_10)
head(dataMrgd_10)

##############################
## 2010 - 2015
##############################
# mapAreas10 <- read.csv('collection\\lao-expanded-area-counts10-15.csv')
mapAreas10 <- read.csv('collection\\lao-expanded-area-counts-w-buffer.csv')

head(mapAreas10)

colnames(dataMrgd_10)
head(dataMrgd_10)

## FCP for CODED
unique(mapAreas10$map_name)
dataMrgd_10$coded1015fcp <- 0
dataMrgd_10$coded1015fcp[dataMrgd_10$coded_updated_2010_2015 == 0] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 0)]
dataMrgd_10$coded1015fcp[dataMrgd_10$coded_updated_2010_2015 == 1] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 1)]
dataMrgd_10$coded1015fcp[dataMrgd_10$coded_updated_2010_2015 == 2] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 2)]
dataMrgd_10$coded1015fcp[dataMrgd_10$coded_updated_2010_2015 == 3] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 3)]
dataMrgd_10$coded1015fcp[dataMrgd_10$coded_updated_2010_2015 == 4] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 4)]
dataMrgd_10$coded1015fcp

dataMrgd_10$coded1015_label<-'update'
dataMrgd_10$coded1015_label[dataMrgd_10$coded_updated_2010_2015 == 0] <- '0_no_data'
dataMrgd_10$coded1015_label[dataMrgd_10$coded_updated_2010_2015 == 1] <- '1_stable_forest'
dataMrgd_10$coded1015_label[dataMrgd_10$coded_updated_2010_2015 == 2] <- '5_not_forest'
dataMrgd_10$coded1015_label[dataMrgd_10$coded_updated_2010_2015 == 3] <- '2_degradation'
dataMrgd_10$coded1015_label[dataMrgd_10$coded_updated_2010_2015 == 4] <- '3_deforestation'
dataMrgd_10$coded1015_label

## FCP for CCDC SMA
colnames(dataMrgd_10)
dataMrgd_10$ccdcsma1015fcp <- 0
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 0] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 0)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 1] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 1)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 2] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 2)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 3] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 3)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 4] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 4)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 5] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 5)]
dataMrgd_10$ccdcsma1015fcp

colnames(dataMrgd_10)
dataMrgd_10$ccdc1015_label <- 0
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 0] <- '0_no_data'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 1] <- '1_stable_forest'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 2] <- '2_degradation'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 3] <- '3_deforestation'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 4] <- '4_reforestation'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 5] <- '5_not_forest'
dataMrgd_10$ccdc1015_label

###### Update with FCDM
dataMrgd_10$FCDM1015fcp <- 0
sort(unique(dataMrgd_10$fcdm))

dataMrgd_10$FCDM1015fcp[dataMrgd_10$fcdm < 2010] <- (
  mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 0)]+
    mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2005)]+
    mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2006)]+
    mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2007)]+
    mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2008)]+
    mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2009)])
dataMrgd_10$FCDM1015fcp[dataMrgd_10$fcdm >= 2010] <- (
  mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2010)]+
    mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2011)]+
    mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2012)]+
    mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2013)]+
    mapAreas10$count[(mapAreas10$map_name == 'a_erpd_fcdm_2005_2014' & mapAreas10$map_value == 2014)])
dataMrgd_10$FCDM1015fcp

dataMrgd_10$fcdm1015[dataMrgd_10$fcdm>= 2010]<- 1
dataMrgd_10$fcdm1015[dataMrgd_10$fcdm< 2010]<- 2

dataMrgd_10$fcdm1015_label[dataMrgd_10$fcdm>= 2010]<- '1_stable'
dataMrgd_10$fcdm1015_label[dataMrgd_10$fcdm< 2010]<- '2_forest_disturbance'

## FCP for FREL
colnames(dataMrgd_10)
dataMrgd_10$FREL1015fcp <- 0

sort(unique(dataMrgd_10$frel_change1015))
sort(unique(mapAreas10$map_value[mapAreas10$map_name == 'a_frel_change1015']))

dataMrgd_10$FREL1015fcp[dataMrgd_10$frel_change1015 == 11] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_frel_change1015' & mapAreas10$map_value == 11)]
dataMrgd_10$FREL1015fcp[dataMrgd_10$frel_change1015 == 22] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_frel_change1015' & mapAreas10$map_value == 22)]
dataMrgd_10$FREL1015fcp[dataMrgd_10$frel_change1015 == 24] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_frel_change1015' & mapAreas10$map_value == 24)]
dataMrgd_10$FREL1015fcp[dataMrgd_10$frel_change1015 == 25] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_frel_change1015' & mapAreas10$map_value == 25)]
dataMrgd_10$FREL1015fcp[dataMrgd_10$frel_change1015 == 42] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_frel_change1015' & mapAreas10$map_value == 42)]
dataMrgd_10$FREL1015fcp[dataMrgd_10$frel_change1015 == 44] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_frel_change1015' & mapAreas10$map_value == 44)]
dataMrgd_10$FREL1015fcp[dataMrgd_10$frel_change1015 == 45] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_frel_change1015' & mapAreas10$map_value == 45)]
dataMrgd_10$FREL1015fcp[dataMrgd_10$frel_change1015 == 54] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_frel_change1015' & mapAreas10$map_value == 54)]
dataMrgd_10$FREL1015fcp[dataMrgd_10$frel_change1015 == 55] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_frel_change1015' & mapAreas10$map_value == 55)]


dataMrgd_10$FREL1015_label <- 0
dataMrgd_10$FREL1015_label[dataMrgd_10$frel_change1015 == 11] <- '1_stable_forest (11)'
dataMrgd_10$FREL1015_label[dataMrgd_10$frel_change1015 == 22] <- '1_stable_forest (22)'
dataMrgd_10$FREL1015_label[dataMrgd_10$frel_change1015 == 44] <- '1_stable_forest (44)'
dataMrgd_10$FREL1015_label[dataMrgd_10$frel_change1015 == 24] <- '2_degradation (24)'
dataMrgd_10$FREL1015_label[dataMrgd_10$frel_change1015 == 25] <- '3_deforestation (25)'
dataMrgd_10$FREL1015_label[dataMrgd_10$frel_change1015 == 45] <- '3_deforestation (45)'
dataMrgd_10$FREL1015_label[dataMrgd_10$frel_change1015 == 42] <- '4_reforestation (42)'
dataMrgd_10$FREL1015_label[dataMrgd_10$frel_change1015 == 54] <- '4_reforestation (54)'
dataMrgd_10$FREL1015_label[dataMrgd_10$frel_change1015 == 55] <- '5_not_forest (55)'

## FCP for Merged Maps
colnames(dataMrgd_10)
dataMrgd_10$ccdcsma1015fcp <- 0
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 0] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 0)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 1] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 1)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 2] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 2)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 3] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 3)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 4] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 4)]
dataMrgd_10$ccdcsma1015fcp[dataMrgd_10$ccdc_2010_2015 == 5] <- 
  mapAreas10$count[(mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11' & mapAreas10$map_value == 5)]
dataMrgd_10$ccdcsma1015fcp

colnames(dataMrgd_10)
dataMrgd_10$ccdc1015_label <- 0
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 0] <- '0_no_data'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 1] <- '1_stable_forest'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 2] <- '2_degradation'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 3] <- '3_deforestation'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 4] <- '4_reforestation'
dataMrgd_10$ccdc1015_label[dataMrgd_10$ccdc_2010_2015 == 5] <- '5_not_forest'
dataMrgd_10$ccdc1015_label

##############################
## test assumptions

length(unique(dataMrgd_10$coded_updated_2010_2015)) ==
  length(unique(mapAreas10$map_value[mapAreas10$map_name == 'a_coded10_15']))

length(unique(dataMrgd_10$ccdc_2010_2015)) ==
  length(unique(mapAreas10$map_value[mapAreas10$map_name == 'a_ccdc_class_2010_2015_v11']))

length(unique(dataMrgd_10$frel_change1015)) ==
length(unique(mapAreas10$map_value[mapAreas10$map_name == 'a_frel_change1015']))

##############################
## 2010 - 2015
##############################
dataMrgd_10$Change.type.[dataMrgd_10$Change.type.==""]<-"1 stable forest"
dataMrgd_10$Change.type.[dataMrgd_10$Change.type.=="1 stable forest" & 
                           dataMrgd_10$land.cover..2015 == "not forest"]<-"1 stable non-forest"
dataMrgd_10$Change.type.[dataMrgd_10$Change.type.=="forest degradation"]<-"2 degradation"
dataMrgd_10$Change.type.[dataMrgd_10$Change.type.=="forest loss"]<-"3 deforestation"
dataMrgd_10$Change.type.[dataMrgd_10$Change.type.=="forest restoration"]<-"4 restoration"
dataMrgd_10$Change.type.[dataMrgd_10$Change.type.=="reforestation"]<-"5 reforestation"
dataMrgd_10$Change.type.[dataMrgd_10$Change.type.=="other"]<-"6 other"

dataMrgd_10$fpc<-28639410+67801723
  
colnames(dataMrgd_10[19:22])

head(dataMrgd_10)
table(dataMrgd_10$Change.type.)
table(dataMrgd_10$coded_updated_2010_2015)
table(dataMrgd_10$ccdc_2010_2015)
table(dataMrgd_10$fcdm1015)
table(dataMrgd_10$frel_change1015)

table(dataMrgd_10$coded_updated_2010_2015, dataMrgd_10$coded1015fcp)
table(dataMrgd_10$ccdc_2010_2015, dataMrgd_10$ccdcsma1015fcp)
table(dataMrgd_10$fcdm1015, dataMrgd_10$FCDM1015fcp)
table(dataMrgd_10$frel_change1015, dataMrgd_10$FREL1015fcp)

table(dataMrgd_10$coded_updated_2010_2015, dataMrgd_10$Change.type.)
table(dataMrgd_10$ccdc_2010_2015, dataMrgd_10$Change.type.)
table(dataMrgd_10$fcdm1015, dataMrgd_10$Change.type.)
table(dataMrgd_10$frel_change1015, dataMrgd_10$Change.type.)

## simple random
strat_design_10 <- svydesign(id = ~1, fpc =~dataMrgd_10$fpc, data = dataMrgd_10)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
activityData_10
write.csv(activityData_10, file = 'results\\change10coded.csv')

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
activityData_10
write.csv(activityData_10, file = 'results\\driver10coded.csv')


## CODED
strat_design_10 <- svydesign(id = ~1, strata = ~coded_updated_2010_2015, fpc = ~coded1015fcp, 
                             data = dataMrgd_10)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
activityData_10
write.csv(activityData_10, file = 'results\\change10coded.csv')

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
activityData_10
write.csv(activityData_10, file = 'results\\driver10coded.csv')

## CCDC-SMA
colnames(dataMrgd_10)
colnames(dataMrgd_10[19:22])
strat_design_10 <- svydesign(id = ~1, strata = ~ccdc_2010_2015, fpc = ~ccdcsma1015fcp, data = dataMrgd_10)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
write.csv(activityData_10, file = 'results\\change10CCDC.csv')

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
write.csv(activityData_10, file = 'results\\driver10CCDC.csv')

## FCDM
colnames(dataMrgd_10)
colnames(dataMrgd_10[19:22])
strat_design_10 <- svydesign(id = ~1, strata = ~fcdm1015, fpc = ~FCDM1015fcp, 
                             data = dataMrgd_10)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
write.csv(activityData_10, file = 'results\\change10FCDM.csv')

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
write.csv(activityData_10, file = 'results\\driver10FCDM.csv')

## FREL
colnames(dataMrgd_10)
colnames(dataMrgd_10[19:22])
strat_design_10 <- svydesign(id = ~1, strata = ~frel_change1015, fpc = ~FREL1015fcp, 
                             data = dataMrgd_10)
strat_design_10

activityData_10 <- svytotal(~Change.type., strat_design_10)
write.csv(activityData_10, file = 'results\\change10FREL.csv')

activityData_10 <- svytotal(~Driver.of.degradation., strat_design_10)
write.csv(activityData_10, file = 'results\\driver10FREL.csv')

############################################
## QA/QC subset
############################################

dataCEO_05_10$random<-rnorm(length(dataCEO_05_10[,1]), mean = 0, sd = 1)
dataCEO_10_15$random<-rnorm(length(dataCEO_10_15[,1]), mean = 0, sd = 1)
write.csv(dataCEO_10_15, 'QAQCSmpl_10_15.csv', row.names = F)
write.csv(dataCEO_05_10, 'QAQCSmpl_05_10.csv', row.names = F)


rawData <- mutate(dataMrgd_10, coded1015fcp = case_when(
  (coded_updated_2010_2015 == 0) ~ 
    mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 0)],
  (coded_updated_2010_2015 == 1) ~
    mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 1)],
  (coded_updated_2010_2015 == 2) ~
    mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 2)],
  (coded_updated_2010_2015 == 3) ~
    mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 3)],
  (coded_updated_2010_2015 == 4) ~ 
    mapAreas10$count[(mapAreas10$map_name == 'a_coded10_15' & mapAreas10$map_value == 4)],
  # All else
  TRUE ~ 999
)
)
