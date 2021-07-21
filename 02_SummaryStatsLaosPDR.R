
library(tidyverse)
library(tidyr)
library(survey)

setwd('C:\\Users\\karis\\Documents\\SilvaCarbon\\laos_degradation\\Maps\\Comparison_2021\\LaosDegradation\\CEOdata')

#######################################################
#######################################################
################## Load map data ######################
#######################################################
#######################################################
#SavData <- read.csv('ProcessedMergedData06202021.csv')
#SavData <- read.csv('ProcessedMergedData06232021.csv')
SavData <- read.csv('ProcessedMergedData07152021.csv')

###########################################################
####### remove unlabeled plots #############################################
###########################################################
colnames(SavData)
head(SavData)
SavData[SavData$FINAL == 'FixMe', ] 

unique(SavData$FINAL)

SavData[SavData$FINAL == 'other', ] 
write.csv(SavData[SavData$FINAL == 'other', ], file = 'QAQC07132021.csv')
SavData<-SavData[SavData$FINAL != 'FixMe', ] 

###########################################################
####### Map Evaluation -- CODED #############################################
###########################################################
SavData[,c("FINAL", 'codedLab')]
table(SavData$codedLab, SavData$FINAL)
write.csv(table(SavData$codedLab, SavData$FINAL), file = 'Results\\intermedResults06232021.csv')

colnames(SavData)
###########################################################
####### area estimates #############################################
###########################################################
strat_design <- svydesign(id = ~1, strata = ~smplStrata, fpc = ~smplStratCount, data = SavData)
strat_design

activityData <- svytotal(~FINAL, strat_design)
activityData
activityData*0.09

diptero <- subset(strat_design, forestType == 'dipterocarp')
DactivityData <- svytotal(~FINAL, diptero)
DactivityData

mixed<- subset(strat_design, forestType == 'mixed')
MactivityData <- svytotal(~FINAL, mixed)
MactivityData

ever <- subset(strat_design, forestType == 'forest evergreen')
EactivityData <- svytotal(~FINAL, ever)
EactivityData

###########################################################
####### Diptero CEO #############################################
###########################################################
DipData<- SavData[SavData$forestType=='dipterocarp',]
unique(DipData$codedLab)
unique(DipData$FINAL)
head(DipData)
write.csv(DipData[DipData$codedLab == 'dipto degradation' & DipData$FINAL == 'degradation',c('LON','LAT','PLOTID', 'plot_id','strata_coded_year',
                                                                                             "O_LC", "O_Change", "O_Ch_type", "O_deg_driver",
                                                                                             "O_def_type", "O_yrChange", "Confidence", "FINAL")], 
          file = 'CEOExploration\\Diptero\\DegradationAgreev2.csv', row.names = F)

write.csv(DipData[DipData$codedLab == 'dipto degradation' & DipData$FINAL == 'stable forest',c('LON','LAT','PLOTID', 'plot_id','strata_coded_year',
                                                                                                "O_LC", "O_Change", "O_Ch_type", "O_deg_driver",
                                                                                                "O_def_type", "O_yrChange", "Confidence", "FINAL")], 
          file = 'CEOExploration\\Diptero\\Commissionv2.csv', row.names = F)


write.csv(DipData[(DipData$codedLab == 'dipto stable' | DipData$codedLab == 'loss') & 
                    DipData$FINAL == 'degradation',c('LON','LAT','PLOTID', 'plot_id','strata_coded_year',
                                                     "O_LC", "O_Change", "O_Ch_type", "O_deg_driver",
                                                     "O_def_type", "O_yrChange", "Confidence", "FINAL")], 
          file = 'CEOExploration\\Diptero\\Omissionv2.csv', row.names = F)


###########################################################
####### Mixed CEO #############################################
###########################################################
MixData<- SavData[SavData$forestType=='mixed',]
unique(MixData$codedLab)
unique(MixData$FINAL)
colnames(MixData)
head(MixData)
write.csv(MixData[MixData$codedLab == 'mixed degradation' & MixData$FINAL == 'degradation',
                  c('LON','LAT','PLOTID', 'plot_id','strata_coded_year',
                    "O_LC", "O_Change", "O_Ch_type", "O_deg_driver",
                    "O_def_type", "O_yrChange", "Confidence", "FINAL")], 
          file = 'CEOExploration\\Mixed\\MixFDegradationAgreev4.csv', row.names = F)

write.csv(MixData[MixData$codedLab == 'mixed degradation' & MixData$FINAL == 'stable forest',
                  c('LON','LAT','PLOTID', 'plot_id','strata_coded_year',
                    "O_LC", "O_Change", "O_Ch_type", "O_deg_driver",
                    "O_def_type", "O_yrChange", "Confidence", "FINAL")], 
          file = 'CEOExploration\\Mixed\\MixFCommissionv4.csv', row.names = F)


write.csv(MixData[(MixData$codedLab == 'mixed stable' | MixData$codedLab == 'loss') & 
                    MixData$FINAL == 'degradation',
                  c('LON','LAT','PLOTID', 'plot_id','strata_coded_year',
                    "O_LC", "O_Change", "O_Ch_type", "O_deg_driver",
                    "O_def_type", "O_yrChange", "Confidence", "FINAL")], 
          file = 'CEOExploration\\Mixed\\MixFOmissionv4.csv', row.names = F)


###########################################################
####### Evergreen CEO #############################################
###########################################################
EverData<- SavData[SavData$forestType=='forest evergreen',]
unique(EverData$codedLab)
unique(EverData$FINAL)
colnames(EverData)
head(EverData)
write.csv(EverData[EverData$codedLab == 'evergreen degradation' & EverData$FINAL == 'degradation',
                   c('LON','LAT','PLOTID', 'plot_id','strata_coded_year',
                     "O_LC", "O_Change", "O_Ch_type", "O_deg_driver",
                     "O_def_type", "O_yrChange", "Confidence", "FINAL")], 
          file = 'CEOExploration\\Evergreen\\EverDegradationAgreev1.csv', row.names = F)

write.csv(EverData[EverData$codedLab == 'evergreen degradation' & EverData$FINAL == 'stable forest',
                   c('LON','LAT','PLOTID', 'plot_id','strata_coded_year',
                     "O_LC", "O_Change", "O_Ch_type", "O_deg_driver",
                     "O_def_type", "O_yrChange", "Confidence", "FINAL")], 
          file = 'CEOExploration\\Evergreen\\EverCommissionv1.csv', row.names = F)


write.csv(EverData[(EverData$codedLab == 'evergreen stable' | EverData$codedLab == 'loss') & 
                     EverData$FINAL == 'degradation',
                   c('LON','LAT','PLOTID', 'plot_id','strata_coded_year',
                     "O_LC", "O_Change", "O_Ch_type", "O_deg_driver",
                     "O_def_type", "O_yrChange", "Confidence", "FINAL")], 
          file = 'CEOExploration\\Evergreen\\EverOmissionv1.csv', row.names = F)
