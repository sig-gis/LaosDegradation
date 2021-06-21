library(tidyverse)
library(tidyr)

setwd('C:\\Users\\karis\\Documents\\SilvaCarbon\\laos_degradation\\Maps\\Comparison_2021\\LaosDegradation\\CEOdata')

#######################################################
#######################################################
################## Load map data ######################
#######################################################
#######################################################
data <- read.csv('ProcessedMergedData06202021.csv')

###########################################################
####### remove unlabeled plots #############################################
###########################################################
colnames(data)
data$PLOTID<- seq(from= 1, to= 304)
data<-data[,c("lon", "lat", "PLOTID", "plot_id", "flagged", "email", "O_LC","O_Change", "O_Ch_type", 
              "O_deg_driver", "O_def_type","O_yrChange", "Confidence", "FINAL", "Post2015", "codedLab", 
              "strata_coded_year", "fcdm", "frel_2015", "frel_2019", "smplStrata", "smplLab", 
              "forestType", "smplStratCount")]

colnames(data)[1]<-'LON'
colnames(data)[2]<-"LAT"
head(data)
data[data$FINAL == 'FixMe', ] 
data<-data[data$FINAL != 'FixMe', ] 

###########################################################
####### Map Evaluation -- CODED #############################################
###########################################################
data[,c("FINAL", 'codedLab')]
table(data$codedLab, data$FINAL)
write.csv(table(data$codedLab, data$FINAL), file = 'Results\\intermedResults06202021.csv')

###########################################################
####### Diptero CEO #############################################
###########################################################
DipData<- data[data$forestType=='dipterocarp',]
unique(DipData$codedLab)
unique(DipData$FINAL)
head(DipData)
write.csv(DipData[DipData$codedLab == 'dipto degradation' & DipData$FINAL == 'degradation',c(1:3)], 
          file = 'CEOExploration\\Diptero\\DegradationAgreev2.csv', row.names = F)

write.csv(DipData[DipData$codedLab == 'dipto degradation' & DipData$FINAL == 'stable forest',c(1:3)], 
          file = 'CEOExploration\\Diptero\\Commissionv2.csv', row.names = F)


write.csv(DipData[(DipData$codedLab == 'dipto stable' | DipData$codedLab == 'loss') & 
                 DipData$FINAL == 'degradation',c(1:3)], 
          file = 'CEOExploration\\Diptero\\Omissionv2.csv', row.names = F)


###########################################################
####### Mixed CEO #############################################
###########################################################
MixData<- data[data$forestType=='mixed',]
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
EverData<- data[data$forestType=='forest evergreen',]
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


###########################################################
####### Commission #############################################
###########################################################
commission<- DipData[DipData$codedLab == 'dipto degradation' & DipData$FINAL == 'stable forest',]
head(commission)

###########################################################
####### Omission #############################################
###########################################################


###########################################################
####### Map Evaluation -- CODED #############################################
###########################################################
sub<- data2[data2$forestType=='dipterocarp',]

sub[,c('pl_plotid','O_LC','J_LC')]
sub[,c('pl_plotid','email','O_Change','J_Change','KK_Change')]
sub[,c('pl_plotid','O_Ch_type','J_Ch_type','KK_Ch_type')]#'CW_Ch_type','email',
sub[,c('pl_plotid','O_deg_driver','J_deg_driver')]
sub[,c('O_def_type','J_def_type')]  
sub[,c('pl_plotid','O_yrChange','J_yrChange')]
sub$J_notes;sub$CW_notes;sub$KK_notes
sub$Confidence;sub$CW_confidence;sub$KK_confidence 

#########################################################
sub<- data2[data2$coded=='dipterocarp degradation',]
sub$O_LC; sub$J_LC
sub$O_Change;sub$J_Change;sub$CW_Change;sub$KK_Change
sub$O_Ch_type;sub$J_Ch_type;sub$CW_Ch_type;sub$KK_Ch_type 
sub$O_deg_driver;sub$J_deg_driver
sub$O_def_type;sub$J_def_type  
sub$O_yrChange;sub$J_yrChange
sub$J_notes;sub$CW_notes;sub$KK_notes
sub$Confidence;sub$CW_confidence;sub$KK_confidence 

sub<- data2[data2$coded=='dipterocarp forest loss',]

table(sub$O_LC); table(sub$J_LC)
table(sub$O_LC,sub$J_LC)
sub[,c('pl_plotid','O_LC','J_LC')]

sub$O_Change;sub$J_Change;sub$KK_Change
sub[,c('pl_plotid','email','O_Change','J_Change','KK_Change')]

sub$O_Ch_type;sub$J_Ch_type;sub$CW_Ch_type;sub$KK_Ch_type 
sub[,c('pl_plotid','O_Ch_type','J_Ch_type','KK_Ch_type')]#'CW_Ch_type','email',


sub[,c('pl_plotid','O_deg_driver','J_deg_driver')]
sub[,c('O_def_type','J_def_type')]  

sub[,c('O_yrChange','J_yrChange')]
sub$J_notes;sub$CW_notes;sub$KK_notes
sub$Confidence;sub$CW_confidence;sub$KK_confidence 


sub<- data2[data2$coded=='dipterocarp, stable',]
table(sub$O_LC); table(sub$J_LC)
table(sub$O_LC,sub$J_LC)
sub[,c('pl_plotid','O_LC','J_LC')]

sub$O_Change;sub$J_Change;sub$CW_Change;sub$KK_Change
sub[,c('pl_plotid','email','O_Change','J_Change','CW_Change','KK_Change')]

sub$O_Ch_type;sub$J_Ch_type;sub$CW_Ch_type;sub$KK_Ch_type 
sub[,c('pl_plotid','O_Ch_type','J_Ch_type','KK_Ch_type')]#'CW_Ch_type','email',

sub[,c('pl_plotid','O_deg_driver','J_deg_driver')]
sub[,c('O_def_type','J_def_type')]  

sub[,c('pl_plotid','O_yrChange','J_yrChange')]
sub$J_notes;sub$CW_notes;sub$KK_notes
sub$Confidence;sub$CW_confidence;sub$KK_confidence 
###########################################################
###########################################################
####### Mixed #############################################
###########################################################
###########################################################
sub<- data2[data2$coded=='mixed degradation',]
sub$O_LC; sub$J_LC
sub$O_Change;sub$J_Change;sub$CW_Change;sub$KK_Change
sub$O_Ch_type;sub$J_Ch_type;sub$CW_Ch_type;sub$KK_Ch_type 
sub$O_deg_driver;sub$J_deg_driver
sub$O_def_type;sub$J_def_type  
sub$O_yrChange;sub$J_yrChange
sub$J_notes;sub$CW_notes;sub$KK_notes
sub$Confidence;sub$CW_confidence;sub$KK_confidence 

sub<- data2[data2$coded=='mixed forest loss',]

table(sub$O_LC); table(sub$J_LC)
table(sub$O_LC,sub$J_LC)
sub[,c('pl_plotid','O_LC','J_LC')]

sub$O_Change;sub$J_Change;sub$CW_Change;sub$KK_Change
sub[,c('pl_plotid','email','O_Change','J_Change','CW_Change','KK_Change')]

sub$O_Ch_type;sub$J_Ch_type;sub$CW_Ch_type;sub$KK_Ch_type 
sub[,c('pl_plotid','O_Ch_type','J_Ch_type','KK_Ch_type')]#'CW_Ch_type','email',


sub[,c('pl_plotid','O_deg_driver','J_deg_driver')]
sub[,c('O_def_type','J_def_type')]  

sub[,c('O_yrChange','J_yrChange')]
sub$J_notes;sub$CW_notes;sub$KK_notes
sub$Confidence;sub$CW_confidence;sub$KK_confidence 


sub<- data2[data2$coded=='mixed, stable',]
table(sub$O_LC); table(sub$J_LC)
table(sub$O_LC,sub$J_LC)
sub[,c('pl_plotid','O_LC','J_LC')]

sub$O_Change;sub$J_Change;sub$CW_Change;sub$KK_Change
sub[,c('pl_plotid','email','O_Change','J_Change','CW_Change','KK_Change')]

sub$O_Ch_type;sub$J_Ch_type;sub$CW_Ch_type;sub$KK_Ch_type 
sub[,c('pl_plotid','O_Ch_type','J_Ch_type','KK_Ch_type')]#'CW_Ch_type','email',

sub[,c('pl_plotid','O_deg_driver','J_deg_driver')]
sub[,c('O_def_type','J_def_type')]  

sub[,c('pl_plotid','O_yrChange','J_yrChange')]
sub$J_notes;sub$CW_notes;sub$KK_notes
sub$Confidence;sub$CW_confidence;sub$KK_confidence 


###########################################################
###########################################################
####### evergreen #########################################
###########################################################
###########################################################
sub<- data2[data2$coded=='evergreen degradation',]
sub$O_LC; sub$J_LC
sub[,c('email','O_Change','J_Change','KK_Change')]
sub[,c('email','O_Ch_type','J_Ch_type','KK_Ch_type')] 
sub[,c('email','O_deg_driver','J_deg_driver')]
sub[,c('email','O_yrChange','J_yrChange')]
sub[,c('email','J_notes','CW_notes','KK_notes')]
sub$Confidence;sub$CW_confidence;sub$KK_confidence 

sub<- data2[data2$coded=='evergreen loss',]
sub$O_LC; sub$J_LC
sub[,c('email','O_Change','J_Change','KK_Change')]
sub[,c('email','O_Ch_type','J_Ch_type','KK_Ch_type')] 
sub[,c('email','O_deg_driver','J_deg_driver')]
sub[,c('email','O_yrChange','J_yrChange')]
sub[,c('email','J_notes','CW_notes','KK_notes')]
sub$Confidence;sub$CW_confidence;sub$KK_confidence 


sub<- data2[data2$coded=='evergreen, stable',]
sub$O_LC; sub$J_LC
sub[,c('email','O_Change','J_Change','KK_Change')]
sub[,c('email','O_Ch_type','J_Ch_type','KK_Ch_type')] 
sub[,c('email','O_deg_driver','J_deg_driver')]
sub[,c('email','O_yrChange','J_yrChange')]
sub[,c('email','J_notes','CW_notes','KK_notes')]
sub$Confidence;sub$CW_confidence;sub$KK_confidence 



