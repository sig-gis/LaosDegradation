library(tidyverse)
library(tidyr)

setwd('C:\\Users\\karis\\Documents\\SilvaCarbon\\laos_degradation\\Maps\\Comparison_2021\\LaosDegradation\\CEOdata')

#######################################################
#######################################################
################## Load map data ######################
#######################################################
#######################################################
data <- read.csv('ProcessedMergedData06172021.csv')

###########################################################
####### remove unlabeled plots #############################################
###########################################################
colnames(data)
data<-data[data$FINAL != 'FixMe', ] 

###########################################################
####### Map Evaluation -- CODED #############################################
###########################################################
data[,c("FINAL", 'codedLab')]
table(data$codedLab, data$FINAL)

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



