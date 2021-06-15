library(tidyverse)
library(tidyr)

setwd('C:\\Users\\karis\\Documents\\SilvaCarbon\\laos_degradation\\Maps\\Comparison_2021\\CEOdata')

#######################################################
#######################################################
################## Load map data ######################
#######################################################
#######################################################
GIS <- read.csv('CeoSmplGISv3.csv')
head(GIS) 
colnames(GIS)

GIS$smplStrata <- GIS$strata_out
GIS <- mutate(GIS, smplStrata = case_when(
  (strata_out == 100) ~ 'stable forest evergreen',
  (strata_out == 111) ~ 'evergreen fcdm',
  (strata_out == 121) ~ 'evergreen frel',
  (strata_out == 131) ~ 'evergreen coded',
  (strata_out == 141) ~ 'evergreen multi agreement',
  (strata_out == 200) ~ 'stable mixed',
  (strata_out == 211) ~ 'mixed fcdm',
  (strata_out == 221) ~ 'mixed coded',
  (strata_out == 231) ~ 'mixed frel',
  (strata_out == 241) ~ 'mixed multi agreement',
  (strata_out == 251) ~ 'mixed buffer',
  (strata_out == 300) ~ 'stable forest dipterocarp',
  (strata_out == 311) ~ 'dipterocarp fcdm',
  (strata_out == 321) ~ 'dipterocarp frel',
  (strata_out == 331) ~ 'dipterocarp coded',
  (strata_out == 341) ~ 'dipterocarp multi agreement',
  (strata_out == 400) ~ 'Non forest',
  (strata_out == 500) ~ 'Non forest',
  TRUE ~ "FixMe"
))
unique(GIS$smplStrata)

GIS$fcdm <- GIS$strata_fcdm
GIS <- mutate(GIS, fcdm = case_when(
  (strata_fcdm == 100) ~ "stable forest evergreen",
  (strata_fcdm == 111) ~ "evergreen fcdm",
  (strata_fcdm == 200) ~ "stable mixed",
  (strata_fcdm == 211) ~ "mixed fcdm",
  (strata_fcdm == 251) ~ "mixed buffer",
  (strata_fcdm == 300) ~ "stable forest dipterocarp",
  (strata_fcdm == 311) ~ "dipterocarp fcdm",
  (strata_fcdm == 400) ~ "regenerating",
  (strata_fcdm == 500) ~ "Non forest",
  TRUE ~ "FixMe"
))
unique(GIS$fcdm)

GIS$coded <- GIS$strata_coded
GIS <- mutate(GIS, coded = case_when(
  (strata_coded == 100) ~ "evergreen, stable",
  (strata_coded == 103) ~ 'evergreen degradation',
  (strata_coded == 104) ~ 'evergreen forest loss',
  (strata_coded == 200) ~ "mixed, stable",
  (strata_coded == 203) ~ 'mixed degradation',
  (strata_coded == 204) ~ 'mixed forest loss',
  (strata_coded == 251) ~ 'mixed forest buffer',
  (strata_coded == 300) ~ 'dipterocarp, stable',
  (strata_coded == 303) ~ 'dipterocarp degradation',
  (strata_coded == 304) ~ 'dipterocarp forest loss',
  (strata_coded == 400) ~ 'Non forest',
  (strata_coded == 500) ~ "Non forest",
  TRUE ~ "FixMe"
))
unique(GIS$coded)

GIS$frel_2015 <- GIS$strata_frel_2015
GIS <- mutate(GIS, frel_2015 = case_when(
  (strata_frel_2015 == 100) ~ "stable forest evergreen 10-15",
  (strata_frel_2015 == 112) ~ 'evergreen FREL 10-15',
  (strata_frel_2015 == 122) ~ 'evergreen regen 10-15',
  (strata_frel_2015 == 200) ~ "stable mixed 10-15",
  (strata_frel_2015 == 212) ~ 'mixed FREL 10-15',
  (strata_frel_2015 == 222) ~ 'mixed regen 10-15',
  (strata_frel_2015 == 251) ~ 'mixed forest buffer',
  (strata_frel_2015 == 300) ~ 'stable forest dipterocarp 10-15',
  (strata_frel_2015 == 313) ~ 'dipterocarp FREL 10-15',
  (strata_frel_2015 == 400) ~ 'Non forest 10-15',
  (strata_frel_2015 == 416) ~ 'plantation FREL 10-15',
  (strata_frel_2015 == 422) ~ 'regen FREL 10-15',
  (strata_frel_2015 == 500) ~ "Non forest 10-15",
  TRUE ~ "FixMe"
))
unique(GIS$frel_2015)

GIS$frel_2019 <- GIS$strata_frel_2019
GIS <- mutate(GIS, frel_2019 = case_when(
  (strata_frel_2019 == 100) ~ "stable forest evergreen",
  (strata_frel_2019 == 200) ~ "stable mixed",
  (strata_frel_2019 == 212) ~ 'mixed FREL 10-15',
  (strata_frel_2019 == 251) ~ 'mixed forest buffer',
  (strata_frel_2019 == 300) ~ 'stable forest dipterocarp',
  (strata_frel_2019 == 313) ~ 'dipterocarp FREL 10-15',
  (strata_frel_2019 == 400) ~ 'Non forest',
  (strata_frel_2019 == 416) ~ 'plantation FREL 10-15',
  (strata_frel_2019 == 422) ~ 'regen FREL 10-15',
  (strata_frel_2019 == 500) ~ "Non forest",
  TRUE ~ "FixMe"
))
unique(GIS$frel_2019)

GIS$forestType <- GIS$svk_forests
GIS <- mutate(GIS, forestType = case_when(
  (forestType == 100) ~ "forest evergreen",
  (forestType == 200) ~ "mixed",
  (forestType == 300) ~ 'dipterocarp',
  (forestType == 400) ~ 'Non forest',
  (forestType == 500) ~ 'Non forest',
  TRUE ~ "FixMe"
))
unique(GIS$forestType)

#######################################################
#######################################################
################## Load CEO data ######################
#######################################################
#######################################################
dataCEO <- read.csv('CEO_CompiledValidationPoints.csv')
head(dataCEO)
colnames(dataCEO)
colnames(dataCEO)[1]<- 'plot_id'

#"Land.cover..2019", 
colnames(dataCEO)[24]<- 'O_LC'
#"Change.", 
colnames(dataCEO)[25]<- 'O_Change'
#"Change.type.", 
colnames(dataCEO)[26]<- 'O_Ch_type'
#"Driver.of.degradation.", 
colnames(dataCEO)[27]<- 'O_deg_driver'
#"Deforestation.type", 
colnames(dataCEO)[28]<- 'O_def_type'
#"Year.of.change", 
colnames(dataCEO)[30]<- 'O_yrChange'

colnames(dataCEO)[33]<- 'J_LC'
colnames(dataCEO)[34]<- 'J_Change'
colnames(dataCEO)[35]<- 'J_Ch_type'
colnames(dataCEO)[36]<- 'J_deg_driver'
colnames(dataCEO)[37]<- 'J_def_type'
colnames(dataCEO)[38]<- 'J_yrChange'
colnames(dataCEO)[39]<- 'J_notes'

colnames(dataCEO)[40]<- 'CW_Change'
colnames(dataCEO)[41]<- 'CW_Ch_type'
colnames(dataCEO)[42]<- 'CW_confidence'
colnames(dataCEO)[43]<- 'CW_notes'

colnames(dataCEO)[44]<- 'KK_Change'
unique(dataCEO$KK_Change)
dataCEO$KK_Change[dataCEO$KK_Change == 'yes\nno']<-'recheck'
dataCEO$KK_Change[dataCEO$KK_Change == 'no\nno']<-'no'
dataCEO$KK_Change[dataCEO$KK_Change == 'yes\nyes']<-'yes'
unique(dataCEO$KK_Change)

colnames(dataCEO)[45]<- 'KK_Ch_type'
dataCEO$KK_Ch_type[dataCEO$KK_Ch_type == 'forest loss\nnone'
                  | dataCEO$KK_Ch_type == "forest loss\nother"
                  | dataCEO$KK_Ch_type ==  'forest degradation\nnone' 
                  | dataCEO$KK_Ch_type ==  'forest loss\nforest degradation']<-'recheck'
dataCEO$KK_Ch_type[dataCEO$KK_Ch_type == "forest degradation\nforest degradation"]<-"forest degradation"
unique(dataCEO$KK_Ch_type)


colnames(dataCEO)[46]<- 'KK_confidence'
colnames(dataCEO)[47]<- 'KK_notes'
dataCEOsub<- dataCEO[,c("plot_id", "sample_id", "lon", "lat", "flagged",'email',
                        #"email", "collection_time", "analysis_duration", 
                        #"imagery_title", "imagery_attributions", "sample_geom", 
                        #"pl_plotid", "pl_sampleid", 
                        "pl_coded", "pl_codedlatear", "pl_fcdm", "pl_fcdmlatear", "pl_frel", 
                        #"pl_lat_info", "pl_lon_info", 
                        "pl_strata_out", "pl_strata_out_name", "pl_union_deg", 
                        
                        "O_LC",'J_LC',
                        "O_Change","J_Change", "CW_Change", "KK_Change",
                        "O_Ch_type","J_Ch_type",  "CW_Ch_type", "KK_Ch_type", 
                        "O_deg_driver","J_deg_driver",
                        "O_def_type","J_def_type",  
                        'O_yrChange',"J_yrChange", 
                        "J_notes","CW_notes",  "KK_notes",
                        "Confidence","CW_confidence", "KK_confidence", 
                        
                        "Pre2015")]
head(dataCEOsub[,c()])
colnames(dataCEOsub)

dataCEOsub <- mutate(dataCEOsub, O_Dynamics = case_when(
  (O_Ch_type == 'forest loss') ~ "loss",
  (O_Ch_type == 'forest degradation') ~ "degradation",
  (O_Ch_type == 'other') ~ 'other',
  (O_Ch_type == 'forest restoration') ~ 'restoration',
  (O_Change == 'no' & O_LC == 'non-forest') ~ 'stable non-forest',
  (O_Change == 'no' & O_LC == 'forest') ~ 'stable forest',
  TRUE ~ "FixMe"
))
unique(dataCEOsub$O_Dynamics)
dataCEOsub[dataCEOsub$O_Dynamics=='FixMe', c('O_LC','O_Ch_type','O_Change')]

dataCEOsub <- mutate(dataCEOsub, J_Dynamics = case_when(
  (J_Ch_type == 'forest loss') ~ "loss",
  (J_Ch_type == 'forest degradation') ~ "degradation",
  (J_Ch_type == 'degradation') ~ "degradation",
  (J_Ch_type == '???') ~ "recheck",
  (J_Ch_type == 'other') ~ 'other',
  (J_Ch_type == 'forest restoration') ~ 'restoration',
  (J_Change == 'no' & J_LC == 'non-forest') ~ 'stable non-forest',
  (J_Change == 'no' & J_LC == 'forest') ~ 'stable forest',
  TRUE ~ "NA"
))
unique(dataCEOsub$J_Dynamics)
dataCEOsub[dataCEOsub$J_Dynamics=='FixMe', c('J_LC','J_Ch_type','J_Change')]
dataCEOsub[dataCEOsub$J_Dynamics=='FixMe', c('J_Ch_type')]#,'J_Change')]

dataCEOsub <- mutate(dataCEOsub, KK_Dynamics = case_when(
  (KK_Ch_type == 'forest loss') ~ "loss",
  (KK_Ch_type == 'forest degradation') ~ "degradation",
  (KK_Ch_type == 'other') ~ 'other',
  (KK_Ch_type == 'forest restoration') ~ 'restoration',
  (KK_Change == 'no' & J_LC == 'non-forest') ~ 'stable non-forest',
  (KK_Change == 'no' & J_LC == 'forest') ~ 'stable forest',
  TRUE ~ KK_Ch_type
))
unique(dataCEOsub$KK_Dynamics)
dataCEOsub[dataCEOsub$O_Dynamics=='FixMe', c('O_LC','O_Ch_type','O_Change')]

dataCEOsub$recheck<-0
dataCEOsub$recheck[(
  (dataCEOsub$O_Dynamics != dataCEOsub$J_Dynamics & dataCEOsub$J_Dynamics != 'NA') |
    (dataCEOsub$O_Dynamics != dataCEOsub$KK_Dynamics & dataCEOsub$KK_Dynamics != "")|
    (dataCEOsub$KK_Dynamics != dataCEOsub$J_Dynamics & dataCEOsub$J_Dynamics != 'NA'& dataCEOsub$KK_Dynamics != "")
)]<-'1'

colnames(dataCEOsub)
recheck<-dataCEOsub[(
  (dataCEOsub$O_Dynamics != dataCEOsub$J_Dynamics & dataCEOsub$J_Dynamics != 'NA') |
    (dataCEOsub$O_Dynamics != dataCEOsub$KK_Dynamics & dataCEOsub$KK_Dynamics != "")|
    (dataCEOsub$KK_Dynamics != dataCEOsub$J_Dynamics & dataCEOsub$J_Dynamics != 'NA'& dataCEOsub$KK_Dynamics != "")
),c('email','O_Dynamics','J_Dynamics','KK_Dynamics')]

data2<- merge(dataCEOsub, GIS, by.x = 'plot_id', by.y = 'plot_id', no.dups = TRUE)
head(data2)
colnames(data2)

table(data2$flagged)
data2<-data2[data2$flagged != TRUE,]

colnames(data2)
###########################################################
###########################################################
####### CODED #############################################
###########################################################
###########################################################
colnames(data2)
recheck<-data2[(data2$recheck == 1),c('plot_id','pl_plotid','lon.x','lat.x',
                                      'forestType','email',
                                      'O_Dynamics',"O_Change","O_Ch_type", "O_deg_driver","O_def_type",'O_yrChange',
                                      'J_Dynamics',"J_Change","J_Ch_type", "J_deg_driver","J_def_type","J_yrChange","J_notes",
                                      'KK_Dynamics',"KK_Change","KK_Ch_type", "KK_notes")]
head(recheck)
write.csv(recheck,file = 'recheck.csv')
###########################################################
###########################################################
####### CODED #############################################
###########################################################
###########################################################

data2[,c("coded", "forestType")]

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



60697.58+
58444.50+
6650.59+
5147.20+
11605.00+
78380.00+
39648.75+
9000.00+
101000.00
