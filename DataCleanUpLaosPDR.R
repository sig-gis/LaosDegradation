library(tidyverse)
library(tidyr)

setwd('C:\\Users\\karis\\Documents\\SilvaCarbon\\laos_degradation\\Maps\\Comparison_2021\\LaosDegradation\\CEOdata')

### Old spreadsheets ###
#GIS <- read.csv('CeoSmplGISv3.csv')
#increase1<-read.csv('ceo-Increased-sample-density-for-CODED-map-and-buffer-sample-data-2021-06-15.csv')
#increase2<-read.csv('ceo-Increased-sample-density-for-CODED-map-and-buffer--v2-sample-data-2021-06-15.csv')

#GISw_buffer <- read.csv('CompiledGisUpdated.csv')
GIS <- read.csv('CompiledForGisUpdated_nobuffers.csv')
dataCEO <- read.csv('CEO_CompiledValidationPoints.csv')
concensus <- read.csv('recheck_JFpreKK.csv')
increase <- read.csv('IncreasedDensity2021-06-16.csv')

#######################################################
#######################################################
################## Load map data ######################
#######################################################
#######################################################
head(GIS) 
colnames(GIS)

unique(GIS$strata_coded)
GIS$codedLab <- GIS$strata_coded
GIS <- mutate(GIS, codedLab = case_when(
  (strata_coded == 100) ~ 'evergreen stable',
  (strata_coded == 103) ~ 'evergreen degradation',
  (strata_coded == 104) ~ 'evergreen forest loss',
  (strata_coded == 200) ~ 'mixed stable',
  (strata_coded == 203) ~ 'mixed degradation',
  (strata_coded == 204) ~ 'mixed loss',
  (strata_coded == 251) ~ 'mixed buffer',
  (strata_coded == 300) ~ 'dipto stable',
  (strata_coded == 303) ~ 'dipto degradation',
  (strata_coded == 304) ~ 'dipto loss',
  TRUE ~ "FixMe"
))
unique(GIS$codedLab)

sort(unique(GIS$strata_out_v2))
GIS$smplStrata <- GIS$strata_out_v2
GIS$smplLab <- GIS$strata_out_v2
GIS <- mutate(GIS, smplLab = case_when(
  (strata_out_v2 == 100) ~ 'stable forest evergreen',
  (strata_out_v2 == 111) ~ 'evergreen fcdm',
  (strata_out_v2 == 121) ~ 'evergreen frel',
  (strata_out_v2 == 131) ~ 'evergreen coded loss',
  (strata_out_v2 == 133) ~ 'evergreen coded degradation',
  (strata_out_v2 == 141) ~ 'evergreen multi agreement',
  (strata_out_v2 == 200) ~ 'stable mixed',
  (strata_out_v2 == 211) ~ 'mixed fcdm',
  (strata_out_v2 == 221) ~ 'mixed frel',
  (strata_out_v2 == 231) ~ 'mixed coded loss',
  (strata_out_v2 == 233) ~ 'mixed coded degradation',
  (strata_out_v2 == 241) ~ 'mixed multi agreement',
  (strata_out_v2 == 251) ~ 'mixed buffer',
  (strata_out_v2 == 300) ~ 'stable forest dipterocarp',
  (strata_out_v2 == 311) ~ 'dipterocarp fcdm',
  (strata_out_v2 == 321) ~ 'dipterocarp frel',
  (strata_out_v2 == 331) ~ 'dipterocarp coded loss',
  (strata_out_v2 == 333) ~ 'dipterocarp coded degradation',
  (strata_out_v2 == 341) ~ 'dipterocarp multi agreement',
  (strata_out_v2 == 400) ~ 'Non forest',
  (strata_out_v2 == 500) ~ 'Non forest',
  TRUE ~ "FixMe"
))
unique(GIS$smplLab)

sort(unique(GIS$strata_out_v1))
GIS$smplStrataOLD <- GIS$strata_out_v1
GIS <- mutate(GIS, smplStrataOLD = case_when(
  (strata_out_v1 == 100) ~ 'stable forest evergreen',
  (strata_out_v1 == 111) ~ 'evergreen fcdm',
  (strata_out_v1 == 121) ~ 'evergreen frel',
  (strata_out_v1 == 131) ~ 'evergreen coded',
  (strata_out_v1 == 141) ~ 'evergreen multi agreement',
  (strata_out_v1 == 200) ~ 'stable mixed',
  (strata_out_v1 == 211) ~ 'mixed fcdm',
  (strata_out_v1 == 221) ~ 'mixed coded',
  (strata_out_v1 == 231) ~ 'mixed frel',
  (strata_out_v1 == 241) ~ 'mixed multi agreement',
  (strata_out_v1 == 251) ~ 'mixed buffer',
  (strata_out_v1 == 300) ~ 'stable forest dipterocarp',
  (strata_out_v1 == 311) ~ 'dipterocarp fcdm',
  (strata_out_v1 == 321) ~ 'dipterocarp frel',
  (strata_out_v1 == 331) ~ 'dipterocarp coded',
  (strata_out_v1 == 341) ~ 'dipterocarp multi agreement',
  (strata_out_v1 == 400) ~ 'Non forest',
  (strata_out_v1 == 500) ~ 'Non forest',
  TRUE ~ "FixMe"
))
unique(GIS$smplStrataOLD)

sort(unique(GIS$strata_fcdm))
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

sort(unique(GIS$strata_frel_2015))
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

sort(unique(GIS$strata_frel_2019))
GIS$frel_2019 <- GIS$strata_frel_2019
GIS <- mutate(GIS, frel_2019 = case_when(
  #GIS$strata_out[GIS$strata_frel_2019 == 0]
  (strata_frel_2019 == 0) ~ "mixed buffer",
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

sort(unique(GIS$svk_forests))
GIS$forestType <- GIS$svk_forests
GIS <- mutate(GIS, forestType = case_when(
  (forestType == 1) ~ "forest evergreen",
  (forestType == 2) ~ "mixed",
  (forestType == 3) ~ 'dipterocarp',
  (forestType == 400) ~ 'Non forest',
  (forestType == 500) ~ 'Non forest',
  TRUE ~ "FixMe"
))
unique(GIS$forestType)

sort(unique(GIS$strata_coded_year))

colnames(GIS)
head(GIS)

#######################################################
#######################################################
################## Load CEO data ######################
#######################################################
#######################################################
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

#######################################################
################## fix labels for KK merge ######################
#######################################################

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

#######################################################
################## subset to just select columns ######################
#######################################################

## Removed
#"email", "collection_time", "analysis_duration", 
#"imagery_title", "imagery_attributions", "sample_geom", 
#"pl_plotid", "pl_sampleid", 
#"pl_lat_info", "pl_lon_info", "sample_id", 

dataCEOsub<- dataCEO[,c("plot_id", "lon", "lat", "flagged",'email',
                        "pl_coded", "pl_codedlatear", "pl_fcdm", "pl_fcdmlatear", "pl_frel", 
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

head(dataCEOsub[,seq(1:9)])
head(dataCEOsub[,seq(10:19)])
head(dataCEOsub[,seq(21:29)])
colnames(dataCEOsub)
rm(dataCEO)
#######################################################
################## simplify labels ######################
#######################################################

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
dataCEOsub[dataCEOsub$J_Dynamics=='NA', c('J_LC','J_Ch_type','J_Change')]
dataCEOsub[dataCEOsub$J_Dynamics=='NA', c('J_Ch_type')]#,'J_Change')]

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
dataCEOsub[dataCEOsub$KK_Dynamics=='recheck', c('O_LC','O_Ch_type','O_Change')]

#######################################################
################## mark rows with inconsistent labels ######################
#######################################################

dataCEOsub$recheck<-0
dataCEOsub$recheck[(
  (dataCEOsub$O_Dynamics != dataCEOsub$J_Dynamics & dataCEOsub$J_Dynamics != 'NA') |
    (dataCEOsub$O_Dynamics != dataCEOsub$KK_Dynamics & dataCEOsub$KK_Dynamics != "")|
    (dataCEOsub$KK_Dynamics != dataCEOsub$J_Dynamics & dataCEOsub$J_Dynamics != 'NA'& dataCEOsub$KK_Dynamics != "")
)]<-'1'

colnames(dataCEOsub)
#recheck<-dataCEOsub[(dataCEOsub$recheck == 1),c('email','O_Dynamics','J_Dynamics','KK_Dynamics')]

#######################################################
################## remove flagged plots ######################
#######################################################

table(dataCEOsub$flagged)
dataCEOsub<-dataCEOsub[dataCEOsub$flagged != TRUE,]

###########################################################
###########################################################
####### consensus ##########################################
###########################################################
###########################################################
head(concensus[,seq(1:5)])
head(concensus[,seq(from = 6, to = 10)])
head(concensus[,seq(from = 11, to = 15)])
head(concensus[,seq(from = 16, to = 20)])
head(concensus[,seq(from = 21, to = 25)])
head(concensus[,seq(from = 26, to = 30)])
head(concensus[,seq(from = 31, to = 34)])

colnames(concensus)[1]<- 'LON'
colnames(concensus)[4]<- 'plot_id'

colnames(concensus)
concensus<-concensus[,c('plot_id',"Jeremy.final.comment", "Suggested.change", "Chittana", "Khamkong",
             "LC_Change_Dynamics","Year_degradation", "notes", "Year_Forest_Loss", "FINAL")]

sort(unique(concensus$FINAL))
concensus <- mutate(concensus, FINAL = case_when(
  (FINAL == 'LOSS') ~ "loss",
  (FINAL == 'DEGRADATION') ~ "degradation",
  #(FINAL == 'other') ~ 'other',
  #(FINAL == 'forest restoration') ~ 'restoration',
  (FINAL == 'No change') ~ 'stable',
  (FINAL == 'not forest') ~ 'stable non-forest',
  (FINAL == 'Not to include in analysis') ~ 'stable non-forest',
  TRUE ~ "NA"
))
unique(concensus$FINAL)

#######################################################
#######################################################
################## merge with CEO data ######################
#######################################################
#######################################################
dataTemp<- merge(dataCEOsub, concensus, by.x = 'plot_id', by.y = 'plot_id', no.dups = TRUE, all = T)
head(dataTemp[dataTemp$recheck == 1,])
colnames(dataTemp)
rm(dataCEOsub, concensus)

unique(dataTemp$FINAL[dataTemp$recheck == 1])
dataTemp$FINAL[(dataTemp$O_Dynamics == dataTemp$J_Dynamics & dataTemp$KK_Dynamics == "")] <-dataTemp$O_Dynamics[(dataTemp$O_Dynamics == dataTemp$J_Dynamics & dataTemp$KK_Dynamics == "")] 
dataTemp$FINAL[(dataTemp$O_Dynamics == dataTemp$J_Dynamics & dataTemp$KK_Dynamics == dataTemp$J_Dynamics)] <- dataTemp$O_Dynamics[(dataTemp$O_Dynamics == dataTemp$J_Dynamics & dataTemp$KK_Dynamics == dataTemp$J_Dynamics)]
dataTemp$FINAL[(dataTemp$O_Dynamics == dataTemp$KK_Dynamics & dataTemp$J_Dynamics == "")]<- dataTemp$O_Dynamics[(dataTemp$O_Dynamics == dataTemp$KK_Dynamics & dataTemp$J_Dynamics == "")]
dataTemp$FINAL[(dataTemp$recheck == 0 & is.na(dataTemp$FINAL) == T)]<-dataTemp$O_Dynamics[(dataTemp$recheck == 0 & is.na(dataTemp$FINAL) == T)] 

#######################################################
#######################################################
################## merge CEO data and increased density ######################
#######################################################
#######################################################
colnames(increase)
head(increase[,seq(1:5)])

# "collection_time", "analysis_duration", "imagery_title", "imagery_attributions",
#"sample_geom", "pl_strata_out", "pl_long", "pl_plotid",

increase<-increase[, c("ï..plot_id", "lon", "lat", "flagged", "email", 
                       #"pl_coded", "pl_codedlatear", "pl_fcdm", "pl_fcdmlatear", "pl_frel", 
                       #"pl_strata_out", "pl_strata_out_name", "pl_union_deg", 
                       "Land.cover..2019", "Change.", "Change.type.",
                       "Driver.of.degradation.", "Deforestation.type", "Year.of.change", "Confidence")]        
colnames(increase)[1]<- 'plot_id'
colnames(increase)[6]<- "O_LC"
colnames(increase)[7]<- "O_Change"
colnames(increase)[8]<- "O_Ch_type"
colnames(increase)[9]<- "O_deg_driver"
colnames(increase)[10]<- "O_def_type"
colnames(increase)[11]<- 'O_yrChange'

unique(increase$O_Ch_type)
unique(dataTemp$FINAL)
table(dataTemp$FINAL)
increase$FINAL<-NULL

increase <- mutate(increase, FINAL = case_when(
  (O_Ch_type == 'forest loss') ~ "loss",
  (O_Ch_type == 'forest degradation') ~ "degradation",
  (O_Ch_type == 'other') ~ 'other',
  (O_Ch_type == 'forest restoration') ~ 'restoration',
  (O_Change == 'no' & O_LC == 'non-forest') ~ 'stable non-forest',
  (O_Change == 'no' & O_LC == 'forest') ~ 'stable forest',
  TRUE ~ "FixMe"
))
unique(increase$FINAL)
increase[increase$FINAL=='FixMe', c('O_LC','O_Ch_type','O_Change')]


colnames(dataTemp)
dataTemp<-dataTemp[, c("plot_id", "lon", "lat", "flagged",'email',
#"pl_coded", "pl_codedlatear", "pl_fcdm", "pl_fcdmlatear", "pl_frel", 
#"pl_strata_out", "pl_strata_out_name", "pl_union_deg", 
"O_LC","O_Change","O_Ch_type","O_deg_driver","O_def_type",'O_yrChange',
"Confidence", 'FINAL')]

colnames(dataTemp)
colnames(increase)

CEOfull<- rbind(dataTemp, increase)
rm(dataTemp,increase)
head(CEOfull)
tail(CEOfull)
CEOfull$Post2015<-0
CEOfull$O_Change[CEOfull$O_yrChange>2000]
CEOfull$Post2015[CEOfull$O_yrChange>2014 ]<-1

#######################################################
#######################################################
################## merge GIS and CEO data ######################
#######################################################
#######################################################

sort(colnames(GIS))
sort(colnames(CEOfull))
fulldata<- merge(CEOfull, GIS[,c(
  "plot_id", 
  "codedLab", "strata_coded_year", 
  "fcdm", 
  "frel_2015", "frel_2019", 
  #"strata_out", 
  "smplStrata", 'smplLab', 
  "forestType")], by.x = 'plot_id', by.y = 'plot_id', no.dups = TRUE)
head(fulldata)
fulldata$forestType

colnames(fulldata)
rm(CEOfull)
rm(GIS)

colnames(fulldata)
write.csv(fulldata,file = 'ProcessedMergedData06172021.csv')

###########################################################
###########################################################
####### CODED #############################################
###########################################################
###########################################################
#colnames(data2)
#recheck<-data2[(data2$recheck == 1),c('plot_id','pl_plotid','lon.x','lat.x',
#                                      'forestType','email',
#                                      'O_Dynamics',"O_Change","O_Ch_type", "O_deg_driver","O_def_type",'O_yrChange',
#                                      'J_Dynamics',"J_Change","J_Ch_type", "J_deg_driver","J_def_type","J_yrChange","J_notes",
#                                      'KK_Dynamics',"KK_Change","KK_Ch_type", "KK_notes")]
#head(recheck)
#write.csv(recheck,file = 'recheck.csv')

