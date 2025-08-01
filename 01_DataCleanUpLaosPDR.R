library(tidyverse)
library(tidyr)

setwd('C:\\Users\\karis\\Documents\\SilvaCarbon\\laos_degradation\\Maps\\Comparison_2021\\LaosDegradation\\CEOdata')

### Old spreadsheets ###
#GIS <- read.csv('CeoSmplGISv3.csv')
#GIS <- read.csv('CompiledForGisUpdated_nobuffers.csv')
#GIS <- read.csv('CompiledForGisUpdated_v5.csv')
#increase1<-read.csv('ceo-Increased-sample-density-for-CODED-map-and-buffer-sample-data-2021-06-15.csv')
#increase2<-read.csv('ceo-Increased-sample-density-for-CODED-map-and-buffer--v2-sample-data-2021-06-15.csv')
#concensus <- read.csv('recheck_JFpreKK.csv')
#concensus <- read.csv('recheck JFv2.csv')
#increase2 <- read.csv('IncreasedDensity2021-06-16.csv')
#smplCount <- read.csv('smplCount.csv')
##recheck <- read.csv('Raw\\Compiled_06222021.csv')

GIS <- read.csv('CompiledForGisUpdated_v6.csv')
dataCEO <- read.csv('CEO_CompiledValidationPoints.csv')
concensus <- read.csv('recheck JFv2_KTRevisions.csv')
increase <- read.csv('IncreasedDensity2021-06-18.csv')
smplCount <- read.csv('smplCountv2.csv')
postStratCodedsmplCount <- read.csv('codedPost_strat_smplCountv2.csv')
recheck <- read.csv('Raw\\ceo-recheck-2021-07-13_noutf8v2-Interpreted.csv')

#######################################################
#######################################################
################## Load map data ######################
#######################################################
#######################################################
head(GIS) 

colnames(GIS)
colnames(GIS)[11]<- 'plot_id'
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

# CODED post stratification
sort(unique(GIS$post_strat_coded))
GIS$post_strat_codedLab <- NULL
GIS <- mutate(GIS, post_strat_codedLab = case_when(
  (post_strat_coded == 100) ~ "stable forest evergreen",
  (post_strat_coded == 111) ~ "evergreen fcdm",
  (post_strat_coded == 121) ~ "evergreen frel",
  (post_strat_coded == 131) ~ "evergreen coded loss",
  (post_strat_coded == 133) ~ "evergreen coded degradation",
  (post_strat_coded == 141) ~ "evergreen multi agreement",
  (post_strat_coded == 143) ~ "evergreen multi agreement - coded degradation",
  (post_strat_coded == 144) ~ "evergreen multi agreement - coded loss",
  (post_strat_coded == 200) ~ "stable mixed",
  (post_strat_coded == 211) ~ "mixed fcdm",
  (post_strat_coded == 221) ~ "mixed frel",
  (post_strat_coded == 231) ~ "mixed coded loss",
  (post_strat_coded == 233) ~ "mixed coded degradation",
  (post_strat_coded == 241) ~ "mixed multi agreement",
  (post_strat_coded == 243) ~ "mixed multi agreement - coded degradation",
  (post_strat_coded == 244) ~ "mixed multi agreement - coded loss",
  (post_strat_coded == 251) ~ "mixed buffer",
  (post_strat_coded == 253) ~ "mixed buffer - coded degradation",
  (post_strat_coded == 254) ~ "mixed buffer - coded loss",
  (post_strat_coded == 300) ~ "stable forest dipterocarp",
  (post_strat_coded == 311) ~ "dipterocarp fcdm",
  (post_strat_coded == 321) ~ "dipterocarp frel",
  (post_strat_coded == 331) ~ "dipterocarp coded loss",
  (post_strat_coded == 333) ~ "dipterocarp coded degradation",
  (post_strat_coded == 341) ~ "dipterocarp multi agreement",
  (post_strat_coded == 344) ~ "dipterocarp multi agreement - coded loss",
  (post_strat_coded == 400) ~ "Non forest",
  (post_strat_coded == 500) ~ "Non forest",
  TRUE ~ "FixMe"
))
unique(GIS$post_strat_codedLab)
GIS$post_strat_coded[GIS$post_strat_codedLab=='FixMe']
colnames(GIS)
head(GIS)

#######################################################
#######################################################
################## add sample weights ######################
#######################################################
#######################################################
head(smplCount)
colnames(smplCount)[3]<-'smplStratCount'
colnames(GIS)
sort(unique(smplCount$Value))
sort(unique(GIS$strata_out_v2))

GIS <- merge(GIS, smplCount[,c(2,3)], 
              by.x = 'strata_out_v2', by.y = 'Value', 
              all.x = T, all.y = F)
head(GIS)
tail(GIS)
unique(GIS$smplStratCount)

#######################################################
#######################################################
################## add coded post strat sample weights ######################
#######################################################
#######################################################
head(postStratCodedsmplCount)
colnames(postStratCodedsmplCount)[2]<-'PSCstrat'
colnames(postStratCodedsmplCount)[3]<-'PSCstratCount'
head(postStratCodedsmplCount)
colnames(GIS)
sort(unique(postStratCodedsmplCount$PSCstrat))
sort(unique(GIS$post_strat_coded))

GIS <- merge(GIS, postStratCodedsmplCount[,c(2,3)], 
             by.x = 'post_strat_coded', by.y = 'PSCstrat', 
             all.x = T, all.y = F)
head(GIS)
tail(GIS)

#######################################################
#######################################################
################## Load CEO data ######################
#######################################################
#######################################################
head(dataCEO)
colnames(dataCEO)

colnames(dataCEO)[1]<- 'plot_id'
dataCEO[dataCEO$plot_id == 140742371,]
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

dataCEOsub$Source <- 'initialSmple'
head(dataCEOsub[,seq(1:9)])
head(dataCEOsub[,seq(10:19)])
head(dataCEOsub[,seq(21:29)])
colnames(dataCEOsub)
rm(dataCEO)

#######################################################
################## remove flagged plots ######################
#######################################################

table(dataCEOsub$flagged)
dataCEOsub<-dataCEOsub[dataCEOsub$flagged != TRUE,]

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
dataCEOsub[dataCEOsub$O_Dynamics=='FixMe', ]
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
dataCEOsub[dataCEOsub$plot_id == 140742371,]

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

colnames(concensus)
colnames(concensus)[1]<- 'LON'
colnames(concensus)[4]<- 'plot_id'
concensus[concensus$plot_id == 140742371,]

colnames(concensus)
concensus<-concensus[,c('plot_id',"Jeremy.final.comment", "Suggested.change", "Chittana", "Khamkong",
             "LC.Change.Dynamics","Year.of.degradation", "notes", "Year.of.Forest.Loss", "FINAL")]

sort(unique(concensus$FINAL))
concensus <- mutate(concensus, FINAL = case_when(
  (FINAL == 'LOSS') ~ "loss",
  (FINAL == 'DEGRADATION') ~ "degradation",
  #(FINAL == 'other') ~ 'other',
  #(FINAL == 'forest restoration') ~ 'restoration',
  (FINAL == 'No change') ~ 'stable',
  (FINAL == 'stable forest') ~ 'stable forest',
  (FINAL == 'stable non-forest') ~ 'stable non-forest',
  (FINAL == 'not forest') ~ 'stable non-forest',
  (FINAL == 'LOSS before 2015') ~ "stable non-forest",
  (FINAL == 'Not to include in analysis') ~ 'stable non-forest',
  TRUE ~ "FixMe"
))
unique(concensus$FINAL)

#######################################################
#######################################################
################## merge with CEO data ######################
#######################################################
#######################################################
dataTemp<- merge(dataCEOsub, concensus[,c('plot_id','FINAL')], by.x = 'plot_id', by.y = 'plot_id', no.dups = TRUE, all = T)
head(dataTemp[dataTemp$recheck == 1,])
colnames(dataTemp)
dataTemp[dataTemp$plot_id == 140742371,]
rm(dataCEOsub, concensus)

dataTemp$Source[dataTemp$recheck== '1']<-'consensus'
dataTemp[dataTemp$recheck== 1, c('FINAL')]
dataTemp[dataTemp$recheck== 0, c('FINAL')]

unique(dataTemp$FINAL[dataTemp$recheck == 1])
dataTemp$FINAL[(dataTemp$recheck == 0 & dataTemp$O_Dynamics == dataTemp$J_Dynamics & dataTemp$KK_Dynamics == "")] <-
  dataTemp$O_Dynamics[(dataTemp$O_Dynamics == dataTemp$J_Dynamics & dataTemp$KK_Dynamics == "")] 
dataTemp$FINAL[(dataTemp$recheck == 0 & dataTemp$O_Dynamics == dataTemp$J_Dynamics & dataTemp$KK_Dynamics == dataTemp$J_Dynamics)] <-
  dataTemp$O_Dynamics[(dataTemp$O_Dynamics == dataTemp$J_Dynamics & dataTemp$KK_Dynamics == dataTemp$J_Dynamics)]
dataTemp$FINAL[(dataTemp$recheck == 0 & dataTemp$O_Dynamics == dataTemp$KK_Dynamics & dataTemp$J_Dynamics == "")]<- 
  dataTemp$O_Dynamics[(dataTemp$O_Dynamics == dataTemp$KK_Dynamics & dataTemp$J_Dynamics == "")]
dataTemp$FINAL[(dataTemp$recheck == 0 & is.na(dataTemp$FINAL) == T)]<-
  dataTemp$O_Dynamics[(dataTemp$recheck == 0 & is.na(dataTemp$FINAL) == T)] 
unique(dataTemp$FINAL)
#######################################################
#######################################################
################## merge CEO data and increased density ######################
#######################################################
#######################################################
colnames(increase)
head(increase[,seq(1:5)])

# "collection_time", "analysis_duration", "imagery_title", "imagery_attributions",
#"sample_geom", "pl_strata_out", "pl_long", "pl_plotid",

increase<-increase[, c("�..plot_id", "lon", "lat", "flagged", "email", 
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
head(increase[increase$O_Ch_type == "", ])
increase$O_Ch_type[increase$O_Ch_type == ""]<-increase$O_LC[increase$O_Ch_type == ""]
unique(increase$O_Ch_type)
head(increase[increase$O_Ch_type == "other", ])

increase$FINAL<-NULL
unique(dataTemp$FINAL)
unique(increase$O_Ch_type)
table(dataTemp$FINAL)
increase <- mutate(increase, FINAL = case_when(
  (O_Ch_type == 'forest loss') ~ "loss",
  (O_Ch_type == 'forest degradation') ~ "degradation",
  (O_Ch_type == 'other' & O_LC == 'forest') ~ 'other',
  (O_Ch_type == 'forest restoration') ~ 'restoration',
  (O_Change == 'no' & O_LC == 'non-forest') ~ 'stable non-forest',
  (O_Change == 'no' & O_LC == 'forest') ~ 'stable forest',
  (O_LC == 'non-forest') ~ 'stable non-forest',
  TRUE ~ "FixMe"
))
unique(increase$FINAL)
head(increase[increase$FINAL == "other", ])
increase[increase$FINAL=='FixMe', c('O_LC','O_Ch_type','O_Change')]

colnames(dataTemp)
dataTemp<-dataTemp[, c("plot_id", "lon", "lat", "flagged",'email',
#"pl_coded", "pl_codedlatear", "pl_fcdm", "pl_fcdmlatear", "pl_frel", 
#"pl_strata_out", "pl_strata_out_name", "pl_union_deg", 
"O_LC","O_Change","O_Ch_type","O_deg_driver","O_def_type",'O_yrChange',
"Confidence", 'FINAL', 'Source')]

increase$Source<-'phase2Smpl'
colnames(dataTemp)
colnames(increase)
CEOfull<- rbind(dataTemp, increase)
CEOfull[CEOfull$plot_id == 140742371,]
dataTemp[dataTemp$plot_id == 140742371,]
increase[increase$plot_id == 140742371,]

rm(dataTemp,increase)
head(CEOfull)
tail(CEOfull)
CEOfull$Post2015<-0
CEOfull$O_Change[CEOfull$O_yrChange>2000]
CEOfull$Post2015[CEOfull$O_yrChange>2014 ]<-1


#######################################################
#######################################################
################## merge rechecked data and CEO data ######################
#######################################################
#######################################################

#recheckOLD <- read.csv('Raw\\Compiled_06222021.csv')
#recheck <- read.csv('Raw\\ceo-recheck-2021-07-13_noutf8v2-Interpreted.csv')

recheck$Project<- 'QAQCincrDensity'
recheck$pl_final<- NA
recheck<-recheck[,c(21,5, 6, 12, 13, 22,
                    14, 15, 16, 17, 18, 19, 20, 3, 4)]
head(recheck)
colnames(recheck)[1]<-"Project"
colnames(recheck)[7]<-"LandCover_2019"
colnames(recheck)[8]<-"Change"
colnames(recheck)[9]<-"Change_type"
colnames(recheck)[10]<-"Driver_of_degradation"
colnames(recheck)[11]<-"Deforestation_type"
colnames(recheck)[13]<-"Change_Yr"
recheck$recheckFinal <- 'update'
head(recheck)

recheck <- mutate(recheck, recheckFinal = case_when(
  (LandCover_2019 == 'non-forest') ~ 'stable non-forest',
  (LandCover_2019 != 'non-forest' & Change_type == 'forest loss') ~ "loss",
  (LandCover_2019 != 'non-forest' & Change_type == 'forest degradation') ~ "degradation",
  (LandCover_2019 != 'non-forest' & Change == 'no')  ~ 'stable forest',
  TRUE ~ 'FixMe'
))

unique(recheck$recheckFinal)
recheck[recheck$recheckFinal == 'FixMe',]
recheck$flagged[recheck$recheckFinal == 'FixMe']<-'yes'
recheck$recheck <- 1
recheck<-recheck[recheck$flagged != 'yes',]

recheck[(recheck$recheckFinal == 'loss' | recheck$recheckFinal == 'degradation'),]
recheck$Change_Yr[(recheck$recheckFinal != 'degradation')]<- NA
colnames(CEOfull)
colnames(recheck)

colnames(recheck)[5]<-"pl_plot_id"
colnames(recheck)
sort(recheck$pl_plot_id)
sort(CEOfull$plot_id)


revisedData<- merge(CEOfull, recheck[,c(
  "pl_plot_id", "lon", "lat", "Project", "LandCover_2019", "Change",       
  "Change_type", "Driver_of_degradation","Deforestation_type",           
  "Change_Yr", 'recheckFinal', 'recheck')], by.x = 'plot_id', by.y = 'pl_plot_id', no.dups = TRUE, all.x = T)
head(revisedData)

revisedData$lon.x[!is.na(revisedData$recheck)]==revisedData$lon.y[!is.na(revisedData$recheck)]
revisedData$lat.x[!is.na(revisedData$recheck)]==revisedData$lat.y[!is.na(revisedData$recheck)]
revisedData$Agreement[!is.na(revisedData$recheck)]<-revisedData$FINAL[!is.na(revisedData$recheck)] == revisedData$recheckFinal[!is.na(revisedData$recheck)]

revisedData$FinalPrevious[!is.na(revisedData$recheck)] <- revisedData$FINAL[!is.na(revisedData$recheck)]
revisedData$Source[!is.na(revisedData$recheck)] <- revisedData$Project[!is.na(revisedData$recheck)]
revisedData$O_yrChange[!is.na(revisedData$recheck)] <- revisedData$Change_Yr[!is.na(revisedData$recheck)]
revisedData$FINAL[!is.na(revisedData$recheck)] <- revisedData$recheckFinal[!is.na(revisedData$recheck)]

colnames(CEOfull)
colnames(revisedData)
revisedData<-revisedData[,c("plot_id", "lon.x", "lat.x", "flagged", 'Agreement', "FinalPrevious", "email", "O_LC", "O_Change",
                             "O_Ch_type", "O_deg_driver", "O_def_type", "O_yrChange", "Confidence", "FINAL", "Source",
                              "Post2015")]
head(revisedData)
colnames(revisedData)[2]<-"lon"
colnames(revisedData)[3]<-"lat"

#######################################################
#######################################################
################## merge GIS and CEO data ######################
#######################################################
#######################################################

sort(colnames(GIS))
sort(colnames(revisedData))
fulldata<- merge(revisedData, GIS[,c(
  "plot_id", 
  "codedLab", "strata_coded_year", 
  "fcdm", 
  "frel_2015", "frel_2019", 
  #"strata_out", 
  "smplStrata", 'smplLab', 
  "forestType", 'smplStratCount')], by.x = 'plot_id', by.y = 'plot_id')#, all.x = T)

#
fulldata$plot_id[is.na(fulldata$codedLab)]
fulldata$lon[is.na(fulldata$codedLab)]
sort(unique(GIS$plot_id))

head(fulldata)
tail(fulldata)
fulldata$forestType

colnames(fulldata)
rm(CEOfull, revisedData)
rm(GIS)

colnames(fulldata)
head(fulldata)
write.csv(fulldata, file = 'ProcessedMergedData07152021.csv', row.names = F)

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

