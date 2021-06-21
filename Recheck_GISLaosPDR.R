library(tidyverse)
library(tidyr)

setwd('C:\\Users\\karis\\Documents\\SilvaCarbon\\laos_degradation\\Maps\\Comparison_2021\\LaosDegradation\\CEOdata')

### Old spreadsheets ###
#GIS <- read.csv('CeoSmplGISv3.csv')
#increase1<-read.csv('ceo-Increased-sample-density-for-CODED-map-and-buffer-sample-data-2021-06-15.csv')
#increase2<-read.csv('ceo-Increased-sample-density-for-CODED-map-and-buffer--v2-sample-data-2021-06-15.csv')
#concensus <- read.csv('recheck_JFpreKK.csv')
#increase2 <- read.csv('IncreasedDensity2021-06-16.csv')

#GIS_buffer <- read.csv('CompiledGisUpdated.csv')
GIS <- read.csv('CompiledForGisUpdated_nobuffers.csv')

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
################## add sample weights ######################
#######################################################
#######################################################
head(smplCount)
colnames(smplCount)[3]<-'smplStratCount'
colnames(GIS)
sort(unique(smplCount$Value))
sort(unique(GIS$strata_out_v2))
unique(GIS$smplStrata)

GIS <- merge(GIS, smplCount[,c(2,3)], 
              by.x = 'strata_out_v2', by.y = 'Value', 
              all.x = T, all.y = F)
head(GIS)
tail(GIS)

