setwd('C:\\Users\\karis\\Documents\\SilvaCarbon\\laos_degradation\\Maps\\Comparison_2021\\CEOdata')


dataCEO <- read.csv('CEO_CompiledValidationPoints.csv')
GIS <- read.csv('CeoSmplGISv3.csv')
head(GIS) 
colnames(GIS)

keyInt <- c(100, 'stable forest evergreen',
111, 'evergreen fcdm',
121, 'evergreen frel',
131, 'evergreen coded',
141, 'evergreen multi agreement',
200, 'stable mixed',
211, 'mixed fcdm',
221, 'mixed coded',
231, 'mixed frel',
241, 'mixed multi agreement',
251, 'mixed buffer',
300, 'stable forest dipterocarp',
311, 'dipterocarp fcdm',
321, 'dipterocarp frel',
331, 'dipterocarp coded',
341, 'dipterocarp multi agreement',
400, 'Non forest',
500, 'Non forest')

GIS$smplStrata <- GIS$strata_out


GIS$coded <- GIS$strata_coded

GIS$fcdm <- GIS$strata_fcdm
GIS$frel_2015 <- GIS$strata_frel_2015
GIS$frel_2019 <- GIS$strata_frel_2019

GIS$forestType <- GIS$svk_forests




head(dataCEO)
colnames(dataCEO)

data<- merge(Key, SmplStrata, by.x = FID, by.y = CEOSMPLGIS, no.dups = TRUE)

head(dataCEO)
colnames(dataCEO)

table(dataCEO$flagged)
