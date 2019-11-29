# update database aangemaakt door Laura

# clear environment
rm(list=ls())

# require packages
require(sf);require(data.table);require(dplyr)

# load earlier prepared db
d1 <- readRDS('matrix.rds') %>% as.data.table()

# load shape file with EAG and match table
s1 <- st_read('data/EAG20180612.gpkg',quiet=T) %>% st_transform(28992)
m1 <- readRDS('data/180727_koppeltabel.rds')

# load parcel dependent covariables
s2 <- st_read('data/191128_percelen_agv.gpkg',quiet = T)

# load raster with interpolated soil data from Job
load('data/meanSoilPerceel50.Rdata')
load('data/xy_soil.Rdata')

  # make measurements db to spatial sf
  sxy <- st_as_sf(nd, coords = c('X', 'Y'), crs = st_crs(soil)) 

  # join EAG code to brp
  p1 <- st_join(soil,s1 %>% select('GAFIDENT','GAFNAAM','GAFOPPVL'),left = T) %>% filter(!duplicated(perc_id))
  
  # join predicted values with measured values
  p1 <- st_join(p1,sxy)

  # convert to data.table
  p1 <- as.data.table(p1)
  
  # replace NA inmeasurements by mean
  cols <- colnames(p1)[!grepl('perc_|be_|geome|^GAF',colnames(p1))]
  p1[,(cols) := lapply(.SD,function(x) mean(x,na.rm=T)),.SDcols = cols,by='perc_id']
  
  # remove duplicates
  p1 <- p1[!duplicated(perc_id),]
  
  # replace prediction with measured when available
  cols = gsub('be_','',colnames(p1)[grepl('^be_',colnames(p1))])
  for(i in cols){p1[!is.na(get(i)),(paste0('be_',i)) := get(i)][,c(i) := NULL]}
  
  # delete undesired columns
  cols <- c('sample','plos1','plos2')
  p1[,c(cols) := NULL]
  
  # calculate eag means and stdev
  cols <- colnames(p1)[grepl('be_',colnames(p1))]
  eag1.mean <- p1[,lapply(.SD,mean,na.rm=T),.SDcols = cols,by=.(GAFIDENT)]
  eag1.sd <- p1[,lapply(.SD,sd,na.rm=T),.SDcols = cols,by=.(GAFIDENT)]
  
  # adapt column names
  setnames(eag1.mean,paste0(colnames(eag1.mean),'_m'))
  setnames(eag1.sd,paste0(colnames(eag1.sd),'_sd'))
  
# add covariables van percelen
  
  # join EAG code to brp (be aware that some parcels are present in two GAFs)
  p2 <- st_join(s2,s1 %>% dplyr::select('GAFIDENT','GAFNAAM','GAFOPPVL'),left = T)
  
  # set as data.table
  p2 <- as.data.table(p2)
  
  # replace values that are already in p1
  cols <- colnames(p2)[grepl('^A_|^PC_',colnames(p2))]
  p2[,c(cols) := NULL]
  
  # calculate eag modal and variance for categorial variables
  cols = colnames(p2)[grepl('^bd50.h|^bd50.s|^bd50.ver|^bd50.hel|bofek|^lcv.es',colnames(p2))]
  
  # make these columns character
  p2[,(cols) := lapply(.SD,as.character),.SDcols = cols]
  p2[,(cols) := lapply(.SD,function(x) fifelse(is.na(x),'onbekend',x)),.SDcols = cols]
  
  # function to derive the modal property
  fmodal <- function(x){names(sort(table(x),decreasing = T))[1]}
 
  # estimate most common property of categorial variables
  eag2.mean <- p2[,lapply(.SD,fmodal),.SDcols = cols,by=.(GAFIDENT)]
  eag2.sd <- p2[,lapply(.SD,uniqueN),.SDcols = cols,by=.(GAFIDENT)]
  
  # calculate eag mean and variance for numerical variables
  cols = colnames(p2)[!grepl('^bd50.h|^bd50.s|^bd50.ver|^bd50.hel|bofek|^lcv.es|^GAF|geom|^id',colnames(p2))]
  eag3.mean <- p2[,lapply(.SD,mean,na.rm=T),.SDcols = cols,by=.(GAFIDENT)]
  eag3.sd <- p2[,lapply(.SD,sd,na.rm=T),.SDcols = cols,by=.(GAFIDENT)]
  
  # adapt column names
  setnames(eag2.mean,paste0(colnames(eag2.mean),'_m'))
  setnames(eag2.sd,paste0(colnames(eag2.sd),'_sd'))
  setnames(eag3.mean,paste0(colnames(eag3.mean),'_m'))
  setnames(eag3.sd,paste0(colnames(eag3.sd),'_sd'))
  
# merge matrix Laura met bodemdata

  # convert matrix Laura to datatable
  setDT(d1)
  
  # reset names
  rn <- readRDS('data/191129 colnames.rds')
  setnames(d1,rn$oud,rn$nieuw)
  
  # remove empty columns
  cols <- colnames(d1)[unlist(d1[,lapply(.SD,function(x) sum(is.na(x))==nrow(d1)),.SDcols=colnames(d1)])]
  d1[,c(cols):=NULL]
  
  # merge with soil data EA and parcel data (mean and variance)
  d2 <- merge(d1,eag1.mean,by.x='eag',by.y='GAFIDENT_m',all.x = T)
  d2 <- merge(d2,eag1.sd,by.x='eag',by.y='GAFIDENT_sd',all.x = T)
  d2 <- merge(d2,eag2.mean,by.x='eag',by.y='GAFIDENT_m',all.x = T)
  d2 <- merge(d2,eag2.sd,by.x='eag',by.y='GAFIDENT_sd',all.x = T)
  d2 <- merge(d2,eag3.mean,by.x='eag',by.y='GAFIDENT_m',all.x = T)
  d2 <- merge(d2,eag3.sd,by.x='eag',by.y='GAFIDENT_sd',all.x = T)
  
  # save updated file
  saveRDS(d2,file='matrix_updated.rds')
  
  # save as csv file
  fwrite(d2,file='data/matrix20191129.csv',sep=';',dec=',')
  