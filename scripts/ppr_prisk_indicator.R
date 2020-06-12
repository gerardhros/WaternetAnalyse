# clear environment
rm(list=ls())

# load packages
require(sf);require(data.table); require(magrittr)

# load parcel data AGV
p1 <- st_read('C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/bodem/blgg/shapes/1599 waterschap AGV.gpkg')

# load risk indicator for surface runoff (Hattum et al., 2011)
p2 <- st_read('C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/project/1729 waternet/risk.gpkg')

# load water in AGV
w1 <- st_read('D:/1729.N.19 AGV data analyse/WaternetAnalyse/data/WaterPerEAG20191205.gpkg') %>% st_transform(28992)

# load in EAGs met landbouwgebied cluster voor maatregelen
w2 <- st_read('C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/project/1729 waternet/eag20191205_landbouwgebied.gpkg') %>% st_transform(28992)

# wet contour, fraction contour within 5m around water bodies
p3 <- st_read('C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/project/1729 waternet/wet_contour.gpkg')

# load informatie maatregel op de kaart
p4 <- st_read('D:/1589.N.19 KKROSG/11 maatregelen op kaart/200602 maatregel op kaart agv.gpkg',layer = '200602 maatregel op kaart agv')
scols <- c('sector','bodem','buisdrains','verdichtng','helling','nabijSloot','X.SlootRand',
           'GT','ow_Bodem','ow_LandMgm','ow_NutBnut','ow_ZuivRou','ow_WatrBhr','ow_top5')
p4 <- p4[,scols]

# load bodemverdichting
p5 <- st_read('C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/project/1729 waternet/bodemverdichting_agv.gpkg') %>% st_transform(28992) %>% st_buffer(0)

# join all parcel data
s3.fin <- p2
s3.fin <- st_join(s3.fin,p1[,c('A_P_AL','A_P_WA','A_P_CC','A_AL_OX','A_FE_OX','A_P_OX')],largest = TRUE)
s3.fin <- st_join(s3.fin,p3[,c('omtrek_nat')],largest = TRUE)
s3.fin <- st_join(s3.fin,p4,largest = TRUE)
s3.fin <- st_join(s3.fin,p5[,c('LEGENDA')],largest = TRUE)
s3.fin <- st_join(s3.fin,st_buffer(w2[,c('LANDBOUWGEB')],0),largest = TRUE)

# add parcel area
s3.fin$opp_parcel <- st_area(s3.fin)
s3.fin$opp_parcel <- as.numeric(s3.fin$opp_parcel) /10000

# clean up dt
s3.dt <- as.data.table(s3.fin)
setnames(s3.dt,'LEGENDA','B_OV_WENR')

# convert bodemverdichting to numeric
s3.dt[B_OV_WENR %in% c('Water','Glastuinbouw, niet beoordeeld','Bebouwing en infrastructuur'), priskcomp := 0]
s3.dt[B_OV_WENR =='Zeer beperkt',priskcomp := 1]
s3.dt[B_OV_WENR =='Beperkt door veenlagen',priskcomp := 1.5]
s3.dt[B_OV_WENR =='Beperkt',priskcomp := 2]
s3.dt[B_OV_WENR =='Matig',priskcomp := 3]
s3.dt[B_OV_WENR =='Groot',priskcomp := 4]
s3.dt[B_OV_WENR =='Zeer groot',priskcomp := 5]

# maximum P retention
s3.dt[,pretmax := A_AL_OX + A_FE_OX]

# P-saturation
s3.dt[,A_P_VG := A_P_OX / (0.5 * (A_AL_OX + A_FE_OX))]

# make prisk indicator
s3.dt[,pri := (2 * frank(dif) + 2 * frank(omtrek_nat) + frank(A_P_CC) + frank(A_P_VG) +  frank(-pretmax) +
                 frank(priskcomp)) / (.N * 8) ,by='GAFIDENT']

# save db with prisk indicator to project drive
saveRDS(s3.dt,file='C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/project/1729 waternet/prisk_indicator.rds')
s3.dt.sf <- st_as_sf(s3.dt)
st_write(s3.dt.sf,dsn='C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/project/1729 waternet/prisk_indicator.gpkg')


# do update for grouping
s3.dt[,c(paste0('m',1:9)) := tstrsplit(ow_top5,",")]

s4.dt <- s3.dt[,.(LANDBOUWGEB,m1,m2,m3,m4,m5,m6,m6,m8,m9)]
s4.dt <- melt(s4.dt,id.vars = 'LANDBOUWGEB',value.name = 'maatregel')
s4.dt <- s4.dt[!is.na(maatregel)]
s4.dt[,N := .N,maatregel]
s4.dt <- unique(s4.dt)
s4.dt[,variable := NULL]
s4.dt[,N := sum(N),by=.(LANDBOUWGEB,maatregel)]
s4.dt <- unique(s4.dt)
s4.dt[,Nrank := frank(-N),by='LANDBOUWGEB']
s4.dt <- s4.dt[Nrank<=8]
setorder(s4.dt,LANDBOUWGEB,Nrank)
s4.dt[,id := 1:.N,by='LANDBOUWGEB']
s4.dt <- dcast(s4.dt,LANDBOUWGEB~id,value.var = 'maatregel')
setnames(s4.dt,c('LANDBOUWGEB',paste0('m',1:8)))
s4.dt[,bestmeasgebied := paste(m1,m2,m3,m4,m5,m6,m7,m8,sep="-")]
s4.dt[,c(paste0('m',1:8)):=NULL]

s3.dt[,pri2 := (2 * frank(dif) + 2 * frank(omtrek_nat) + frank(A_P_CC) + frank(A_P_VG) +  frank(-pretmax) +
                 frank(priskcomp)) / (.N * 8) ,by='LANDBOUWGEB']
s5.dt <- s3.dt[,.(LANDBOUWGEB,GAFIDENT,pri2)]
s5.dt[, perc_perc_high := round(sum(pri2>0.5) * 100 / sum(pri2>=0),1),by='GAFIDENT']
s5.dt <- unique(s5.dt[,.(GAFIDENT,perc_perc_high)])

s3.dt <- merge(s3.dt,s4.dt,by ='LANDBOUWGEB')
s3.eag <- merge(w2,s4.dt,by='LANDBOUWGEB')
s3.eag <- merge(s3.eag,s5.dt,by='GAFIDENT',all.x = TRUE)
s3.eag <- as.data.table(s3.eag)
s3.eag[is.na(perc_perc_high),perc_perc_high := 0]
s3.eag <- st_as_sf(s3.eag)

st_write(s3.eag,dsn='C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/project/1729 waternet/deelgebied_maatregelen.gpkg')
