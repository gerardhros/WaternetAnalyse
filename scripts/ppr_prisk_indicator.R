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

# wet contour, fraction contour within 5m around water bodies
p3 <- st_read('C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/project/1729 waternet/wet_contour.gpkg')

# load informatie maatregel op de kaart
p4 <- st_read('D:/1589.N.19 KKROSG/11 maatregelen op kaart/200602 maatregel op kaart agv.gpkg',layer = '200602 maatregel op kaart agv')
scols <- c('sector','bodem','buisdrains','verdichtng','helling','nabijSloot','X.SlootRand',
           'GT','ow_Bodem','ow_LandMgm','ow_NutBnut','ow_ZuivRou','ow_WatrBhr')
p4 <- p4[,scols]

# load bodemverdichting
p5 <- st_read('C:/Users/gerard.ros.nmi/Agrocares/NMI_Data - Documenten/project/1729 waternet/bodemverdichting_agv.gpkg') %>% st_transform(28992)
p5 <- p5 %>% st_buffer(0)

# join all parcel data
s3.fin <- p2
s3.fin <- st_join(s3.fin,p1[,c('A_P_AL','A_P_WA','A_P_CC','A_AL_OX','A_FE_OX','A_P_OX')],largest = TRUE)
s3.fin <- st_join(s3.fin,p3[,c('omtrek_nat')],largest = TRUE)
s3.fin <- st_join(s3.fin,p4,largest = TRUE)
s3.fin <- st_join(s3.fin,p5[,c('LEGENDA')],largest = TRUE)

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
