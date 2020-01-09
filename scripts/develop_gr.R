# aanmaken data files vanuit development

  require(sf); require(data.table)

  # aanmaken eag file
  
  s1 <- st_read('development/Ecologische_analysegebieden_20180612.shp')
  s1 <- s1 %>% st_transform(28992)
  st_write(s1,'data/EAG20180612.gpkg')

  # aanmaken koppeltabel
  d1 <- readxl::read_xlsx('development/180727_koppeltabel.xlsx')
  d1 <- as.data.table(d1)
  saveRDS(d1,'data/180727_koppeltabel.rds')

  # aanmaken perceel covariabelen perperceel
  onedrive <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Agrocares/NMI_Data - Documenten/")
  fields <- st_read(paste0(onedrive, "project/NMI_Bodemschat/products/fields/fields_final.gpkg"), stringsAsFactors = FALSE)
  waterschappen <- st_read(paste0(onedrive, "topo/waterschappen/raw/2019_waterschappen_grenzen.gpkg"))
  agv <- waterschappen[11,]
  fields.agv <- unlist(st_intersects(agv, fields))
  fields.agv <- fields[fields.agv, ]
  st_write(fields.agv, "data/191128_percelen_agv.gpkg")    
  
  # column names matrix Laura simplified
  rn <- readxl::read_xlsx('development/191129 colnames.xlsx') %>% as.data.table()
  saveRDS(rn,'data/191129 colnames.rds')  
  
  # aanpassen shape file from shp to gpkg
  s1 <- st_read('data/EAG20170611.shp') %>% st_transform(28992)
  st_write(s1,'data/EAG20170611.gpkg')
  s1 <- st_read('data/WBPKRW20170611.shp') %>% st_transform(28992)
  st_write(s1,'data/WBPKRW20170611.gpkg')
  s1 <- st_read('development/EAG_20190717.shp') %>% st_transform(28992)
  st_write(s1,'data/EAG_20190717.gpkg')
  s1 <- st_read('development/EAG_20190717_simplified.shp') %>% st_transform(28992)
  st_write(s1,'data/EAG_20190717_simplified.gpkg')
  s1 <- st_read('development/GAF.shp') %>% st_transform(28992)
  st_write(s1,'data/GAF.gpkg')
  s1 <- st_read('development/WBPKRW20170611_simplified.shp') %>% st_transform(28992)
  st_write(s1,'data/WBPKRW20170611_simplified.gpkg')
  
  
  # inladen toxiciteitsdata
  simoni <- fread("toxiciteit/overzicht_toxiciteit_2018_2017_2016_2013_2012.csv",stringsAsFactors = F)
  saveRDS(simoni,'data/simoni.rds')
  