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

  # aanpassen shape file from shp to gpkg van Laura
  s1 <- st_read('GIS/KRWwaterdelen_AGV_concept_Februari2020.shp') %>% st_set_crs(28992) %>% st_transform(28992)
  st_write(s1,'../data/WBPKRW20200525.gpkg')
  
  EAG <- st_read('../GIS/EAGs_20210709.shp') %>% st_transform(28992)
  st_write(EAG,'data/EAG_20210709.gpkg', layer_options= c("OVERWRITE=YES"))
  
  #aanmaken water per eag
  s1 <- st_read('development/GIS/Watervlakken.shp') %>% st_transform(28992)
  EAG <- sf::st_read("development/GIS/EAG20210709.shp", quiet = T) %>% sf::st_transform(proj4.rd)
  #intersect water per eag and union all water within an eag
  clp1 <- st_intersection(s1, EAG) %>% group_by(GAFIDENT) %>% summarise()
  st_write(clp1,'./data/WaterPerEAG_20210709.gpkg',layer_options= c("OVERWRITE=YES"), append = F)

# importeren en converteren fychem data
  wq <- read.csv("../wq/ImportWQ.csv", header = TRUE, na.strings = " ", sep=";", dec =".", stringsAsFactors = F)
  saveRDS(wq, "data/ImportWQ", compress = "xz")

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
  
  # import en aanpassen hybi met gecorrigeerde data
  # gegevens hydrobiologie
  hybi1 <- fread("development/HB_tot2000.csv", stringsAsFactors = F)
  hybi2 <- fread("development/HB_2000tm2021.csv", stringsAsFactors = F)
  hybi <- rbind(hybi1,hybi2, fill =T)
  colnames(hybi) <- gsub(" ", ".", colnames(hybi), fixed=TRUE)
  saveRDS(hybi,'data/alles_reliable.rds')  
  
  #chemie
  wq <- fread("development/fysische_chemie_tm2022.csv")
  saveRDS(wq,'data/ImportWQ.rds')
  
  

  