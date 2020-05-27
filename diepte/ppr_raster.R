# This script prepares raster data of waterways

library(raster)

source('diepte/funs_rasterize.R')

## define folder names
# parent directory of one drive
fdnm <-  "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI"
iloc_onedrive <- paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")), "/", fdnm, "/")
# project folder of AGV
iloc_project <- paste0(iloc_onedrive, "NMI_Data - Documents/project/WaternetAnalyse/")

# file name of EAG polygons
#eag_fn <- "data/EAG20191205.gpkg" # this one does not cover all areas
eag_fn <- "data/EAG20190717_simplified.gpkg"

# file name of waterways polygon
water_fn <-  "data/WaterPerEAG20191205.gpkg"


# Make a raster template for the extent of EAG
rs_template <- create_raster_template(eag_fn, res = 100)

# Rasterize EAG polygons
if(file.exists(paste0(iloc_project, "diepte/eag_r.RData"))){
  load(paste0(iloc_project, "diepte/eag_r.RData")) # this loads eag_r & tb_eag 
} else {
  eag <- st_read(eag_fn) %>% st_transform(28992)
  tb_eag <- data.table(eag_id = 1:length(unique(eag$EAGIDENT)),EAG = unique(eag$EAGIDENT))
  eag_r <- rasterize_eag(tb_eag, "data/EAG20191205.gpkg", rs_template)# (THIS TAKES CA. 15 MIN!!!)
}

# Rasterize waterways
if(file.exists(paste0(iloc_project, "diepte/water_r.RData"))){
  load(paste0(iloc_project, "diepte/water_r.RData"))
} else {
  # (THIS COST 214 minutes!!)
  water_r <- rasteize_water(water_fn, rs_template)
}

# crop EAG raster only for waterways
water_eag_r <- mask(eag_r, water_r)

#save(water_eag_r, file = paste0(iloc_project, "diepte/water_eag_r.RData"))
#writeRaster(water_eag_r, filename = paste0(iloc_project, "diepte/water_eag_r.tif"))
#write.csv(tb_eag, file = paste0(iloc_project, "diepte/tb_eag.csv"), row.names = F)


## Rasterize water depth for all waterways
# Load data
ww_fn <- paste0(iloc_project, "diepte/200512_oeverpunten_corrected.gpkg")
ww <- st_read(ww_fn)%>% st_transform(28992)

# when pnt_breedte = 0, remove the record
setDT(ww)
ww <- ww[pnt_breedte != 0, ]
ww <- st_as_sf(ww)
#split raster of resolution 100m into 25m
water_eag_r25 <- disaggregate(water_eag_r, fact = 4)
# rasterize
pnt_bd_rs <- rasterize(ww, water_eag_r25, field = "pnt_breedte", fun = mean)
#writeRaster(pnt_bd_rs, filename = paste0(iloc_project, "diepte/temp_pnt_bd_rs.tif"))
# # check how many measurement points overlap with the rater
# temp_overlap <-extract(pnt_bd_rs, loc_sf) # -> 3432/ 7372 points overlap (within lijnvormig, 3148/5019 points overlap)


# # temp # rasterize waterways (from polygon) with smaller resolution
# rs_template25 <- create_raster_template(eag_fn, res = 25)
# rs_template100 <- create_raster_template(eag_fn, res = 100)
# water <- st_read(water_fn) %>% st_transform(28992)
# tb_eag <- data.table(eag_id = 1:length(unique(water$GAFIDENT)),EAG = unique(water$GAFIDENT))
# water <- left_join(water, tb_eag, by = c("GAFIDENT" = "EAG"))
# water_rs <- fasterize(water, rs_template25, field = "eag_id")
# water_rs100 <- fasterize(water, rs_template100, field = "eag_id")
# temp<-extract(water_rs, loc_sf)  # 3441 / 7372 points overlaied with waterways
# temp100<-extract(water_rs100, loc_sf) # 2616 / 7372 points  overlaied with waterways
# 
# load(paste0(iloc_project, "diepte/water_eag_r.RData"))
# temp_line<-extract(water_eag_r, loc_sf) # 7363 / 7372 points  overlaied with waterways
# # --> So, it is necessary to rasterize from lines!! 