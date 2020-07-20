# This script prepares raster data of waterways

library(raster)

source('diepte/funs_rasterize.R')
source('diepte/fun_iloc.R')

## define folder names
iloc <- fun_iloc(fdnm = "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI")
iloc_onedrive <- iloc[1]
iloc_project <- iloc[2]
iloc_afk <- iloc[3]



# file name of EAG polygons
eag_fn <- "data/EAG20191205.gpkg"


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
  eag_r <- rasterize_eag(tb_eag, eag_fn, rs_template)# (THIS TAKES CA. 15 MIN!!!)
}

# Rasterize waterways (both from lines and polygons)
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



