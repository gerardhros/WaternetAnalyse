rm(list = ls())

library(data.table); library(dplyr); library(sf); library(ggplot2); 
library(fasterize);  library(raster);  library(sp); library(tmap);
library(OBIC)
library(rgeos)
library(automap)

# source functions
source('scripts/ppr_funs.R')
source('diepte/funs_rasterize.R')
source('diepte/funs_datappr.R')

source("C:/code/metal_cokriging/scripts/functions.R") #cross-validation of kriging

fdnm <-  "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI"
# parent directory of one drive
iloc_onedrive <- paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")), "/", fdnm, "/")
# project folder of AGV
iloc_project <- paste0(iloc_onedrive, "NMI_Data - Documents/project/WaternetAnalyse/")
# raw data folder of alkalvig project (Job's)
iloc_afk <- paste0(iloc_onedrive, "NMISite - 1781.N.19 Oorzaken en oplossingen afkalving sloten veenweide/ml studie/data/raw/")

## Load files ---------------

# locaties van alle metingen (water, biologie, en slootbodem)
locaties <- fread('data/Location.csv')

# locaties van EAG oppervlaktes (this includes "type")
eag_wl <- fread('data/EAG_Opp_kenmerken_20200218.csv')
eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]

# gegevens hydrobiologie (only data after 2006) (In fewparameter, "WATDTE_m" and "SLIBDTE_m" are included)
hybi_ori <- readRDS('data/alles_reliable.rds')
hybi_ori <- ppr_hybi(db = hybi_ori, syear = 2006, wtype = eag_wl, mlocs = locaties)


## pre-process files ------------
# overwrite GAF of locaties
locaties[, GAFIDENT := as.integer(substr(EAGIDENT, 1, 4))]

# delete a record with obscure EAG
eag_wl <- eag_wl[GAFIDENT != "3???-EAG-1", ]
# add EAGIDENT and correct GAFIDENT (Because GAFIDENT is actually EAGIDENT in the original file)
eag_wl[, EAGIDENT := GAFIDENT]
eag_wl[, GAFIDENT := as.integer(substr(EAGIDENT, 1, 4))]

# delete record of meetwaarde == -999
hybi <- hybi_ori[meetwaarde != -999, ]

# remove completely same records
hybi <- remove_duplicate(hybi[, .(locatiecode, datum, jaar, fewsparameter, meetwaarde, eenheid)])

# add a parameter TOTDTE_m (= WATDTE_m + SLIBDTE_m)
hybi <- calc_totdepth(hybi)

# merge location info to hybi
hybi <- merge(hybi, locaties[, .(CODE, XCOORD, YCOORD, MORFOLOGIE)], by.x = "locatiecode", by.y = "CODE", all.x = T)

# Exclude hybi data with unrealistic WATDTE_M and WATBTE_m from line features
hybi <- remove_unrealistic(hybi, max_d = 5, max_b = 50, print = FALSE)




# # load shape of sloot
# water <- st_read("data/WaterPerEAG20191205.gpkg") %>% st_transform(28992)
# # merge EAG 
# water <- merge(water, eag_wl, by = "GAFIDENT")


## Agregation per GAF and per EAG- ----
# Make a raster template for the extent of EAG
rs_template <- create_raster_template("data/EAG20191205.gpkg")

# Rasterize GAF polygons 
if(file.exists(paste0(iloc_project, "diepte/gaf_r.RData"))){
  load(paste0(iloc_project, "diepte/gaf_r.RData"))
} else {
  gar_r <- rasterize_gaf("data/GAF.gpkg", rs_template) # (THIS TAKES CA. 15 MIN!!!)
}

# # Rasterize EAG polygons
if(file.exists(paste0(iloc_project, "diepte/eag_r.RData"))){
  load(paste0(iloc_project, "diepte/eag_r.RData")) # this loads eag_r & tb_eag 
} else {
  tb_eag <- data.table(eag_id = 1:length(unique(eag$EAGIDENT)),EAG = unique(eag$EAGIDENT))
  eag_r <- rasterize_eag(tb_eag, "data/EAG20191205.gpkg", rs_template)# (THIS TAKES CA. 15 MIN!!!)
}

# Rasterize waterways
if(file.exists(paste0(iloc_project, "diepte/water_r.RData"))){
  load(paste0(iloc_project, "diepte/water_r.RData"))
} else {
  # (THIS COST 214 minutes!!)
  water_r <- rasteize_water("data/WaterPerEAG20191205.gpkg", rs_template)
}

## make a raster of EAG-median values
# water depth
eag_med_watdte <- raster_eag_med(hybi, year2u = 2015:2019, para2u = "WATDTE_m", locaties, eag_wl)
tm_shape(eag_med_watdte) + tm_raster(title = "WATDTE_M")  + tm_layout(legend.position = c("right","bottom"))
# slib depth
eag_med_slibdte <- raster_eag_med(hybi, year2u = 2015:2019, para2u = "SLIBDTE_m", locaties, eag_wl)
tm_shape(eag_med_slibdte) + tm_raster(title = "SLIBDTE_M")  + tm_layout(legend.position = c("right","bottom"))
# total depth (water + slib)
eag_med_totdte <- raster_eag_med(hybi, year2u = 2015:2019, para2u = "TOTDTE_m", locaties, eag_wl)
tm_shape(eag_med_totdte) + tm_raster(title = "TOTDTE_M")  + tm_layout(legend.position = c("right","bottom"))

# ## Draw map of measurement points
# # convert hybi water depth data to sf object
# watdte_sf <- st_as_sf(hybi[fewsparameter == "WATDTE_m" & jaar == 2019,], coords = c("XCOORD", "YCOORD"), crs = 28992)
# eag <- st_read("data/EAG20191205.gpkg") %>% st_transform(28992)
# # draw maps
# tm_shape(eag) + tm_polygons() +
#   tm_shape(watdte_sf) + tm_dots(col = "meetwaarde", size = 0.3) 


## Location statistics -----

# Make location-based summary of data 
dt_loc <- location_summary(hybi)

dt_loc <- merge(dt_loc, locaties[, .(CODE, XCOORD, YCOORD, EAGIDENT, GAFIDENT, MORFOLOGIE, WATERTYPE)], by.x = "locatiecode", by.y = "CODE", all.x= T)

# convert location-based hybi data to sf
loc_sf <- st_as_sf(dt_loc, coords = c("XCOORD", "YCOORD"), crs = 28992)

#write_sf(loc_sf, paste0(iloc_project, "diepte/loc_sf.gpkg"))


## Extract waterpeil info around measurement point ------

# waterpeil file name
waterpeil_fn <- paste0(iloc_afk, "peilgebieden.gpkg")

# get values
loc_sf <- get_waterpeil(loc_sf, waterpeil_fn)



## Extract soil type ("zand"/"klei"/"veen") of measurement points ------

# rasterstack file name
fac_rs_fn <- paste0(iloc_onedrive, "NMI_Data - Documents/rasterstack/products/fac_rs.RData")

# get values
loc_sf <- get_soiltype(loc_sf, fac_rs_fn)


## Extract seepage data of measurement points ------

# seepage point data name
kwel_fn <- paste0(iloc_afk, "kwel.gpkg")

# Rasterize seepage point data (& filling gaps based on neighbours)
if(file.exists(paste0(iloc_project, "diepte/kwel_rf.RData"))){
  # load previously saved raster object
  load(paste0(iloc_project, "diepte/kwel_rf.RData"))
} else {
  kwel_rf <- rasterize_kwel(kwel_fn)
}

# Get seepage value for measurement points
loc_sf <- get_seepage(loc_sf, kwel_rf)




## Extract theoretical water depth around measurement points ------
# file name of theoretical water depth
watth_fn <- paste0(iloc_project, "diepte/HydrovakkenLegger2015/HydrovakkenLegger2015.shp")

# Get theoretical water depth
# (Note: data is available only for majoy waterways)
loc_sf <- get_theowater(watth_fn, loc_sf)
  






