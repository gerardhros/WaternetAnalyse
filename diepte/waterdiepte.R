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


## define folder names
# parent directory of one drive
fdnm <-  "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI"
iloc_onedrive <- paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")), "/", fdnm, "/")
# project folder of AGV
iloc_project <- paste0(iloc_onedrive, "NMI_Data - Documents/project/WaternetAnalyse/")
# raw data folder of alkalvig project (Job's)
iloc_afk <- paste0(iloc_onedrive, "NMISite - 1781.N.19 Oorzaken en oplossingen afkalving sloten veenweide/ml studie/data/raw/")

eag_fn <- "data/EAG20191205.gpkg" # this one does not cover all areas
#eag_fn <- "data/EAG20190717_simplified.gpkg"

## Load files ---------------

# locaties van alle metingen (water, biologie, en slootbodem)
locaties <- fread('data/Location.csv')

# locaties van EAG oppervlaktes (this includes "type")
eag_wl <- fread('data/EAG_Opp_kenmerken_20200218.csv')
eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]

# gegevens hydrobiologie (only data after 2006) (In fewparameter, "WATDTE_m" and "SLIBDTE_m" are included)
hybi_ori <- readRDS('data/alles_reliable.rds')
hybi_ori <- ppr_hybi(db = hybi_ori, syear = 2006, wtype = eag_wl, mlocs = locaties)

# Make a raster template for the extent of EAG
rs_template <- create_raster_template(eag_fn, res = 100)


## Pre-process files ------------
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


## Location statistics -----

# Make location-based summary of data 
dt_loc <- location_summary(hybi, year2u = 2006:2019)
# Merge location info
dt_loc <- merge(dt_loc, locaties[, .(CODE, XCOORD, YCOORD, EAGIDENT, GAFIDENT, MORFOLOGIE, WATERTYPE)], by.x = "locatiecode", by.y = "CODE", all.x= T)
# convert location-based hybi data to sf
loc_sf <- st_as_sf(dt_loc, coords = c("XCOORD", "YCOORD"), crs = 28992)

#write_sf(loc_sf, paste0(iloc_project, "diepte/loc_sf.gpkg"))



## Extract waterpeil info around measurement point ------
# waterpeil file name
waterpeil_fn <- paste0(iloc_afk, "peilgebieden.gpkg")

# rasterize waterpeil
waterpeil_rs <- rasterize_waterpeil(waterpeil_fn, rs_template)

# get values
loc_sf <- get_value_from_raster(loc_sf, waterpeil_rs)
#loc_sf <- get_waterpeil(loc_sf, waterpeil_fn)


## Extract soil type ("1: zand"/"2: klei"/"3: veen") of measurement points ------

# rasterstack file name
fac_rs_fn <- paste0(iloc_onedrive, "NMI_Data - Documents/rasterstack/products/fac_rs.RData")

# Make a raster with the same extent as other rasters of this project
soilcode_rs <- rasterize_soilcode(fac_rs_fn, rs_template)

# get values
loc_sf <- get_value_from_raster(loc_sf, soilcode_rs)
loc_sf$soiltypen <- as.factor(loc_sf$soiltypen)
#loc_sf <- get_soiltype(loc_sf, fac_rs_fn)


## Extract seepage data of measurement points ------

# seepage point data name
kwel_fn <- paste0(iloc_afk, "kwel.gpkg")

# Rasterize seepage point data (& filling gaps based on neighbours)
if(file.exists(paste0(iloc_project, "diepte/kwel_rs.RData"))){
  # load previously saved raster object
  load(paste0(iloc_project, "diepte/kwel_rs.RData"))
} else {
  kwel_rs <- rasterize_kwel(kwel_fn, rs_template)
}

# Get seepage value for measurement points
loc_sf <- get_value_from_raster(loc_sf, kwel_rs)


## Extract theoretical water depth around measurement points ------
# file name of theoretical water depth
watth_fn <- paste0(iloc_project, "diepte/HydrovakkenLegger2015/HydrovakkenLegger2015.shp")

# Get theoretical water depth
# (Note: data is available only for majoy waterways)
loc_sf <- get_theowater(watth_fn, loc_sf)
  

## Extract water width around measurement points ------
ww_fn <- paste0(iloc_project, "diepte/200512_oeverpunten_corrected.gpkg")

# Get valuve of water width and water surface AHN from water shore data
# (THIS TAKES CA. 8 MIN)
loc_sf <- get_width_ahn(loc_sf, ww_fn, eag_fn)
#loc_sf <- get_width_ahn(loc_sf, ww_fn, eag_fn, update = TRUE) # re-calculate distance and save loc_v

# When meausred width is available, use that. Otherwise use water width which was calculated on GIS
setDT(loc_sf)
loc_sf[, breedte := med_wb]
loc_sf[is.na(med_wb), breedte := pnt_breedte]
loc_sf <- st_as_sf(loc_sf)


#st_write(loc_sf, paste0(iloc_project, "diepte/loc_sf.gpkg"), append = FALSE)





# # draw maps of measurement points & EAG boundary
# tm_shape(loc_sf) + tm_dots(col = "med_wd", size = 0.2) +
#   tm_shape(eag) + tm_polygons(alpha = 0)

