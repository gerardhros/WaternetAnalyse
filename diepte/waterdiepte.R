rm(list = ls())

library(data.table); library(dplyr); library(sf); library(ggplot2); 
library(fasterize);  library(raster);  library(sp); library(tmap)
#library(rgeos)
library(automap)

# source functions
source('scripts/ppr_funs.R')
source('diepte/funs_rasterize.R')

source("C:/code/metal_cokriging/scripts/functions.R") #cross-validation of kriging

## Load files ---------------

# locaties van alle metingen (water, biologie, en slootbodem)
locaties <- fread('data/Location.csv')

# locaties van EAG oppervlaktes (this includes "type")
eag_wl <- fread('data/EAG_Opp_kenmerken_20200218.csv')
eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]

# gegevens hydrobiologie (only data after 2006) (In fewparameter, "WATDTE_m" and "SLIBDTE_m" are included)
hybi <- readRDS('data/alles_reliable.rds')
hybi <- ppr_hybi(db = hybi, syear = 2006, wtype = eag_wl, mlocs = locaties)


## pre-process files 

# overwrite GAF of locaties
locaties[, GAFIDENT := as.integer(substr(EAGIDENT, 1, 4))]

# delete a record with obscure EAG
eag_wl <- eag_wl[GAFIDENT != "3???-EAG-1", ]
# add EAGIDENT and correct GAFIDENT (Because GAFIDENT is actually EAGIDENT in the original file)
eag_wl[, EAGIDENT := GAFIDENT]
eag_wl[, GAFIDENT := as.integer(substr(EAGIDENT, 1, 4))]

# delete record of meetwaarde == -999
hybi <- hybi[meetwaarde != -999, ]

# THe following function is saved in funs_merge.R
#' Check if there are completely identical rows. If so, remove them (after giving a warning)
remove_duplicate <- function(dt){
  dt_uni <- unique(dt)
  if(nrow(dt) != nrow (dt_uni)){
    print(paste0("There are ", nrow(dt) - nrow (dt_uni), " completely identical rows."))
    print("These rows were removed.")
  }
  return(dt_uni)
}

hybi <- remove_duplicate(hybi[, .(locatiecode, datum, jaar, fewsparameter, meetwaarde, eenheid)])

# make a parameter TOTDTE_M (= WATDTE_m + SLIBDTE_m)
hybi_dc <- dcast(hybi[fewsparameter %in% c("WATDTE_m", "SLIBDTE_m"),], 
                 locatiecode + datum ~ fewsparameter,
                 value.var = "meetwaarde",
                 fun.aggregate = median)
hybi_dc[!is.na(WATDTE_m) & !is.na(SLIBDTE_m), TOTDTE_m := WATDTE_m + SLIBDTE_m]
hybi_totd <- hybi_dc[!is.na(TOTDTE_m), .(locatiecode, datum, TOTDTE_m)]
hybi_totd[, jaar := substr(datum, 1, 4)]
hybi_totd[, eenheid := "m"]
hybi_totd[, fewsparameter := "TOTDTE_m"]
hybi_totd[, meetwaarde := TOTDTE_m][, TOTDTE_m := NULL]
# add total depth to hybi
hybi <- rbind(hybi, hybi_totd)



# # load shape of sloot
# water <- st_read("data/WaterPerEAG20191205.gpkg") %>% st_transform(28992)
# # merge EAG 
# water <- merge(water, eag_wl, by = "GAFIDENT")


## Agregation per GAF and per EAG- ----

# rasterize GAF polygons 
#gar_r <- raster(gaf_r, filename = "diepte/gaf_r.tif") # check if this syntax works
load("diepte/gaf_r.RData")
#gar_r <- rasterize_gaf() # (THIS TAKES CA. 15 MIN!!!)

# # rasterize EAG polygons
load("diepte/eag_r.RData") # this loads eag_r & tb_eag 
# res <- rasterize_eag(tb_eag)# (THIS TAKES CA. 15 MIN!!!)
# eag_r <- res$eag_r
# tb_eag <- res$tb_eag


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



# Kriging 

i <- 1

#for (i in 1:nrow(gaf_sum))){
  gaf_id_i <- gaf_sum$GAFIDENT[i]
  if(gaf_sum[GAFIDENT == gaf_id_i, N_location] < 5){
    print(paste0("there are only ", gaf_sum[GAFIDENT == gaf_id_i, N_location], " measurement locations in this GAF."))
  } else {
    print(gaf_sum[GAFIDENT == gaf_id_i,])
    # convert point measurements of the EAG to Spatial Object 
    dt_sp <- loc_sum[GAFIDENT == gaf_id_i, ]
    sp::coordinates(dt_sp) <- ~ XCOORD + YCOORD
   # dt_sp <- dt2[GAFIDENT == gaf_id_i, ]
    #sp::coordinates(dt_sp) <- ~ locatie.x + locatie.y
    dt_sp@proj4string <- CRS(projargs = "+init=epsg:28992")
    
    # # convert raster (for the extent of the EAG polygon) to Spatial object
    # # (Probebly there are much easier way to do this)
    # setDT(eag)
    # eag_i <- st_as_sf(eag[GAFIDENT == dt_m[i, EAGIDENT],]) # select the EAG
    # coord_i <- sf::st_coordinates(eag_i) # get coodinates of node
    # box_i <- c( min(temp[,1]),  max(temp[,1]), min(temp[,2]), max(temp[,2])) #xmin, ymin, xmax, ymax
    # eag_out <- crop(eag_med, box_i) # crop raster
    # eag_out <- projectRaster(eag_out, crs = CRS("+init=epsg:28992"))
    # eag_out_sp <- as(eag_out, "SpatialGridDataFrame") # convert to Spatial object
    
    gaf_out <- projectRaster(overlay2(gaf_r, gaf_r,  gaf_id_i), crs = CRS("+init=epsg:28992"))
    gaf_out_sp <-as(gaf_out, "SpatialGridDataFrame") 
    
    ## ordinary kriging
    res_krige <- autoKrige(med ~ 1,
                           input_data = dt_sp,
                           new_data = gaf_out_sp,
                           model = "Sph",
                           verbose = FALSE)
    # cross validation of kriging and show results
    cv_result <- crossvalidation_kriging("med", dt_sp)
    print("Result cross validation")
    print(summary(cv_result$res.krige.cv))
    cv_result$gp # show graph of observed vs predicted
    
    # store prediction
    res.pred <- raster(res_krige$krige_output[1])
    plot(res_krige)
    
    # show map
    setDT(gaf)
    gaf_e <- st_as_sf(gaf[GAFIDENT == gaf_id_i,])
    gaf <- st_as_sf(gaf)
    #pred <- crop(res.pred, gaf)
    dt_e <- loc_sum[GAFIDENT == gaf_id_i, ]
    ggplot() + geom_sf(data = gaf_e) +
      geom_point(data= dt_e, aes(x = XCOORD, y = YCOORD, col = med)) +
      labs(col = "WATDTE_m") +
      scale_color_gradient(low="blue", high="red")+
      ggtitle(paste0("GAF ", gaf_id_i))
  }
  
#}






# convert hybi to sf object
hybi_w <- merge(hybi[fewsparameter == "WATDTE_m" & jaar == 2019, ],
                     locaties[,.(CODE, XCOORD, YCOORD)], 
                     by.x = "locatiecode", by.y = "CODE", all.x = T)
watdte_sf <- st_as_sf(hybi_w, coords = c("XCOORD", "YCOORD"), crs = 28992)
eag <- st_read("data/EAG20191205.gpkg") %>% st_transform(28992)
# draw maps
tm_shape(eag) + tm_polygons() +
  tm_shape(watdte_sf) + tm_dots(col = "meetwaarde", size = 0.3) 
  



# # check nr of measurement locations per EAG
# n_red_eag <- dt2[fewsparameter == "WATDTE_m", .N, by = .(EAGIDENT, locatiecode)]
# n_loc_eac <- n_red_eag[, .N, by = EAGIDENT]

# draw histogram (nr of locations per EAG)
ggplot(dt_m) + geom_histogram(aes(y = N)) + facet_wrap(.~type) +
  xlab("Number of EAG") + ylab("Number of locations with depth records in hybi")

# # example map of 1 eag with maximum number of data points
# setDT(eag)
# eag2u <- n_loc_eac[N == max(N), EAGIDENT] # EAG with the most number of records
# #eag2u <- n_eag[type == "sloot" & N>100, EAGIDENT][1] #EAG of sloot with many records
# eag_e <- st_as_sf(eag[GAFIDENT == eag2u,])
# dt_e <- dt[EAGIDENT == eag2u & fewsparameter == "WATDTE_m",]
# ggplot() + geom_sf(data = eag_e) +
#   geom_point(data= dt_e, aes(x = locatie.x, y = locatie.y, col = meetwaarde)) +
#   labs(col = "WATDTE_m") +
#   ggtitle(eag2u)





# temp # draw map
ggplot(water) + geom_sf() 
