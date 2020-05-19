rm(list = ls())

library(data.table); library(dplyr); library(sf); library(ggplot2); 
library(fasterize);  library(raster);  library(sp)
#library(rgeos)
library(automap)

# source functions
source('scripts/ppr_funs.R')



# locaties van alle metingen (water, biologie, en slootbodem)
locaties <- fread('data/Location.csv')

# locaties van EAG oppervlaktes (this includes "type")
eag_wl <- fread('data/EAG_Opp_kenmerken_20200218.csv')
eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]

# gegevens hydrobiologie (only data after 2006) (In fewparameter, "WATDTE_m" and "SLIBDTE_m" are included)
hybi <- readRDS('data/alles_reliable.rds')
hybi <- ppr_hybi(db = hybi, syear = 2006, wtype = eag_wl, mlocs = locaties)

# # temp # convert hybi to sf object
# hybi_sf <- st_as_sf(hybi, coords = c("locatie.x", "locatie.y"), crs = 28992, agr = "constant")
# st_write(hybi_sf, "diepte/hybi_sf.gpkg")

# # load shape of sloot
# water <- st_read("data/WaterPerEAG20191205.gpkg") %>% st_transform(28992)
# # merge EAG 
# water <- merge(water, eag_wl, by = "GAFIDENT")

# load shape of EAG
eag <- st_read("data/EAG20191205.gpkg") %>% st_transform(28992)


## Rasterize EAG shape-----------
# make a template raster
res <- 100
rs_template <- raster(extent(eag))
res(rs_template) <- c(res, res)
crs(rs_template) <- CRS("+init=epsg:28992")

# convert EAG to integer
tb_eag <- data.table(eag_id = 1:length(unique(locaties$EAGIDENT[locaties$EAGIDENT != ""])),
                     EAG = unique(locaties$EAGIDENT[locaties$EAGIDENT != ""]))
# tb_eag <- data.table(eag_id = 1:length(unique(eag_wl$GAFIDENT)),
#                      EAG = unique(eag_wl$GAFIDENT))
eag <- left_join(eag, tb_eag, by = c("GAFIDENT" = "EAG"))

# First rasterize from lines
eag_l <- sf::as_Spatial(st_cast(eag, "MULTILINESTRING")) #convert to spatial line object
eag_l_r <- rasterize(eag_l, rs_template, field = "eag_id") # this takes lots of time! ca. 13 min
                                                          # fun = "length" does not work for projected map
# writeRaster(eag_l_r, filename = "diepte/eag_l_r.tif", overwrite = T)
# Second, rasterize polygons
eag_p_r <- fasterize(eag, rs_template, field = "eag_id", fun = "last") 
# and then, overlay (giving a priority to the rasterized lines)
eag_r <- overlay(eag_l_r, eag_p_r, fun = function(x, y) {
  x[is.na(x[])] <- y[is.na(x[])]
  return(x)
})
# writeRaster(eag_r, filename = "diepte/eag_r.tif", overwrite = T)


# merge location info to hybi
dt <- merge(hybi, locaties[,.(CODE, EAGIDENT, GAFIDENT)], by.x = "locatiecode", by.y = "CODE", all.x =T)
# delete record of meetwaarde == -999
dt <- dt[meetwaarde != -999, ]

# choose data of the target parameter and of specific years 
year2u <- 2015:2019
para2u <- "WATDTE_m"
dt2 <- dt[fewsparameter == para2u & jaar %in% year2u,]



# summary per location & eag (median depth, sd, N)
loc_sum <- dt2[,.(med = median(meetwaarde),
                 N_record = .N,
                 sd = sd(meetwaarde)),
              by = .(EAGIDENT, locatiecode)]
# summary per EAG (median, number of measurement location, sd)
eag_sum <- loc_sum[, .(med = median(med), # median of location medians
                       N_location = .N,
                       sd = sd(med)), 
                   by = EAGIDENT]

# merge info on EAG level
dt_m <- data.table(EAGIDENT = unique(eag_wl$GAFIDENT))
dt_m <- merge(dt_m, eag_wl[, .(GAFIDENT, type)], by.x = "EAGIDENT", by.y = "GAFIDENT", all.x = T)
dt_m <- merge(dt_m, eag_sum , by = "EAGIDENT", all.x = T)

eag_med <- eag_r # initialization
values(eag_med) <- NA_integer_ 

#' assign values in raster 1 depending on values in raster 2
overlay1 <- function(ras1, ras2, id, val){
  ol <- overlay(ras1, ras2, fun = function(x, y) {
    x[y == id] <- val
    return(x)
  })
  return(ol)
}

#' assign NA in raster 1 unless the value in raster 2 is 'id' 
overlay2 <- function(ras1, ras2, id){
  ol <- overlay(ras1, ras2, fun = function(x, y) {
    x[y != id] <- NA
    return(x)
  })
  return(ol)
}


# loop over EAG
for (i in 1:nrow(dt_m)){
  # ID number of the EAG
  eag_id_i <- tb_eag[EAG == dt_m[i, EAGIDENT], eag_id] 
  if(is.na(dt_m[i, N_location])){
    # when there is no measurement locations in the EAG, assign NA
    eag_med <- overlay1(eag_med, eag_r, eag_id_i, NA)
  } else if(dt_m[i, N_location] < 10){
    # when there are less than 3 measurement locations in the EAG
    # assin median value of the locations
    eag_med <- overlay1(eag_med, eag_r, eag_id_i, dt_m[i, med])
  }
}
  
# Kriging 
# convert point measurements of the EAG to Spatial Object 
dt_sp <- dt2[EAGIDENT == dt_m[1, EAGIDENT], ]
sp::coordinates(dt_sp) <- ~ locatie.x + locatie.y
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

eag_out <- projectRaster(overlay2(eag_r, eag_r,  eag_id_i), crs = CRS("+init=epsg:28992"))
eag_out_sp <-as(eag_out, "SpatialGridDataFrame") 

## TO CHECK: 
res_krige <- autoKrige(meetwaarde ~ 1,
          input_data = dt_sp,
          new_data = eag_out_sp,
          model = "Sph",
          #block = c(1000, 1000), # for block kriging
          verbose = FALSE)













# # check nr of measurement locations per EAG
# n_red_eag <- dt2[fewsparameter == "WATDTE_m", .N, by = .(EAGIDENT, locatiecode)]
# n_loc_eac <- n_red_eag[, .N, by = EAGIDENT]

# draw histogram (nr of locations per EAG)
ggplot(dt_m) + geom_histogram(aes(y = N)) + facet_wrap(.~type) +
  xlab("Number of EAG") + ylab("Number of locations with depth records in hybi")

# example map of 1 eag with maximum number of data points
setDT(eag)
eag2u <- n_loc_eac[N == max(N), EAGIDENT] # EAG with the most number of records
#eag2u <- n_eag[type == "sloot" & N>100, EAGIDENT][1] #EAG of sloot with many records
eag_e <- st_as_sf(eag[GAFIDENT == eag2u,])
dt_e <- dt[EAGIDENT == eag2u & fewsparameter == "WATDTE_m",]
ggplot() + geom_sf(data = eag_e) +
  geom_point(data= dt_e, aes(x = locatie.x, y = locatie.y, col = meetwaarde)) +
  labs(col = "WATDTE_m") +
  ggtitle(eag2u)





# temp # draw map
ggplot(water) + geom_sf() 
