#' Make a raster template for the extent of EAG
create_raster_template <- function(eag_fn){
  
  # load shape of EAG
  eag <- st_read(eag_fn) %>% st_transform(28992)
  
  # make a template raster
  res <- 100
  rs_template <- raster(extent(eag))
  res(rs_template) <- c(res, res)
  crs(rs_template) <- CRS("+init=epsg:28992")
  
  return(rs_template)
}

#' Rasterize polygons (first from lines, and then polygon) 
rasterize_polygon_line <- function(polygon, rs_template, field2u){
  
  # First rasterize from lines
  polygon_l <- sf::as_Spatial(st_cast(polygon, "MULTILINESTRING")) #convert to spatial line object
  polygon_l_r <- rasterize(polygon_l, rs_template, field = field2u) # this takes lots of time! 
  # fun = "length" does not work for projected map
  
  # Second, rasterize polygons
  polygon_p_r <- fasterize(polygon, rs_template, field = field2u, fun = "last") 
  
  # and then, overlay (giving a priority to the rasterized lines)
  polygon_r <- overlay(polygon_l_r, polygon_p_r, fun = function(x, y) {
    x[is.na(x[])] <- y[is.na(x[])]
    return(x)
  })
  
  return(polygon_r)
}


#'Rasterize GAF polygons
#'
rasterize_gaf <- function(gaf_fn, rs_template){
  # load shape of GAF
  gaf <- st_read(gaf_fn) %>% st_transform(28992)
  # remove 1 GAF (GAFIDENT = 3???)
  setDT(gaf)
  gaf <- gaf[GAFIDENT != "3???", ]
  gaf <- st_as_sf(gaf)
  
  # # make a template raster
  # res <- 100
  # rs_template <- raster(extent(gaf))
  # res(rs_template) <- c(res, res)
  # crs(rs_template) <- CRS("+init=epsg:28992")
  
  gaf$GAFIDENT <- as.integer(gaf$GAFIDENT)
  
  # raterize GAF polygons (first from lines, and then polygon) 
  gaf_r <- rasterize_polygon_line(gaf, rs_template, "GAFIDENT")
  
  # gaf_l <- sf::as_Spatial(st_cast(gaf, "MULTILINESTRING")) #convert to spatial line object
  # gaf_l_r <- rasterize(gaf_l, rs_template, field = "GAFIDENT") # this takes lots of time! 
  #                                                             # fun = "length" does not work for projected map
  # # Second, rasterize polygons
  # gaf_p_r <- fasterize(gaf, rs_template, field = "GAFIDENT", fun = "last") 
  # # and then, overlay (giving a priority to the rasterized lines)
  # gaf_r <- overlay(gaf_l_r, gaf_p_r, fun = function(x, y) {
  #   x[is.na(x[])] <- y[is.na(x[])]
  #   return(x)
  # })

  # save(gaf_r, file = paste0(iloc_project, "diepte/gaf_r.RData"))
  return(gaf_r)
}

#'Rasterize EAG polygons
#'
rasterize_eag <- function(tb_eag, eag_fn, rs_template){
  # load shape of EAG
  eag <- st_read(eag_fn) %>% st_transform(28992)
  setDT(eag)
  # delete a record with obscure EAG
  eag <- eag[GAFIDENT != "3???-EAG-1", ]
  # add EAGIDENT and correct GAFIDENT (Because GAFIDENT is actually EAGIDENT in the original file)
  eag[, EAGIDENT := GAFIDENT]
  eag[, GAFIDENT := as.integer(substr(EAGIDENT, 1, 4))]
  
  # convert EAG to integer
  eag <- left_join(eag, tb_eag, by = c("EAGIDENT" = "EAG"))
  eag <- st_as_sf(eag)
  
  # raterize EAG polygons (first from lines, and then polygon) 
  eag_r <- rasterize_polygon_line(eag, rs_template, "eag_id")
  
  # # First rasterize from lines
  # eag_l <- sf::as_Spatial(st_cast(eag, "MULTILINESTRING")) #convert to spatial line object
  # eag_l_r <- rasterize(eag_l, rs_template, field = "eag_id") # this takes lots of time! ca. 13 min
  # # fun = "length" does not work for projected map
  # # Second, rasterize polygons
  # eag_p_r <- fasterize(eag, rs_template, field = "eag_id", fun = "last") 
  # # and then, overlay (giving a priority to the rasterized lines)
  # eag_r <- overlay(eag_l_r, eag_p_r, fun = function(x, y) {
  #   x[is.na(x[])] <- y[is.na(x[])]
  #   return(x)
  # })

  #save(eag_r, tb_eag, file = paste0(iloc_project, "diepte/eag_r.RData"))
  
  return(eag_r)
}


#' rasterize water ways in the same raster configuration as 'eag'
rasteize_water <- function(water_fn,  rs_template){
  # load shape of water
  water <- st_read(water_fn) %>% st_transform(28992)
  
  # add a dummy variable for rasterization
  water <- mutate(water, val = 1)
  
  # raterize water polygons (first from lines, and then polygon) 
  water_r <- rasterize_polygon_line(water, rs_template, "val")
  
  #save(water_r, file = paste0(iloc_project, "diepte/water_r.RData"))
  
   return(water_r)
 }


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

# make a raster of EAG-median values
raster_eag_med <- function(hybi, year2u, para2u, locaties, eag_wl){
  
  # year2u <- 2015:2019
  # para2u <- "WATDTE_m"
  
  dt2 <- hybi[fewsparameter == para2u & jaar %in% year2u,]
  
  
  # calculate location medians over years
  loc_sum <- dt2[,.(med = median(meetwaarde),
                    N_record = .N,
                    sd = sd(meetwaarde)),
                 by = .(locatiecode)]
  # merge location info to location medians
  loc_sum <- merge(loc_sum, locaties[, .(CODE, EAGIDENT, GAFIDENT, XCOORD, YCOORD)], 
                   by.x = "locatiecode", by.y = "CODE", all.x = T)
  
  # # summary per GAF
  # gaf_sum <- loc_sum[, .(N_location = .N),by = GAFIDENT]
  
  
  # summary per EAG (median, number of measurement location, sd)
  eag_sum <- loc_sum[, .(N_location = .N,
                         med = median(med), # median of location medians
                         sd = sd(med)),by = EAGIDENT]
  
  # merge EAG statistics to eag_wl
  dt_m <- merge(eag_wl[, .(EAGIDENT, GAFIDENT, type)], eag_sum , by = "EAGIDENT", all.x = T)
  
  
  
  ## Interpolate sloot depth ----
  
  eag_med <- eag_r # initialization of raster
  values(eag_med) <- NA_integer_ 
  
  # loop over EAG
  for (i in 1:nrow(dt_m)){
    # ID number of the EAG
    eag_id_i <- tb_eag[EAG == dt_m[i, EAGIDENT], eag_id] 
    if(is.na(dt_m[i, N_location])){
      # when there is no measurement locations in the EAG, assign NA
      eag_med <- overlay1(eag_med, eag_r, eag_id_i, NA)
    } else {
      #} else if(dt_m[i, N_location] < 10){
      # when there are less than 3 measurement locations in the EAG
      # assin median value of the locations
      eag_med <- overlay1(eag_med, eag_r, eag_id_i, dt_m[i, med])
    }
  }
  
  return(eag_med)
}

