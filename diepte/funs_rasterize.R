#' Make a raster template for the extent of EAG
#' 
create_raster_template <- function(eag_fn, res){
  
  # load shape of EAG
  eag <- st_read(eag_fn) %>% st_transform(28992)
  
  # make a template raster
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


#' Make a raster of EAG-median values
#' @param dt_loc (datatable) data table containing location statistics. 
#' @param para2u (CHAR) A column name of 'dt_loc' to be analyzed
#' @param water_eag_r (raster) raster of waterways, labelled with EAG. 
#' @param tb_eag (datatable) corresponding table of EAG name and EAG code ('eag_id') used in 'eag_r'
raster_eag_med <- function(dt_loc, para2u, water_eag_r, tb_eag){

  # para2u <- "med_wd"
  
  dt_loc <- copy(dt_loc)
  dt_loc[, var := mget(para2u)]
  
  # summary per EAG (median, number of measurement location, sd)
  eag_sum <- dt_loc[, .(N_location = .N,
                         med = median(var, na.rm = T), # median of location medians
                         sd = sd(var)),
                    by = EAGIDENT]
  
  # merge EAG statistics to tb_eag (table of all EAG's)
  dt_m <- merge(tb_eag, eag_sum , by.x = "EAG", by.y = "EAGIDENT", all.x = T)
  
  
  
  ## rasterize per EAG
  eag_med <- water_eag_r # initialization of raster
  values(eag_med) <- NA_integer_ 
  
  # loop over EAG
  for (i in 1:nrow(dt_m)){
    # ID number of the EAG
    eag_id_i <- tb_eag[EAG == dt_m[i, EAG], eag_id] 
    if(is.na(dt_m[i, N_location])){
      # when there is no measurement locations in the EAG, assign NA
      eag_med <- overlay1(eag_med, water_eag_r, eag_id_i, NA)
    } else {
      # assin median value of the locations
      eag_med <- overlay1(eag_med, water_eag_r, eag_id_i, dt_m[i, med])
    }
  }
  
  names(eag_med) <- paste0("eag_",para2u)
  
  return(eag_med)
}

#' Rasterize waterpeil polygons
#' @import fasterize
rasterize_waterpeil <- function(waterpeil_fn, rs_template){
  # load shape
  waterpeil <- st_read(waterpeil_fn) %>% st_transform(28992)
  
  # fix self intersecting geometries of waterpeil shape
  waterpeil_sp <- as(waterpeil, "Spatial") %>% spTransform(CRS("+init=epsg:28992")) %>% gBuffer(byid=TRUE, width=0)
  waterpeil2 <- st_as_sf(waterpeil_sp) %>% st_transform(crs = 28992)
  
  # make a variable "PEIL"
  setDT(waterpeil2)
  waterpeil2[, PEIL := NA_real_]
  waterpeil2[BEHEER == "vast", PEIL := VASTPEIL]
  waterpeil2[BEHEER == "flexibel", PEIL := (ONDERPEIL + BOVENPEIL)/2]
  waterpeil2[BEHEER == "seizoensgebonden", PEIL := (WINTERPEIL + ZOMERPEIL)/2]
  waterpeil2 <- st_as_sf(waterpeil2)
  
  waterpeil_rs <- fasterize(waterpeil2, rs_template, field = "PEIL")
  names(waterpeil_rs) <- "PEIL"
  
  return(waterpeil_rs)
}

#' Raterize soil code
#' This script convert the existing soilcode raster to the extent of this project.
#' 1: sand, 2: clay, 3: peat
#' 
#' @import OBIC
rasterize_soilcode <- function(fac_rs_fn, rs_template){
  # load raster stack
  load(fac_rs_fn)
  
  # resample
  soilcode_rs <- resample(fac_rs[["soilcodeID"]], rs_template, method = "ngb")
  
  # make a conversion table 
  soils.obic <- as.data.table(OBIC::soils.obic)
  soils.obic[, soilcodeID := (1:nrow(soils.obic))-0.1]
  soils.obic[, soilcodeID2 := soilcodeID+0.2]
  soils.obic[soiltype.n == "zand", becomes := 1]
  soils.obic[soiltype.n == "klei", becomes := 2]
  soils.obic[soiltype.n == "veen", becomes := 3]
  soils.obic[, (c("soiltype", "soiltype.ph", "soiltype.n", "soiltype.m")) := NULL]
  
  # reclassify 
  soilcode_rs2 <- reclassify(soilcode_rs, soils.obic)
  
  names(soilcode_rs2) <- "soiltypen"
  
  return(soilcode_rs2) 
}

#' Rasterize seepage point data
rasterize_kwel <- function(kwel_fn, rs_template){
  # load seepage point shape
  kwel <- st_read(kwel_fn) %>% st_transform(28992)
  
  ## Create a raster template ----
  
  # create a convex hull polygon
  ch <- st_convex_hull(st_union(kwel)) 
  ch_sp <-  as(ch, "Spatial") %>% spTransform(CRS("+init=epsg:28992"))
  # mask template with convex hull
  values(rs_template) <- -999
  rs <- mask(rs_template, ch_sp) 
  
  ## Convert to raster & filling gaps -------
  kwel_r <- rasterize(kwel, rs, field = "KWEL", fun = median)
  # fill in missing cells based on neighbouring cells 
  # (first with 3 x 3 windows)
  kwel_rs <- focal(kwel_r, w = matrix(1,3,3), fun = median, 
                   pad = T, padValue = NA, #additional 'virtual' rows and columns are padded to x such that there are no edge effects
                   na.rm = TRUE, NAonly = TRUE)
  # (and then with 5 x 5 windows)
  kwel_rs <- focal(kwel_rs, w = matrix(1,5,5), fun = median, 
                   pad = T, padValue = NA, #additional 'virtual' rows and columns are padded to x such that there are no edge effects
                   na.rm = TRUE, NAonly = TRUE)
  
  names(kwel_rs) <- "KWEL"
  
  #save(kwel_rs, file = paste0(iloc_project, "diepte/kwel_rs.RData"))
  
  # ## Interpolate point data to raster by kriging -----
  # # convert point measurements of the EAG to Spatial Object 
  # kwel_sp <- as(kwel, "Spatial") %>% spTransform(CRS("+init=epsg:28992"))
  # 
  # # convert raster to spatial object
  # rs_sp <- as(rs, "SpatialGridDataFrame") 
  # 
  # ## ordinary kriging
  # res_krige <- autoKrige(KWEL ~ 1,
  #                        input_data = kwel_sp,
  #                        new_data = rs_sp,
  #                        model = "Sph",
  #                        verbose = FALSE)
  # # -> ERROR!!! out of dynamic memory (try local kriging?)
  # 
  # # check results
  # #plot(res_krige)
  # 
  # # store prediction
  # kwel_rs <- raster(res.krige$krige_output[1])
  
  return(kwel_rs) 
  
}


#' Raterize OM
#' This script convert the existing OM raster to the extent of this project.
#' 
rasterize_om <- function(num_rs_fn, rs_template){
  
  # load raster stack
  load(num_rs_fn)
  
  # resample
  om_rs <- resample(num_rs[["A_OS_GV"]], rs_template, method = "ngb")
  
  names(om_rs) <- "A_OS_GV"
  
  return(om_rs) 
}
