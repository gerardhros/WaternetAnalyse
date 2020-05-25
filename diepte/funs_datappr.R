
#' Check if there are completely identical rows. If so, remove them (after giving a warning)
remove_duplicate <- function(dt){
  dt_uni <- unique(dt)
  if(nrow(dt) != nrow (dt_uni)){
    print(paste0("There are ", nrow(dt) - nrow (dt_uni), " completely identical rows."))
    print("These rows were removed.")
  }
  return(dt_uni)
}

## Exclude hybi data with unrealistic WATDTE_M and WATBTE_m from line features
remove_unrealistic <- function(hybi, max_d = 5, max_b = 50, print = FALSE){
  dexc <- hybi[fewsparameter == "WATDTE_m" & MORFOLOGIE == "lijnvormig"& meetwaarde > max_d,]
  print(paste0(nrow(dexc), " record was removed, because the ditch depth (WATDTE_M) is unrealistically high: >", max_d, "m"))
  if(print == TRUE){
    print(dexc)
  }
  hybi <- hybi[!(fewsparameter == "WATDTE_m" & MORFOLOGIE == "lijnvormig" & meetwaarde > max_d), ]
  
  bexc <- hybi[fewsparameter == "WATERBTE_m" & MORFOLOGIE == "lijnvormig" & meetwaarde > max_b,]
  print(paste0(nrow(bexc), " record was removed, because the ditch width (WATERBTE_m) is unrealistically wide: >", max_b, "m"))
  if(print == TRUE){
    print(bexc)
  }
  hybi <- hybi[!(fewsparameter == "WATERBTE_m" & MORFOLOGIE == "lijnvormig" & meetwaarde > max_b),]
  
  b0exc <- hybi[fewsparameter == "WATERBTE_m" & MORFOLOGIE == "lijnvormig" & meetwaarde == 0,]
  print(paste0(nrow(b0exc), " record was removed, because the ditch width (WATERBTE_m) is 0"))
  if(print == TRUE){
    print(b0exc)
  }
  hybi <- hybi[!(fewsparameter == "WATERBTE_m" & MORFOLOGIE == "lijnvormig" & meetwaarde == 0),]
  
  return(hybi)
}

#' Make location-based summary of data 
location_summary <- function(hybi){
  hybi_dc2 <- dcast(hybi[fewsparameter %in% c("WATDTE_m", "SLIBDTE_m", "WATERBTE_m"),], 
                    locatiecode + datum ~ fewsparameter,
                    value.var = "meetwaarde",
                    fun.aggregate = median)
  hybi_dc2 <- hybi_dc2[!is.na(WATDTE_m) & !is.na(WATERBTE_m),]
  
  loc_sum_wd <- hybi[fewsparameter == "WATDTE_m",.(med_wd = median(meetwaarde),
                                                   N_record_wd = .N,
                                                   sd_wd = sd(meetwaarde)),
                     by = .(locatiecode)]
  loc_sum_wb <- hybi[fewsparameter == "WATERBTE_m",.(med_wb = median(meetwaarde),
                                                     N_record_wb = .N,
                                                     sd_wb = sd(meetwaarde)),
                     by = .(locatiecode)]
  loc_sum_sd <- hybi[fewsparameter == "SLIBDTE_m",.(med_sd = median(meetwaarde),
                                                    N_record_sd = .N,
                                                    sd_sd = sd(meetwaarde)),
                     by = .(locatiecode)]
  
  loc_sum_td <- hybi[fewsparameter == "TOTDTE_m",.(med_td = median(meetwaarde),
                                                   N_record_td = .N,
                                                   sd_td = sd(meetwaarde)),
                     by = .(locatiecode)]
  
  dt_loc <- data.table(locatiecode = unique(hybi$locatiecode))
  dt_loc <- merge(dt_loc, loc_sum_wd, by = "locatiecode", all.x= T)
  dt_loc <- merge(dt_loc, loc_sum_wb, by = "locatiecode", all.x= T)
  dt_loc <- merge(dt_loc, loc_sum_sd, by = "locatiecode", all.x= T)
  dt_loc <- merge(dt_loc, loc_sum_td, by = "locatiecode", all.x= T)
  
  return(dt_loc)
}


#' make a parameter TOTDTE_M (= WATDTE_m + SLIBDTE_m)
calc_totdepth <- function(hybi){
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
  return(hybi)
}


#' Extract waterpeil info of measurement point
#' 
#' @import rgeos
#' @imort data.table
#' @import sp
#' @import sf
get_waterpeil <- function(loc_sf, waterpeil_fn){
  
  # load shape
  waterpeil <- st_read(waterpeil_fn) %>% st_transform(28992)
  
  # fix self intersecting geometries of waterpeil shape
  waterpeil_sp <- as(waterpeil, "Spatial") %>% spTransform(CRS("+init=epsg:28992")) %>% gBuffer(byid=TRUE, width=0)
  
  waterpeil2 <- st_as_sf(waterpeil_sp) %>% st_transform(crs = st_crs(loc_sf))
  
  # intersect points and polygons
  wp <- st_intersection(loc_sf, waterpeil2)
  
  # make a variable "PEIL"
  setDT(wp)
  wp[, PEIL := NA_real_]
  wp[BEHEER == "vast", PEIL := VASTPEIL]
  wp[BEHEER == "flexibel", PEIL := (ONDERPEIL + BOVENPEIL)/2]
  wp[BEHEER == "seizoensgebonden", PEIL := (WINTERPEIL + ZOMERPEIL)/2]
  
  # Since some points (N=4) overlap with multiple polygons, compute median
  wp_m <- wp[, .(PEIL = median(PEIL)), by = locatiecode]

  loc_sf <- left_join(loc_sf, wp_m, by = "locatiecode")
  
  return(loc_sf)
  
}

#' Extract soil type ("zand"/"klei"/"veen") of measurement point
#' 
#' @import OBIC
get_soiltype <- function(loc_sf, fac_rs_fn){
  # load rasterstack 'fac_rs'
  load(fac_rs_fn)
  
  soils.obic <- as.data.table(OBIC::soils.obic)
  soils.obic[, soilcodeID := 1:nrow(soils.obic)]
  
  # extract soil type per measurement point
  loc_sf <- mutate(loc_sf, ID = 1:nrow(loc_sf))
  sc <- extract(fac_rs[["soilcodeID"]], loc_sf, df = TRUE)
  loc_sf <- merge(loc_sf, sc, by = "ID")
  loc_sf <- merge(loc_sf, soils.obic[, .(soilcodeID, soiltype.n)], by = "soilcodeID")
  
  return(loc_sf)
}

#' Rasterize seepage point data (by kriging)
rasterize_kwel <- function(kwel_fn){
  # load seepage point shape
  kwel <- st_read(kwel_fn) %>% st_transform(28992)
 
  ## Create a raster template ----
  # create a convex hull polygon
  ch <- st_convex_hull(st_union(kwel)) 
  ch_sp <-  as(ch, "Spatial") %>% spTransform(CRS("+init=epsg:28992"))
  # create a raster template
  rs_template <- raster(extent(kwel_sp))
  res(rs_template) <- c(100, 100)
  crs(rs_template) <- CRS("+init=epsg:28992")
  values(rs_template) <- -999
  rs <- mask(rs_template, ch_sp) 
  
  ## Convert to raster & filling gaps -------
  kwel_r <- rasterize(kwel, rs, field = "KWEL", fun = median)
  # fill in missing cells based on neighbouring cells 
  # (first with 3 x 3 windows)
  kwel_rf <- focal(kwel_r, w = matrix(1,3,3), fun = median, 
                   pad = T, padValue = NA, #additional 'virtual' rows and columns are padded to x such that there are no edge effects
                   na.rm = TRUE, NAonly = TRUE)
  # (and then with 5 x 5 windows)
  kwel_rf <- focal(kwel_rf, w = matrix(1,5,5), fun = median, 
                   pad = T, padValue = NA, #additional 'virtual' rows and columns are padded to x such that there are no edge effects
                   na.rm = TRUE, NAonly = TRUE)
  
  names(kwel_rf) <- "KWEL"
  
  
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

  #save(kwel_rf, file = paste0(iloc_project, "diepte/kwel_rf.RData"))
  
  return(kwel_rf) 
  
}


#' Get seepage value for measurement points
get_seepage <- function(loc_sf, kwel_rf){
  
  loc_sf <- mutate(loc_sf, IDseq = 1:nrow(loc_sf))
  kw_e <- extract(kwel_rf, loc_sf, df = TRUE)
  loc_sf <- merge(loc_sf, kw_e, by.x = "IDseq", by.y = "ID", all.x = T)
  setDT(loc_sf)
  loc_sf[, IDseq := NULL]
  loc_sf <- st_as_sf(loc_sf)
  
  return(loc_sf)
}

#' Get theoretical water depth around measurement points
get_theowater <- function(watth_fn, loc_sf){
  # Extract water depth of main waterways
  majorwater <- st_read(watth_fn)%>% st_transform(28992)
  
  # intersect buffers around measurement points and waterways
  loc_int_10 <- st_intersects(st_buffer(loc_sf, 10), majorwater, sparse = FALSE)
  # loc_int_20 <- st_intersects(st_buffer(loc_sf, 20), majorwater, sparse = FALSE)
  # loc_int_30 <- st_intersects(st_buffer(loc_sf, 30), majorwater, sparse = FALSE)
  
  #temp10 <- rowSums(loc_int_10)
  
  # Compute depth of waterways within Xm buffer from measurement points.
  # When more than 1 waterways fall within the buffer, then take the median value.
  theo_dep <- data.table(locatiecode = loc_sf$locatiecode,
                         theo_dep  = NA_real_)
  for (i in 1:nrow(loc_int_10)){
    # for each measurement point
    depthi <- loc_int_10[i,] * majorwater$WATERDIEPT
    depthi[depthi == 0] <- NA
    theo_dep$theo_dep[i] <- median(depthi, na.rm = TRUE)
  }
  
  loc_sf2 <- merge(loc_sf, theo_dep, by = "locatiecode", all.x = T)
  
  return(loc_sf2)
}