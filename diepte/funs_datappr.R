
#' Check if there are completely identical rows. If so, remove them (after giving a warning)
remove_duplicate <- function(dt){
  dt_uni <- unique(dt)
  if(nrow(dt) != nrow (dt_uni)){
    print(paste0("There are ", nrow(dt) - nrow (dt_uni), " completely identical rows. They were removed."))
  }
  return(dt_uni)
}

#' Exclude hybi data with unrealistic WATDTE_M and WATBTE_m from line features
#' Criteria to remove (for lijnformig feasures only):  WATDTE_M > max_d, WATBTE_m > max_b, or WATBTE_m  = 0
remove_unrealistic <- function(hybi, max_d = 5, max_b = 50, print = FALSE){
  dexc <- hybi[fewsparameter == "WATDTE_m" & MORFOLOGIE == "lijnvormig"& meetwaarde > max_d,]
  print(paste0(nrow(dexc), " record was removed, because the ditch depth (WATDTE_M) is unrealistically large: >", max_d, "m"))
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


#' Make location-based summary of hybi data (median, SD, N of records)
location_summary <- function(hybi,  year2u){

  hybi <- copy(hybi)
  # choose relevant years only
  hybi <- hybi[jaar %in% year2u,]

  # hybi_dc2 <- dcast(hybi[fewsparameter %in% c("WATDTE_m", "SLIBDTE_m", "WATERBTE_m"),],
  #                   locatiecode + datum ~ fewsparameter,
  #                   value.var = "meetwaarde",
  #                   fun.aggregate = median)
  # hybi_dc2 <- hybi_dc2[!is.na(WATDTE_m) & !is.na(WATERBTE_m),]

  loc_sum_wd <- hybi[fewsparameter == "WATDTE_m",.(med_wd = median(meetwaarde, na.rm = T),
                                                   N_record_wd = .N,
                                                   sd_wd = sd(meetwaarde)),
                     by = .(locatiecode)]
  loc_sum_wb <- hybi[fewsparameter == "WATERBTE_m",.(med_wb = median(meetwaarde, na.rm = T),
                                                     N_record_wb = .N,
                                                     sd_wb = sd(meetwaarde)),
                     by = .(locatiecode)]
  loc_sum_sd <- hybi[fewsparameter == "SLIBDTE_m",.(med_sd = median(meetwaarde, na.rm = T),
                                                    N_record_sd = .N,
                                                    sd_sd = sd(meetwaarde)),
                     by = .(locatiecode)]

  loc_sum_td <- hybi[fewsparameter == "TOTDTE_m",.(med_td = median(meetwaarde, na.rm = T),
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




#' #' Extract waterpeil info of measurement point
#' #'
#' #' @import rgeos
#' #' @imort data.table
#' #' @import sp
#' #' @import sf
#' get_waterpeil <- function(loc_sf, waterpeil_fn){
#'
#'   # load shape
#'   waterpeil <- st_read(waterpeil_fn) %>% st_transform(28992)
#'
#'   # fix self intersecting geometries of waterpeil shape
#'   waterpeil_sp <- as(waterpeil, "Spatial") %>% spTransform(CRS("+init=epsg:28992")) %>% gBuffer(byid=TRUE, width=0)
#'
#'   waterpeil2 <- st_as_sf(waterpeil_sp) %>% st_transform(crs = 28992)
#'
#'   # intersect points and polygons
#'   wp <- st_intersection(loc_sf, waterpeil2)
#'
#'   # make a variable "PEIL"
#'   setDT(wp)
#'   wp[, PEIL := NA_real_]
#'   wp[BEHEER == "vast", PEIL := VASTPEIL]
#'   wp[BEHEER == "flexibel", PEIL := (ONDERPEIL + BOVENPEIL)/2]
#'   wp[BEHEER == "seizoensgebonden", PEIL := (WINTERPEIL + ZOMERPEIL)/2]
#'
#'   # Since some points (N=4) overlap with multiple polygons, compute median
#'   wp_m <- wp[, .(PEIL = median(PEIL)), by = locatiecode]
#'
#'   loc_sf <- left_join(loc_sf, wp_m, by = "locatiecode")
#'
#'   return(loc_sf)
#'
#' }

#' #' Extract soil type ("zand"/"klei"/"veen") of measurement point
#' #'
#' #' @import OBIC
#' get_soiltype <- function(loc_sf, fac_rs_fn){
#'   # load rasterstack 'fac_rs'
#'   load(fac_rs_fn)
#'
#'   soils.obic <- as.data.table(OBIC::soils.obic)
#'   soils.obic[, soilcodeID := 1:nrow(soils.obic)]
#'
#'   # extract soil type per measurement point
#'   loc_sf <- mutate(loc_sf, ID = 1:nrow(loc_sf))
#'   sc <- extract(fac_rs[["soilcodeID"]], loc_sf, df = TRUE)
#'   loc_sf <- merge(loc_sf, sc, by = "ID")
#'   loc_sf <- merge(loc_sf, soils.obic[, .(soilcodeID, soiltype.n)], by = "soilcodeID")
#'
#'   return(loc_sf)
#' }

#' get raster values from raster
#' @param loc_sf (sf object)
#' @raster raster (raster object)
get_value_from_raster <- function(loc_sf, raster, buffer = NULL){
  loc_sf <- mutate(loc_sf, ID = 1:nrow(loc_sf))
  if(is.null(buffer)){
    sc <- extract(raster, loc_sf, df = TRUE)
  } else {
    sc <- extract(raster, loc_sf, buffer = buffer, fun  = mean, df = TRUE)
  }
  loc_sf <- merge(loc_sf, sc, by = "ID")
  setDT(loc_sf)
  loc_sf[, ID := NULL]
  loc_sf <- st_as_sf(loc_sf)

  return(loc_sf)
}





#' Get theoretical water depth around measurement points
#'
get_theowater <- function(watth_fn, loc_sf){
  # load shape
  majorwater <- st_read(watth_fn, quiet = T)%>% st_transform(28992)

  # intersect buffers around measurement points and waterways
  loc_int_10 <- st_intersects(st_buffer(loc_sf, 10), majorwater, sparse = FALSE)
  # loc_int_20 <- st_intersects(st_buffer(loc_sf, 20), majorwater, sparse = FALSE)
  # loc_int_30 <- st_intersects(st_buffer(loc_sf, 30), majorwater, sparse = FALSE)

  #temp10 <- rowSums(loc_int_10)

  # Compute depth of waterways within Xm buffer from measurement points.
  # When more than 1 waterways fall within the buffer, then take the median value.
  # TO DO: also get width data!
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


#'Get valuve of water width and water surface AHN from water shore data
#'
#'This function obtains, for each measurement points, values of water width and water surface AHN.
#'The value of the nearest point (with maximum distance of 30m, and within the same EAG) is taken.
#'
#'@param loc_sf (sf object) sf point objects of location-level measurement data
#'Need to include a column 'EAGIDENT' which stores EAG id
#'@param ww_fn (CHAR) file name of point shape of water shore data.
#'Need to include columns 'pnt_ahn' and 'pnt_breedte'
#'@param eag_fn (CHAR) file name of polygon shape of EAG boudaries.
#'Need to include a column 'GAFIDENT' which stores EAG id
#'@param update (boolean) whether the distance is calculated again (TRUE) or the previously saved data is used (FALSE)
#'@param loc_v_fn (CHAR) File name of data table in which water width and AHN of nearest shore point for each hybi measurement point is stored.
#'Needed only wen update = FALSE
#'
#'@import data.table
#'@import sf
#'@import dplyr
get_width_ahn <- function(loc_sf, ww_fn, eag_fn, update = FALSE, loc_v_fn = NULL){

  if(file.exists(loc_v_fn) & update == FALSE){
    load(loc_v_fn)
  } else {

    # Load data
    ww <- st_read(ww_fn, quiet = T)%>% st_transform(28992)

    # when pnt_breedte = 0, remove the record
    setDT(ww)
    ww <- ww[pnt_breedte != 0, ]
    ww <- st_as_sf(ww)

    ## Label ww points with EAG
    # load shape of EAG
    eag <- st_read(eag_fn, quiet = T) %>% st_transform(28992)
    ww_eag <- st_intersects(ww, eag, sparse = FALSE)

    # get EAG per ww point
    EAGi <- rep(NA, length.out = nrow(ww)) #initialization
    for (i in 1:nrow(ww)){
      EAGi[i] <- eag$GAFIDENT[ww_eag[i,]][1]
    }
    ww <- mutate(ww, EAGIDENT = EAGi)


    # For each point of loc_sf, get width and AHN values from the nearest point
    # (with batch per EAG, because otherwize a memory error occurs)

    # initialization of data table to store results
    loc_v <- data.table(locatiecode = loc_sf$locatiecode,
                        pnt_ahn = NA_real_,
                        pnt_breedte  = NA_real_)

    uni_eag <- unique(eag$GAFIDENT)
    for (e in 1:length(uni_eag)){
      # select measurement locations and ww points of the EAG
      loc_e <-loc_sf[!is.na(loc_sf$EAGIDENT) & loc_sf$EAGIDENT == uni_eag[e],]
      ww_e <- ww[!is.na(ww$EAGIDENT) & ww$EAGIDENT == uni_eag[e],]

      # Proceed only when points exist in this EAG
      if (nrow(ww_e) != 0 & nrow(loc_e) != 0){

        # compute distance between points
        loc_dist <- as.data.table(st_distance(loc_e, ww_e))
        # get index of column nr of minimum distance
        colminind <- apply(loc_dist, 1, function(x) which.min(x))
        # minimum distance
        colmin <- apply(loc_dist, 1, min)
        # When distance is > 30m, then don't get value
        colminind[colmin > 30] <- NA
        # store values of pnt_ahn and pnt_breedte of the closest point
        loc_v[match(loc_e$locatiecode, locatiecode),  pnt_ahn := ww_e$pnt_ahn[colminind]]
        loc_v[match(loc_e$locatiecode, locatiecode),  pnt_breedte := ww_e$pnt_breedte[colminind]]
      }
    }

    save(loc_v, file = paste0(iloc_project, "diepte/loc_v.RData"))

  }

  # merge
  loc_sf <- merge(loc_sf, loc_v, by = "locatiecode", all.x = T)

  return(loc_sf)
}


