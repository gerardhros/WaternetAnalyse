# Kriging per GAF

# 'loc_sum' and 'gaf_sum' does not exist anymore.



## Kriging -------

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



# # draw histogram (nr of locations per EAG)
# ggplot(dt_m) + geom_histogram(aes(y = N)) + facet_wrap(.~type) +
#   xlab("Number of EAG") + ylab("Number of locations with depth records in hybi")

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
