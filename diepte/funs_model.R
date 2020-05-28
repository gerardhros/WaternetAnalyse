#' Compute R2
calc_rsq <- function (x, y){
  r2 <- cor(x, y, use =  "pairwise.complete.obs") ^ 2
  return(r2)
} 

#' Compute RMSE
#' @param x (NUM) modelled values
#' @param y (NUM) observed values
calc_rmse <- function(x, y){
  rmse <- sqrt(mean((x - y)^2, na.rm = T))
  return(rmse)
}

# compute RMSE of test set
test_rmse <- function(loc_sf, var_res, var_pred){
  setDT(loc_sf)
  rmse_test <- calc_rmse(unlist(loc_sf[set == "test", ..var_res]), unlist(loc_sf[set == "test", ..var_pred]))
  print(paste0("RMSE of testing set: ", round(rmse_test, 3)))
  return(rmse_test)
}

# compute R2 of test set
test_r2 <- function(loc_sf, var_res, var_pred){
  setDT(loc_sf)
  r2_test <- calc_rsq(loc_sf[set == "test",..var_res], loc_sf[set == "test", ..var_pred])
  print(paste0("R2 of testing set: ", round(r2_test, 3)))
  return(r2_test)
}



#' Prepare dataset for training and testing
#' 
#' @param loc_sf (sf object) 
#' @param var_id (CHAR) A column name of 'loc_sf' where ID of the record is stored
#' @param var_res (CHAR) A column name of 'loc_sf' where response variable is stored
#' @param var_cov (CHAR) column names of 'loc_sf' where explanatory variables are stored
#' @param fr_train (NUM) Fraction of dataset which is used for training
#' 
#' @return loc_t (datatable) data table containing only relevant columns (var_id, var_res var_cov) and complete cases.
#' COlumn name of var_res is 'varres'. Training set and testing set is identified in column 'set'


ppr_dataset <- function(loc_sf, var_id, var_res, var_cov, fr_train){
  setDT(loc_sf)
  # Select complete records
  var <- c(var_id, var_res, var_cov)
  loc_t <- loc_sf[complete.cases(loc_sf[, ..var]), ..var]
  
  # change column name of response variables
  setnames(loc_t, old = var_res, new = "varres")
  
  # Split training and test dataset
  set.seed(123)
  train <- sample(1:nrow(loc_t),nrow(loc_t) * fr_train)
  # add train or not
  loc_t[train, set := "training"]
  loc_t[!train, set := "test"]
  print(paste0("training N = ", loc_t[set == "training", .N], 
               ", testing N = ", loc_t[set == "test", .N]))
  
  return(loc_t)
}


#' Conduct random forest model
#' 
#' @param loc_t (datatable) data table containing 'var_id', varres, 'var_cov', and set. 
#' @param var_id (CHAR) A column name of 'loc_t' where ID of the record is stored
#' @param var_cov (CHAR) column names of 'loc_t' where explanatory variables are stored
#' @param check_mtry (boolean) Whether random forest error are tested with different number of Variables randomly chosen at each split
#' 
#' @import data.table
#' @import randomForest
#' 
#' @return ls_rf (list) A List containing the following elements:
#' loc_t (data.table): Same as the input table 'loc_t', with following added columns:\
#' "pred_rf" (predicted values by random forest), "resid_rf" (residual values)
#' rf_res (list) results of random forest
#' 
fun_rf <- function(loc_t, var_id, var_cov, check_mtry = FALSE,
                   mtry = round(length(var_cov)/3, 0),
                   ntree = 400){

  
  # Randam forest model
  #rf_res <- randomForest(varres ~ ., data = loc_t[,.SD, .SDcols = !var_id], subset = train,
  cols <- c("varres", var_cov)
  rf_res <- randomForest(varres ~ ., data = loc_t[set == "training", ..cols],
                         mtry = mtry, ntree = ntree)
  print(rf_res)

  #prediction
  loc_t[, pred_rf := predict(rf_res, loc_t)]
  loc_t[, resid_rf := varres - pred_rf]
  
  ## Check different numbers of Variables randomly chosen at each split (mtry)
  if (check_mtry == TRUE){
    print("Testing different numbers of Variables randomly chosen at each split (mtry)...")
    # Check OBB errors in errors in test set
    nvar <- length(var_cov) # number of explanatory variables
    oob.err=double(nvar)
    test.err=double(nvar)
    #mtry is no of Variables randomly chosen at each split
    for(mtry_t in 1:nvar)
    {
      #rf <- randomForest(varres ~ ., data = loc_t[,.SD, .SDcols = !var_id], subset = train, mtry=mtry_t, ntree=ntree)
      cols <- c("varres", var_cov)
      rf <- randomForest(varres ~ ., data = loc_t[set == "training", ..cols], 
                         mtry=mtry_t, ntree=ntree)
      oob.err[mtry_t] = rf$mse[ntree] #Error of all Trees fitted
      
      pred <- predict(rf, loc_t[-train,]) #Predictions on Test Set for each Tree
      test.err[mtry_t] = with(loc_t[-train,], mean( (varres - pred)^2)) #Mean Squared Test Error
      
      cat(mtry_t," ") #printing the output to the console
      
    }
    # plot errors
    dt_f <- data.table(mtry = rep(1:nvar, times = 2),
                       val = c(oob.err, test.err),
                       type = c(rep("oob.err", nvar),
                                rep("test.err", nvar)))
    gp_error <- ggplot(dt_f) + geom_point(aes(x = mtry, y = val, col = type), size = 3) +
      geom_line(aes(x = mtry, y = val, col = type), size = 1) +
      xlab("Number of Predictors Considered at each Split") + ylab("Mean Squared Error") +
      scale_color_discrete(name="", breaks=c("oob.err", "test.err"),
                           labels=c("Out of Bag Error", "Test Error"))  +
      theme(legend.position="top")
    show(gp_error)

  }
  
  ls_rf <- list()
  ls_rf$loc_t <- loc_t
  ls_rf$rf_res <- rf_res
  if(check_mtry == TRUE){
    ls_rf$gp_error <- gp_error
  }

  return(ls_rf)
  
}


#' Ordinary kriging of residuals of random forest
#' 
#' @param loc_sf (sf object) sf point object  which include column 'resid_rf' and 'pred_rf'
#' @param water_eag_r (raster) raster data of waterways
#' 
#' @return ls_kr (list)
#' ls_kr$resid_rs (raster) kriged residuals
#' ls_kr$loc_sf (sf object) Same as the input loc_sf, with a extra column 'pred_rf_kr'
#' ls_kr$res_krige  (list) result of kriging
#' 
fun_krige <- function(loc_sf, water_eag_r){
  
  setDT(loc_sf)
  loc_sf_rf <- loc_sf[!is.na(resid_rf),]
  loc_sf_rf <- st_as_sf(loc_sf_rf)
  loc_sf <- st_as_sf(loc_sf)
  
  # convert point shape to spatial object
  loc_sp <- as(loc_sf_rf, "Spatial") %>% spTransform(CRS("+init=epsg:28992"))
  
  # convert raster to spatial object
  crs(water_eag_r) <- CRS("+init=epsg:28992")
  rs_sp <- as(water_eag_r, "SpatialGridDataFrame")
  
  ## ordinary kriging (THIS TAKES CA. 12 minutes!)
  res_krige <- autoKrige(resid_rf ~ 1,
                         input_data = loc_sp,
                         new_data = rs_sp,
                         model = "Sph",
                         verbose = FALSE)
  # check results
  #plot(res_krige)
  
  # store predicted residuals
  resid_rs <- raster(res_krige$krige_output[1])
  names(resid_rs) <- "resid_rf_kr"
  
  # get predicted residual values for measurement points
  loc_sf <- get_value_from_raster(loc_sf, resid_rs)
  # calculate predicted waterdepth (RF prediction + kriged residuals)
  loc_sf <- mutate(loc_sf, pred_rf_kr = pred_rf + resid_rf_kr)
  
  #save(res_krige, resid_rs, file = paste0(iloc_project, "diepte/rf_6var.RData"))
  
  # store results
  ls_kr <- list()
  ls_kr$resid_rs <- resid_rs
  ls_kr$loc_sf <- loc_sf
  ls_kr$res_krige <- res_krige
  
  return(ls_kr)
  
}


#' Get EAG-median values of med_wd
#' 
#' @param loc_sf (sf object)
#' 
fun_eagmed <- function(loc_sf, var_res){
  # Make a raster of EAG-median values
  eag_med_wd <- raster_eag_med(as.data.table(loc_sf), var_res, water_eag_r, tb_eag)
  #tm_shape(eag_med_wd) + tm_raster(title = "WATDTE_m")  + tm_layout(legend.position = c("right","bottom"))
  
  loc_sf <- st_as_sf(loc_sf)
  # Get values for measurement points
  loc_sf <- get_value_from_raster(loc_sf, eag_med_wd)
  
  
  # store results
  ls_eag <- list()
  ls_eag$loc_sf <- loc_sf
  ls_eag$eag_med_wd <- eag_med_wd
  
  return(ls_eag)
  
}

#' Conduct step-wise multipvariate regression analysis
#' 
#' @param loc_t (data table) Data table which includes following columns: varres, set, and 'var_cov'
#' @param var_cov (CHAR)

fun_lm <- function(loc_t, var_cov){
  colslm <- c("varres", var_cov[ var_cov != "WATERTYPE"]) # exclude watertype
  lmres <- lm(varres ~ ., data = loc_t[set == "training", ..colslm])
  
  # step-wise selection
  model.step <- step(lmres, direction = "both", trace = 0)
  # store prediction
  loc_t[, pred_lm := predict(model.step, loc_t)]
  
  ls_lm <- list()
  ls_lm$loc_t <- loc_t
  ls_lm$model.step <- model.step
  
  return(ls_lm)
  
}