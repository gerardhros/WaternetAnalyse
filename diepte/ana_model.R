# This scripts compares different models to interpolate water depth
# and make the final prediction of water depth (as raster)

rm(list = ls())

library(data.table); library(dplyr); library(sf); library(raster); library(tmap)
library(randomForest);library(ggplot2)
library(automap); library(cowplot)


# set default theme of ggplot
theme_set(theme_light() + theme(legend.position = "right"))


source('diepte/funs_datappr.R')
source('diepte/funs_rasterize.R')
source('diepte/funs_model.R')
source('diepte/fun_iloc.R') 

## define folder names
iloc <- fun_iloc(fdnm = "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI")
iloc_onedrive <- iloc[1]
iloc_project <- iloc[2]
iloc_afk <- iloc[3]


# Load location data (with all covariable values)
loc_sf <-  st_read(paste0(iloc_project, "diepte/loc_sf.gpkg")) %>% st_transform(28992)

# Load raster data of waterways, identified for each EAG. THis was made with 'ppr_raster.R'
load(paste0(iloc_project, "diepte/water_eag_r.RData"))
# Load table of EAG id's (used in the raster data)
tb_eag <- as.data.table(read.csv(file = paste0(iloc_project, "diepte/tb_eag.csv")))



## Preparation ----------------
# Column with point ID names
var_id <- "locatiecode"

# fraction of training set
fr_train <- 0.8

# Whether kriging of residuals is conducted (this takes ca. 12 min extra)
run_krige <- TRUE

##  Define response & explanatory variables ----------------
## water depth
var_res <- "med_wd"
var_res_nm <- "waterdiepte (m)"
#var_cov <-  c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "med_sd")
#var_cov <- c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "med_sd", "pnt_ahn", "theo_dep")
var_cov <- c("breedte", "PEIL", "soiltypen", "KWEL", "pnt_ahn", "A_OS_GV") # variables which are available as area-covering raster
# var_cov <- c("breedte", "PEIL", "soiltypen", "KWEL", "pnt_ahn", "A_OS_GV", "med_sd") # variables which are available as area-covering raste + slibdiepte

# ## slib depth
#  var_res <- "med_sd"
#  var_res_nm <- "slibdiepte (m)"
# # var_cov <- c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "afw_ahn", "A_OS_GV")
#  var_cov <- c("breedte", "PEIL", "soiltypen", "KWEL", "pnt_ahn", "A_OS_GV")# variables which are available as area-covering raster


# ## total depth
# var_res <- "med_td"
# var_res_nm <- "water + slib diepte (m)"
# var_cov <-c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "afw_ahn", "A_OS_GV")
#  
# ## deviation from peil
# var_res <- "afw_ahn"
# var_res_nm <- "afwijking van peil (m)"
# var_cov <-c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "A_OS_GV", "med_sd")
# 


# compare_models <- function(loc_sf, var_id, var_res, var_cov, fr_train, run_krige, water_eag_r, tb_eag){


# make a dataset with relevant columns and rows, split test & training sets
loc_t <- ppr_dataset(loc_sf, var_id, var_res, var_cov, fr_train)

## Random forest --------
# Run random forest
ls_rf <- fun_rf(loc_t, var_id, var_cov, 
                check_mtry = FALSE, 
                #check_mtry = TRUE, # Checking differetn mtry. this costs extra 30 sec
                mtry = round(length(var_cov)/3, 0),
                ntree = 400)



# merge prediction and residuals to sf object
cols <- c("locatiecode", "pred_rf", "resid_rf", "set")
loc_sf <- merge(loc_sf, ls_rf$loc_t[, ..cols], by = "locatiecode", all.x = T)

# compute RMSE and R2 of test set
test_rmse_rf <- test_rmse(loc_sf, var_res, "pred_rf")
test_r2_rf <- test_r2(loc_sf, var_res, "pred_rf")


## kriging residuals -------------
if(run_krige == TRUE){
  ls_kr <- fun_krige(loc_sf, water_eag_r) # this costs ca. 12 minutes
  loc_sf <- ls_kr$loc_sf
  
  # show result of kriging
  plot(ls_kr$res_krige)

# compute RMSE and R2 of test set
test_rmse_kr <- test_rmse(loc_sf, var_res, "pred_rf_kr")
test_r2_kr <- test_r2(loc_sf, var_res, "pred_rf_kr")
}


## Step-wise linear regression ----------------

ls_lm <- fun_lm(loc_t, var_cov)

# merge prediction of lm to loc_sf
cols = c("locatiecode", "pred_lm")
loc_sf <- merge(loc_sf, ls_lm$loc_t[, ..cols], by = "locatiecode", all.x = T)

# compute RMSE and R2 of test set
test_rmse_lm <- test_rmse(loc_sf, var_res, "pred_lm")
test_r2_lm <- test_r2(loc_sf, var_res, "pred_lm")


## EAG-median model -----------
# Get EAG-median values of med_wd for measurement points
ls_eag <- fun_eagmed(loc_sf, var_res, water_eag_r, tb_eag)
loc_sf <- ls_eag$loc_sf

# compute RMSE and R2 of test set
test_rmse_eag <- test_rmse(loc_sf, var_res, paste0("eag_", var_res))
test_r2_eag <- test_r2(loc_sf, var_res, paste0("eag_", var_res))



## Visualisation of models -----------

gp_rf <- ggplot(loc_sf) + 
  geom_point(aes_string(x = var_res, y = "pred_rf", colour = "set")) +
  xlim(c(0,3))+
  xlab(paste0(var_res_nm, " gemeten")) + ylab(paste0(var_res_nm, " model")) +
  labs(title = "RF model",
       subtitle = paste0("R2 test set: ", round(test_r2_rf, 2)),
       col = "")

if(run_krige == TRUE){
  gp_kr <- ggplot(loc_sf) + 
    geom_point(aes_string(x = var_res, y = "pred_rf_kr", colour = "set")) +
    xlim(c(0,3))+
    xlab(paste0(var_res_nm, " gemeten")) + ylab(paste0(var_res_nm, " model")) +
    labs(title = "RF model + residual kriging",
         subtitle = paste0("R2 test set: ", round(test_r2_kr, 2)),
         col = "")
}

gp_lm <- ggplot(loc_sf) + 
  geom_point(aes_string(x = var_res, y = "pred_lm", colour = "set")) +
  xlim(c(0,3))+
  xlab(paste0(var_res_nm, " gemeten")) + ylab(paste0(var_res_nm, " model")) +
  labs(title = "Linear model",
       subtitle = paste0("R2 test set: ", round(test_r2_lm, 2)),
       col = "")

gp_eag <- ggplot(loc_sf) + 
  geom_point(aes_string(x = var_res, y = paste0("eag_", var_res), colour = "set")) +
  xlim(c(0,3)) + 
  xlab(paste0(var_res_nm, " gemeten")) + ylab(paste0(var_res_nm, " model")) +
  labs(title = "EAG-median",
       subtitle = paste0("R2 test set: ", round(test_r2_eag, 2)),
       col = "")

if(run_krige == TRUE){
  gp <- plot_grid(gp_lm, gp_eag, gp_rf, gp_kr, ls_rf$gp_imp, ncol=2)
} else {
  gp <- plot_grid(gp_lm, gp_eag, gp_rf, ls_rf$gp_imp, ncol=2)
}
show(gp)


## Make a raster data of predicted water depth by random forest model -------------
# change layer name of "pnt_breedte" to "breedte" (the variable name used in the random forest model)
names(covars_rs)[5] <- 'breedte'
# make spatial prediction
pred_wd_rf_rs <- raster::predict(model=ls_rf$rf_res, object=covars_rs)

##  Combine Random forest model en EAG-median model ------------
# change resolution ofEAG-mean (to match RF results)
pred_wd_eag <- disaggregate(ls_eag$eag_med_wd, fact = 4)

# When the prediction of RF is NA, use the EAG-median value
pred_wd_rs <- overlay(pred_wd_rf_rs,pred_wd_eag, fun = function(x, y) {
  x <- ifelse(is.na(x), y, x)
  return(x)
})




#return(list(var_res, var_cov, ls_lm, ls_rf, ls_eag, ls_kr, loc_sf, gp))

# save(var_res, var_cov, ls_lm, ls_rf, ls_eag, ls_kr, loc_sf, gp,
#      file = paste0(iloc_project, "diepte/temp_models_med_sd_6var_rasteravailable.RData"))

# }
# 
# compare_models(loc_sf, var_id, var_res, var_cov, fr_train, run_krige, water_eag_r, tb_eag)
#   


