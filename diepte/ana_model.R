
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

## define folder names (iloc_onedrive, iloc_project, iloc_afk)
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

# temp # check data availability
cols <- c("locatiecode", "med_wd", "med_wb", "med_sd", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", 
          "EAGIDENT", "GAFIDENT", "theo_dep", "pnt_ahn", "pnt_breedte", "breedte", "afw_ahn")
# check availability of data
setDT(loc_sf)
Nrecord <- loc_sf[, lapply(.SD, function(x) length(x[!is.na(x) & x != ""])), .SDcols = cols]
Nrecord2 <- loc_sf[, lapply(.SD, function(x) length(x[!is.na(x)])), .SDcols = cols]


## Preparation ----------------
# Column with point ID names
var_id <- "locatiecode"

# fraction of training set
fr_train <- 0.8

# Define response & explanatory variables
# water depth
var_res <- "med_wd"
var_res_nm <- "waterdiepte (m)"
#var_cov <-  c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "med_sd")
#var_cov <- c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "med_sd", "pnt_ahn", "theo_dep")
var_cov <- c("breedte", "PEIL", "soiltypen", "KWEL", "pnt_ahn", "A_OS_GV") # variables which are available as area-covering raster
# var_cov <- c("breedte", "PEIL", "soiltypen", "KWEL", "pnt_ahn", "A_OS_GV", "med_sd") # variables which are available as area-covering raste + slibdiepter

# # # slib depth
#  var_res <- "med_sd"
#  var_res_nm <- "slibdiepte (m)"
# # var_cov <- c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "afw_ahn", "A_OS_GV")
#  var_cov <- c("breedte", "PEIL", "soiltypen", "KWEL", "pnt_ahn", "A_OS_GV")# variables which are available as area-covering raster

# 
# # total depth
# var_res <- "med_td"
# var_res_nm <- "water + slib diepte (m)"
# var_cov <-c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "afw_ahn", "A_OS_GV")
# 
# # deviation from peil
# var_res <- "afw_ahn"
# var_res_nm <- "afwijking van peil (m)"
# var_cov <-c("breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "A_OS_GV", "med_sd")

loc_t <- ppr_dataset(loc_sf, var_id, var_res, var_cov, fr_train)

## Random forest --------
# Run random forest
ls_rf <- fun_rf(loc_t, var_id, var_cov, 
                check_mtry = FALSE, 
                #check_mtry = TRUE, # Checking differetn mtry. this costs extra 30 sec
                mtry = round(length(var_cov)/3, 0),
                ntree = 400)

# show  figure of variable importance
varImpPlot(ls_rf$rf_res, main = "importance of variables")
# # show figure of error vs number of trees
# plot(rf_res, main = "Error vs number of trees")

# merge prediction and residuals to sf object
cols <- c("locatiecode", "pred_rf", "resid_rf", "set")
loc_sf <- merge(loc_sf, ls_rf$loc_t[, ..cols], by = "locatiecode", all.x = T)

# compute RMSE and R2 of test set
test_rmse_rf <- test_rmse(loc_sf, var_res, "pred_rf")
test_r2_rf <- test_r2(loc_sf, var_res, "pred_rf")


## kriging residuals -------------
ls_kr <- fun_krige(loc_sf, water_eag_r) # this costs ca. 12 minutes
loc_sf <- ls_kr$loc_sf

# show result of kriging
plot(ls_kr$res_krige)

# compute RMSE and R2 of test set
test_rmse_kr <- test_rmse(loc_sf, var_res, "pred_rf_kr")
test_r2_kr <- test_r2(loc_sf, var_res, "pred_rf_kr")



## Step-wise linear regression ----------------

ls_lm <- fun_lm(loc_t, var_cov)

# merge prediction of lm to loc_sf
cols = c("locatiecode", "pred_lm")
loc_sf <- merge(loc_sf, ls_lm$loc_t[, ..cols], by = "locatiecode", all.x = T)

# compute RMSE and R2 of test set
test_rmse_lm <- test_rmse(loc_sf, var_res, "pred_lm")
test_r2_lm <- test_r2(loc_sf, var_res, "pred_lm")


## EAG-median model -----------

ls_eag <- fun_eagmed(loc_sf, var_res)

loc_sf <- ls_eag$loc_sf

# compute RMSE and R2 of test set
test_rmse_eag <- test_rmse(loc_sf, var_res, paste0("eag_", var_res))
test_r2_eag <- test_r2(loc_sf, var_res, paste0("eag_", var_res))



## Visualisation of models -----------
loc_sf$set <- as.factor(loc_sf$set)
gp_rf <- ggplot(loc_sf) + 
  geom_point(aes_string(x = var_res, y = "pred_rf", colour = "set")) +
  xlim(c(0,3))+
  xlab(paste0(var_res_nm, " gemeten")) + ylab(paste0(var_res_nm, " model")) +
  labs(title = "RF model",
       subtitle = paste0("R2 test set: ", round(test_r2_rf, 2)),
       col = "")

gp_kr <- ggplot(loc_sf) + 
  geom_point(aes_string(x = var_res, y = "pred_rf_kr", colour = "set")) +
  xlim(c(0,3))+
  xlab(paste0(var_res_nm, " gemeten")) + ylab(paste0(var_res_nm, " model")) +
  labs(title = "RF model + residual kriging",
       subtitle = paste0("R2 test set: ", round(test_r2_kr, 2)),
       col = "")

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

gp <- plot_grid(gp_rf, gp_kr, gp_lm, gp_eag, ncol=2)
show(gp)

# save(var_res, var_cov, ls_lm, ls_rf, ls_eag, ls_kr, loc_sf, gp,
#      file = paste0(iloc_project, "diepte/temp_models_med_sd_6var_rasteravailable.RData"))


# # slib depth
# eag_med_slibdte <- raster_eag_med(loc_sf, "med_sd", water_eag_r, tb_eag)
# tm_shape(eag_med_slibdte) + tm_raster(title = "SLIBDTE_m")  + tm_layout(legend.position = c("right","bottom"))
# # total depth (water + slib)
# eag_med_totdte <- raster_eag_med(loc_sf, "med_td", water_eag_r, tb_eag)
# tm_shape(eag_med_totdte) + tm_raster(title = "TOTDTE_m")  + tm_layout(legend.position = c("right","bottom"))



# ## XGBoost
# require(xgboost)
# xgboost(data = as.matrix(loc_t[train,]), label = train, max.depth = 2, eta = 1, nthread = 2, nrounds = 2)
