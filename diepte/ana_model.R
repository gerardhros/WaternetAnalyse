
rm(list = ls())

library(data.table); library(dplyr); library(sf); library(raster); library(tmap)
library(randomForest);library(ggplot2)

source('diepte/funs_datappr.R')
source('diepte/funs_rasterize.R')

## define folder names
# parent directory of one drive
fdnm <-  "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI"
iloc_onedrive <- paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")), "/", fdnm, "/")
# project folder of AGV
iloc_project <- paste0(iloc_onedrive, "NMI_Data - Documents/project/WaternetAnalyse/")


# Load location data (with all covariable values)
loc_sf <-  st_read(paste0(iloc_project, "diepte/loc_sf.gpkg")) %>% st_transform(28992)

# Load raster data of waterways, identified for each EAG. THis was made with 'ppr_raster.R'
load(paste0(iloc_project, "diepte/water_eag_r.RData"))
# Load table of EAG id's (used in the raster data)
tb_eag <- as.data.table(read.csv(file = paste0(iloc_project, "diepte/tb_eag.csv")))



# convert factor variables to factor
setDT(loc_sf)
fvar <- c("EAGIDENT", "GAFIDENT", "MORFOLOGIE", "WATERTYPE", "soiltypen" )
loc_sf[, (fvar) := lapply(.SD, as.factor), .SDcols = fvar]

# temp # check data availability
cols <- c("locatiecode", "med_wd", "med_wb", "med_sd", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", 
          "EAGIDENT", "GAFIDENT", "theo_dep", "pnt_ahn", "pnt_breedte", "breedte")
# check availability of data
Nrecord <- loc_sf[, lapply(.SD, function(x) length(x[!is.na(x) & x != ""])), .SDcols = cols]
Nrecord2 <- loc_sf[, lapply(.SD, function(x) length(x[!is.na(x)])), .SDcols = cols]

# choose variable to use
# note: GAF and EAG cannot be included because RF "Can not handle categorical predictors with more than 53 categories."
#var <- c("locatiecode", "med_wd", "med_wb", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE")
#var <- c("locatiecode", "med_wd", "breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "pnt_ahn", "med_sd")
#var <- c("locatiecode", "med_wd",  "breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "pnt_ahn", "med_sd", "theo_dep")
var <- c("locatiecode", "med_wd", "breedte", "MORFOLOGIE", "PEIL", "soiltypen", "KWEL", "WATERTYPE", "med_sd")

# Select complete records
loc_t <- loc_sf[complete.cases(loc_sf[, ..var]), ..var]


# Split training and test dataset
set.seed(123)
train <- sample(1:nrow(loc_t),nrow(loc_t) * 0.8)



## Step-wise linear regression ----------------
lmres <- lm(med_wd ~ ., data = loc_t[, !"locatiecode"])
# lmres2 <- lm(med_wd ~ med_wb + PEIL + soiltypen + KWEL, 
#              data = loc_c[MORFOLOGIE == "lijnvormig",])
# lmres3 <- lm(med_wd ~ med_wb + PEIL + soiltypen + KWEL + theo_dep, 
#              data = loc_c2[MORFOLOGIE == "lijnvormig",])
model.step <- step(lmres, direction = "both", trace = 0)


## Random forest -------------------

# Check OBB errors in errors in test set
nvar <- length(var) - 2 # number of explanatory variables
oob.err=double(nvar)
test.err=double(nvar)
#mtry is no of Variables randomly chosen at each split
for(mtry in 1:nvar)
{
  rf <- randomForest(med_wd ~ ., data = loc_t[, !"locatiecode"], subset = train, mtry=mtry, ntree=400)
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted

  pred <- predict(rf, loc_t[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry] = with(loc_t[-train,], mean( (med_wd - pred)^2)) #Mean Squared Test Error

  cat(mtry," ") #printing the output to the console

}
# plot errors
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),
        type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))


# final model
rf_res <- randomForest(med_wd ~ ., data = loc_t[, !"locatiecode"], subset = train,
                       mtry = 2, ntree = 400)
# plot error vs number of trees
plot(rf_res, main = "Error vs number of trees")
# plot importance of variables
varImpPlot(rf_res, main = "importance of variables") 
#prediction
loc_t[, pred_rf := predict(rf_res, loc_t)]
loc_t[, resid_rf := med_wd - pred_rf]
# add train or not
loc_t[train, set := "training"]
loc_t[!train, set := "test"]

# Compute RMSE and R2 of test set
rmse_rf <- calc_rmse(loc_sf[set == "test", med_wd], loc_sf[set == "test", pred_rf])
r2_rf <- calc_rsq(loc_sf[set == "test", med_wd], loc_sf[set == "test", pred_rf])


# merge prediction and residuals to sf object
loc_sf <- merge(loc_sf, loc_t[, .(locatiecode, pred_rf, resid_rf, set)], by = "locatiecode", all.x=T)
loc_sf_rf  <- loc_sf[!is.na(resid_rf),]
loc_sf_rf <- st_as_sf(loc_sf_rf)

tm_shape(loc_sf_rf) + tm_dots(col = "resid_rf", size = 0.2)

## kriging residuals -------------
# convert point shape to spatial object
loc_sp <- as(loc_sf_rf, "Spatial") %>% spTransform(CRS("+init=epsg:28992"))

# convert raster to spatial object
load(paste0(iloc_project, "diepte/water_eag_r.RData"))
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
loc_sf <- st_as_sf(loc_sf)
loc_sf <- get_value_from_raster(loc_sf, resid_rs)
# calculate predicted waterdepth (RF prediction + kriged residuals)
setDT(loc_sf)
loc_sf[, pred_rf_kr := pred_rf + resid_rf_kr]

# compute RMSE and R2 of test set
rmse_rf_kr <- calc_rmse(loc_sf[set == "test", med_wd], loc_sf[set == "test", pred_rf_kr])
r2_rf_kr <- calc_rsq(loc_sf[set == "test", med_wd], loc_sf[set == "test", pred_rf_kr])

#save(res_krige, resid_rs, file = paste0(iloc_project, "diepte/rf_6var.RData"))




## EAG-median model ----

eag_med_watdte <- raster_eag_med(as.data.table(loc_sf), "med_wd", water_eag_r, tb_eag)
names(eag_med_watdte) <- "eag_med_watdte"
#save(eag_med_watdte, file = paste0(iloc_project, "diepte/eag_med_watdte.RData"))
tm_shape(eag_med_watdte) + tm_raster(title = "WATDTE_m")  + tm_layout(legend.position = c("right","bottom"))

# Get values for measurement points
loc_sf <- st_as_sf(loc_sf)
loc_sf <- get_value_from_raster(loc_sf, eag_med_watdte)

# compute RMSE and R2 of test set
rmse_eagmedian <- calc_rmse(loc_sf[set == "test", eag_med_watdte][set == "test", med_wd], loc_sf[set == "test", eag_med_watdte])
r2_eagmedian <- calc_rsq(loc_sf[set == "test", eag_med_watdte][set == "test", med_wd], loc_sf[set == "test", eag_med_watdte])



ggplot(loc_sf[loc_sf$pred_rf != "",]) + 
  geom_point(aes(x = med_wd, y = eag_med_watdte, 
                 #col = MORFOLOGIE)) +
                 col = as.factor(set))) +
  labs(col = "") +
  xlim(c(0,2.5)) + 
  xlab("waterdiepte (m) gemeten") + ylab("waterdiepte (m) EAG-median")

ggplot(loc_sf) + geom_point(aes(x = med_wd, y = pred_rf_kr, col = as.factor(set))) +
  labs(col = "") +
  xlim(c(0,2.5))+
  xlab("waterdiepte (m) gemeten") + ylab("waterdiepte (m) RF model + kriging")

ggplot(loc_sf) + geom_point(aes(x = med_wd, y = pred_rf, col = as.factor(set))) +
  labs(col = "") +
  xlim(c(0,2.5))+
  xlab("waterdiepte (m) gemeten") + ylab("waterdiepte (m) RF model")

# # slib depth
# eag_med_slibdte <- raster_eag_med(loc_sf, "med_sd", water_eag_r, tb_eag)
# tm_shape(eag_med_slibdte) + tm_raster(title = "SLIBDTE_m")  + tm_layout(legend.position = c("right","bottom"))
# # total depth (water + slib)
# eag_med_totdte <- raster_eag_med(loc_sf, "med_td", water_eag_r, tb_eag)
# tm_shape(eag_med_totdte) + tm_raster(title = "TOTDTE_m")  + tm_layout(legend.position = c("right","bottom"))



# ## XGBoost
# require(xgboost)
# xgboost(data = as.matrix(loc_t[train,]), label = train, max.depth = 2, eta = 1, nthread = 2, nrounds = 2)
