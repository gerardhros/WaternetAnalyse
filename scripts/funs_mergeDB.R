
#'check if XY coordinates of database are all included in the location database
#'@return miss_loc (CHAR) a vector of names of location codes of loc_dt which are not included in loc_loc
check_missing_xy <- function(loc_dt, loc_loc, name_dt){
  nr_na <- length(loc_dt[is.na(match(loc_dt, loc_loc))])
  miss_loc <- unique(loc_dt)[is.na(match(unique(loc_dt), unique(loc_loc)))]
  if(length(miss_loc) > 0){
    print(paste0("WARNING: ", length(miss_loc), " location codes of dataset ", name_dt, " don't exist in dataset locaties."))
    print(paste0("Missing locations are: ", ifelse(length(miss_loc) <= 5, 
                                                   paste(miss_loc, collapse= ","),
                                                   paste0(paste(miss_loc[1:5], collapse= ", "), " etc."))))
    return(miss_loc)
  }
}

#' Check if there are completely identical rows. If so, remove them (after giving a warning)
remove_duplicate <- function(dt){
  dt_uni <- unique(dt)
  if(nrow(dt) != nrow (dt_uni)){
    print(paste0("There are ", nrow(dt) - nrow (dt_uni), " completely identical rows."))
    print("These rows were removed.")
  }
  return(dt_uni)
}

#' Make a table with area median values
#' 
#'@param bod (data table) data table of slootbodemanalyse. 
#'@param locaties (data table)
#'@param col_para (CHAR) column names which will be included in the output data table
#'@param col_paraid (CHAR) column name of bod in which parameter name is stored.
#'@param ave_area (CHAR) Column name of database locaties for area ID code. This is primarily used to calculate median.
#'@param ave_area2 (CHAR) Column name of database locaties for area ID code. This is secondly used to calculate median.

#'@return bod_e (data table) data table of area-median values of parameter 'col_paraid', for all locations listed in locaties.
#'
fun_areaave_bod <- function(bod, locaties, col_para,
                        col_paraid = "parm.fews", 
                        ave_area = "EAGIDENT", ave_area2 = "GAFIDENT"){
  
  # change column name
  setnames(bod, old = c(col_paraid), new = c("parm.fews"))
  setnames(locaties, old = c(ave_area, ave_area2), new = c("area1", "area2"))
  
  # check, for each parameter, if same unit is used for all records
  checkunit <- function(x){
    unit <- unique(x)
    if(length(unit) == 1){ unit <- unit} else {unit <- "variable"}
  }
  # make a table of parameters & unit
  unit_tb <- bod[, .(eenheid= checkunit(eenheid)), by = parm.fews]
  if(length(bod$eenheid_ag[bod$eenheid_ag == "variable"])>1){
    print(paste0("WARNING: Following parameters contain more than 1 units: ",
                 paste(bod$parm.fews[bod$eenheid_ag == "variable"], collapse = ",")))
  }
  
  # merge area IDs to bod
  bod <- merge(bod, locaties[, .(CODE, area1, area2)], by.x = "loc.code", by.y = "CODE")
  
  # Compute median values by area type 1
  bod_med <- bod[, .(med = median(meetwaarde, na.rm = TRUE)), by = .(parm.fews, area1)]
  # Compute median values by area type 1
  bod_med2 <- bod[, .(med2 = median(meetwaarde, na.rm = TRUE)), by = .(parm.fews, area2)] 
  # Compute median values of all records
  bod_med3 <- bod[, .(med3 = median(meetwaarde, na.rm = TRUE)), by = .(parm.fews)] 
  
  # Make a data table of locations x parameters (all combinations)
  bod_e <- CJ(CODE = unique(locaties$CODE),
              parm.fews = unique(bod$parm.fews))
  # add area IDs
  bod_e <- merge(bod_e, locaties[, .(CODE, area1, area2)], by = "CODE")
  # add unit
  bod_e <- merge(bod_e,  unit_tb, by = "parm.fews")
  
  # merge median values to location x parameter table
  bod_e <- merge(bod_e, bod_med, by = c("area1", "parm.fews"), all.x = T)
  bod_e <- merge(bod_e, bod_med2, by = c("area2", "parm.fews"), all.x = T)
  bod_e <- merge(bod_e, bod_med3, by = c("parm.fews"), all.x = T)
  
  # use median of area1, otherwise median of area2 otherwise median of all data
  bod_e[, meetwaarde := ifelse(!is.na(med), med, ifelse(!is.na(med2), med2, med3))]

  ## Match column names with other datasets
  # change column names
  setnames(bod_e, old = c("CODE", "parm.fews"), new = c("locatiecode", "fewsparameter"))
  # add other necessary column names
  cols <- col_para[is.na(match(col_para, names(bod_e)))]
  bod_e[, (cols) := NA]
  # remove unnecessary column names
  cols2 <- names(bod_e)[is.na(match(names(bod_e), col_para))]
  bod_e[, (cols2) := NULL]
  # order columns
  setcolorder(bod_e, col_para)
  
  return(bod_e)
}

# merge water and stoffen balans based on EAG, GAF, 
fun_areamerge_dat <- function(dat, locaties, col_dat_para){
  
  ## Check if all locaions in 'locaties' have corresponding records in dat
  # Check if EAG IDs of 'locaties' exist in 'dat'
  EAG_indat <- unique(locaties$EAGIDENT)[!is.na(match(unique(locaties$EAGIDENT), unique(dat$EAG)))]
  # Check  if  GAF IDs of 'locaties' exist in 'dat'
  GAF_indat <- unique(locaties$GAFIDENT)[!is.na(match(unique(locaties$GAFIDENT), unique(dat$GAF)))]
  # Check if WL IDs of 'locaties' exist in 'dat'
  WL_indat <- unique(locaties$OWMIDENT)[!is.na(match(unique(locaties$OWMIDENT), unique(dat$KRW)))]
  
  # Give warning if some locations in 'locaties' miss corresponding records in dat
  miss_dat <- locaties[is.na(match(EAGIDENT,  EAG_indat)) & is.na(match(GAFIDENT, GAF_indat)) & is.na(match(OWMIDENT,WL_indat)),]
  if(nrow(miss_dat > 0)){
    print(paste0("WARNING: ", nrow(miss_dat)," (out of ",  nrow(locaties), ") locations of dataset locaties don't have corresonding data in dataset dat."))
  }
  
  
  
  # Melt 'dat' by parameter 
  dat_dc <- melt(dat, id.vars = c("EAG", "GAF", "KRW", "date"), measure.vars = col_dat_para, 
                 variable.name = "parm.fews", value.name = "meetwaarde")
  # Remove rows with meetwaarde = NA
  dat_dc <- dat_dc[!is.na(meetwaarde),]
  
  # Make a data table of locations x parameters x date (all combinations)
  dat_e <- CJ(CODE = unique(locaties$CODE),
              parm.fews = col_dat_para)
              #date = unique(dat$date))  # -> Error: cannot allocate vector of size 749.1 Mb
  
  # Add EAG, GAF, and WL
  dat_e <- merge(dat_e, locaties[, .(CODE, EAGIDENT, GAFIDENT, OWMIDENT)], by = "CODE")
  
  # use identical class for GAF
  dat_e$GAFIDENT <- as.character(dat_e$GAFIDENT)
  dat_dc$GAF <- as.character(dat_dc$GAF)
  
  # Join parameter values based on EAG, only for the locations whose location CODE are included in dat
  # when data of multiple dates are available, they are added as separate rows. 
  dat_e_eag <- merge(dat_e[!is.na(match(EAGIDENT, EAG_indat)), ], dat_dc, 
                by.x = c("parm.fews", "EAGIDENT"), by.y = c("parm.fews", "EAG"), all.x = T, allow.cartesian=TRUE)
  
  # Join parameter values based on GAF, only for the locations whose location CODE are included in dat
  dat_e_gaf <- merge(dat_e[is.na(match(EAGIDENT, EAG_indat)) & !is.na(match(GAFIDENT, GAF_indat)), ], dat_dc, 
                     by.x = c("parm.fews", "GAFIDENT"), by.y = c("parm.fews", "GAF"), all.x = T, allow.cartesian=TRUE)
  
  # Join parameter values based on WL, only for the locations whose location CODE are included in dat
  dat_e_wl <- merge(dat_e[is.na(match(EAGIDENT, EAG_indat)) & is.na(match(GAFIDENT, GAF_indat)) & !is.na(match(OWMIDENT, WL_indat)), ], 
                    dat_dc, 
                     by.x = c("parm.fews", "OWMIDENT"), by.y = c("parm.fews", "KRW"), all.x = T, allow.cartesian=TRUE)
  
}