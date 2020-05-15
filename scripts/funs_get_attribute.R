

#' Make a table with area median values of slootbodem analysis
#' 
#'@param bod (data table) data table of slootbodemanalyse. 
#'The location IDs should be stored in the column 'loc.code'.
#'@param locaties (data table) data table of location information, in which all location IDs of 'bod' are included. 
#'The location IDs should include following columns: CODE, NAAM, XCOORD, YCOORD, GAFIDENT, EAGIDENT, OWMIDENT
#'@param col_para (CHAR) column names which will be included in the output data table
#'@param loc_new (CHAR) vector of location ID's for which area average is calculated. All IDs need to be included in locaties$CODE.
#'@param col_paraid (CHAR) column name of bod in which parameter ID name is stored.
#'@param ave_area (CHAR) Column name of database locaties for area ID code. This is primarily used to calculate median.
#'@param ave_area2 (CHAR) Column name of database locaties for area ID code. This is secondly used to calculate median.

#'@return bod_e (data table) data table of area-median values of parameters 'col_paraid', for all point locations specified in "loc_new".
#'The number of row is number of locations x number of parameters (i.e. length(loc_new) x length(unique(bod$parm.fews))).
#'Included columns are those defined with 'col_para'
#'
fun_areaave_bod <- function(bod, locaties, col_para,
                            loc_new = unique(locaties$CODE), 
                            col_paraid = "parm.fews", 
                            ave_area = "EAGIDENT", ave_area2 = "GAFIDENT"){
  bod <- copy(bod)
  locaties <- copy(locaties)
  
  # check if all XY-coordinate of bod exist in locaties
  check_missing_xy(bod$loc.code, locaties$CODE, "bod")
  
  # Check if all new location ID's are included in locatie$CODE
  miss_loc_bod <- check_missing_xy(loc_new, locaties$CODE, "new location")
  
  # change column name
  setnames(bod, old = c(col_paraid), new = c("parm.fews"))
  # duplicate area columns with different name
  locaties[, area1 := mget(ave_area)]
  locaties[, area2 := mget(ave_area2)]
  
  # check, for each parameter, consistency of attribute (e.g. if the same unit is used for all records)
  checkunit <- function(x){
    unit <- unique(x)
    if(length(unit) == 1){ unit <- unit} else {unit <- "variable"}
  }
  # make a table of parameters & unit
  unit_tb <- bod[, .(eenheid= checkunit(eenheid)), by = parm.fews]
  if(unit_tb[eenheid == "variable", .N] > 1){
    print(paste0("WARNING: Following parameters contain more than 1 units: ",
                 paste(unit_tb[eenheid == "variable", parm.fews], collapse = ",")))
  }
  # make a table of parameters & parameter names
  paranaam_tb <- bod[, .(fewsparameternaam= checkunit(fewsparameternaam)), by = parm.fews]
  
  
  # merge area IDs to bod
  bod <- merge(bod, locaties[, .(CODE, area1, area2)], by.x = "loc.code", by.y = "CODE")
  
  # Compute median values by area type 1
  bod_med <- bod[, .(med = median(meetwaarde, na.rm = TRUE)), by = .(parm.fews, area1)]
  # Compute median values by area type 2
  bod_med2 <- bod[, .(med2 = median(meetwaarde, na.rm = TRUE)), by = .(parm.fews, area2)] 
  # Compute median values of all records
  bod_med3 <- bod[, .(med3 = median(meetwaarde, na.rm = TRUE)), by = .(parm.fews)] 
  
  # Make a data table of locations x parameters (all combinations)
  bod_e <- CJ(CODE = loc_new,
              parm.fews = unique(bod$parm.fews))
  # add area IDs and location information
  bod_e <- merge(bod_e, locaties[, .(CODE, area1, area2, NAAM, XCOORD, YCOORD, GAFIDENT, EAGIDENT, OWMIDENT)], by = "CODE")
  # add unit
  bod_e <- merge(bod_e,  unit_tb, by = "parm.fews")
  # add parameter name
  bod_e <- merge(bod_e,  paranaam_tb, by = "parm.fews")

  
  
  # merge median values to location x parameter table
  bod_e <- merge(bod_e, bod_med, by = c("area1", "parm.fews"), all.x = T)
  bod_e <- merge(bod_e, bod_med2, by = c("area2", "parm.fews"), all.x = T)
  bod_e <- merge(bod_e, bod_med3, by = c("parm.fews"), all.x = T)
  
  # use median of area1, otherwise median of area2 otherwise median of all data
  bod_e[, meetwaarde := ifelse(!is.na(med), med, ifelse(!is.na(med2), med2, med3))]
  
  ## Match column names with other datasets
  # change column names
  setnames(bod_e, old = c("CODE", "parm.fews", "NAAM"), new = c("locatiecode", "fewsparameter",  "locatie_naam"))
  # add source
  bod_e[, bron := paste0("bod_", ave_area, "_median")]
  # add other necessary column names
  cols <- col_para[!(col_para %in% names(bod_e))]
  bod_e[, (cols) := NA]
  # remove unnecessary column names
  cols2 <- names(bod_e)[!(names(bod_e) %in% col_para)]
  bod_e[, (cols2) := NULL]
  # order columns
  setcolorder(bod_e, col_para)
  
  return(bod_e)
}

#' Merge water and stoffen balans based on EAG, GAF, or WL
#' 
#' @param dat (data table) data table of water and element balance. 
#'@param locaties (data table) data table of location information, in which all location IDs of 'bod' are included. 
#'The location IDs should include following columns: CODE, NAAM, XCOORD, YCOORD, GAFIDENT, EAGIDENT, OWMIDENT
#'@param col_dat_para (CHAR) a vector of column names of dat (parameter names), whose value will be stored (in melted form) in the output table
#'@param col_para (CHAR) column names which will be included in the output data table
#'@param loc_new (CHAR) vector of location ID's which are to be included in the output table.
#'@param jaar_sel (INT) years of records which are included in the output table. When jaar_sel = NULL, all years are taken.

#'@return dat_e_agg (data table) data table of water balance data, for all point locations specified in "loc_new".
#'The number of row is number of locations x dates x number of parameters (i.e. at maximum, length(loc_new) x length(unique(dat$date)) x length(col_dat_para)).
#'Included columns are those defined with 'col_para'
#'
fun_areamerge_dat <- function(dat, locaties, col_dat_para, col_para, 
                              loc_new = unique(locaties$CODE), jaar_sel = NULL){
  
  dat <- copy(dat)
  locaties <- copy(locaties)
  
  # choose only specific years
  if(!is.null(jaar_sel)){
    dat <- dat[jaar %in% jaar_sel,]
  }
  
  # choose locations to get dat values
  locaties <- locaties[CODE %in% loc_new,]
  
  
  ## Check if all locaions in 'locaties' have d records in dat
  # Check if EAG IDs of 'locaties' exist in 'dat'
  EAG_indat <- unique(locaties$EAGIDENT)[unique(locaties$EAGIDENT) %in% unique(dat$EAG)]
  # Check  if  GAF IDs of 'locaties' exist in 'dat'
  GAF_indat <- unique(locaties$GAFIDENT)[unique(locaties$GAFIDENT) %in% unique(dat$GAF)]
  # Check if WL IDs of 'locaties' exist in 'dat'
  WL_indat <- unique(locaties$OWMIDENT)[unique(locaties$OWMIDENT) %in% unique(dat$KRW)]
   
  # Give warning if some locations in 'locaties' miss corresponding records in dat
  miss_dat <- locaties[!(EAGIDENT %in%  EAG_indat) & !(GAFIDENT %in% GAF_indat) & !(OWMIDENT %in% WL_indat),]
  if(nrow(miss_dat > 0)){
    print(paste0("WARNING: ", nrow(miss_dat)," (out of ",  nrow(locaties), ") locations of dataset locaties don't have corresonding data in dataset dat."))
  }
  
  # Melt 'dat' by parameter 
  dat_dc <- melt(dat, id.vars = c("EAG", "GAF", "KRW", "date", "pol"), measure.vars = col_dat_para, 
                 variable.name = "parm.fews", value.name = "meetwaarde")
  # Remove rows with meetwaarde = NA
  dat_dc <- dat_dc[!is.na(meetwaarde),]
  
  # Make a data table of locations x parameters x date (all combinations)
  dat_e <- CJ(CODE = loc_new,
              parm.fews = col_dat_para)
  
  # Add EAG, GAF, and WL
  dat_e <- merge(dat_e, locaties[, .(CODE, NAAM, XCOORD, YCOORD, EAGIDENT, GAFIDENT, OWMIDENT)], by = "CODE")
  
  # use identical data class for GAF
  dat_e$GAFIDENT <- as.character(dat_e$GAFIDENT)
  dat_dc$GAF <- as.character(dat_dc$GAF)
  
  # Join parameter values based on EAG, only for the locations whose location CODE are included in dat
  # when data of multiple dates are available, they are added as separate rows. 
  dat_e_eag <- merge(dat_e[EAGIDENT %in% EAG_indat, ], dat_dc, 
                     by.x = c("parm.fews", "EAGIDENT"), by.y = c("parm.fews", "EAG"), all.x = T, allow.cartesian=TRUE)
  dat_e_eag[, (c("GAF", "KRW")) := NULL]
  dat_e_eag[, join_level := "EAG"]
  print(paste0("Water balance data of ",  length(locaties$CODE[locaties$EAGIDENT %in% EAG_indat]), 
               " locations were joined based on EAG,"))
  
  # Join parameter values based on GAF, only for the locations whose location CODE are included in dat
  dat_e_gaf <- merge(dat_e[!(EAGIDENT %in% EAG_indat) & GAFIDENT %in% GAF_indat, ], dat_dc, 
                     by.x = c("parm.fews", "GAFIDENT"), by.y = c("parm.fews", "GAF"), all.x = T, allow.cartesian=TRUE)
  dat_e_gaf[, (c("EAG", "KRW")) := NULL]
  dat_e_gaf[, join_level := "GAF"]
  print(paste0("Water balance data of ", 
               length(locaties$CODE[!(locaties$EAGIDENT %in% EAG_indat) & locaties$GAFIDENT %in% GAF_indat]),
               " locations were joined based on GAF"))
  
  # Join parameter values based on WL, only for the locations whose location CODE are included in dat
  dat_e_wl <- merge(dat_e[!(EAGIDENT %in% EAG_indat) & !(GAFIDENT %in% GAF_indat) & OWMIDENT %in% WL_indat, ], 
                    dat_dc, 
                    by.x = c("parm.fews", "OWMIDENT"), by.y = c("parm.fews", "KRW"), all.x = T, allow.cartesian=TRUE)
  dat_e_wl[, (c("EAG", "GAF")) := NULL]
  dat_e_wl[, join_level := "WL"]
  print(paste0("Water balance data of ", 
               length(locaties$CODE[!(locaties$EAGIDENT %in% EAG_indat) & !(locaties$GAFIDENT %in% GAF_indat) & 
                                      locaties$OWMIDENT %in% WL_indat]),
               " locations were joined based on WL"))
  
  # combine database
  dat_e_agg <- rbind(dat_e_eag, dat_e_gaf, dat_e_wl)
  
  ## Match column names with other datasets
  # change column names
  setnames(dat_e_agg, old = c("CODE", "parm.fews", "NAAM", "date", "pol"), 
           new = c("locatiecode", "fewsparameter",  "locatie_naam", "datum", "monsterident"))
  # add source
  dat_e_agg[, bron := paste0("dat_", join_level)]
  # add other necessary column names
  cols <- col_para[!(col_para %in% names(dat_e_agg))]
  if(length(cols)>0) {dat_e_agg[, (cols) := NA]}
  # remove unnecessary column names
  cols2 <- names(dat_e_agg)[!(names(dat_e_agg) %in% col_para)]
  if(length(cols2)>0) {dat_e_agg[, (cols2) := NULL]}
  # order columns
  setcolorder(dat_e_agg, col_para)
  
  # remove NA records
  dat_e_agg <- dat_e_agg[!is.na(meetwaarde), ]
  
  return(dat_e_agg)
  
}