
#'check if XY coordinates of database are all included in the location database
#'@return miss_loc (CHAR) a vector of names of location codes of loc_dt which are not included in loc_loc
check_missing_xy <- function(loc_dt, loc_loc, name_dt){
  nr_na <- length(loc_dt[!(loc_dt %in% loc_loc)])
  miss_loc <- unique(loc_dt)[!(unique(loc_dt) %in% unique(loc_loc))]
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


#' merge location info into wq
#' 
merge_wq <- function(locaties, wq, col_para){
  
  # check if all XY-coordinate of wq exist in locaties (otherwise, give a warning)
  miss_loc_wq<- check_missing_xy(wq$locatiecode, locaties$CODE, "wq")
  
  # check if there are completely identical rows. If so, remove them (after giving a warning)
  wq <- remove_duplicate(wq)
  
  # Merge location information
  db1 <- merge(wq[, ..col_para], 
               locaties[, .(CODE, NAAM, XCOORD, YCOORD, GAFIDENT, EAGIDENT, OWMIDENT)],
               by.x = "locatiecode", by.y = "CODE", all.x = TRUE)
  db1[, bron := "wq"]
  

  
  return(db1)
}

#' merge location info into hybi
#' 
merge_hybi <- function(locaties, hybi, col_para){
  # check if all XY-coordinate of hybi exist in locaties (otherwise, give a warning)
  miss_loc_hybi<- check_missing_xy(hybi$locatiecode, locaties$CODE, "hybi")
  
  # check if there is overlap in parameters between hybi and wq
  dup_para <- unique(hybi$fewsparameter)[unique(hybi$fewsparameter) %in% unique(wq$fewsparameter)]
  if(length(dup_para) > 0){
    print(paste0("WARNING: following parameters are included both in hybi and wq: ", paste(dup_para, collapse = ",")))
  }
  
  # check if there are completely identical rows. If so, remove them (after giving a warning)
  hybi <- remove_duplicate(hybi)
  
  # Merge location information
  db2 <- merge(hybi[, ..col_para], 
               locaties[, .(CODE, NAAM, XCOORD, YCOORD, GAFIDENT, EAGIDENT, OWMIDENT)],
               by.x = "locatiecode", by.y = "CODE", all.x = TRUE)
  db2[, bron := "hybi"]
  
  
  return(db2)
}

merge_ekr <- function(locaties, EKRset, hybi, col_para){
  
  # check if all XY-coordinate of EKR exist in locaties (otherwise, give a warning)
  miss_loc_ekr <- check_missing_xy(EKRset$locatiecode, locaties$CODE, "EKRset")
  
  
  # check if there are completely identical rows. If so, remove them (after giving a warning)
  EKRset <- remove_duplicate(EKRset)
  
  # Get date from hybi (because the datum of EKRset is not correct)
  EKRset[, datum_ekr := datum][, datum := NULL]
  # dcast hybi
  # TO DO: check if there are more than 1 date per year
  hybi_dc <- dcast(hybi[, .(locatiecode, datum, jaar)], locatiecode ~ jaar, value.var = 'datum', fun.aggregate = last)
  hybi_m <- melt(hybi_dc, id.vars = "locatiecode", variable.name = "jaar", value.name = "datum", na.rm = TRUE)
  hybi_m$jaar <- as.integer(as.character(hybi_m$jaar))
  # add correct datum (Here, records whose locations don't exist in hybi are excluded.)
  EKRset2 <- merge(EKRset, hybi_m, 
                   by = c("locatiecode", "jaar"), all.x = FALSE)
  
  # Merge location information
  db3 <- merge(EKRset2[, ..col_para], 
               locaties[, .(CODE, NAAM, XCOORD, YCOORD, GAFIDENT, EAGIDENT, OWMIDENT)],
               by.x = "locatiecode", by.y = "CODE", all.x = TRUE)
  db3[, bron := "ekr"]

  return(db3)  
}

  