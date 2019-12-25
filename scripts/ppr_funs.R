# preprocess functions to clean up data.bases
# Laura Moria & Gerard H. Ros, december-19

# hydrobiological data
ppr_hybi <- function(db,syear = NULL,wtype = NULL,mlocs = NULL){
  
  # four inputs are possible
  # db: the database downloaded from FEWS, a data.table
  # syear: a numeric value to filter data for the years after syear 
  # wtype: a data.table with watertype per EAG
  # mlocs: a data.table with x and y coordinates for all measurements including relevant EAG
  
  # make a local copy
  db <- copy(db)
  
  # adapt few properties
  db[, datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
  db[, jaar := year(datum)]
  db[limietsymbool == '<', meetwaarde := meetwaarde * 0.5]
  db[fewsparameter == 'WATDTE_m' & jaar == 2006 & planvanaanpak == 'PVA MAFY LIJN 2006 WP', meetwaarde := meetwaarde * 0.1]
  
  # filter measurements on periode since syear (2000)
  db <- db[jaar>=syear]
  
  # remove measurements with following code
  sel <- unique(db$locatiecode)[grepl('meter$',unique(db$locatiecode))]
  
  # merge with locaties and watertypes (a few samples in 3300-EAG are also removed)
  db <- merge(db[!locatiecode %in% sel,],mlocs[,c('CODE','EAGIDENT')],by.x ='locatiecode', by.y = 'CODE')
  db <- merge(db,wtype[,c('watertype','GAFIDENT')],by.x ='EAGIDENT', by.y = 'GAFIDENT')
  
  # add codes (is this really needed?)
  db[,locatie.EAG := EAGIDENT]
  db[,locatie.KRW.watertype := watertype]
  
  # remove columns with no data or non-relevant information (judgement gerard)
  cols <- colnames(db)[unlist(db[,lapply(.SD,function(x) sum(is.na(x))==nrow(db))])]
  cols <- c(cols,'watertype','EAGIDENT','locatiecode','monsternemer','planvanaanpak','analist',
            'bron','bemonsteringsprotocol','analyseprotocol','analysecode','locatie.referentievlakzcoord',
            'locatie.meetprogrammahistorie','locatie.meetprogrammaactueel','locatie.meetnethistorie',
            'locatie.meetnetactueel','opmerkingmeting','externereferentie','veldapparaat')
  db[,c(cols) := NULL]
  
  # return updated database
  return(db)
}
