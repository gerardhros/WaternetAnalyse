# preprocess functions to clean up data.bases
# Laura Moria & Gerard H. Ros, december-19

# hydrobiological data
ppr_hybi <- function(db,syear = NULL,wtype = NULL,mlocs = NULL){
  
  # input description
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

ppr_ekr <- function(ekr1,ekr2){
  
  # combine both EKR from KRW and overig water into one data.table
  db <- rbindlist(list(ekr1,ekr2), fill=TRUE)
  
  # remove columns without information
  cols <- colnames(db)[unlist(db[,lapply(.SD,function(x) sum(is.na(x))==nrow(db))])]
  db[,c(cols):= NULL]
  
  # remove some columns based on judgement Gerard
  cols <- c('MEETNET_HISTORIE','REFERENTIEVLAK','GLOBALID','GN_CREATED_USER','GN_CREATED_DATE',
            'GN_LAST_EDITED_USER','GN_LAST_EDITED_DATE','MEETNET_ACTUEEL','FEWSFILTER_HISTORIE',
            'FEWSFILTER_ACTUEEL','PROGRAMMA_HISTORIE','PROGRAMMA_ACTUEEL','.',
            'LigtInGeoObjectCode','ï..Meetobject.namespace','CAS.nummer','Compartiment.code',
            'Begintijd','Eindtijd')
  # ensure that cols are present in colnames db
  cols[cols %in% colnames(db)]

  # remove columns
  db[,c(cols):=NULL]
  hoi <- ""
  # return updated database
  return(db)
  
}

ppr_slootbodem <- function(db,wtype = NULL){
  
  # make a local copy
  db <- copy(db)
  
  # adapt few properties
  db[,datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
  db[,jaar := year(datum)]
  db[limietsymbool == '<',meetwaarde := meetwaarde * 0.5] 
  
  # merge with GAFIDENT from eag_wl (be aware: EAG 3300 are few missing)
  db <- merge(db,wtype[,c('watertype','GAFIDENT')],by.x='locatie EAG',by.y = 'GAFIDENT',all = FALSE)
  
  # wijzig relevante namen van bodemfews database
  cols <- colnames(db)
  setnames(db,c('locatie EAG','locatiecode','locatie omschrijving','locatie x','locatie y','locatie z','fewsparameter','compartiment'),
           c('loc.eag','loc.code','loc.oms','loc.x','loc.y','loc.z','parm.fews','parm.compartiment'))
  
  # adapt unit sign in parm.fews to simply reference
  db[,parm.fews := gsub("/","_",parm.fews)]
  
  # select properties and dcast table
  db <- dcast(db, loc.eag+loc.code+loc.oms+loc.x+loc.y+loc.z+datum+jaar ~ parm.fews+parm.compartiment, value.var = "meetwaarde", fun.aggregate = mean)
  
  # adapt P measurement into one class, and NA gets class 8
  db[,klasseP := cut(Ptot_gP_kg_dg_BS,breaks = c(0,0.5,1,1.5,2.5,5,10,1000),labels=1:7)]
  db[,klasseP := factor(klasseP,levels=1:8)]
  db[is.na(klasseP), klasseP := 8]
  
  # remove columns without information
  cols <- colnames(db)[unlist(db[,lapply(.SD,function(x) sum(is.na(x))==nrow(db))])]
  db[,c(cols):= NULL]
  
  # return updated database
  return(db)
}

ppr_wq <- function(db,syear = NULL,wtype = NULL,mlocs = NULL){
  
  # make local copies
  db <- copy(db)
  
  # adapt wq database
  db[,jaar := year(datum)]
  db[,maand := month(datum)]
  db[limietsymbool == '<',meetwaarde := meetwaarde * 0.5]
  
  # delete years before 2000
  db <- db[jaar>=syear,]
  
  # merge with locaties, remove older EAGIDENT and older watertype with new one
  db[,c('EAGIDENT','watertype') := NULL]
  db <- merge(db,mlocs[,c('CODE','EAGIDENT')], by.x ='locatiecode', by.y = 'CODE',all.x = T)
  db <- merge(db,wtype[,c('watertype','GAFIDENT')], by.x ='EAGIDENT', by.y = 'GAFIDENT', all =FALSE)
  
  # replace other column with same info (is that needed Laura?)
  db[,locatie.EAG := EAGIDENT]
  db[,locatie.KRW.watertype := watertype]
  
  # select relevant data
  srow <- c("IONEN","NUTRI","ALG","VELD","ALGEN","LICHT")
  
  # subset waterquality data
  db <- db[fewsparametercategorie %in% srow]
  
  # adjust fews parameter names to avoid complicated columns names
  db[,fewsparameter := gsub("/","_",fewsparameter)]
  db[,eenheid := gsub("/","_",eenheid)]
  
  # remove columns with no data or non-relevant information (judgement gerard)
  cols <- c('locatie.referentievlakzcoord','locatie.meetprogrammahistorie','locatie.meetprogrammaactueel',
            'locatie.meetnethistorie','locatie.meetnetactueel','fewsparametereenheidequivalent',
            'fewsparametereenheidreferentie')
  db[,c(cols) := NULL]
  
  # return output wq parameters
  return(db)
  
}

ppr_pcditch <- function(db){
  
  # make local copy
  db <- copy(db)
  
  # rename columns
  setnames(db,c('EAG','GAF','EAGnaam','plv_o2','plv','opp','diepte','fr_moeras','strijklengte',
                          'debiet','inflow','extinctie','sedimenttype','pc_helder_troebel',
                          'pc_troebel_helder','lake_ditch_vol','morfologie','systeemgrens','p_bel_year'))
  
  # remove columns
  cols <- c('opp','EAGnaam')
  db[,c(cols):=NULL]
 
  # return updated database
  return(db) 
}

# adapt water balance properties
ppr_wbal <- function(db){
  
  # make local copy
  db <- copy(as.data.table(db))
  
  # make some adjustements (source: Laura Moria)
  db[GAF == '8070',watertype := 'M3']
  
  # add year
 
  # add soil type, given watertype
  db[,bodem := i_bt1]
  db[is.na(i_bt1) & watertype %in% c('M8','M10','M27','M25'),bodem := 'VEEN']
  db[is.na(i_bt1) & watertype %in% c("M3", "M1a","M30","M14","M11","M6a","M7b","M6b","M7a"),bodem := 'KLEI']
  
  # add total P-load
  db[,wp_tot_sum := wp_min_sum + wp_inc_sum]
  
  # return updated database
  return(db) 
  
}

ppr_wbalfiles <- function(wdir,EAG.sf = gEAG,kopTab = kopTab){
  
  # this function select the relevant file names of the excel waterbalances.
  # two checks are done: 
  # are all files given in koppeltabel available
  # are files missing given the most recent EAG shape (so those EAGs do not have a balance)
  
  # select file names in the directory where waterbalansen are stored
  files <- list.files(wdir)
  
  # select only the xlsx files (all are renewed to xlsx)
  files <- files[grepl('xlsx$',files)]
  
  # what are the unique file names before first underscore
  filesn <- unique(gsub("(.+?)(\\_.*)", "\\1", files))
  
  # use the files with the highest score in the name
  #files <- sapply(1:length(filesn),function(x) max(files[grep(filesn[x],files)])[1])
  
  # use files that are given in kopTab and print warning when files are missing
  bal_mis <- kopTab[!balans %in% files,namen]
  
  # print warning 1
  if(length(bal_mis)>1){
    print(paste0('warning: the waterbalans is missing for ',length(bal_mis),' eags, as given in kopTab'))}
  
  # check missing files in EAG shape
  eag.sf <- as.data.table(EAG.sf)
  eag.sf <- eag.sf[,.(GAFIDENT,geom)]
  eag.sf[,EAGIDENT := GAFIDENT]
  eag.sf[,GAFIDENT := paste0(substr(GAFIDENT,1,4),'-GAF')]
  
  # what EAGs do not have a water balance already
  bal_mis <- unique(eag.sf[!(EAGIDENT %in% kopTab$namen | GAFIDENT %in% kopTab$namen),EAGIDENT])
  bal_mis <- as.character(bal_mis)
  
  # print warning 2
  if(length(bal_mis)>1){
    print(paste0('warning: the waterbalans is missing for ',length(bal_mis),' eags, as given in shape EAG'))}
  
  # select the files that can be read in
  files <- files[files %in% kopTab$balans]
  
  # avoid duplicated names
  files <- unique(files)
  
  # return file names of water balances that can be read in
  return(files)
}
