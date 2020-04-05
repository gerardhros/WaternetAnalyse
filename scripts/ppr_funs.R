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
  db[,meetwaarde := as.numeric(meetwaarde)]
  db[limietsymbool == '<', meetwaarde := meetwaarde * 0.5]
  db[fewsparameter == 'WATDTE_m' & jaar == 2006 & planvanaanpak == 'PVA MAFY LIJN 2006 WP', meetwaarde := meetwaarde * 0.1]
  
  # filter measurements on periode since syear (2000)
  db <- db[jaar>=syear]
  
  # remove measurements with following code
  sel <- unique(db$locatiecode)[grepl('meter$',unique(db$locatiecode))]
  
  # merge with locaties and watertypes (a few samples in 3300-EAG are also removed)
  db <- merge(db[!locatiecode %in% sel,],mlocs[,c('CODE','EAGIDENT',"OWMIDENT")],by.x ='locatiecode', by.y = 'CODE')
  db <- merge(db,wtype[,c('watertype','GAFIDENT')],by.x ='EAGIDENT', by.y = 'GAFIDENT')
  
  # add codes (is this really needed?)
  db[,locatie.EAG := EAGIDENT]
  db[,locatie.KRW.watertype := watertype]
  db[,locatie.KRWmeetpuntlocatie := OWMIDENT]
  
  # remove columns with no data or non-relevant information (judgement gerard)
  cols <- colnames(db)[unlist(db[,lapply(.SD,function(x) sum(is.na(x))==nrow(db))])]
  cols <- c(cols,'watertype','EAGIDENT','monsternemer','planvanaanpak','analist',
            'bron','bemonsteringsprotocol','analyseprotocol','locatie.referentievlakzcoord',
            'locatie.meetprogrammaactueel',
            'locatie.meetnetactueel','opmerkingmeting','externereferentie')
  db[,c(cols) := NULL]
  
  # return updated database
  return(db)
}

ppr_ekr <- function(ekr1, ekr2, eag_wl, doelen){
  
  # combine both EKR from KRW and overig water into one data.table
  db <- rbindlist(list(ekr1,ekr2), fill=TRUE)
  
  # remove columns without information
  cols <- colnames(db)[unlist(db[,lapply(.SD,function(x) sum(is.na(x))==nrow(db))])]
  db[,c(cols):= NULL]
  
  # remove some columns based on judgement Gerard
  cols <- c('MEETNET_HISTORIE','REFERENTIEVLAK','GLOBALID','GN_CREATED_USER','GN_CREATED_DATE',
            'GN_LAST_EDITED_USER','GN_LAST_EDITED_DATE','MEETNET_ACTUEEL','FEWSFILTER_HISTORIE',
            'FEWSFILTER_ACTUEEL','PROGRAMMA_HISTORIE','PROGRAMMA_ACTUEEL','.',
            'LigtInGeoObjectCode','ï..Meetobject.namespace','CAS.nummer',
            'Begintijd','Eindtijd','Doel','bronddoel',"HandelingsperspectiefWBP",'KRWwatertype.code.y')
  # ensure that cols are present in colnames db
  cols <- cols[cols %in% colnames(db)]
  # remove columns
  db[,c(cols):=NULL]
  db[,GHPR := gsub(' $','',GHPR)]
  
  # make local copy (only within this function)
  doelen1 <- copy(doelen)
  # mean GEP per id (en niet per eag zoals in de doelenset staat)
  doelgeb <- doelen1[,.(GEP = mean(Doel,na.rm=TRUE), GEP_2022 = mean(Doel_2022,na.rm=TRUE)),by =.(HoortBijGeoobject.identificatie,bronddoel,GHPR)]
  # make copy, add new id where NL11_ is removed
  doelgeb2 <- copy(doelgeb)
  doelgeb2[,HoortBijGeoobject.identificatie := sapply(strsplit(HoortBijGeoobject.identificatie, '_'), `[`, 2)]
  doelgeb <- rbind(doelgeb,doelgeb2)
  
  # merge with doelen
  db <- merge(db, doelgeb, by = c('HoortBijGeoobject.identificatie','GHPR'), all.x = TRUE, allow.cartesian =T)
  
  # add namen per waterlichaam en eag
  db$EAGIDENT[is.na(db$EAGIDENT)] <- sapply(strsplit(db$HoortBijGeoobject.identificatie[is.na(db$EAGIDENT)], '_'), `[`, 2) 
  
  eag_wl[,waterlichaam := sapply(strsplit(KRWmonitoringslocatie_SGBP3, '_'), `[`, 2)]
  d3 <- merge(db[!is.na(db$EAGIDENT),], eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], by.x = c('EAGIDENT'),
              by.y = c('GAFIDENT'), all.x = TRUE)
  eag_wl2 <- dcast(eag_wl, KRW_SGBP3+KRWmonitoringslocatie_SGBP3+SGBP3_NAAM+waterlichaam~., fun.aggregate = mean)
  d4 <- merge(db[is.na(db$EAGIDENT),], eag_wl2[,c('KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM',"waterlichaam")], by.x = c('HoortBijGeoobject.identificatie'),
              by.y = c('waterlichaam'), all.x = TRUE, allow.cartesian =TRUE)
  d3 <- smartbind(d3,d4)
  d3$waterlichaam <- ifelse(!is.na(d3$SGBP3_NAAM), d3$SGBP3_NAAM, d3$GAFNAAM)
  d3 <- d3[!is.na(d3$waterlichaam),] #visdata verwijderen
  d3 <- as.data.table(d3)
  
  # namen aanpassen
  d3$facet_wrap_code <- as.factor(mapvalues(d3$Waardebepalingsmethode.code,
                                            from = c("Maatlatten2018 Fytoplankton", "Maatlatten2018 Macrofauna", "Maatlatten2018 Ov. waterflora", "Maatlatten2018 Vis"),
                                            to = c("Fytoplankton", "Macrofauna", "Waterflora", "Vis")))
  # return updated database
  return(d3)
}

ppr_slootbodem_kl <- function(db, wtype = NULL,mlocs = NULL){
  
  # make a local copy
  db <- copy(db)
  
  # adapt few properties
  db[,datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
  db[,jaar := year(datum)]
  db[limietsymbool == '<',meetwaarde := meetwaarde * 0.5] 
  
  # merge with GAFIDENT from eag_wl (be aware: EAG 3300 are few missing)
  db <- merge(db, mlocs[,c('CODE','EAGIDENT')],by.x ='locatiecode', by.y = 'CODE')
  db <- merge(db, wtype[,c('watertype','GAFIDENT')],by.x='locatie EAG',by.y = 'GAFIDENT',all.x = TRUE)
  
  # wijzig relevante namen van bodemfews database
  cols <- colnames(db)
  setnames(db,c('locatie EAG','locatiecode','locatie omschrijving','locatie x','locatie y','locatie z','fewsparameter','compartiment'),
           c('loc.eag','loc.code','loc.oms','loc.x','loc.y','loc.z','parm.fews','parm.compartiment'))
  
  # adapt unit sign in parm.fews to simply reference
  db[,parm.fews := gsub("/","_",parm.fews)]
  
  # select properties and dcast table
  bod_klasse <- data.table::dcast(db, loc.eag+loc.code+loc.oms+loc.x+loc.y+loc.z+datum+jaar ~ parm.fews+parm.compartiment, value.var = "meetwaarde", fun.aggregate = mean)
  
  # adapt P measurement into one class, and NA gets class 8
  bod_klasse[,klasseP := cut(Ptot_gP_kg_dg_BS,breaks = c(0,0.5,1,1.5,2.5,5,10,1000),labels=1:7)]
  bod_klasse[,klasseP := factor(klasseP,levels=1:8)]
  bod_klasse[is.na(klasseP), klasseP := 8]
  
  # remove columns without information
  cols <- colnames(bod_klasse)[unlist(bod_klasse[,lapply(.SD,function(x) sum(is.na(x))==nrow(bod_klasse))])]
  bod_klasse[,c(cols):= NULL]
  
  # return updated database

  return(bod_klasse)
}

ppr_slootbodem <- function(db, wtype = NULL,mlocs = NULL){
  
  # make a local copy
  db <- copy(db)
  
  # adapt few properties
  db[,datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
  db[,jaar := year(datum)]
  db[limietsymbool == '<',meetwaarde := meetwaarde * 0.5] 
  
  # merge with GAFIDENT from eag_wl (be aware: EAG 3300 are few missing)
  db <- merge(db, mlocs[,c('CODE','EAGIDENT')],by.x ='locatiecode', by.y = 'CODE')
  db <- merge(db, wtype[,c('watertype','GAFIDENT')],by.x='locatie EAG',by.y = 'GAFIDENT',all.x = TRUE)
  
  # wijzig relevante namen van bodemfews database
  cols <- colnames(db)
  setnames(db,c('locatie EAG','locatiecode','locatie omschrijving','locatie x','locatie y','locatie z','fewsparameter','compartiment'),
           c('loc.eag','loc.code','loc.oms','loc.x','loc.y','loc.z','parm.fews','parm.compartiment'))
  
  # adapt unit sign in parm.fews to simply reference
  db[,parm.fews := gsub("/","_",parm.fews)]
  
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
  db <- merge(db,mlocs[,c('CODE','EAGIDENT','OWMIDENT')], by.x ='locatiecode', by.y = 'CODE',all.x = T)
  db <- merge(db,wtype[,c('watertype','GAFIDENT')], by.x ='EAGIDENT', by.y = 'GAFIDENT', all =FALSE)
  
  # replace other column with same info (is that needed Laura?)
  db[,locatie.EAG := EAGIDENT]
  db[,locatie.KRW.watertype := watertype]
  db[,locatie.KRWmeetpuntlocatie := OWMIDENT]
  
  # select relevant data
  srow <- c("IONEN","NUTRI","ALG","VELD","ALGEN","LICHT")
  
  # subset waterquality data
  db <- db[fewsparametercategorie %in% srow]
  
  # adjust fews parameter names to avoid complicated columns names
  db[,fewsparameter := gsub("/","_",fewsparameter)]
  db[,eenheid := gsub("/","_",eenheid)]
  
  # remove columns with no data or non-relevant information (judgement gerard)
  cols <- c('locatie.referentievlakzcoord','locatie.meetprogrammaactueel',
            'locatie.meetnetactueel',
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

# read in the lastest data from ESF oordelen
ppr_esf_oordeel <- function(){
  
  # select the latest file with ESF oordelen
  fname <- list.files('data')
  fname <- sort(fname[grepl('^esfKRW',fname)],decreasing = TRUE)[1]
  
  # read ESF oordelen
  d1 <- fread(paste0('data/',fname)) 
  
  # which colnames are character
  cols <- colnames(d1)[sapply(d1, is.character)]
  
  # trim character columns from starting and ending space
  d1[,(cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)),.SDcols = cols]
  
  # hier later ook eags of gafs aan toevoegen
  
  # return ESF oordelen
  return(d1)
  
}

# read in the lastest data from maatregelen
ppr_maatregelen <- function(){
  
  # select the latest file with maatregelen
  fname <- list.files('data')
  fname <- sort(fname[grepl('^maatregelenKRW',fname)],decreasing = TRUE)[1]
  
  # read maatregelen
  d1 <- fread(paste0('data/',fname)) 
  
  # which colnames are character
  cols <- colnames(d1)[sapply(d1, is.character)]
  
  # trim character columns from starting and ending space
  d1[,(cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)),.SDcols = cols]
  
  # select only the unique rows
  d1 <- unique(d1) 
  
  # return maatregelen
  return(d1)
  
}

# read the latest file with doelen
ppr_doelen <- function(){
 
  # select the latest file with doelen
  fname <- list.files('hydrobiologie')
  fname <- fname[grepl('^doelen',tolower(fname))][1]
  
  # read in the latest file from
  d1 <- fread(paste0('hydrobiologie/',fname))
  
  # convert column to numeric
  d1[,Doel_2022 := as.numeric(Doel_2022)]
  
  # return doelen
  return(d1)
}
  
