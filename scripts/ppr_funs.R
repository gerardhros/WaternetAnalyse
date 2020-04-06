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
  db <- merge.data.table(db[!locatiecode %in% sel,],mlocs[,c('CODE','EAGIDENT',"OWMIDENT")],by.x ='locatiecode', by.y = 'CODE')
  db <- merge.data.table(db,wtype[,c('watertype','GAFIDENT')],by.x ='EAGIDENT', by.y = 'GAFIDENT')
  
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
  db <- data.table::rbindlist(list(ekr1,ekr2), fill=TRUE)
  
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
  d3 <- merge.data.table(db[!is.na(db$EAGIDENT),], eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], by.x = c('EAGIDENT'),
              by.y = c('GAFIDENT'), all.x = TRUE)

  eag_wl2 <- unique(eag_wl[,.(KRW_SGBP3,KRWmonitoringslocatie_SGBP3,SGBP3_NAAM,waterlichaam)])
  d4 <- merge.data.table(db[is.na(db$EAGIDENT),], eag_wl2[,c('KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM','waterlichaam')], by.x = c('HoortBijGeoobject.identificatie'),
              by.y = c('waterlichaam'), all.x = TRUE, allow.cartesian =TRUE)
  d3 <- rbind(d3,d4,fill=TRUE)

  # add changes of Laura (check later) 
  d3[,waterlichaam := fifelse(!is.na(SGBP3_NAAM), SGBP3_NAAM, GAFNAAM)]
  # delete visdata
  d3 <- d3[!is.na(waterlichaam),]
  
  # namen aanpassen
  d3[,facet_wrap_code <- as.factor(gsub("Maatlatten2018 ","",Waardebepalingsmethode.code))]
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
  db <- merge.data.table(db, mlocs[,c('CODE','EAGIDENT')],by.x ='locatiecode', by.y = 'CODE')
  db <- merge.data.table(db, wtype[,c('watertype','GAFIDENT')],by.x='locatie EAG',by.y = 'GAFIDENT',all.x = TRUE)
  
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
  db <- merge.data.table(db,mlocs[,c('CODE','EAGIDENT','OWMIDENT')], by.x ='locatiecode', by.y = 'CODE',all.x = T)
  db <- merge.data.table(db,wtype[,c('watertype','GAFIDENT')], by.x ='EAGIDENT', by.y = 'GAFIDENT', all =FALSE)
  
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
  d1 <- data.table::fread(paste0('data/',fname)) 
  
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
  d1 <- data.table::fread(paste0('data/',fname)) 
  
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
  d1 <- data.table::fread(paste0('hydrobiologie/',fname))
  
  # convert column to numeric
  d1[,Doel_2022 := as.numeric(Doel_2022)]
  
  # return doelen
  return(d1)
}
  
ppr_pmaps <- function(dat, Overzicht_kp, hybi, nomogram){
  
  # make local copy of soil and waterbalance data
  d1 <- copy(dat)
  
  # update soiltype d1
  d1[,bodem := i_bt1]
  d1[is.na(i_bt1) & watertype %in% c('M8','M10','M27','M25'), bodem := "VEEN"]
  d1[is.na(i_bt1) & watertype %in% c("M3", "M1a","M30","M14","M11","M6a","M7b","M6b","M7a"), bodem:='KLEI']
  
  # remove initiator data and filter only recent years
  cols <- colnames(d1)[grepl('^i_|^b_',colnames(d1))]
  dg <- d1[jaar %in% 2010:2018,][,c(cols):=NULL]
  
  # addgroup and estimate meerjarig mean for numeric values
  colg = c('pol','EAG','GAF','KRW','naam','namen','watertype', 'bodem','a_inlaat1','a_inlaat2',
                'a_inlaat3','a_inlaat4','a_inlaat5','a_uitlaat1','a_uitlaat2','a_uitlaat3','a_uitlaat4')
  cols = colnames(dg)[!colnames(dg) %in% c(colg,'seiz','versie')]
  dg <- dg[,lapply(.SD,mean,na.rm=T),.SDcols = cols,by = colg]
  
  # add total sum of P load
  dg[,wp_tot_sum := wp_min_sum + wp_inc_sum]
  
  # mean water depth per EAG
  mdPtb <- hybi[jaar %in% 2010:2017 & fewsparameter == 'WATDTE_m']
  mdPtb <- mdPtb[,.(meetwaarde = median(meetwaarde,na.rm = TRUE)),by='locatie.EAG']
  mdPtb[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # mean water depth per GAF
  mdPtbG <- hybi[jaar %in% 2010:2017 & fewsparameter == 'WATDTE_m']
  mdPtbG <- mdPtbG[,.(meetwaarde = median(meetwaarde,na.rm = TRUE)),by='locatie.afaanvoergebied']
  mdPtbG[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # mean water depth per KRW meetpunt
  mdPtbK <- hybi[jaar %in% 2010:2017 & fewsparameter == 'WATDTE_m']
  mdPtbK <- mdPtbK[,.(meetwaarde = median(meetwaarde,na.rm = TRUE)),by='locatie.KRWmeetpuntlocatie']
  mdPtbK[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # merge met kP ----------------------------------------------------------
  
  # koppel waterdiepte per eag en afvoergebied aan water en stoffenbalans
  dgwatdte  <- merge.data.table(dg[is.na(GAF),], mdPtb, by.x = 'EAG', by.y = 'locatie.EAG', all.x = F)
  dgwatdteG <- merge.data.table(dg[is.na(EAG),], mdPtbG, by.x = 'GAF', by.y = 'locatie.afaanvoergebied', all.x = F)
  dgwatdteK <- merge.data.table(dg[is.na(EAG) & is.na(GAF),], mdPtbK, by.x = 'KRW', by.y = 'locatie.KRWmeetpuntlocatie', all.x = T)
  dgwatdte <- rbind(dgwatdte,dgwatdteG,dgwatdteK,fill = TRUE)  # mis 1 balans
  
  # update merged table
  dgwatdte[,watdte := meetwaarde][, meetwaarde := NULL]
  dgwatdte[watdte > 0.7, watdteF := '(0.5,0.7]']
  
  # retreive kP from meta-model PCditch ----
  
    # make local copy and simplify debiet column name
    dbnomogram <- copy(nomogram)
    setnames(dbnomogram,"debiet (mm/dag)","debiet",skip_absent=TRUE)
  
    # add depth category, similar to dbhybi dataset
    dbnomogram[,watdteF := cut(watdte_m, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
    # model to predict kP as function of debiet (given soil and water depth)
    m1 <- lm(kP~bodemtype*watdteF*debiet*I(debiet^0.5)*I(debiet^2)*I(debiet^3),data=dbnomogram)
  
    # predict kP for dataset (suppress warnings ivm rank-deficient fit)
    suppressWarnings(dgwatdte[,kP := predict(m1,newdata = data.frame(debiet = w_debiet, bodemtype = tolower(bodem), watdteF = watdteF))])
  
    # renamed by Laura
    dgwatdte[,kPDitch := kP]
  
    # calc critical P-concentration 
    dgwatdte[,PvskPDitch := wp_min_sum / kP]
  
  # koppel kp plassen obv invoertabel per EAG ----
  
    # make local copu
    kP_plas <- copy(Overzicht_kP)
    
    # relevant columns to be merged
    cols <- colnames(kP_plas)[grepl('^pc_|^lake|^p_bel|^EAG$|^GAF$',colnames(kP_plas))]
  
    # merge per EAG and per GAF, and combine both (assuming its either EAG or GAF)
    PvskPplas1 <- merge.data.table(dgwatdte[watertype %in% c('M20','M27','M25',"M14") & !is.na(EAG),],
                        kP_plas[,mget(cols)],by='EAG',all.y = TRUE,all.x = FALSE)
    PvskPplas2 <- merge.data.table(dgwatdte[watertype %in% c('M20','M27','M25',"M14") & !is.na(GAF),],
                        kP_plas[,mget(cols)],by = 'GAF',all.y = TRUE,all.x = FALSE) 
    pvskp <- rbindlist(list(PvskPplas1,PvskPplas2),fill = TRUE)
    
    # merge plas kP with original water balance db
    dgwatdte <- merge.data.table(dgwatdte, pvskp[,c('pol','EAG','pc_troebel_helder', 'p_bel_year', 
                                         'pc_helder_troebel', 'lake_ditch_vol')], by = c('pol','EAG'), all = TRUE)
    
  # calc PvskP for lakes
  dgwatdte[!is.na(p_bel_year),wp_min_sum := p_bel_year]
  dgwatdte[,PvskPlake := wp_min_sum / pc_helder_troebel]
  
  # remove rows without estimated P-belasting
  dgwatdte <- dgwatdte[!is.na(wp_min_sum)]
  
  return(dgwatdte)
 
}

ppr_tabelPerWL3jaargemEAG_incl2022 <- function (EKRset,eag_wl, doelen){
  
  # make local copy (only within this function)
  doelen1 <- copy(doelen)
  d1 <- copy(EKRset)
  
  # calculate mean per groep
  colgroup <-c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code',
               'Waardebepalingsmethode.code','GHPR_level','GHPR','level','jaar')
  d1 <- d1[jaar > 2008,.(waarde = mean(Numeriekewaarde,na.rm=TRUE)),by=colgroup]
  
  # rename columns and order data.table
  setnames(d1,colgroup,c('id','EAGIDENT','watertype','wbmethode','GHPR_level','GHPR','level','jaar'))
  setorder(d1,EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode,-jaar)
  
  # add year number (given ordered set), and take only three most recent years
  d1 <- d1[,yearid := seq_len(.N),by=.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode)][yearid < 4]
  
  # calculate mean EKR per group over the three years
  d1 <- d1[,.(EKR = mean(waarde,na.rm=T)),by =.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode)]
  
  # remove empty spaces in GHPR needed for joining later
  # LM: bij mij gaan joins hierdoor juist mis
  d1[,GHPR := gsub(' $','',GHPR)]
  
  # merge with doelen
  
  # rename columns doelen object
  setnames(doelen1,c('HoortBijGeoobject.identificatie'),c('id'))
  
  # mean GEP per object
  doelgeb <- doelen1[,.(GEP = mean(Doel,na.rm=TRUE), GEP_2022 = mean(Doel_2022,na.rm=TRUE)),by =.(id,bronddoel,GHPR)]
  
  # make copy, add new id where NL11_ is removed
  doelgeb2 <- copy(doelgeb)
  doelgeb2[,id := sapply(strsplit(id, '_'), `[`, 2)]
  
  doelgeb <- rbind(doelgeb,doelgeb2)
  
  # merge with doelen
  d2 <- merge.data.table(d1, doelgeb, by = c('id','GHPR'), all.x = TRUE)
  
  # add classification for EKR
  d2[EKR < GEP/3,oordeel := 'slecht']
  d2[EKR >= GEP/3 & EKR < 2 * GEP / 3,oordeel := 'ontoereikend']
  d2[EKR >= 2 * GEP / 3,oordeel := 'matig']
  d2[EKR >= GEP, oordeel := 'goed']
  
  # add classification for EKR
  d2[EKR < GEP_2022/3,oordeel_2022 := 'slecht']
  d2[EKR >= GEP_2022/3 & EKR < 2 * GEP_2022 / 3, oordeel_2022 := 'ontoereikend']
  d2[EKR >= 2 * GEP_2022 / 3,oordeel_2022 := 'matig']
  d2[EKR >= GEP_2022, oordeel_2022 := 'goed']
  
  # add type water body and join by EAG-GAF
  eag_wl[,waterlichaam := sapply(strsplit(KRWmonitoringslocatie_SGBP3, '_'), `[`, 2)]
  d3 <- merge.data.table(d2[!is.na(EAGIDENT),], eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], 
              by.x = c('EAGIDENT'),
              by.y = c('GAFIDENT'), all.x = TRUE)
  
  # add type water body and join by waterlichaam
  eag_wl2 <- unique(eag_wl[,.(KRW_SGBP3,KRWmonitoringslocatie_SGBP3,SGBP3_NAAM,waterlichaam)])
  d4 <- merge.data.table(d2[is.na(EAGIDENT),], eag_wl2[,c('KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM','waterlichaam')], 
              by.x = c('id'),
              by.y = c('waterlichaam'), all.x = TRUE)
  d3 <- rbind(d3,d4,fill=TRUE)
  
  # return the object
  return(d3)
}

# ekr-plot for factsheet
ppr_ekrplot <- function(ekr_score){
  
  # make local copy
  dt <- copy(ekr_score)
  
  ## build background [Kan eleganter..]
  bg <- unique(dt[, c("id", "GEP", "GEP_2022", "facet_wrap_code")])
  
  # add boundaries for old GEP
  bg[,c('goed_ymin_old','goed_ymax_old') := .(GEP,Inf)]
  bg[,c('matig_ymin_old','matig_ymax_old') := .(GEP / 3 * 2,GEP)]
  bg[,c('ontoereikend_ymin_old','ontoereikend_ymax_old') := .(GEP / 3,GEP / 3 * 2)]
  bg[,c('slecht_ymin_old','slecht_ymax_old') := .(0,GEP / 3)]
  
  # add boundaries for new GEP
  bg[,c('goed_ymin_new','goed_ymax_new') := .(GEP_2022,1)]
  bg[,c('matig_ymin_new','matig_ymax_new') := .(GEP_2022 / 3 * 2,GEP_2022)]
  bg[,c('ontoereikend_ymin_new','ontoereikend_ymax_new') := .(GEP_2022 / 3,GEP_2022 / 3 * 2)]
  bg[,c('slecht_ymin_new','slecht_ymax_new') := .(0,GEP_2022 / 3)]
  
  # reformat 
  bg_gather <- melt(bg,id.vars = c('id','GEP','GEP_2022','facet_wrap_code'),
                    variable.name = 'doelen',value.name = 'waarde')
  bg_gather[,sgbp_version := fifelse(grepl('_new$',doelen),'new','old')]
  bg_gather[,varrange := fifelse(grepl('_ymin_',doelen),'ymin','ymax')]
  bg_gather[,doelen := gsub("(.+?)(\\_.*)", "\\1", doelen)]
  bg_spr <- dcast(bg_gather,id+GEP+GEP_2022+facet_wrap_code+doelen+sgbp_version~varrange,value.var='waarde')
  
  # update xmin, xmax and SGBP version
  bg_spr[sgbp_version=='old',c('xmin','xmax','sgbp_version') := .(0,0.5,'SGBP2')]
  bg_spr[sgbp_version=='new',c('xmin','xmax','sgbp_version') := .(0.5,1,'SGBP3')]
  bg_spr[,Oordeel := as.factor(doelen)]
  
  #Create a custom color scale
  myColors <- c("#00FF00", "#FFFF33", "#FF8000", "#FF0000")
  names(myColors) <- levels(bg_spr$doelen)
  
  ## make plot
  plot <- ggplot(dt, aes(x = id, y = EKR)) +
    geom_rect(data = bg_spr, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                  group = id, fill = Oordeel), alpha = 0.3) +
    scale_fill_manual(values = myColors) +
    geom_text(
      aes(x = xmin+0.25, y = 0.9, label = sgbp_version),
      data = bg_spr, check_overlap = TRUE,size = 3)+ 
    geom_vline(xintercept = 0.5, color = "lightgrey") +
    # geom_point() +
    geom_segment(aes(x = 0, xend = 1, 
                     y = EKR, yend = EKR, linetype = "Huidige toestand"), 
                 col = "black", cex = 1.2) + # linetype = 2 -> dashed
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_linetype_manual("",values= c("Huidige toestand" = 1))+
    facet_grid(cols = vars(facet_wrap_code)) +
    theme_minimal()+ 
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom")
  return(plot)
}

# maak plot van p VS Kp
ppr_pvskpplot <- function(pvskpsel){
  
  # make local copy
  d1 <- copy(pvskpsel)
  
  # colomns needed
  cols <- colnames(d1)[!grepl('^a_in|^a_uit|EAG|GAF|KRW|pol|watertype|^bodem|^naam|^namen|watdteF|lake_ditch',colnames(d1))]
  
  # estimate mean by name and select only those with a name
  d1 <- d1[,lapply(.SD,mean,na.rm=T),.SDcols = cols,by='naam'][!is.na(naam)]
  
  # set measurement to zero when not available
  d1[is.na(wp_meting_mgm2d), wp_meting_mgm2d := 0]
  
  # set colors
  colWat1 <-  c("green4","darkorange", "darkred", "red", "red1",  "red2","red3", "brown",  "blue", "black","darkgreen", "grey")  
  colWat2 <- adjustcolor(colWat1, alpha.f = 0.2)
  colWat <- paste(c(colWat2,"yellow", colWat1))
  
  # select columns
  cols <- colnames(d1)[grepl('^naam|^wp_|^kPDi|^pc_helder',colnames(d1))]
  cols <- cols[!grepl('_sum$|_gm3$',cols)]
  d1 <- d1[,mget(cols)][,wp_meting_mgm2d := -wp_meting_mgm2d]
  
  # reshape data.table for figure
  d2 <- melt(d1,id.vars = c('naam','kPDitch','pc_helder_troebel'),
       variable.name = 'source',value.name = 'value')
  
  # plot figure
  plot <- ggplot(d2) +
    geom_bar(aes(x = naam, y = value, fill = source), stat = 'identity') +
    geom_point(aes(x = naam, y= kPDitch), shape = 20, size = 6, fill = "red", colour = "red")+
    geom_point(aes(x = naam, y= pc_helder_troebel), shape = 20, size = 6, fill = "salmon",
               colour = "salmon")+
    xlab('') + ylab('mg P/m2/dag') +
    #ggtitle("Fosfor- en kritische belasting per deelgebied")+
    theme_classic() +
    guides(shape = guide_legend(override.aes = list(size = 4)),
           color = guide_legend(override.aes = list(size = 4))) +
    theme(legend.title = element_blank(), 
          legend.text  = element_text(size = 6),
          legend.key.size = unit(0.9, "lines"),
          legend.position = "right")+
    theme(axis.text.x = element_text(angle = 30, hjust =1))+
    scale_fill_manual(values = colWat)
 
  return(plot)
}

# make empty plots for factsheets when data is missing
plotEmpty <-function(db,type){
  
  # plot Pwbal
  if(type=='Pwbal'){
    plot <-  ggplot(db) +
      geom_bar(aes(x = GAF, y = pload), stat = 'identity') +
      xlab('') + ylab('mg P/m2/dag')+
      theme_classic() + ylim(0,2) +
      theme(legend.title = element_blank(), 
            legend.text  = element_text(size = 6),
            legend.key.size = unit(0.9, "lines"),
            legend.position = "right")+
      theme(axis.text.x = element_text(angle = 30, hjust =1)) +
      annotate("text", x = nrow(db) * 0.6 , y=1, 
               label = "P-belasting en bronnen\nzijn (nog) niet bekend.",
               hjust = 'middle',size=8,color='blue') +
      theme(axis.text =element_text(colour="black"))
  }
  
  if(type=='plotLichtklimaat'){
    
    plot <-  ggplot(db) +
      geom_bar(aes(x = GAF, y = Lext), stat = 'identity') +
      xlab('') + ylab('Vertical extinctie')+
      theme_classic() + ylim(0,4) +
      theme(legend.title = element_blank(), 
            legend.text  = element_text(size = 6),
            legend.key.size = unit(0.9, "lines"),
            legend.position = "right")+
      theme(axis.text.x = element_text(angle = 30, hjust =1)) +
      annotate("text", x = nrow(db) * 0.6 , y=2, 
               label = "Gegevens over het lichtklimaat\nzijn voor deze EAGs\n(nog) niet bekend.",
               hjust = 'middle',size=6,color='blue') +
      theme(axis.text =element_text(colour="black"))
  }
  
  if(type == 'plotWaterdiepte'){
    
    plot <- ggplot(db, aes(x= GAF, y= wd, col = krwwt))+
      geom_boxplot() +
      theme_minimal()+ scale_y_reverse(limits=c(3.5,0)) + 
      guides(col=guide_legend(title="KRW watertype"))+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle=0,colour = 'black'),
        axis.text.y = element_text(size= 7, hjust=2,colour = 'black'),
        axis.ticks =  element_line(colour = "black"), 
        axis.line = element_line(colour='black'),
        panel.background = element_blank(), 
        plot.background = element_blank() )+ 
      annotate("text", x = nrow(db) * 0.6 , y=1.6, 
               label = "Metingen waterdiepte \nzijn (nog) niet bekend.",
               hjust = 'middle',size=6,color='blue') +
      ggtitle('') +
      labs(x= '', y = 'waterdiepte (m)\n')
  }
  
  if(type=='plotbodFW'){
    
    plot <- ggplot(db, aes(x= GAF, y= plv, fill = ijzerval))+
      geom_boxplot() +
      theme_minimal()+ ylim(-0.5,0.5)+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle = 0,colour = 'black'),
        axis.text.y = element_text(size= 7,colour = 'black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank())+
      annotate("text", x = nrow(db) * 0.6 , y=0, 
               label = "Potentiele nalevering \nis (nog) niet bekend.",
               hjust = 'middle',size=6,color='blue') +
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), 
                        labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Potentiele nalevering") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
  
  if(type=='plotqPW'){
    
    plot <- ggplot(db, aes(x= GAF, y= plv, fill = ijzerval))+
      geom_boxplot() + ylim(-0.5,1.5)+
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle = 0,colour='black'),
        axis.text.y = element_text(size= 7, colour='black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(), 
        plot.background = element_blank())+
      annotate("text", x = nrow(db) * 0.6 , y=0.8, 
               label = "Actuele nalevering \nis (nog) niet bekend.",
               hjust = 'middle',size=6,color='blue') +
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Actuele nalevering uit de waterbodem\nobv poriewatermetingen") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
  
  
  # return plot
  return(plot)
  
}

# functie vor lichtklimaat en waterdiepte-----------
ppr_extinctie1 <- function(wq, hybi, parameter = c('VEC', 'WATDTE_m')){
  
  # median depth of hydrobiological data
  medianewd <- as.character(median(hybi[fewsparameter %in% parameter,meetwaarde],na.rm = T))
  
  # mean extinctie
  wq1 <- wq[fewsparameter %in% parameter & jaar > 2015 & meetwaarde > 0,]
  wq1 <- wq1[!is.na(locatie.KRW.watertype) & !is.na(locatie.EAG) & locatie.EAG != '',]
  meanext <- mean(wq1$meetwaarde)
  
  # plot figure
  p <- ggplot(wq1, aes(x= locatie.EAG, y= meetwaarde))+
    geom_boxplot() +
    geom_hline(aes(yintercept = 3.22, col = '1 meter'), show.legend = T)+ #vec voor 1 meter >4%
    geom_hline(aes(yintercept = log(25)/median(hybi$meetwaarde), col = paste0(medianewd, ' meter (mediane diepte)')), show.legend = T)+ #vec voor 4 meter >4%
    geom_hline(aes(yintercept = 0.46, col = '7 meter'), show.legend = T)+ #vec+ voor 7 meter 4%
    guides(col=guide_legend(title="4 % licht voor waterplanten op"))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 9, angle=40),
      axis.text.y = element_text(size= 9, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank(),
      axis.title=element_text(size=9) )+
    theme(legend.title = element_text(size = 10, face = 'bold'), 
          legend.text  = element_text(size = 10),
          legend.key.size = unit(0.9, "lines"),
          legend.position = "right")+
    ggtitle('') +
    labs(x= 'Ecologisch analysegebied', y = 'Verticale extinctie')
  # return plot
  return(p)
  
}



ppr_waterdieptesloot <- function(hybi, parameter = c('WATDTE_m')){
  
  # diepte4licht <- log(25)/1.2
  hybi2 <- hybi[fewsparameter %in% parameter & jaar == max(jaar),]
  
  # remove values that cannot exists (negative depths)
  hybi2[meetwaarde < 0, meetwaarde := NA]
  
  p<- ggplot(hybi2, aes(x= locatie.EAG, y= meetwaarde, col = locatie.KRW.watertype))+
    geom_boxplot() +
    theme_minimal()+ scale_y_reverse(limits=c(max(hybi2$meetwaarde)+0.1,0)) + 
    guides(col=guide_legend(title="KRW watertype"))+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 7, angle=0, colour = 'black'),
      axis.text.y = element_text(size= 7, hjust=2, colour = 'black'),
      axis.ticks =  element_line(colour = "black"), 
      axis.line = element_line(colour='black'),
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+ 
    ggtitle('') +
    labs(x= '', y = 'waterdiepte (m)\n')
 
  # return
  return(p)
  
}

ppr_plotbod <- function(bod1, type='grid'){
  
  # dcast slootbodem 
  selb <- dcast(bod1, loc.eag+loc.code+loc.oms+loc.x+loc.y+loc.z+datum+jaar ~ parm.fews+parm.compartiment, value.var = "meetwaarde", fun.aggregate = mean)
  
  # calculate relevant ratios
  selb[,FESPFWratio := (Fe_mg_l_ng_BS/55.845 - Stot_mg_l_ng_BS/32065)/(Ptot_mgP_l_ng_BS/30.974)]
  selb[,FESPDWratio := (Fe_mg_kg_dg_BS/55.845-Stot_mg_kg_dg_BS/32.065)/(Ptot_gP_kg_dg_BS*1000/30.974)]
  
  # add SP-ratio
  if(is.null(selb$Stot_mg_l_PW & selb$Stot_mg_l_nf_PW)){
    selb[!is.na(SO4_mg_l_PW),FESPPWratio := (Fe_ug_l_nf_PW*0.001/55.845 - SO4_mg_l_PW/96.06)/(Ptot_mgP_l_nf_PW/30.974)]
  }
  if(is.null(selb$Stot_mg_l_nf_PW)){
    selb[!is.na(Stot_mg_l_PW),FESPPWratio := (Fe_ug_l_nf_PW*0.001/55.845 - Stot_mg_l_PW/32.06)/(Ptot_mgP_l_nf_PW/30.974)]
  }
  if(!is.null(selb$Stot_mg_l_nf_PW)){
    selb[!is.na(Stot_mg_l_nf_PW),FESPPWratio := (Fe_ug_l_nf_PW*0.001/55.845 - Stot_mg_l_nf_PW/32.065)/(Ptot_mgP_l_nf_PW/30.974)]
  }
  
  # filter only op samples where FESPFWratio, FESPDWratio and FESPPWratio are present
  selb <- selb[!(is.na(FESPFWratio)|is.na(FESPDWratio)|is.na(FESPPWratio))]
  
  # calculate nalevering
  selb[,nlvrFW := 0.0247 * Ptot_mgP_l_ng_BS - 1.6035]
  selb[,nlvrDW := 0.0077 * Ptot_gP_kg_dg_BS * 1000 - 4.7259]
  selb[,nlvrPW := 0.8095 * Ptot_mgP_l_nf_PW - 0.2905]
  
  # add categories
  selb[,classFESPFWratio := cut(FESPFWratio, breaks = c((min(FESPFWratio)-1), 1.4, 4, max(FESPFWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  selb[,classFESPDWratio := cut(FESPDWratio, breaks = c((min(FESPDWratio)-1), 1.4, 4, max(FESPDWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  selb[,classFESPPWratio := cut(FESPPWratio, breaks = c((min(FESPPWratio)-1), 1.4, 4, max(FESPPWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  
  plotFW <- ggplot(selb, aes(x= loc.eag, y= nlvrFW, fill = classFESPFWratio))+
    geom_boxplot() +
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 7, angle = 0, colour='black'),
      axis.text.y = element_text(size= 7, colour='black'),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      panel.background = element_blank(),
      plot.background = element_blank()
    )+
    scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
    ggtitle( "Potentiele nalevering") +
    labs(x="",y="P mg/m2/dag\n", fill = '')
  
  if(!is.null(selb$FESPPWratio)){
    qPW <- ggplot(selb, aes(x= loc.eag, y= nlvrPW, fill = classFESPPWratio))+
      geom_boxplot() +
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle = 0,colour='black'),
        axis.text.y = element_text(size= 7,colour='black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(), 
        plot.background = element_blank()
      )+
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Actuele nalevering uit de waterbodem\nobv poriewatermetingen") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
  if(is.null(selb$FESPPWratio)){out = plotFW} 
  if(!is.null(selb$FESPPWratio)){out = arrangeGrob(plotFW, qPW)}
  
  if(type=='plotFW'){out = plotFW}
  if(type=='plotqPW'){out = qPW}
  
  return(grid.draw(out))
}