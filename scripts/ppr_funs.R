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

ppr_ekr <- function(krwset, ovwatset, eag_wl, doelen){

  # if(!is.null(ovwatset) & !is.null(krwset)){
  # # correctie toetsresultaten van KRW waterlichamen weg uit de set van overig water obv EAG, nieuwe gegegevens 2020 juli lijkt de overlap vooral fout te zitten in KRW set
  # # er zitten foutieve watertypen in de bronbestanden, dit heb ik nog niet opgelost
  # tomatch <- unique(krwset$EAG) ; tomatch <- tomatch[!is.na(tomatch)]
  # pattern <- paste(tomatch, collapse = "|")
  # ovwatset <- ovwatset[!grepl(pattern, ovwatset$EAGIDENT, fixed =F),]
  # # correctie meetlocaties eruit die niet meer in een HoortbijGEO EAG liggen omdat EAG herbegrensd zijn of locatie verlegd
  # # extract EAG uit hoortbij, match EAGIDENT en extract, delete nomatch
  # ovwatset <- ovwatset[!mapply(grepl, ovwatset$EAGIDENT,  ovwatset$HoortBijGeoobject.identificatie, fixed = F) == 0,]
  # }

  # combine both EKR from KRW and overig water into one data.table
  db <- data.table::rbindlist(list(krwset,ovwatset), fill=TRUE)

  # rijen weg zonder informatie
  db <- db[!is.na(db$Numeriekewaarde),]

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
  doelgeb2$HoortBijGeoobject.identificatie <- gsub("^NL11_*","",doelgeb2$HoortBijGeoobject.identificatie)
  doelgeb <- rbind(doelgeb,doelgeb2)

  # merge with doelen
  db <- merge(db, doelgeb, by = c('HoortBijGeoobject.identificatie','GHPR'), all.x = TRUE, allow.cartesian =T)

  # add namen per eag
  d3 <- merge.data.table(db[!is.na(db$EAGIDENT),], eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], by.x = c('EAGIDENT'),
              by.y = c('GAFIDENT'), all.x = TRUE)
  # koppel op WL naam
  eag_wl[,waterlichaam := sapply(strsplit(KRWmonitoringslocatie_SGBP3, '_'), `[`, 2)]
  eag_wl2 <- unique(eag_wl[,.(KRW_SGBP3,KRWmonitoringslocatie_SGBP3,SGBP3_NAAM,waterlichaam)])
  d4 <- merge.data.table(db[is.na(db$EAGIDENT),], eag_wl2[,c('KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM','waterlichaam')], by.x = c('HoortBijGeoobject.identificatie'),
              by.y = c('waterlichaam'), all.x = TRUE, allow.cartesian =TRUE)
  d3 <- rbind(d3,d4,fill=TRUE)

  # add naam van een toetsgebied (WL of EAG naam)
  d3[,waterlichaam := fifelse(!(is.na(SGBP3_NAAM)|SGBP3_NAAM == ""), SGBP3_NAAM, GAFNAAM)]
  
  # delete visdata die niet geaggregeerd is (middelen van meetlocaties klopt niet)
  d3 <- d3[!(!is.na(CODE) & Waardebepalingsmethode.code == "Maatlatten2018 Vis"),]

  # namen aanpassen
  d3[,facet_wrap_code := as.factor(gsub("Maatlatten2018 ","",Waardebepalingsmethode.code))]
  # return updated database

  # noodgreep omdat er fouten zitten in de toetsgegevens
  d3$KRWwatertype.code[d3$Identificatie == 'VaartenRondeHoep'] <- 'M8'
  d3$KRWwatertype.code[d3$Identificatie == 'VaartenZevenhoven'] <- 'M1a'

  # alleen nieuwe maatlatten
  d3 <- d3[!Waardebepalingsmethode.code %in% c("Maatlatten2012 Ov. waterflora","Maatlatten2012 Vis"),]

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

ppr_wbalfiles <- function(dir_bal,EAG.sf = gEAG,kopTab = kopTab){

  # this function select the relevant file names of the excel waterbalances.
  # two checks are done:
  # are all files given in koppeltabel available
  # are files missing given the most recent EAG shape (so those EAGs do not have a balance)
  # wdir <- dir_bal

  # select file names in the directory where waterbalansen are stored
  files <- list.files(dir_bal)

  # select only the xlsx files (all are renewed to xlsx)
  files <- files[grepl('xlsx$',files)]

  # what are the unique file names before first underscore
  filesn <- unique(gsub("(.+?)(\\_.*)", "\\1", files))

  # use the files with the highest score in the name
  #files <- sapply(1:length(filesn),function(x) max(files[grep(filesn[x],files)])[1])

  # use files that are given in kopTab and print warning when files are missing
  bal_mis <- kopTab[!balans %in% files, namen]
  kop_mis <- as.data.frame(files[!files %in% kopTab$balans])
  # write.csv2(kop_mis)


  # print warning 0
  if(length(kop_mis)>1){
    print(paste0('warning: the waterbalans is missing in kopTab for ',length(kop_mis),' balances'))}
  
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
  fname <- list.files('data',pattern = '.csv$')
  fname <- sort(fname[grepl('^esfKRW',fname)],decreasing = TRUE)[1]

  # read ESF oordelen
  d1 <- data.table::fread(paste0('data/',fname))

  # which colnames are character
  cols <- colnames(d1)[sapply(d1, is.character)]

  # trim character columns from starting and ending space
  d1[,(cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)),.SDcols = cols]
  #d1[,MotiveringWijzigingToestandWKP:=NULL]

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

  # remove columns with no data or non-relevant information (judgement gerard)
  cols <- colnames(d1)[unlist(d1[,lapply(.SD,function(x) sum(is.na(x))==nrow(d1))])]
  d1[,c(cols) := NULL]

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

ppr_tabelPerWL3jaargemEAG_incl2022 <- function (EKRset){

  # make local copy (only within this function)
  d1 <- copy(EKRset)

  # calculate mean per groep
  colgroup <-c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code','Waardebepalingsmethode.code',
               'facet_wrap_code','GHPR_level','GHPR','level','jaar','GEP','GEP_2022','waterlichaam','KRW_SGBP3')
  d1 <- d1[jaar > 2008,.(waarde = mean(Numeriekewaarde,na.rm=TRUE)),by=colgroup]

  # rename columns and order data.table
  setnames(d1,colgroup,c('id','EAGIDENT','watertype','wbmethode','facet_wrap_code','GHPR_level',
                         'GHPR','level','jaar','GEP','GEP_2022','waterlichaam','KRW_SGBP3'))
  setorder(d1,EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode,-jaar,GEP, GEP_2022, waterlichaam, KRW_SGBP3)

  # columns to group
  colg <- c('EAGIDENT','id','watertype','GHPR_level','GHPR','level','wbmethode','facet_wrap_code', 'GEP', 'GEP_2022', 'waterlichaam', 'KRW_SGBP3')

  # add year number (given ordered set), and take only three most recent years
  d1 <- d1[,yearid := seq_len(.N),by = colg][yearid < 4]

  # calculate mean EKR per group over the three years
  d1 <- d1[,.(EKR = mean(waarde,na.rm=T)),by = colg]

  # remove empty spaces in GHPR needed for joining later
  # LM: bij mij gaan joins hierdoor juist mis
  d1[,GHPR := gsub(' $','',GHPR)]

  # add classification for EKR
  d1[EKR < GEP/3,oordeel := 'slecht']
  d1[EKR >= GEP/3 & EKR < 2 * GEP / 3,oordeel := 'ontoereikend']
  d1[EKR >= 2 * GEP / 3,oordeel := 'matig']
  d1[EKR >= GEP, oordeel := 'goed']

  # add classification for EKR
  d1[EKR < GEP_2022/3,oordeel_2022 := 'slecht']
  d1[EKR >= GEP_2022/3 & EKR < 2 * GEP_2022 / 3, oordeel_2022 := 'ontoereikend']
  d1[EKR >= 2 * GEP_2022 / 3,oordeel_2022 := 'matig']
  d1[EKR >= GEP_2022, oordeel_2022 := 'goed']

  # return the object
  return(d1)
}

# ekr-plot for factsheet
ppr_ekrplot <- function(ekr_score){

  # make local copy
  dt <- copy(ekr_score)

  ## build background
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
  bg_spr <- dcast.data.table(bg_gather,id+GEP+GEP_2022+facet_wrap_code+doelen+sgbp_version~varrange,value.var='waarde')

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

# plot EKR versie 2
ppr_ekrplot2 <- function(ekr_score){

  # make local copy
  dt <- copy(ekr_score)

  # facet per maatlat en EAG (als FS niveau is GAF en meerdere EAGs)
  dt[KRW_SGBP3 == "",KRW_SGBP3 := NA]
  dt[,facet_wrap_code := as.character(facet_wrap_code)]
  dt[,wlmt := fifelse(is.na(KRW_SGBP3), paste0(EAGIDENT," ",facet_wrap_code), facet_wrap_code)]

  # build background [Kan eleganter..]
  bg <- unique(dt[, c("id", "GEP_2022", "wlmt")])

  # add boundaries for new GEP
  bg[,c('goed_ymin_new','goed_ymax_new') := .(GEP_2022,1)]
  bg[,c('matig_ymin_new','matig_ymax_new') := .(GEP_2022 / 3 * 2,GEP_2022)]
  bg[,c('ontoereikend_ymin_new','ontoereikend_ymax_new') := .(GEP_2022 / 3,GEP_2022 / 3 * 2)]
  bg[,c('slecht_ymin_new','slecht_ymax_new') := .(0,GEP_2022 / 3)]

  # reformat
  bg_gather <- melt(bg,id.vars = c('id','GEP_2022','wlmt'),
                    variable.name = 'doelen',value.name = 'waarde')
  bg_gather[,sgbp_version := fifelse(grepl('_new$',doelen),'new','old')]
  bg_gather[,varrange := fifelse(grepl('_ymin_',doelen),'ymin','ymax')]
  bg_gather[,doelen := gsub("(.+?)(\\_.*)", "\\1", doelen)]
  bg_spr <- dcast.data.table(bg_gather,id+GEP_2022+wlmt+doelen+sgbp_version~varrange,value.var='waarde')

  # add sgbp version
  bg_spr[sgbp_version=='new',sgbp_version := 'SGBP3']
  bg_spr[,Oordeel := as.factor(doelen)]

  #Create a custom color scale
  myColors <- c("#00FF00", "#FFFF33", "#FF8000", "#FF0000")
  names(myColors) <- levels(bg_spr$doelen)

  ## make plot
  plot <- ggplot(dt, aes(x = id, y = EKR)) +
    geom_rect(data = bg_spr, inherit.aes = FALSE,
              aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax,
                  fill = Oordeel), alpha = 0.3) +
    scale_fill_manual(values = myColors) +
    geom_segment(aes(x = 0, xend = 1,
                     y = EKR, yend = EKR, linetype = "Huidige toestand"),
                 col = "black", cex = 1.4) + # linetype = 2 -> dashed
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    scale_linetype_manual("",values= c("Huidige toestand" = 1))+
    facet_grid(cols = vars(wlmt)) +
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          strip.text.x = element_text(size = 8), # maatlat
          #strip.text.y = element_text(size = 6), # y as
          axis.text.x = element_blank(), #
          axis.text.y = element_text(size= 7), # ekrscores
          axis.title = element_text(size= 7),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_line(),
          panel.ontop = F,
          legend.title = element_text(size = 8),
          legend.text  = element_text(size = 7),
          legend.position = "bottom")
  return(plot)
}

# plot EKR background
plotEKRlijnfs <- function(z, gebied = NULL){

  if(!is.null(gebied)){
    z <- z[!is.na(z$EAGIDENT),]
    z$KRW_SGBP3 <- ""
  }

  z <- z %>%
    dplyr::arrange(GHPR_level) %>%               # sort your dataframe
    dplyr::mutate(GHPR = factor(GHPR, unique(GHPR))) # reset your factor-column based on that order
  z$jaar <- as.numeric(z$jaar)
  z$Numeriekewaarde <- as.numeric(z$Numeriekewaarde)
  z$facet_wrap_code <- as.character(z$facet_wrap_code)
  z$wlmt <- ifelse(is.na(z$KRW_SGBP3)|z$KRW_SGBP3 == "", paste0(z$EAGIDENT," ",z$facet_wrap_code), z$facet_wrap_code)

  z <- z %>%
    dplyr::group_by(waterlichaam, wlmt ,GHPR , GHPR_level, level, jaar)%>%
    dplyr::summarise_at(c('Numeriekewaarde'),mean)

  z <- z %>%
    ungroup(waterlichaam)

  ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col = GHPR, group = GHPR))+
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    scale_y_continuous(limits= c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    scale_x_continuous(limits= c(2006, 2020), breaks=c(2006, 2008, 2010, 2012, 2014,2016,2018))+
    facet_grid(vars(level),vars(wlmt))+
    ylab('')+xlab('')+
    guides(col=guide_legend(title=""))+
    ggtitle("", subtitle = z$waterlichaam)+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7), #waardebepmethode
      strip.text.y = element_text(size = 2), #level
      axis.text.x = element_text(size= 5, angle=90), #jaar
      axis.text.y = element_text(size= 5),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 7),
      legend.text  = element_text(size = 6),
      legend.key.size = unit(0.9, "lines"),
      legend.position = "right")
}

# maak plot van p VS Kp
ppr_pvskpplot <- function(pvskpsel){

  # make local copy
  d1 <- copy(pvskpsel)

  # colomns needed
  cols <- colnames(d1)[!grepl('^a_in|^a_uit|EAG|GAF|KRW|pol|watertype|^bodem|^naam|^namen|watdteF|lake_ditch',colnames(d1))]

  # estimate mean by name and select only those with a name
  d1 <- d1[,lapply(.SD,mean,na.rm=T),.SDcols = cols, by=c('naam','watertype')][!is.na(naam)]

  # set measurement to zero when not available
  d1[is.na(wp_meting_mgm2d), wp_meting_mgm2d := 0]

  # set colors
  colWat1 <-  c("green4","darkorange", "darkred", "red", "red1",  "red2","red3", "brown",  "blue", "black","darkgreen", "grey")
  colWat2 <- adjustcolor(colWat1, alpha.f = 0.2)
  colWat <- paste(c(colWat2,"yellow", colWat1))

  # select columns
  cols <- colnames(d1)[grepl('^naam|^wp_|^kPDi|^pc_helder|^watertype',colnames(d1))]
  cols <- cols[!grepl('_sum$|_gm3$',cols)]
  d1 <- d1[,mget(cols)][,wp_meting_mgm2d := -wp_meting_mgm2d]

  # reshape data.table for figure
  d2 <- melt.data.table(d1,id.vars = c('naam','kPDitch','pc_helder_troebel','watertype'),
        variable.name = 'source',value.name = 'value', variable.factor = FALSE)
  d2$kP <- ifelse(d2$watertype %in% c('M14','M27','M20','M25','M11'), d2$pc_helder_troebel, d2$kPDitch)
  d2$naam <- str_squish(d2$naam)
  d2$naam <- str_trunc(d2$naam, 15, "left")

  # plot figure
  plot <- ggplot(d2) +
    geom_bar(aes(x = naam, y = value, fill = source), stat = 'identity') +
    geom_point(aes(x = naam, y= kP), shape = 20, size = 6, fill = "red",
                 colour = "red")+
    geom_point(aes(x = naam, y= pc_helder_troebel), shape = 20, size = 6, fill = "salmon",
               colour = "salmon")+
    xlab('') + ylab('mg P/m2/dag')+
    #ggtitle("Fosfor- en kritische belasting per deelgebied")+
    theme_classic() +
    guides(shape = guide_legend(override.aes = list(size = 4)),
           color = guide_legend(override.aes = list(size = 4))) +
    theme(legend.title = element_blank(),
          legend.text  = element_text(size = 7),
          legend.key.size = unit(0.9, "lines"),
          legend.position = "right")+
    theme(axis.text.x = element_text(size = 7, angle = 30, hjust = 1))+
    scale_fill_manual(values = colWat)

  return(plot)
}

# make empty plots for factsheets when data is missing
plotEmpty <-function(db,type){

  # middle x-axis
  midax = if(nrow(db)==1) 1 else nrow(db)*0.5 + 0.5

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
      annotate("text", x = midax , y=1,
               label = "P-belasting en bronnen\nzijn (nog) niet beschikbaar.",
               hjust = 'middle',size=5,color='blue') +
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
      theme(axis.text.x = element_text(angle = 30, hjust =1),
            axis.ticks =  element_line(colour = "black"),
            axis.line = element_line(colour='black')) +
      annotate("text", x = midax , y=2,
               label = "Gegevens over het lichtklimaat\nzijn voor deze EAGs\n(nog) niet bekend.",
               hjust = 'middle',size=5,color='blue') +
      theme(axis.text =element_text(colour="black"))
  }

  if(type == 'plotWaterdiepte'){

    plot <- ggplot(db, aes(x= GAF, y= wd, col = krwwt))+
      geom_boxplot() +
      theme_minimal()+ scale_y_reverse(limits=c(3.5,0)) +
      guides(col=guide_legend(title="KRW watertype"))+
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(size= 7, angle=0,colour = 'black'),
        axis.text.y = element_text(size= 7, hjust=2,colour = 'black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank() )+
      annotate("text", x = midax , y=1.6,
               label = "Metingen waterdiepte \nzijn (nog) niet bekend.",
               hjust = 'middle',size=5,color='blue') +
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
        axis.text.x = element_text(size= 7, angle = 30, hjust = 1, colour = 'black'),
        axis.text.y = element_text(size= 7,colour = 'black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank())+
      annotate("text", x = midax , y=0,
               label = "Potentiele nalevering \nis (nog) niet bekend.",
               hjust = 'middle',size=5,color='blue') +
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
        axis.text.x = element_text(size= 7, angle = 30, hjust = 1, colour='black'),
        axis.text.y = element_text(size= 7, colour='black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank())+
      annotate("text", x = midax , y=0.8,
               label = "Actuele nalevering \nis (nog) niet bekend.",
               hjust = 'middle',size=5,color='blue') +
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Actuele nalevering uit de waterbodem\nobv poriewatermetingen") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
  
  if(type=='plotekrplot'){
    plot <- ggplot(db) + geom_point() + xlim(0, 10) + ylim(0, 100) +
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks =  element_blank(),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank())+
      annotate("text", x = 5 , y = 50,
               label = "Ecologische toestand en doelen \nzijn (nog) niet bekend.",
               hjust = 'middle', size=5, color='blue') +
      labs(x="",y=" ", fill = '')
  
   
  }


  # return plot
  return(plot)

}

# functie vor lichtklimaat en waterdiepte
ppr_extinctie1 <- function(wq, hybi, parameter = c('VEC', 'WATDTE_m')){

  # median depth of hydrobiological data
  medianewd <- median(hybi[fewsparameter %in% parameter,meetwaarde],na.rm = T)
  # meax depth of hydrobiological data
  maxwd <- max(hybi[fewsparameter %in% parameter,meetwaarde],na.rm = T)

  # mean extinctie
  wq1 <- wq[fewsparameter %in% parameter & jaar > 2015 & meetwaarde > 0,]
  wq1 <- wq1[!is.na(locatie.KRW.watertype) & !is.na(locatie.EAG) & locatie.EAG != '',]
  meanext <- mean(wq1$meetwaarde)

  # plot figure
  p <- ggplot(wq1, aes(x= locatie.EAG, y= meetwaarde))+
    geom_boxplot() +
    geom_hline(aes(yintercept = log(25)/0.5, col = '0.5 meter'), show.legend = T)+
    geom_hline(aes(yintercept = log(25), col = '1 meter'), show.legend = T)+ #vec voor 1 meter >4%
    geom_hline(aes(yintercept = log(25)/maxwd, col = paste0(as.character(maxwd), ' meter (max diepte bemonsterd)')), show.legend = T)+ #vec voor 4 meter >4%
    geom_hline(aes(yintercept = log(25)/medianewd, col = paste0(as.character(medianewd), ' meter (mediane diepte)')), show.legend = T)+ #vec voor 4 meter >4%
    geom_hline(aes(yintercept = log(25)/7, col = '7 meter'), show.legend = T)+ #vec+ voor 7 meter 4%
    guides(col=guide_legend(title="4 % licht voor waterplanten op"))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size= 7), # labels
      axis.text.y = element_text(size= 7),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      panel.background = element_blank(),
      plot.background = element_blank())+
    theme(axis.text.x = element_text(angle = 30, hjust =1)) +
    theme(legend.title = element_text(size = 6, face = 'bold'),
          legend.text  = element_text(size = 6),
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
      axis.text.x = element_text(angle = 30, hjust =1),
      axis.text.y = element_text(size= 7),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.title = element_text(size=7),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black')
      )+
    theme(legend.title = element_text(size = 7, face = 'bold'),
          legend.text  = element_text(size = 7),
          legend.key.size = unit(0.9, "lines"),
          legend.position = "right")+
    ggtitle('') +
    labs(x= '', y = 'waterdiepte (m)\n')

  # return
  return(p)

}

ppr_plotbod <- function(bod1, type='grid'){

  # dcast slootbodem
  selb <- dcast.data.table(bod1, loc.eag+loc.code+loc.oms+loc.x+loc.y+loc.z+datum+jaar ~ parm.fews+parm.compartiment, value.var = "meetwaarde", fun.aggregate = mean)

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
      title = element_text(size= 7),
      axis.text.x = element_text(size= 7),
      axis.text.y = element_text(size= 7),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.title=element_text(size=7) )+
    theme(legend.title = element_text(size = 7, face = 'bold'),
          legend.text  = element_text(size = 7),
          legend.key.size = unit(0.9, "lines"),
          legend.position = "right")+
    scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
    ggtitle( "Potentiele nalevering") +
    labs(x="",y="P mg/m2/dag\n", fill = '')

  if(!is.null(selb$FESPPWratio)){
    qPW <- ggplot(selb, aes(x= loc.eag, y= nlvrPW, fill = classFESPPWratio))+
      geom_boxplot() +
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        title = element_text(size= 7),
        axis.text.x = element_text(size= 7),
        axis.text.y = element_text(size= 7),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title=element_text(size=7) )+
      theme(legend.title = element_text(size = 7, face = 'bold'),
            legend.text  = element_text(size = 7),
            legend.key.size = unit(0.9, "lines"),
            legend.position = "right")+
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Actuele nalevering uit de waterbodem obv poriewatermetingen") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
  if(is.null(selb$FESPPWratio)){out = plotFW}
  if(!is.null(selb$FESPPWratio)){out = arrangeGrob(plotFW, qPW)}

  # plot figure
  if(type=='plotFW'){out = plotFW}
  if(type=='plotqPW'){out = qPW}
  #if(type=='grid'){grid.newpage();grid.draw(out)}
  if(type=='grid'){out = arrangeGrob(plotFW, qPW)}

  # return output
  return(out)
}
