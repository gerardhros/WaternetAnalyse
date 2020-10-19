# prepare final database
# Laura Moria & Gerard H. Ros, november-19

  # clear environment
  rm(list=ls())
  
  # require packages
  require(data.table);require(sf);require(dplyr)

  # source functions
  source('scripts/ppr_funs.R')
  source('scripts/calc_funs.R')
  source('scripts/createOutput_gr.R')
  
  # inladen basis bestanden ----
  
    # water types
    watertypen <- fread('data/KRWWatertype.csv')
    
    # locaties van alle metingen (water, biologie, en slootbodem)
    locaties <- fread('data/Location.csv')
    
    # locaties van EAG oppervlaktes
    eag_wl <- fread('data/EAG_Opp_kenmerken_20200218.csv')
    eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]
    
    # shape with EAG
    gEAG <- sf::st_read("data/EAG20191205.gpkg",quiet = T) %>% sf::st_transform(28992)
  
    # KRW doelen 
    doelen <- ppr_doelen()
   
    # nonogram
    nomogram <- fread('data/nomogram.csv')
    
    # waterbalans data (made by loadBalances)
    dat <- readRDS("pbelasting/dat.rds")  
    dat[,date := as.POSIXct(paste0(jaar,"-",maand,"-01"), format = '%Y-%m-%d')]
    
    # gegevens hydrobiologie
    hybi <- readRDS('data/alles_reliable.rds')
    hybi <- ppr_hybi(db = hybi, syear = 2000, wtype = eag_wl, mlocs = locaties)
    
    # EKR sets KRW en overig water
    EKRset1 <- readRDS('hydrobiologie/EKRsetKRW.rds') %>% as.data.table()
    EKRset2 <- readRDS('hydrobiologie/EKRsetOvWater.rds') %>% as.data.table()
    EKRset <- ppr_ekr(krwset = EKRset1, ovwatset = EKRset2,eag_wl = eag_wl, doelen = doelen)
    
      # noodgreep omdat er fouten zitten in de toetsgegevens
      EKRset$KRWwatertype.code[EKRset$Identificatie == 'VaartenRondeHoep'] <- 'M8'
      EKRset$KRWwatertype.code[EKRset$Identificatie == 'VaartenZevenhoven'] <- 'M1a'
      
      # select alleen nieuwe maatlatten
      EKRset <- EKRset[!Waardebepalingsmethode.code %in% c("Maatlatten2012 Ov. waterflora","Maatlatten2012 Vis"),]
      
    
    # slootbodem measurements
    bod  <- fread("data/bodemfews.csv")
    bod <- ppr_slootbodem(db = bod, wtype = eag_wl, mlocs = locaties)
    
    # waterquality measurements
    wq  <- readRDS("data/ImportWQ.rds") %>% as.data.table()
    wq <-  ppr_wq(db = wq, syear = 2000, wtype = eag_wl, mlocs = locaties)
    
    # datafile with P-load PC Ditch
    Overzicht_kP <- fread('data/Overzicht_kP.csv') 
    Overzicht_kP <- ppr_pcditch(db = Overzicht_kP)
    
    # toxiciteitsdata simoni
    simoni <- readRDS('data/simoni.rds')
    
  # update, filter and clean up databases -----------
   
    # load waterbalances (only when needed)
    if(FALSE){dat <- loadBalances_lm(dir_bal = dir_bal,kopTab = kopTab,sfile = FALSE)}
    
  # calculate means per EAG -----------
    
    # calculate mean EKR per EAG
    krw <- calc_mean_EKR(db = EKRset, nyears = 3,pEAG = TRUE, pYEAR = FALSE, pSEASON = FALSE)

    # rename GHPR in more readible (and less long names)
    krw[,GPHRnew := renameGHPR(GHPR)]
    krw[,wbmethode := renameWbmethode(wbmethode)]
    
    # remove all data rows without EAGIDENT
    krw <- krw[!is.na(EAGIDENT)]
    
    # dcast om maatlatten aprt te beschouwen en relaties tussen maatlatten te leggen
    krw <- dcast(krw, EAGIDENT + id + watertype ~ GPHRnew + wbmethode, 
                 value.var = "EKR", fun.aggregate = mean)
    # add GAF code
    krw[,GAF := substr(EAGIDENT, 1, 4)]
    
    # pvskp 
    PvskP <- makePmaps(dbwbal = dat, dbhybi = hybi,dbnomogram = nomogram,
                       dbov_kP = Overzicht_kP, dbeag_wl = eag_wl)
    
    # bodem toevoegen 
    gemwaterbod <- bodsam(bod,cmean = TRUE)
    
    # hybi indicatoren matrix maken obv mediaan per gebied en gemiddelde over jaren van laatste drie meetjaren ------
    # compartiment slecteren en soms EZ en soms OW: nog niet gedaan
    d4 <- calcMeanHybi(dbhybi = hybi)
    
    # this is for wq1
    srow <- c("IONEN","NUTRI","ALG","VELD","ALGEN","LICHT")
    wq1 <- wq[fewsparametercategorie %in% srow & jaar > 2015]
    # dcast, aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
    wq1 <- dcast(wq1,locatie.EAG ~ fewsparameter+fewsparameterparameterfractie+fewsparametereenheidequivalent+eenheid, 
                mean, value.var = c("meetwaarde"))
    # reset names
    colnames(wq1) <- gsub("___|__","_", colnames(wq1))
    
    # merge tot matrix -----
    matrix1 <- merge(PvskP, krw, by.x = 'EAG', by.y = 'EAGIDENT', all.y = TRUE)
    matrix2 <- merge(matrix1,gemwaterbod,by.x = 'EAG', by.y = 'loc.eag', all.x = TRUE)
    matrix4 <- merge(matrix2, d4,by.x = 'EAG', by.y = 'locatie.EAG', all.x = TRUE)
    matrix5 <- merge(matrix4,wq1, by.x = 'EAG', by.y = 'locatie.EAG', all.x = TRUE)
    
    saveRDS(matrix5,'matrix/201005_matrix.rds')
    
  # calculate means per sampling point -----------
    
    # calculate mean EKR per point
    krw.mp <- calc_mean_EKR(db = EKRset, nyears = 3,pEAG = FALSE, pYEAR = TRUE, pSEASON = TRUE)
    
    # rename GHPR in more readible (and less long names)
    krw.mp[,GPHRnew := renameGHPR(GHPR)]
    krw.mp[,wbmethode := renameWbmethode(wbmethode)]
    
    # select maatlat 2018, and remove maatlat vis en maatlat 2012
    krw.mp <- krw.mp[grepl('ml_2018',wbmethode) & !grepl('vis$',wbmethode)]
    
    # dcast om maatlatten aprt te beschouwen en relaties tussen maatlatten te leggen
    krw.mp.ekr <- dcast(krw.mp, mpid + jaar + season + watertype + EAGIDENT  + KRW_SGBP3 ~ GPHRnew + wbmethode, 
                        value.var = "EKR", fun.aggregate = mean)
    
    # add GAF code
    krw.mp.ekr[,GAF := substr(EAGIDENT, 1, 4)]

    # update mp-id
    krw.mp.ekr[,mpid2 := tstrsplit(mpid,'_',keep=1)]
    
    # add info for db Locaties
    loc.sel <- locaties[,.(CODE,WATERTYPE,MORFOLOGIE,BODEMSOORT,
                           GAFIDENT,EAGIDENT,OWMIDENT,XCOORD,YCOORD)]
    setnames(loc.sel,paste0('loc_',colnames(loc.sel)))
    krw.mp.ekr <- merge(krw.mp.ekr,loc.sel,by.x = 'mpid2', by.y = 'loc_CODE',all.x = TRUE)
    
    # add info waterkwaliteitsmetingen, given season and year
    wq.sel <- wq[,.(locatiecode,fewsparameter,meetwaarde,jaar,maand)]
    wq.sel[,season := fifelse(maand %in% 4:10,'summer','winter')]
    wq.sel <- wq.sel[,.(meetwaarde = mean(meetwaarde,na.rm=T)),by=.(locatiecode,fewsparameter,jaar,season)]
    wq.par <- c('P','PO4','KJN','N','NH4','ZICHT','NO3NO2','CL',
               'FEO','O2','PH','T','DOC','WATDTE','S','CHLFA',
               'FLUOGROE','FLUOBLAU','FLUODIAT','FLUOCRYP','FLUOTOT')
    wq.sel <- wq.sel[fewsparameter %in% wq.par]
    wq.sel <- wq.sel[,fewsparameter := tolower(fewsparameter)]
    wq.sel <- dcast(wq.sel,locatiecode+jaar+season~fewsparameter,value.var = 'meetwaarde')
    
    wq.sel2 <- unique(wq[,.(locatiecode)])
    wq.sel2 <- merge(wq.sel2,loc.sel[,.(loc_CODE,loc_XCOORD,loc_YCOORD,loc_EAGIDENT)],by.x='locatiecode',by.y = 'loc_CODE',all.x = TRUE)
    wq.sel2 <- sf::st_as_sf(wq.sel2,coords = c('loc_XCOORD','loc_YCOORD'),crs = 28992)
    
    krw.mp.ekr.sf <- unique(krw.mp.ekr[!is.na(loc_XCOORD),.(mpid2,loc_XCOORD,loc_YCOORD,EAGIDENT)])
    krw.mp.ekr.sf <- sf::st_as_sf(krw.mp.ekr.sf,coords = c('loc_XCOORD','loc_YCOORD'),crs = 28992)
  
    wq.kop <- sf::st_intersection(st_buffer(krw.mp.ekr.sf,dist = 1000),wq.sel2)
    wq.kop <- as.data.table(wq.kop)
    wq.kop <- wq.kop[EAGIDENT==loc_EAGIDENT][,geometry := NULL]
    
    wq.kop.mean <- merge(krw.mp.ekr[,.(mpid2,jaar,season)],wq.kop[,.(mpid2,locatiecode)],by='mpid2',
                         allow.cartesian = TRUE,all.x = TRUE)
    wq.kop.mean <- merge(wq.kop.mean, wq.sel,by=c('locatiecode','jaar','season'),all.x=TRUE)
    wq.kop.mean <- melt(wq.kop.mean,id.vars = c('mpid2','locatiecode','jaar','season'),variable.name = 'fewsparameter',value.name = 'meetwaarde')
    wq.kop.mean <- wq.kop.mean[!is.na(meetwaarde)]
    wq.kop.mean <- wq.kop.mean[,.(meetwaarde = median(meetwaarde,na.rm=TRUE)),by=.(mpid2,jaar,season,fewsparameter)]
    
    
    krw.mp.ekr <- merge(krw.mp.ekr)
    test = krw.mp.ekr$mpid2
    test = test[!test %in% wq.kop.mean$mpid2]
                        
    
    PvskP.mp <- makePmaps(dbwbal = dat, dbhybi = hybi,dbnomogram = nomogram,
                       dbov_kP = Overzicht_kP, dbeag_wl = eag_wl)
        
    
    