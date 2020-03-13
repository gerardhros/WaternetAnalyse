# prepare final database
# Laura Moria & Gerard H. Ros, november-19

  # clear environment
  rm(list=ls())
  
  # require packages
  require(data.table);require(sf);require(dplyr)

  # source functions
  source('scripts/ppr_funs.R')
  source('scripts/calc_funs.R')
  
  # inladen basis bestanden ----
  
    # water types
    watertypen <- fread('data/KRWWatertype.csv')
    
    # locaties van alle metingen (water, biologie, en slootbodem)
    locaties <- fread('data/Location.csv')
    
    # locaties van EAG oppervlaktes
    eag_wl <- fread('data/Oppervlaktes EAGs en Water.csv')
  
    # shape with EAG
    gEAG <- st_read('data/EAG20191205.gpkg',quiet = TRUE)

    # KRW doelen 
    doelen <- fread('hydrobiologie/doelen.csv')

    # nonogram
    nomogram <- fread('data/nomogram.csv')
    
    # waterbalans data (made by loadBalances)
    dat <- readRDS("pbelasting/dat.rds") 
    
    # gegevens hydrobiologie
    hybi <- readRDS('data/alles_reliable.rds')
    
    # EKR sets KRW en overig water
    EKRset1 <- readRDS('hydrobiologie/EKRset_KRW.rds') %>% as.data.table()
    EKRset2 <- readRDS('hydrobiologie/EKRset_OvWater.rds') %>% as.data.table()
    
    # slootbodem measurements
    bod  <- fread("data/bodemfews.csv")
    
    # waterquality measurements
    wq  <- readRDS("data/ImportWQ.rds") %>% as.data.table()
    
    # datafile with P-load PC Ditch
    Overzicht_kP <- fread('data/Overzicht_kP.csv') 
    
    # toxiciteitsdata simoni
    simoni <- readRDS('data/simoni.rds')
    
  # update, filter and clean up databases -----------
   
    # EKR measurements
    EKRset <- ppr_ekr(ekr1 = EKRset1,ekr2 = EKRset2)
 
    # hybi measurements
    hybi <- ppr_hybi(db = hybi,syear = 2000,wtype = eag_wl,mlocs = locaties)
    
    # slootbodemdata
    bod <- ppr_slootbodem(db = bod, wtype = eag_wl,mlocs = locaties)
    
    # water quality
    wq <-  ppr_wq(db = wq, syear = 2000, wtype = eag_wl, mlocs = locaties)
      
    # Pload and limits from PC Ditch
    Overzicht_kP <- ppr_pcditch(db = Overzicht_kP)

    # load waterbalances (only when needed)
    if(FALSE){dat <- loadBalances_lm(dir_bal = dir_bal,kopTab = kopTab,sfile = FALSE)}
    
  # calculate means per EAG or meetpunt -----------
    
    # calculate mean EKR per EAG
    krw <- calc_mean_EKR(db = EKRset, nyears = 3,pEAG = TRUE, pYEAR = FALSE, pSEASON = FALSE)

    # give GEP and oordeel
    krw[,c('GEP','oordeel') := eval_EKR(id,GHPR,EKR,doelen)]
    
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
    
    saveRDS(matrix5,'../matrix/matrix.rds')
    
