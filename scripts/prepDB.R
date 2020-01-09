# prepare final database
# Laura Moria & Gerard H. Ros, november-19

  # clear environment
  rm(list=ls())
  
  # require packages
  require(data.table);require(sf);require(dplyr)

  # source functions
  source('scripts/ppr_funs.R')
  
  # inladen basis bestanden ----
  
    # water types
    watertypen <- fread('data/KRWWatertype.csv')
    
    # locaties van alle metingen (water, biologie, en slootbodem)
    locaties <- fread('data/Location.csv')
    
    # locaties van EAG oppervlaktes
    eag_wl <- fread('data/Oppervlaktes EAGs en Water.csv')
  
    # shape with EAG
    gEAG <- st_read('data/EAG20180612.gpkg',quiet = TRUE)

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
    bod <- ppr_slootbodem(db = bod, wtype = eag_wl)
    
    # water quality
    wq <-  ppr_wq(db = wq, syear = 2000, wtype = eag_wl, mlocs = locaties)
      
    # Pload and limits from PC Ditch
    Overzicht_kP <- ppr_pcditch(db = Overzicht_kP)

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
    
    
    dat <- loadBalances_lm(dir_bal = dir_bal,kopTab = kopTab,sfile = FALSE)
    
  
# make final matrix
    
     makeMatrix <- function(EKRset, bod, wq, hybi, dat){
      
      #laatste 3 meetjaren EKR scores ---------
      
        # remove some data rows
        EKRinp <- EKRset[!Grootheid.code %in% c("AANTPVLME", "SOORTRDM")]
      
        # derive mean EKR for last three years
        krw <- tabelPerWL3jaargemEAG(EKRset = EKRinp,gEAG = gEAG,doelen)
      
        # rename GHPR in more readible (and less long names)
        krw[,GPHRnew := renameGHPR(GHPR)]
        krw[,wbmethode := renameWbmethode(wbmethode)]
        
        # remove all data rows without EAGIDENT
        krw <- krw[!is.na(EAGIDENT)]
        
      # dcst om maatlatten aprt te beschouwen en relaties tussen maatlatten te leggen
      krw <- dcast(krw, EAGIDENT + geo_id + watertype ~ GPHRnew + wbmethode, 
                    value.var = "EKR", fun.aggregate = mean)
      
      # koppel op jaar? nee want dat doet P belasting en bodem ook niet en dan zijn er te veel mismatches
      krw[,GAF := substr(EAGIDENT, 1, 4)]
      
      # p vskp ---------------
      PvskP <- makePmaps(dbwbal = dat, dbhybi = hybi,dbnomogram = nomogram,
                         dbov_kP = Overzicht_kP, dbeag_wl = eag_wl)

      # bodem toevoegen ------------------
      gemwaterbod <- bodsam(bod,cmean = TRUE)
      
      # hybi indicatoren matrix maken obv mediaan per gebied en gemiddelde over jaren van laatste drie meetjaren ------
      # compartiment slecteren en soms EZ en soms OW: nog niet gedaan
      d4 <- calcMeanHybi(dbhybi = hybi)
      
      # add water quality data
      wq <- copy(wq)

        
     # this is for wq1
      db <- db[fewsparametercategorie %in% srow & jaar > 2015]
      # dcast, aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.)
      db <- dcast(db,locatie.EAG ~ fewsparameter+fewsparameterparameterfractie+fewsparametereenheidequivalent+eenheid, 
                  mean, value.var = c("meetwaarde"))
      
      # rename columns
      # misschien zomerhalfjaargemiddelden toevoegen?
      setnames(db,gsub("___","_",colnames(db)))
      setnames(db,gsub("__","_",colnames(db)))
      
   # merge tot matrix -----
      
      matrix1 <- sp::merge(PvskP, krw, by.x = 'EAG', by.y = 'EAGIDENT', all.y = TRUE)
      
      matrix2 <-sp::merge(matrix1,gemwaterbod,by.x = 'EAG', by.y = 'locatie.EAG', all.x = TRUE)
      
      matrix4 <- sp::merge(matrix2, d4,by.x = 'EAG', by.y = 'locatie.EAG', all.x = TRUE)
      
      matrix5 <- sp::merge(matrix4,wq1, by.x = 'EAG', by.y = 'locatie.EAG', all.x = TRUE)
      
      saveRDS(matrix5,'../matrix/matrix.rds')
      write.table(matrix5, file = paste("../matrix/matrix",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
      return(matrix5)
    }
    

     # part of tabelPerWL3jaargemEAG --- 
     # add type water body
     db[,wl := sapply(strsplit(id, '_'), `[`, 2)]
     db[is.na(wl), wl := paste0('gewogen_',id)]
     db[wl=='OvWa',wl := sapply(strsplit(id, '_'), `[`, 3)]
     db[!is.na(EAGIDENT), wl := EAGIDENT]
     
     # merge with EAG shape
     d3 <- merge(d2, gEAG[,c('GAFIDENT','GAFNAAM')], by.x = 'wl',by.y = 'GAFIDENT', all.x = TRUE)
     
     # setnames
     setnames(d3,c('wl','id'),c('waterlichaam','geo_id'))
     
     # remove the geo component
     d3[,geom := NULL]
    
    
    
    