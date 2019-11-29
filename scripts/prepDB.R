# prepare final database
# Laura Moria & Gerard H. Ros, november-19

  # inladen basis bestanden ----
  
  # water types
  watertypen <- fread('data/KRWWatertype.csv')
  
  # locaties van alle metingen (water, biologie, en slootbodem)
  locaties <- fread('data/Location.csv')
  
  # locaties van EAG oppervlaktes
  eag_wl <- fread('data/Oppervlaktes EAGs en Water.csv')

  # inladen hydrobiologische gegevens ------
  
    # inladen gegevens hydrobiologie
    hybi <- readRDS('data/alles_reliable.rds')
    
    # adapt few properties
    hybi[, datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
    hybi[, jaar := year(datum)]
    hybi[limietsymbool == '<', meetwaarde := meetwaarde * 0.5]
    hybi[fewsparameter == 'WATDTE_m' & jaar == 2006 & planvanaanpak == 'PVA MAFY LIJN 2006 WP', meetwaarde := meetwaarde]
    
    # merge with locaties
    hybi <- merge(hybi,locaties[,c('CODE','EAGIDENT')],by.x ='locatiecode', by.y = 'CODE')
    hybi <- merge(hybi,eag_wl[,c('watertype','GAFIDENT')],by.x ='EAGIDENT', by.y = 'GAFIDENT')
    
    # add codes (is this really needed?)
    hybi[,locatie.EAG := EAGIDENT]
    hybi[,locatie.KRW.watertype := watertype]
  
    # remove columns with no data
    cols <- colnames(hybi)[unlist(hybi[,lapply(.SD,function(x) sum(is.na(x))==nrow(hybi))])]
    hybi[,c(cols) := NULL]
    
   # inladen EKR sets KRW en overig water ------  
  
    # inladen EKR sets KRW en overig water
    EKRset1 <- readRDS('hydrobiologie/EKRset_KRW.rds') %>% as.data.table()
    EKRset2 <- readRDS('hydrobiologie/EKRset_OvWater.rds') %>% as.data.table()
    EKRset <- rbind(EKRset1,EKRset2,fill=T)
    
    # add coordinates
    EKRset[,XCOORD := as.integer(XCOORD)]
    EKRset[,YCOORD := as.integer(YCOORD)]
    
    # remove sets
    rm(EKRset1,EKRset2)
    
    # remove columns without information
    cols <- colnames(EKRset)[unlist(EKRset[,lapply(.SD,function(x) sum(is.na(x))==nrow(EKRset))])]
    EKRset[,c(cols):= NULL]
  
  # inladen KRW doelen 
  doelen <- fread('hydrobiologie/doelen.csv')
  
  
  
    