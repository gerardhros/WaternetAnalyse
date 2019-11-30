# prepare final database
# Laura Moria & Gerard H. Ros, november-19

  # require packages
  require(data.table);require(sf);require(dplyr)

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
    
    # filter measurements on periode since 2000
    hybi <- hybi[jaar>=2000]
    
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
    
    # remove sets
    rm(EKRset1,EKRset2)
    
    # remove columns without information
    cols <- colnames(EKRset)[unlist(EKRset[,lapply(.SD,function(x) sum(is.na(x))==nrow(EKRset))])]
    EKRset[,c(cols):= NULL]
  
  # inladen KRW doelen 
  doelen <- fread('hydrobiologie/doelen.csv')
  
  # inladen waterbalans data (made by loadBalances)
  dat <- readRDS("pbelasting/dat.rds") 
  
  # load datafile with P-load PC Ditch-----------
  
    # load datafile
    Overzicht_kP <- fread('data/Overzicht_kP.csv') 
    
    # rename columns
    setnames(Overzicht_kP,c('EAG','GAF','EAGnaam','plv_o2','plv','opp','diepte','fr_moeras','strijklengte',
                            'debiet','inflow','extinctie','sedimenttype','pgrens_helder_troebel',
                            'pgrens_troebel_helder','type','morfologie','systeemgrens','p_load'))
  
    # remove columns
    cols <- c('opp','EAGnaam')
    Overzicht_kP[,c(cols):=NULL]
    
    # load nonogram
    nomogram <- fread('data/nomogram.csv')
  
    