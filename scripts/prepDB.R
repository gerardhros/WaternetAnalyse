# prepare final database
# Laura Moria & Gerard H. Ros, november-19

  # inladen basis bestanden ----
  
  # water types
  watertypen <- fread('data/KRWWatertype.csv')
  
  # locaties (van wat?)
  locaties <- fread('data/Location.csv')
  
  # locaties van EAG oppervlaktes
  eag_wl <- fread('data/Oppervlaktes EAGs en Water.csv')

  # inladen gegevens hydrobiologie
  
  
  
  hybi <- read.csv("../hydrobiologie/alles_reliable.csv", header = TRUE, na.strings = " ", sep=";", dec =".", 
                  stringsAsFactors = F)
  hybi$datum <- as.Date(hybi$datum, format = "%Y-%m-%d %H:%M")
  hybi$jaar <- format(hybi$datum, '%Y')
  hybi$jaar <- as.numeric(hybi$jaar)
  hybi$meetwaarde[hybi$limietsymbool == '<'] <- hybi$meetwaarde[hybi$limietsymbool == '<']/2 # meetwaarden
  hybi$meetwaarde[hybi$fewsparameter == 'WATDTE_m' & hybi$jaar == 2006 & hybi$planvanaanpak == 'PVA MAFY LIJN 2006 WP'] <-
    hybi$meetwaarde[hybi$fewsparameter == 'WATDTE_m' & hybi$jaar == 2006 & hybi$planvanaanpak == 'PVA MAFY LIJN 2006 WP']/10
  hybi <- merge(hybi, locaties[,c('CODE','EAGIDENT')], by.x ='locatiecode', by.y = 'CODE', all.x =FALSE, all.y=FALSE) # correctie van eags naar nieuwe shape (moet eruit als dataset is verbeterd)
  hybi$locatie.EAG <- hybi$EAGIDENT
  hybi <- merge(hybi, eag_wl[,c('watertype','GAFIDENT')], by.x ='EAGIDENT', by.y = 'GAFIDENT', all.x =FALSE, all.y=FALSE)
  hybi$locatie.KRW.watertype <- hybi$watertype
    
  
  
  # inladen EKR sets KRW en overig water
  EKRset1 <- readRDS('hydrobiologie/EKRset_KRW.rds') %>% as.data.table()
  EKRset2 <- readRDS('hydrobiologie/EKRset_OvWater.rds') %>% as.data.table()
  EKRset <- rbind(EKRset1,EKRset2,fill=T)
  
  # add coordinates
  EKRset[,XCOORD := as.integer(XCOORD)]
  EKRset[,YCOORD := as.integer(YCOORD)]
  rm(EKRset1,EKRset2)
  
  # remove columns without information
  cols <- colnames(EKRset)[unlist(EKRset[,lapply(.SD,function(x) sum(is.na(x))==nrow(EKRset))])]
  EKRset[,c(cols):= NULL]
  
  # inladen KRW doelen 
  doelen <- fread('hydrobiologie/doelen.csv')
  
  
  
    