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

  # shape met EAG
  gEAG <- st_read('data/EAG20180612.gpkg',quiet = TRUE)
  
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
  
  # add slootbodem data
    
    # load csv file with measurement data
    bod  <- fread("data/bodemfews.csv")
    
    # adapt file
    bod[,datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
    bod[,jaar := year(datum)]
    bod[limietsymbool == '<',meetwaarde := meetwaarde * 0.5] 
    
    # merge with GAFIDENT from eag_wl
    bod <- merge(bod,eag_wl[,c('watertype','GAFIDENT')],by.x='locatie EAG',by.y = 'GAFIDENT',all = FALSE)
    
    # wijzig relevante namen van bodemfews database
    cols <- colnames(bod)
    setnames(bod,c('locatie EAG','locatiecode','locatie omschrijving','locatie x','locatie y','locatie z','fewsparameter','compartiment'),
             c('loc.eag','loc.code','loc.oms','loc.x','loc.y','loc.z','parm.fews','parm.compartiment'))
    
    # select properties and dcast table
    selb <- dcast(bod, loc.eag+loc.code+loc.oms+loc.x+loc.y+loc.z+datum+jaar ~ parm.fews+parm.compartiment, value.var = "meetwaarde", fun.aggregate = mean)
    
    # adapt P measurement into one class, and NA gets class 8
    selb[,klasseP := cut(`Ptot_gP/kg_dg_BS`,breaks = c(0,0.5,1,1.5,2.5,5,10,1000),labels=1:7)]
    selb[,klasseP := factor(klasseP,levels=1:8)]
    selb[is.na(klasseP), klasseP := 8]
    
  # load water quality information
  
    # load file rds
    wq  <- readRDS("data/ImportWQ.rds") %>% as.data.table()
    
    # adapt wq database
    wq[,jaar := year(datum)]
    wq[,maand := month(datum)]
    wq[limietsymbool == '<',meetwaarde := meetwaarde * 0.5]
    
    # delete years before 2000
    wq <- wq[jaar>1999,]
    
    # merge with locaties, remove older EAGIDENT with new one
    wq[,EAGIDENT := NULL]
    wq <- merge(wq,locaties[,c('CODE','EAGIDENT')], by.x ='locatiecode', by.y = 'CODE',all.x = T)
    wq <- merge(wq,eag_wl[,c('watertype','GAFIDENT')], by.x ='EAGIDENT', by.y = 'GAFIDENT', all =FALSE)
    
  # simoni : toxiciteitsdata
    
    # inladen van database
    simoni <- readRDS('data/simoni.rds')

# make final matrix
    
    
    # calculate mean EKR score per EAG over last three years
    
    tabelPerWL3jaargemEAG <- function (EKRset,gEAG,doelen){
      
      # calculate mean per groep
      colgroup <-c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code.y',
                    'Waardebepalingsmethode.code','GHPR_level','GHPR','level','jaar')
      d1 <- EKRset[,.(waarde = mean(Numeriekewaarde,na.rm=TRUE)),by=colgroup]
             
      # rename columns and order data.table
      setnames(d1,colgroup,c('id','EAGIDENT','watertype','wbmethode','GHPR_level','GHPR','level','jaar'))
      setorder(d1,EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode,-jaar)
      
      # add year number (given ordered set), and take only three most recent years
      d1 <- d1[,yearid := seq_len(.N),by=.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode)][yearid < 4]
      
      # calculate mean EKR per group over the three years
      d1 <- d1[,.(EKR = mean(waarde,na.rm=T)),by =.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode)]
      
      # remove empty spaces in GHPR needed for joining later
      d1[,GHPR := gsub(' ','',GHPR)]
      
      # merge with doelen
      
      # rename columns doelen object
      setnames(doelen,c('HoortBijGeoobject.identificatie'),c('id'))
      
      # mean GEP per object
      doelgeb <- doelen[,.(GEP = mean(Doel,na.rm=TRUE)),by =.(id,bronddoel,GHPR)]
      
      # merge with doelen
      d2 <- merge(d1, doelgeb, by = c('id','GHPR'), all.x = TRUE)
      
      # add classification for EKR
      d2[EKR < GEP/3,oordeel := 'slecht']
      d2[EKR >= GEP/3 & EKR < 2 * GEP / 3,oordeel := 'ontoereikend']
      d2[EKR >= 2 * GEP / 3,oordeel := 'matig']
      d2[EKR >= GEP, oordeel := 'goed']
      
      # add type water body
      d2[,wl := sapply(strsplit(id, '_'), `[`, 2)]
      d2[is.na(wl), wl := paste0('gewogen_',id)]
      d2[wl=='OvWa',wl := sapply(strsplit(id, '_'), `[`, 3)]
      d2[!is.na(EAGIDENT), wl := EAGIDENT]
      
      # merge with EAG shape
      d3 <- merge(d2, gEAG[,c('GAFIDENT','GAFNAAM')], by.x = 'wl',by.y = 'GAFIDENT', all.x = TRUE)
      
      # return the object
      return(d3)
     }
    
  
      
    makeMatrix <- function(EKRset, bod, wq, hybi, dat){
      #laatste 3 meetjaren EKR scores ---------
      krw <- tabelPerWL3jaargemEAG(EKRset[!EKRset$Grootheid.code %in% c("AANTPVLME", "SOORTRDM"),])
      # dcst om maatlatten aprt te beschouwen en relaties tussen maatlatten te leggen
      krw <- dcast(krw, EAGIDENT+HoortBijGeoobject.identificatie+KRWwatertype.code.y ~ GHPR+Waardebepalingsmethode.code, value.var = "EKR", fun.aggregate = mean)
      # koppel op jaar? nee want dat doet P belasting en bodem ook niet en dan zijn er te veel mismatches
      krw$GAF <- substr(krw$EAGIDENT, 1, 4)
      krw <- krw[!is.na(krw$EAGIDENT),]
      
      # p vskp ---------------
      PvskP <- makePmaps(dat)
      PvskP <- PvskP[!is.na(PvskP$pol),]
      PvskP <- PvskP[!is.na(PvskP$EAG) |!is.na(PvskP$GAF),]
      PvskPeag <- PvskP[!is.na(PvskP$EAG),]
      PvskPgaf <- PvskP[!is.na(PvskP$GAF),]
      eag_wl$GAF<- substr(eag_wl$GAFIDENT, 1, 4)
      PvskPgaf <- merge(PvskPgaf,eag_wl[,c("GAFIDENT","GAF")], by = "GAF")
      PvskPgaf$EAG <- PvskPgaf$GAFIDENT; PvskPgaf$GAFIDENT <- NULL
      PvskPgaf <- PvskPgaf[!PvskPgaf$EAG %in% PvskP$EAG,]
      PvskP <- smartbind(PvskPeag, PvskPgaf)
      
      # bodem toevoegen ------------------
      gemwaterbod <- bodsam(bod)
      gemwaterbod <- gemwaterbod  %>%
        group_by(locatie.EAG,jaar) %>% 
        summarize_if(is.numeric, median)
      
      # hybi indicatoren matrix maken obv mediaan per gebied en gemiddelde over jaren van laatste drie meetjaren ------
      # compartiment slecteren en soms EZ en soms OW: nog niet gedaan
      b = dcast(hybi, locatiecode+locatie.EAG+locatie.KRW.watertype+compartiment+jaar~fewsparameter+parametercode+parameterfractie, 
                value.var = "meetwaarde", fun.aggregate = mean)
      b$DTEZICHT <- ifelse(b$ZICHT_m_ZICHT_/b$WATDTE_m_WATDTE_ > 1, NaN, b$ZICHT_m_ZICHT_/b$WATDTE_m_WATDTE_)
      b$DTEZICHTfac <- cut(b$DTEZICHT, breaks = c('0.1','0.2','0.3','0.4','0.6', '0.8','1.0'))
      b <- b[!is.na(b$DTEZICHTfac) & !is.na(b$jaar) &!is.na(b$locatie.EAG),]
      b <- b[order(b$jaar),]
      
      c = b %>%
        dplyr::select(locatie.EAG,
                      locatie.KRW.watertype,
                      jaar,
                      bedsubmers =`PTN_BEDKG_%_SUBMSPTN_` , 
                      draadwieren =`PTN_BEDKG_%_FLAB_SUBMS` ,
                      FLAB = `PTN_BEDKG_%_FLAB_DRIJVD` ,
                      bedemers= `PTN_BEDKG_%_EMSPTN_`,
                      taludhoek = TALBVWTR_graad_TALBVWTR_ ,
                      doorzicht= ZICHT_m_ZICHT_ ,
                      waterdiepte = WATDTE_m_WATDTE_,
                      dieptedoorzicht = DTEZICHT  ,
                      dieptedoorzichtfac = DTEZICHTfac,
        )%>%
        group_by(locatie.EAG,locatie.KRW.watertype,jaar) %>% 
        summarize_if(is.numeric,median,na.rm =TRUE)
      
      d3 <- c %>% 
        arrange(locatie.EAG,locatie.KRW.watertype,desc(jaar)) %>% 
        group_by(locatie.EAG,locatie.KRW.watertype) %>% 
        top_n(3, wt = jaar) 
      
      d4 = d3 %>%
        group_by(locatie.EAG,locatie.KRW.watertype) %>% 
        summarize_all(mean)
      
      #misschien zomerhalfjaargemiddelden toevoegen?
      wq1 <- dcast(wq[wq$jaar > 2015 & wq$fewsparametercategorie %in% c("IONEN","NUTRI","ALG","VELD","ALGEN","LICHT"),],locatie.EAG ~ fewsparameter+fewsparameterparameterfractie+fewsparametereenheidequivalent+eenheid, mean, value.var = c("meetwaarde"))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
      colnames(wq1) <- gsub("___","_", colnames(wq1))
      colnames(wq1) <- gsub("__","_", colnames(wq1))
      
      # merge tot matrix -----
      
      matrix1 <- sp::merge(PvskP, 
                           krw, 
                           by.x = 'EAG', by.y = 'EAGIDENT', all.y = TRUE)
      
      matrix2 <-sp::merge(matrix1, 
                          gemwaterbod, 
                          by.x = 'EAG', by.y = 'locatie.EAG', all.x = TRUE)
      
      matrix4 <- sp::merge(matrix2, 
                           d4, 
                           by.x = 'EAG', by.y = 'locatie.EAG', all.x = TRUE)
      
      matrix5 <- sp::merge(matrix4, 
                           wq1, 
                           by.x = 'EAG', by.y = 'locatie.EAG', all.x = TRUE)
      
      saveRDS(matrix5,'../matrix/matrix.rds')
      write.table(matrix5, file = paste("../matrix/matrix",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
      return(matrix5)
    }
    
    
    
    
    
    