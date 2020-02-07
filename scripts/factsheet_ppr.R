# factsheet preprocess
# authors: Laura Moria, Sven Verweij en Gerard Ros

# clear environment
rm(list=ls())  

# source functions
source('../scripts/ppr_funs.R')
source('../scripts/loadPackages.R')
source('../scripts/factsheetfuncties.R')

#  Directories and names------------------------------------------
dirGIS <-"../data"

# other settings ---------------------------
proj4.rd <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.87740,4.0725 +units=m +no_defs")
proj4.google <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4.osm <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
col <- c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red")
labels <- c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2")

# inladen basis bestanden ----

  # ESF oordelen
  ESFoordelen <- fread('../data/esfKRW_20200107.csv') # hier later ook eags of gafs aan toevoegen
  cols <- colnames(ESFoordelen[,11:25])
  ESFoordelen[,(cols) := lapply(.SD, function(x) trim(x)),.SDcols = cols]

  # maatregelen
  maatregelen <- fread('../data/maatregelenKRW_20200124.csv')# hier later ook eags of gafs aan toevoegen
  cols <- c('HoortbijKRWWaterlichaam2021','HoortbijKRWWaterlichaamNaam2021')
  maatregelen[,(cols):=NULL]
  maatregelen <- unique(maatregelen) 
  cols <- colnames(maatregelen[,2:25])
  maatregelen[,(cols) := lapply(.SD, function(x) trim(x)),.SDcols = cols]
  

## data voor kaart ----------
gEAG<- st_read("../data/EAG20191205.gpkg") %>% st_transform(proj4.rd)
waterpereag1 <- st_read("../data/WaterPerEAG20191205.gpkg") %>% st_transform(proj4.rd)
waterschappen  <- importOGR("2019_gemeentegrenzen_kustlijn_simplified.shp", dirGIS, proj4.rd)

# inladen hydrobiologische gegevens -----------
  # inladen gegevens hydrobiologie
  hybi <- readRDS('../data/alles_reliable.rds')
  
  # water types
  watertypen <- fread('../data/KRWWatertype.csv')
  
  # locaties van alle metingen (water, biologie, en slootbodem)
  locaties <- fread('../data/Location.csv')

  ## aanvullende eag data, krwwatertype niet overal ingevuld en stedelijk landelijk voor EST
  ## let op: als nieuwe EAG (gEAG) dan deze tabel aanpassen en aanvullen
  eag_wl <- fread('../data/EAG_Opp_kenmerken.csv')
  #eag_wl <- eag_wl[is.na(eag_wl$Einddatum),] # sommige EAGs bestaan niet meer in EAGs, maar wel in krw dataset.       Deze opnieuw maken. vaarten ronde hoep, westeramstel, ronde venen en zevenhoven en vecht waterlichaam koppeling en watertype aangepast in         eag_opp-kenmerken aangepast omdat er anders geen kaart wordt weergegeven en geen ekrset data wordt geselecteerd.
  
  # KRW doelen 
  doelen <- fread('../hydrobiologie/doelen.csv')
  
  # nonogram
  nomogram <- fread('../data/nomogram.csv')
  
  # waterbalans data (made by loadBalances)
  dat <- readRDS("../pbelasting/dat.rds") 
  
  # EKR sets KRW en overig water
  EKRset1 <- readRDS('../hydrobiologie/EKRsetKRW.rds') %>% as.data.table()
  EKRset2 <- readRDS('../hydrobiologie/EKRsetOvWater.rds') %>% as.data.table()
  
  # slootbodem measurements
  bod  <- fread("../data/bodemfews.csv")

  # waterquality measurements
  wq  <- readRDS("../data/ImportWQ.rds") %>% as.data.table()
  
  # datafile with P-load PC Ditch
  Overzicht_kP <- fread('../data/Overzicht_kP.csv') 
  
  # toxiciteitsdata simoni
  simoni <- readRDS('../data/simoni.rds')

# update, filter and clean up databases -----------

  # EKR measurements
  EKRset <- ppr_ekr(ekr1 = EKRset1,ekr2 = EKRset2)
  
  # hybi measurements
  hybi <- ppr_hybi(db = hybi, syear = 2000, wtype = eag_wl, mlocs = locaties)
  
  # slootbodemdata, hier moeten 2 verschillende datatables uitkomen
  bod <- ppr_slootbodem(db = bod, wtype = eag_wl, mlocs = locaties)
  
  # water quality
  wq <-  ppr_wq(db = wq, syear = 2000, wtype = eag_wl, mlocs = locaties)
  
  # Pload and limits from PC Ditch
  Overzicht_kP <- ppr_pcditch(db = Overzicht_kP)

# data voor EST tekening ----------------------

  # dit bestand moet af en toe geupdate obv nieuwe hybi en EAG data
  EST <- fread('../hydrobiologie/EST.csv')

  # data voor P belasting
  dat <- readRDS("../pbelasting/dat.rds") # data kan worden gecreerd obv script: loadbalances, selectie waterlichamen goed doorlopen en mogelijk namen aanpassen van pol zodat ze korter worden
  dat$date <- as.POSIXct(paste0(dat$jaar,"-",dat$maand,"-01"), format = '%Y-%m-%d') 
  Overzicht_kP <- importCSV('../pbelasting/input/Overzicht_kP.csv', path = getwd()) 
  nomogram <- importCSV('../pbelasting/input/nomogram.csv', path = getwd())
  pvskp <- makePmaps(dat, Overzicht_kp, hybi, nomogram) 
  
# extractfunctie for relevant properties needed for factsheet
  factsheetExtract <- function(i,db_ESFoordelen,){
    
    # subset ESFoordelen
    waterlichamenwl <- ESFoordelen[i,] 
    
    # waterlichaam of eagcode
    wl <- waterlichamenwl$OWL
    
    # hier een koppeltabel nodig voor selectie toetsgebied
    namewl <- dcast(EKRset, HoortBijGeoobject.identificatie+GeoObject.identificatie+EAGIDENT~.)
    wlname <- namewl[GeoObject.identificatie %in% wl | EAGIDENT %in% wl,HoortBijGeoobject.identificatie]
    
    # select data
    eagwl <- eag_wl[KRWWaterlichaamcode %in% wl | GAFIDENT %in% wl,]
      
    # get title
    my_title <- paste0(waterlichamenwl$OWMNAAM)
    pvskpsel <- pvskp[pvskp$KRW %in% wl|pvskp$EAG %in% eagwl$GAFIDENT|
                        pvskp$GAF %in% substr(eagwl$GAFIDENT, 1, 4),] 
    ESF <- waterlichamenwl
    maatregelen1 <- maatregelen[maatregelen$HoortbijKRWWaterlichaam %in% wl,]
    waterlichamenwl$motstat <- waterlichamenwl$MotiveringBegrenzing
    waterlichamenwl$prov <- waterlichamenwl$Provincies
    waterlichamenwl$gem <- waterlichamenwl$Gemeenten
    waterlichamenwl$typebesch <- watertypen$Omschrijving[watertypen$Code %in% unique(eagwl$watertype)]
    wq1 <- wq[wq$EAGIDENT %in% eagwl$GAFIDENT,]
    hybi1 <- hybi[hybi$locatie.EAG %in% eagwl$GAFIDENT,]
    bod1 <- bod[bod$EAGIDENT %in% eagwl$GAFIDENT,]
    EKRset1 <- EKRset[EKRset$HoortBijGeoobject.identificatie %in% wlname,] 
    EKRset2 <- EKRset[EKRset$EAGIDENT %in% eagwl$GAFIDENT,]
    if(nrow(EKRset1) == 0){
      EKRset1 <- EKRset2 } # ingewikkeld, nu niet voor EAG
    gEAG_sel <- gEAG[gEAG$GAFIDENT %in% eagwl$GAFIDENT, ]
    waterpereag_sel <- waterpereag1[waterpereag1$GAFIDENT %in% eagwl$GAFIDENT, ]
    deelgebieden <- unique(as.data.table(gEAG_sel[,c('GAFIDENT','GAFNAAM')])[,geom:=NULL], sorted=TRUE,
                           keep.rownames=FALSE)
    deelgebieden <- as.data.frame(deelgebieden)
    deelgebieden$samen <- paste0(deelgebieden$GAFIDENT," (",deelgebieden$GAFNAAM,")")
    EST_sel <- EST[EST$EAG %in% eagwl$GAFIDENT,]
    
    # return list with relevant properties
    return(out)
  }
  
  