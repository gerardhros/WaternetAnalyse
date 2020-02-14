# factsheet preprocess
# authors: Laura Moria, Sven Verweij en Gerard Ros

# source functions
source('scripts/ppr_funs.R')
source('scripts/loadPackages.R')
source('scripts/factsheetfuncties.R')

#  Directories and names------------------------------------------
# dirGIS <-"data"

# other settings ---------------------------
proj4.rd <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.87740,4.0725 +units=m +no_defs")
proj4.google <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4.osm <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
col <- c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red")
labels <- c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2")

# set progress bar
pb <- txtProgressBar(max = 11, style=3);pbc <- 0

# inladen basis bestanden ----

  # ESF oordelen
  ESFoordelen <- fread('data/esfKRW_20200107.csv') # hier later ook eags of gafs aan toevoegen
  cols <- colnames(ESFoordelen[,11:25])
  ESFoordelen[,(cols) := lapply(.SD, function(x) trim(x)),.SDcols = cols]

  # maatregelen
  maatregelen <- fread('data/maatregelenKRW_20200124.csv')# hier later ook eags of gafs aan toevoegen
  cols <- c('HoortbijKRWWaterlichaam2021','HoortbijKRWWaterlichaamNaam2021')
  maatregelen[,(cols):=NULL]
  maatregelen <- unique(maatregelen) 
  cols <- colnames(maatregelen[,2:25])
  maatregelen[,(cols) := lapply(.SD, function(x) trim(x)),.SDcols = cols]
  
  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
## data voor kaart ----------

  # shape met alle EAGs
  gEAG <- st_read("data/EAG20191205.gpkg",quiet = T) %>% st_transform(proj4.rd)
  
  # shape met al het water in EAG
  waterpereag1 <- st_read("data/WaterPerEAG20191205.gpkg",quiet = T) %>% st_transform(proj4.rd)

  # shape met waterschapsgebied
  waterschappen  <- st_read("data/2019_gemeentegrenzen_kustlijn_simplified.shp",quiet = T) %>% st_transform(proj4.rd)

  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
# inladen hydrobiologische gegevens -----------
  
  # inladen gegevens hydrobiologie
  hybi <- readRDS('data/alles_reliable.rds')
  
  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
  # water types
  watertypen <- fread('data/KRWWatertype.csv')
  
  # locaties van alle metingen (water, biologie, en slootbodem)
  locaties <- fread('data/Location.csv')

  ## aanvullende eag data, krwwatertype niet overal ingevuld en stedelijk landelijk voor EST
  ## let op: als nieuwe EAG (gEAG) dan deze tabel aanpassen en aanvullen
  eag_wl <- fread('data/EAG_Opp_kenmerken.csv')
  #eag_wl <- eag_wl[is.na(eag_wl$Einddatum),] # sommige EAGs bestaan niet meer in EAGs, maar wel in krw dataset.       Deze opnieuw maken. vaarten ronde hoep, westeramstel, ronde venen en zevenhoven en vecht waterlichaam koppeling en watertype aangepast in         eag_opp-kenmerken aangepast omdat er anders geen kaart wordt weergegeven en geen ekrset data wordt geselecteerd.
  
  # KRW doelen 
  doelen <- fread('hydrobiologie/doelen.csv')
  
  # nonogram
  nomogram <- fread('data/nomogram.csv')
  
  # waterbalans data (made by loadBalances)
  dat <- readRDS("pbelasting/dat.rds") 
  
  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
  # EKR sets KRW en overig water
  EKRset1 <- readRDS('hydrobiologie/EKRsetKRW.rds') %>% as.data.table()
  EKRset2 <- readRDS('hydrobiologie/EKRsetOvWater.rds') %>% as.data.table()
  
  # slootbodem measurements
  bod  <- fread("data/bodemfews.csv")

  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
  # waterquality measurements
  wq  <- readRDS("data/ImportWQ.rds") %>% as.data.table()
  
  # datafile with P-load PC Ditch
  Overzicht_kP <- fread('data/Overzicht_kP.csv') 
  
  # toxiciteitsdata simoni
  simoni <- readRDS('data/simoni.rds')
  
  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
# update, filter and clean up databases -----------

  # EKR measurements
  EKRset <- ppr_ekr(ekr1 = EKRset1,ekr2 = EKRset2)
  
  # hybi measurements
  hybi <- ppr_hybi(db = hybi, syear = 2000, wtype = eag_wl, mlocs = locaties)
  
  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
  # slootbodemdata, hier moeten 2 verschillende datatables uitkomen
  bod <- ppr_slootbodem(db = bod, wtype = eag_wl, mlocs = locaties)
  
  # water quality
  wq <-  ppr_wq(db = wq, syear = 2000, wtype = eag_wl, mlocs = locaties)
  
  # Pload and limits from PC Ditch
  Overzicht_kP <- ppr_pcditch(db = Overzicht_kP)

  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
# data voor EST tekening ----------------------

  # dit bestand moet af en toe geupdate obv nieuwe hybi en EAG data
  EST <- fread('hydrobiologie/EST.csv')

  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
  # data voor P belasting (override the existing ones, this need to be updated)
  dat <- readRDS("pbelasting/dat.rds") # data kan worden gecreerd obv script: loadbalances, selectie waterlichamen goed doorlopen en mogelijk namen aanpassen van pol zodat ze korter worden
  dat$date <- as.POSIXct(paste0(dat$jaar,"-",dat$maand,"-01"), format = '%Y-%m-%d') 
  Overzicht_kP <- importCSV('pbelasting/input/Overzicht_kP.csv', path = getwd()) 
  nomogram <- importCSV('pbelasting/input/nomogram.csv', path = getwd())
  
  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
  # data voor P belasting (override the existing ones, this need to be updated)
  pvskp <- makePmaps(dat, Overzicht_kp, hybi, nomogram) 
  pvskp <- as.data.table(pvskp)
  
  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc) 
  
# make one object with bron data
  brondata <- list(ESFoordelen = ESFoordelen, 
                   EKRset = EKRset,
                   eag_wl = eag_wl,
                   Overzicht_kP = Overzicht_kP,
                   nomogram = nomogram,
                   pvskp = pvskp, 
                   dat = dat,
                   wq = wq, 
                   hybi = hybi, 
                   bod = bod,
                   gEAG = gEAG, 
                   waterpereag1 = waterpereag1, 
                   waterschappen = waterschappen,
                   watertypen = watertypen,
                   EST = EST,
                   doelen = doelen,
                   maatregelen = maatregelen)
 
   # send message to console
  print('Thanks for waiting. all data is succesfully loaded and saved in object brondata')
  
# --- extractfunctie for relevant properties needed for factsheet ----
  factsheetExtract <- function(i,brondata,splot = TRUE){ with(brondata, {
    
    # subset ESFoordelen and get ESF
    waterlichamenwl <- ESFoordelen[i,] 
    ESF <- ESFoordelen[i,] 
    
    # waterlichaam of eagcode
    wl <- waterlichamenwl$OWL
    
    # what is the name of the waterbody?
    wlname <- unique(EKRset[GeoObject.identificatie %in% wl | EAGIDENT %in% wl,HoortBijGeoobject.identificatie])[1]
    
    # select data
    eagwl <- eag_wl[KRWWaterlichaamcode %in% wl | GAFIDENT %in% wl,]
      
    # get title
    my_title <- paste0(waterlichamenwl$OWMNAAM)
    
    # get P load vs critical p load
    pvskpsel <- pvskp[KRW %in% wl | EAG %in% eagwl$GAFIDENT | GAF %in% substr(eagwl$GAFIDENT, 1, 4),] 
    
    # get maatregelen
    maatregelen1 <- maatregelen[HoortbijKRWWaterlichaam %in% wl,]
    
    # update waterlichaam 
    waterlichamenwl[,motstat := MotiveringBegrenzing]
    waterlichamenwl[,prov := Provincies]
    waterlichamenwl[,gem := Gemeenten]
    waterlichamenwl[,typebesch := watertypen[Code %in% unique(eagwl$watertype),Omschrijving]]
    
    # get water quality for relevant EAG
    wq1 <- wq[EAGIDENT %in% eagwl$GAFIDENT,]
    
    # get hydrobiological data
    hybi1 <- hybi[locatie.EAG %in% eagwl$GAFIDENT,]
    
    # get soil ditch properties
    bod1 <- bod[EAGIDENT %in% eagwl$GAFIDENT,]
    
    # get values from EKR
    if (wlname %in% EKRset$HoortBijGeoobject.identificatie){
      EKRset1 <- EKRset[HoortBijGeoobject.identificatie %in% wlname,]   
    } else {
      EKRset1 <- EKRset[EAGIDENT %in% eagwl$GAFIDENT,]
    }
    
    EKRset2 <- EKRset[EAGIDENT %in% eagwl$GAFIDENT,]
    
    # get shape of EAG
    gEAG_sel <- gEAG[gEAG$GAFIDENT %in% eagwl$GAFIDENT, ]
    
    # get shape of water per eag
    waterpereag_sel <- waterpereag1[waterpereag1$GAFIDENT %in% eagwl$GAFIDENT, ]
    
    # get deelgebieden
    deelgebieden <- gEAG_sel[,c('GAFIDENT','GAFNAAM')] %>% st_set_geometry(NULL) %>% as.data.table
    deelgebieden[,samen := paste0(GAFIDENT," (",GAFNAAM,")")]

    # get hydrobiological data
    EST_sel <- EST[EAG %in% eagwl$GAFIDENT,]
    
    # get Ecosystem Status
    
    if(nrow(EST_sel)>0){
      
      ## merge Ecosysteemtoestanden met EAGs: er mag maar 1 watertype per factsheet zijn. Dat is zo als of KRW WL of EAG uitgangspunt zijn.
      dtEST2 <- merge(EST_sel, eagwl, by.x = "EAG", by.y  = "GAFIDENT")
      
      ## gather O en W for the last year
      dtEST4 <- dtEST2[,yearid := frank(-jaar, ties.method = 'dense')][yearid <= 1] 
      
      # extract mean O and W for all EAGs
      dtEST4 <- dtEST4[,lapply(.SD, function(x) mean(as.numeric(x))),.SDcols = 3:19] 
      dtEST4 <- melt(dtEST4,measure=patterns(O='^O',W='^W'))
      
      # EST typenummer van degene die het meest voorkomt
      dtEST4 <- dtEST4[,lapply(.SD,which.max),.SDcols = c('O','W')] 
      
      # make selection name for esticons
      ESTnaam1 <- paste0(c('W','O'),c(dtEST4[,.(W,O)]),collapse = '_')
      ESTnaam2 <- unique(dtEST2$watertype)
      ESTnaam2 <- ifelse(ESTnaam2 == 'M20','DM',ifelse(ESTnaam2 %in% c('M14','M27',"M25"),'OM',ifelse(ESTnaam2 %in% c('M1a','M8',"M10"),'Sl','K')))
      ESTnaam3 <- ifelse(unique(dtEST2$StedelijkLandelijk) == 'Stedelijk','St','L') 
      
      # final ESTnaam
      ESTnaam <- paste0("esticon/",ESTnaam1,'_',ESTnaam2,'_', ESTnaam3, ".jpg")
      if(unique(dtEST2$KRWWaterlichaamcode) %in% c('NL11_3_8')){ESTnaam <- "esticon/W6_O7_DM_L.jpg"} 
      if(unique(dtEST2$KRWWaterlichaamcode) %in% c('NL11_5_1')){ESTnaam <- "esticon/W4_O6_OM_L.jpg"} 
    }
    
    ## plot locatie EAG binnen beheergebied AGV
    
    # bounding box needed
    bboxEAG <- st_bbox(gEAG)
    
    # create map
    mapEAG <- ggplot() +
      geom_sf(data = waterschappen, color = 'white',fill="white", size = 1) +
      geom_sf(data = gEAG,
              color = 'grey', fill = "white", size = 0.2) +
      geom_sf(data = waterpereag_sel,
              color = NA, fill = '#3498DB') +
      geom_sf(data = gEAG_sel,
              color = '#d73027', fill = NA, size = 1.5) +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "#e0f3f8"), 
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            #panel.border = element_rect(colour = "lightgrey", fill=NA, size=1),
            legend.position="none") +
      coord_sf(xlim = c(bboxEAG$xmin,bboxEAG$xmax), ylim = c(bboxEAG$ymin,bboxEAG$ymax), datum = NA)
    
    # create dir to save plots
    if(!wlname %in% list.files('factsheets/routput')){dir.create(paste0('factsheets/routput/',wlname))}
    
    if(splot){
      # save plot
      mapEAG + ggsave(paste0('factsheets/routput/',wlname,'/mapEAG.png'),width = 10,height = 10,units='cm',dpi=500)
      mapEAG <- paste0('factsheets/routput/',wlname,'/mapEAG.png')
    }
    
    ## plot locatie deelgebieden binnen EAG
    
    # bounding box needed
    bboxEAG <- st_bbox(gEAG_sel)
    
    # create map
    mapDEELGEBIED <-ggplot() +
      geom_sf(data = waterschappen, color = 'white',fill="white", size = 1) +
      geom_sf(data = waterpereag_sel,color = NA, fill = '#3498DB') +
      geom_sf(data = gEAG_sel,color = '#d73027', fill = NA, size = 1.5) +
      geom_sf_label(data = gEAG_sel, aes(label = GAFIDENT))+
      #geom_text(data=randomMap.df, aes(label = id, x = Longitude, y = Latitude)) +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "#e0f3f8"),  ## azure
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            #panel.border = element_rect(colour = "lightgrey", fill=NA, size=1),
            legend.position="none") +
      coord_sf(xlim = c(bboxEAG$xmin,bboxEAG$xmax), ylim = c(bboxEAG$ymin,bboxEAG$ymax), datum = NA)
    
    if(splot){
      ggplot2::ggsave(mapDEELGEBIED,file=paste0('factsheets/routput/',wlname,'/mapDEELGEBIED.png'),width = 10,height = 10,units='cm',dpi=500)
      mapDEELGEBIED <- paste0('factsheets/routput/',wlname,'/mapDEELGEBIED.png')
    }
    
    
    # --- make EKR plot ------------
    
    ## calculate EKR scores from EKRset1
    ekr_scores <- tabelPerWL3jaargemEAG_incl2022(EKRset = EKRset1, doelen = doelen)
    ekr_scores <- as.data.table(ekr_scores)
    
    ## make neat titles, alleen hoofdmaatlatten
    ekr_scores[,facet_wrap_code := as.factor(mapvalues(Waardebepalingsmethode.code, 
                from = c("Maatlatten2012 Fytoplankton", "Maatlatten2012 Macrofauna", "Maatlatten2018 Ov. waterflora", "Maatlatten2018 Vis"),
                to = c("Fytoplankton", "Macrofauna", "Ov. waterflora", "Vis")))]
    
    # subset 1, en zoek laagste score (old: ekr_scores_sel2)
    ekr_scores1 <- ekr_scores[!Waardebepalingsmethode.code %in% c("Maatlatten2012 Ov. waterflora","Maatlatten2012 Vis") & level == 1]
    ekr_scores1[,oordeelsort := EKR/GEP]
    d3 <- ekr_scores1[oordeelsort==min(oordeelsort,na.rm=T),]
    
    # subset 2, en zoek laagste score (old: ekr_scores_sel2_deel)
    ekr_scores2 <- ekr_scores[facet_wrap_code %in% d3$facet_wrap_code & level == 2,]
    d3_deel <- ekr_scores2[EKR==min(EKR,na.rm=T),]
    
    ## calculate score per deelmaatlat from EKRset2
    ekr_scores <- tabelPerWL3jaargemEAG_incl2022(EKRset = EKRset2, doelen = doelen)
    setDT(ekr_scores)
    
    # subset 1, en zoek laagste score (old: ekr_scores_sel2_deel)
    ekr_scores3 <- ekr_scores[Waardebepalingsmethode.code =="Maatlatten2018 Ov. waterflora" & 
                              level == 3 & !(GHPR %in% c('Bedekking Grote drijfbladplanten','Bedekking Kruidlaag')),]
    ## make neat titles, alleen hoofdmaatlatten
    ekr_scores3[,facet_wrap_code := as.factor(mapvalues(Waardebepalingsmethode.code, 
                 from = c("Maatlatten2018 Ov. waterflora"),
                 to = c("Ov. waterflora")))]
    ekr_scores3[,GHPR := as.factor(mapvalues(GHPR,from = c("Waterdiepte"),to = c("Vestigingsdiepte waterplanten")))]
    d3_deelptn <- ekr_scores2[EKR==min(EKR,na.rm=T),]
    
    # create map
    mapEKR <- ekrplot(ekr_scores1)
    
    if(splot){
      ggplot2::ggsave(mapEKR,file=paste0('factsheets/routput/',wlname,'/mapEKR.png'),width = 13,height = 8,
                      units='cm',dpi=500)
      mapEKR <- paste0('factsheets/routput/',wlname,'/mapEKR.png')
    }
    
    # --- Ecologische SleutelFactoren ----- (ESF tabel) ------
    ESFtab = data.table(esf = paste0('ESF',1:8),
                        kleur = as.factor(as.numeric(ESF[,3:10])),
                        oms = as.factor(ESF[,17:24]))
    
    # add oordeel
    ESFtab[kleur==1,OORDEEL := 'groennummer.jpg']
    ESFtab[kleur==2,OORDEEL := 'oranjenummer.jpg']
    ESFtab[kleur==3,OORDEEL := 'roodnummer.jpg']
    ESFtab[!kleur %in% 1:3,OORDEEL := 'grijsnummer.jpg']
    
    # add link to figuur for html as well as latex
    ESFtab[,OORDEEL := paste0(substr(esf, 4, 5),OORDEEL)]
    ESFtab[,piclatex := paste0('esf/',OORDEEL)]
    ESFtab[,OORDEEL := sprintf("![esficon](%s){width=50px}", paste0("esf/",OORDEEL, " "))]
    
    # select relevant columns
    ESFtab <- ESFtab[,.(esf,OORDEEL,kleur,oms,piclatex)]
    
    
    # --- uitgevoerde maatregelen ----------
    
    # uitgevoerd in SGBP 1 en 2, in planvorming of in fasering dan wel ingetrokken of vervangen
    maatregelen1[,Uitgevoerd1 := pmax(0,as.numeric(Uitgevoerd),na.rm=T)+pmax(0,as.numeric(Uitvoering),na.rm=T)]
    maatregelen1[,Plan := as.numeric(Plan)]
    maatregelen1[,Gefaseerd := as.numeric(Gefaseerd)]
   
    # percentage per type
    periode <- c("SGBP1 2009-2015", "SGBP2 2015-2021", "SGBP1 2006-2015")
    rates <- list(
      rate_uit = nrow(maatregelen1[SGBPPeriode.omschrijving %in% periode & Uitgevoerd1 > 0,]),
      rate_max = nrow(maatregelen1[SGBPPeriode.omschrijving %in% periode]),
      rate_plan = nrow(maatregelen1[Plan > 0 & SGBPPeriode.omschrijving %in% periode,]),
      rate_fase = nrow(maatregelen1[Gefaseerd > 0 & SGBPPeriode.omschrijving %in% periode,]),
      rate_del = nrow(maatregelen1[Vervangen > 0 | Ingetrokken > 0  & SGBPPeriode.omschrijving %in% periode,]),
      rate_nieuw = nrow(maatregelen1[SGBPPeriode.omschrijving %in% c("SGBP3 2021-2027"),]))

    # add nieuw information to maatregelen
    maatregelen1[,Gebiedspartner := `gebiedspartner (gesprekspartner bij financiering, uitvoering of beleid)`]
    maatregelen1[,SGBPPeriode := SGBPPeriode.omschrijving]
    maatregelen1[,Initiatiefnemer := Initiatiefnemer.naam]
    maatregelen1[,esffrst := substr(esf,1,4)]
    maatregelen1[nchar(esffrst)==0, esffrst := NA]
    
    # join measures with ESF-tabel
    cols <- c('Naam','Toelichting','SGBPPeriode','esffrst','Initiatiefnemer','Gebiedspartner','UitvoeringIn',"afweging")
    
    maatregelen2 <- merge(ESFtab, maatregelen1[,mget(cols)],by.x = 'esf', by.y = 'esffrst', all.y = T) 
    
    # als meerdere esf aan een maatregel gekoppeld zijn dan wordt de eerste geselecteerd
    maatregelen2[,ESFoordeel := OORDEEL]
    cols <- c('ESFoordeel','SGBPPeriode','Naam','Toelichting','Initiatiefnemer','Gebiedspartner','UitvoeringIn','afweging')
    maatregelen2 <- maatregelen2[,mget(cols)] 
    setorder(maatregelen2,-SGBPPeriode)
    
    if(nrow(maatregelen2)>0){
      maatregelen2 <- maatregelen2 %>% group_by(ESFoordeel) %>% arrange(ESFoordeel, desc(ESFoordeel))
    }
    
    # --- plot ESF1: productiviteit ----
    if(nrow(pvskpsel)>0){
      if(sum(is.na(pvskpsel$naam))==0){plotPwbal = pvskpplot(pvskpsel)
      
      if(splot){
        ggplot2::ggsave(plotPwbal,file=paste0('factsheets/routput/',wlname,'/plotPwbal.png'),width = 13,height = 8,
                        units='cm',dpi=500)
        plotPwbal <- paste0('factsheets/routput/',wlname,'/plotPwbal.png')
      }
      } else {
        plotPwbal = NULL}
      }
    
    # --- plot ESF 2: lichtklimaat
    if(nrow(wq1[fewsparameter == 'VEC' & jaar > '2015',]) > 0) {
      plotLichtklimaat = extinctie1(wq = copy(wq1)[jaar > '2015'], 
                                    hybi = copy(hybi1), 
                                    parameter = c('VEC','WATDTE_m'))
      if(splot){
        ggplot2::ggsave(plotLichtklimaat,file=paste0('factsheets/routput/',wlname,'/plotLichtklimaat.png'),width = 13,height = 8,
                        units='cm',dpi=500)
        plotLichtklimaat <- paste0('factsheets/routput/',wlname,'/plotLichtklimaat.png')
      }
      
    } else {
      plotLichtklimaat = NULL
    }
    
    # --- plot ESF 4: waterdiepte
    if(nrow(hybi1[fewsparameter == 'WATDTE_m',])>0){
      hybi2 <- hybi1[!is.na(fewsparameter == 'WATDTE_m'),]
      plotWaterdiepte = waterdieptesloot(hybi2, parameter = c('WATDTE_m'))  
      
      if(splot){
        ggplot2::ggsave(plotWaterdiepte,file=paste0('factsheets/routput/',wlname,'/plotWaterdiepte.png'),width = 13,height = 8,
                        units='cm',dpi=500)
        plotWaterdiepte <- paste0('factsheets/routput/',wlname,'/plotWaterdiepte.png')
      }
      
    } else {
      plotWaterdiepte = NULL
    }
    
    # --- plot ESF3 : waterbodem
    if(nrow(bod1) > 0) {plotWaterbodem = plotbod(bod1)
    
    if(splot){
      ggplot2::ggsave(plotWaterbodem,file=paste0('factsheets/routput/',wlname,'/plotWaterbodem.png'),width = 13,height = 8,
                      units='cm',dpi=500)
      plotWaterbodem <- paste0('factsheets/routput/',wlname,'/plotWaterbodem.png')
    }
    
    } else {plotWaterbodem = NULL}
    
    # make a list to store the output
    out <- list(waterlichamenwl = waterlichamenwl,
                wlname = wlname,
                my_title = my_title,
                eagwl = eagwl,
                deelgebieden = deelgebieden,
                ESTnaam = ESTnaam,
                mapEAG = mapEAG,
                mapDEELGEBIED = mapDEELGEBIED,
                mapEKR = mapEKR,
                plotPwbal = plotPwbal,
                plotLichtklimaat = plotLichtklimaat,
                plotWaterdiepte = plotWaterdiepte,
                plotWaterbodem = plotWaterbodem,
                ESFtab = ESFtab,
                maatregelen1 = maatregelen1,
                maatregelen2 = maatregelen2,
                rates = rates,
                d3 = d3,
                d3_deel = d3_deel,
                d3_deelptn = d3_deelptn
                )
    
    # return list with relevant properties
    return(out)})
  }
  
  
  # remove all data except brondata en factsheetExtract
  rm(list=setdiff(ls(), c("brondata","factsheetExtract")))
  