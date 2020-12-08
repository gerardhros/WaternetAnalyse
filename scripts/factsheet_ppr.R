# factsheet preprocess
# authors: Laura Moria, Sven Verweij en Gerard Ros

# Directories and names------------------------------------------

# other settings ---------------------------
proj4.rd <- 28992 #sp::CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.87740,4.0725 +units=m +no_defs")
proj4.google <-4326 #sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
#proj4.osm <- sp::CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
col <- c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red")
labels <- c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2")

# set progress bar
pb <- txtProgressBar(max = 8, style=3);pbc <- 0

# inladen basis bestanden ----

  # read in the latest version of ESFoordelen from 'data'
  ESFoordelen <- ppr_esf_oordeel()

  # informatie over maatregelen
  maatregelen <- ppr_maatregelen()
  maatregelen <- maatregelen[!maatregelen$SGBPPeriode.omschrijving %in% c('Niet uitvoeren','Niet opnemen in SGBP3', "niet opnemen in SGBP3"),]

  # shape met alle EAGs
  gEAG <- sf::st_read("data/EAG20191205.gpkg",quiet = T) %>% sf::st_transform(proj4.rd)

  # shape met al het water in EAG
  waterpereag1 <- sf::st_read("data/WaterPerEAG20191205.gpkg",quiet = T) %>% sf::st_transform(proj4.rd)

  # shape met waterschapsgebied
  waterschappen  <- sf::st_read("data/2019_gemeentegrenzen_kustlijn_simplified.shp",quiet = T) %>%sf::st_transform(proj4.rd)

  # water types
  watertypen <- data.table::fread('data/KRWWatertype.csv')

  # locaties van alle metingen (water, biologie, en slootbodem)
  locaties <- data.table::fread('data/Location.csv')

  # nonogram
  nomogram <- data.table::fread('data/nomogram.csv')

  # datafile with P-load PC Ditch
  Overzicht_kP <- data.table::fread('data/Overzicht_kP.csv')
  Overzicht_kP <- ppr_pcditch(db = Overzicht_kP)

  # toxiciteitsdata simoni
  simoni <- readRDS('data/simoni.rds')

  ## aanvullende eag data, krwwatertype niet overal ingevuld en stedelijk landelijk voor EST
  eag_wl <- data.table::fread('data/EAG_Opp_kenmerken_20200701.csv')
  eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]

  # KRW doelen
  doelen <- ppr_doelen()

# inladen data meetnetten and clean up

  # hydrobiologische gegevens
  hybi <- readRDS('data/alles_reliable.rds')
  hybi <- ppr_hybi(db = hybi, syear = 2000, wtype = eag_wl, mlocs = locaties)

    # show progress
    pbc <- pbc + 1; setTxtProgressBar(pb,pbc)

  # waterbalans data (made by loadBalances)
  dat <- readRDS("pbelasting/dat.rds")
  dat[,date := as.POSIXct(paste0(jaar,"-",maand,"-01"), format = '%Y-%m-%d')]

    # show progress
    pbc <- pbc + 1; setTxtProgressBar(pb,pbc)

  # EKR sets KRW en overig water
  EKRset1 <- readRDS('hydrobiologie/EKRsetKRW.rds') %>% as.data.table()
  EKRset2 <- readRDS('hydrobiologie/EKRsetOvWater.rds') %>% as.data.table()
  EKRset <- ppr_ekr(krwset = EKRset1, ovwatset = EKRset2,eag_wl = eag_wl, doelen = doelen)


    # show progress
    pbc <- pbc + 1; setTxtProgressBar(pb,pbc)

  # slootbodem measurements
  bod <- data.table::fread("data/bodemfews.csv")
  bod <- ppr_slootbodem(db = bod, wtype = eag_wl, mlocs = locaties)

    # show progress
    pbc <- pbc + 1; setTxtProgressBar(pb,pbc)

  # waterquality measurements
  wq  <- readRDS("data/ImportWQ.rds") %>% as.data.table()
  wq <-  ppr_wq(db = wq, syear = 2000, wtype = eag_wl, mlocs = locaties)

    # show progress
    pbc <- pbc + 1; setTxtProgressBar(pb,pbc)

# data voor EST tekening ----------------------
  # dit bestand moet af en toe geupdate obv nieuwe hybi en EAG data
  EST <- data.table::fread('hydrobiologie/EST.csv')

  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc)

# data voor P belasting (override the existing ones, this need to be updated) ----------------------
  #nomogram <- importCSV('pbelasting/input/nomogram.csv', path = getwd())
  #Overzicht_kP <- importCSV('pbelasting/input/Overzicht_kP.csv', path = getwd())
  #pvskp <- makePmaps(dat, Overzicht_kp, hybi, nomogram)
  #pvskp <- as.data.table(pvskp)

  # show progress
  pbc <- pbc + 1; setTxtProgressBar(pb,pbc)

  # data voor P belasting
  pvskp <- ppr_pmaps(dat, Overzicht_kp, hybi, nomogram)

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

# --- extractfunctie for relevant properties needed for single factsheet ----

  factsheetExtract <- function(i,brondata,splot = TRUE){ with(brondata, {
    # i <- 177
    # subset data ----
    # subset ESFoordelen and get ESF
    waterlichamenwl <- ESFoordelen[i,]
    ESF <- ESFoordelen[i,]

    # waterlichaam of eagcode
    wl <- waterlichamenwl$OWL_SGBP3
    wl <- unlist(strsplit(wl, split=c(', ',',','_'), fixed=TRUE))

    # select data for water body and extract name
    if(wl %in% eag_wl$KRW_SGBP3){

      # als eenheid factsheet KRW wl
      eagwl <- eag_wl[KRW_SGBP3 %in% wl,]

      # extract the name as text after the first underscore
      wlname <- sub('.*_', '',unique(eagwl[,KRWmonitoringslocatie_SGBP3]))

    } else {

      # als eenheid factsheet is eag of gaf
      eagwl <- eag_wl[GAFIDENT %in% wl|substr(GAFIDENT,1,4) %in% wl,]

      # extract the name and remove prefix 'NL11_'
      wlname <- unique(EKRset[EAGIDENT %in% eagwl$GAFIDENT, HoortBijGeoobject.identificatie])
      wlname <- gsub("NL11_","",wlname)
      if(identical(wlname, character(0))){wlname <- "nietbeschikbaar"}
    }

    # get title
    my_title <- paste0(waterlichamenwl$OWMNAAM_SGBP3)
    my_title2 <- gsub('\\/','',gsub(' ','',my_title))
    my_title2 <- gsub(',','_',gsub(' ','',my_title2))
    
    # get P load vs critical p load
    pvskpsel <- pvskp[KRW %in% wl | EAG %in% eagwl$GAFIDENT | GAF %in% c(wl,substr(eagwl$GAFIDENT, 1, 4)),]

    # get maatregelen
    maatregelen1 <- maatregelen[HoortbijKRWWaterlichaam2021 %in% wl,]

    # update waterlichaam
    waterlichamenwl[,motstat := MotiveringBegrenzing]
    waterlichamenwl[,prov := Provincies]
    waterlichamenwl[,gem := Gemeenten]
    waterlichamenwl[,typebesch := paste0(tolower(watertypen[Code %in% unique(eagwl$watertype),Omschrijving]),collapse = ', ')]

    # get water quality for relevant EAG
    wq1 <- wq[EAGIDENT %in% eagwl$GAFIDENT,]

    # get hydrobiological data
    hybi1 <- hybi[locatie.EAG %in% eagwl$GAFIDENT,]

    # get soil ditch properties
    if('EAGIDENT' %in% colnames(bod)){
      bod1 <- bod[EAGIDENT %in% eagwl$GAFIDENT,]
    } else {
      bod1 <- bod[loc.eag %in% eagwl$GAFIDENT,]
    }

    # get values from EKR
    if (wlname[1] %in% EKRset$HoortBijGeoobject.identificatie){
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
    deelgebieden <- as.data.table(eagwl[,c('GAFIDENT','GAFNAAM')])
    deelgebieden[,samen := paste0(GAFIDENT," (",GAFNAAM,")")]

    # get hydrobiological data
    EST_sel <- EST[EAG %in% eagwl$GAFIDENT,]

    # get Ecosystem Status ----

    if(nrow(EST_sel)>0){

      ## merge Ecosysteemtoestanden met EAGs: er mag maar 1 watertype per factsheet zijn. Dat is zo als of KRW WL of EAG uitgangspunt zijn.
      cols <- colnames(EST_sel)[grepl('^EAG|jaar|^O|^W|SGBP3|GAFNAAM|KRW',colnames(EST_sel))]
      dtEST2 <- merge.data.table(EST_sel[,mget(cols)], eagwl[,c('GAFIDENT','watertype','StedelijkLandelijk')], by.x = "EAG", by.y  = "GAFIDENT")

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
      ESTnaam2 <- ifelse(ESTnaam2 == 'M20','DM',
                         ifelse(ESTnaam2 %in% c('M14','M27',"M25"),'OM',
                                ifelse(ESTnaam2 %in% c('M1a','M8',"M10"),'Sl','K')))
      ESTnaam3 <- ifelse(unique(dtEST2$StedelijkLandelijk) == 'Stedelijk','St','L')

      # final ESTnaam
      ESTnaam <- paste0("esticon/",ESTnaam1,'_',ESTnaam2,'_', ESTnaam3, ".jpg")
      ESTnaam <- ESTnaam[1]

      if(unique(dtEST2$KRW_SGBP3) %in% c('NL11_3_8')){ESTnaam <- "esticon/W6_O7_DM_L.jpg"}
      if(unique(dtEST2$KRW_SGBP3) %in% c('NL11_5_1')){ESTnaam <- "esticon/W4_O6_OM_L.jpg"}
      if(unique(dtEST2$KRW_SGBP3) %in% c('NL11_1_2')){ESTnaam <- "esticon/W6_O6_K_St.jpg"}
      if(unique(dtEST2$KRW_SGBP3) %in% c('NL11_1_1')){ESTnaam <- "esticon/W6_O6_K_St.jpg"}
    }else(ESTnaam <- "NietBeschikbaar")

    # --- make kaarten plots ----
    ## plot locatie EAG binnen beheergebied AGV

    # bounding box needed
    bboxEAG <- st_bbox(gEAG)

    # create map
    mapEAG <- ggplot2::ggplot() +
      geom_sf(data = waterschappen, color = 'white',fill="white", size = 1) +
      geom_sf(data = gEAG,
              color = 'grey25', fill = "white", size = 0.2) +
      geom_sf(data = waterpereag_sel,
              color = NA, fill = '#345bdb') +#3498DB
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

    # make filename for directory
    pdir <- gsub(',| ','_',my_title2)
    
    # create dir to save plots
    if(!pdir %in% list.files('factsheets/routput')){
      dir.create(paste0('factsheets/routput/',pdir))}

    # save plot, and location where map is saved
    if(splot){mapEAG + ggsave(paste0('factsheets/routput/',pdir,'/mapEAG.png'),
                              width = 11,height = 10,units='cm',dpi=500)    }
    mapEAG <- paste0('routput/',pdir,'/mapEAG.png')

    ## plot locatie deelgebieden binnen EAG

    # bounding box needed
    bboxEAG <- st_bbox(gEAG_sel)

    # create map
    mapDEELGEBIED <-ggplot2::ggplot() +
      geom_sf(data = waterschappen, color = 'white',fill="white", size = 1) +
      geom_sf(data = gEAG, color = 'grey25', fill = "white", size = 0.2) +
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
      coord_sf(xlim = c(bboxEAG$xmin,bboxEAG$xmax) + c(-250,250),
               ylim = c(bboxEAG$ymin,bboxEAG$ymax) + c(-250,250), datum = NA)

    # save plot, and location where map is saved
    if(splot){ggplot2::ggsave(mapDEELGEBIED,file=paste0('factsheets/routput/',pdir,'/mapDEELGEBIED.png'), units='cm',dpi=600)}
    mapDEELGEBIED <- paste0('routput/',pdir,'/mapDEELGEBIED.png')

    # --- make EKR plot ------------

    ## calculate EKR scores from EKRset1
    ekr_scores <- ppr_tabelPerWL3jaargemEAG_incl2022(EKRset = EKRset1)

    # subset 1, en zoek laagste score (old: ekr_scores_sel2)
    ekr_scores1 <- ekr_scores[!is.na(id) & level == 1]
    ekr_scores1[,oordeelsort := EKR / GEP_2022]
    d3 <- ekr_scores1[oordeelsort==min(oordeelsort,na.rm=T),]
    # subset 2, en zoek laagste score (old: ekr_scores_sel2_deel)
    ekr_scores2 <- ekr_scores[facet_wrap_code %in% d3$facet_wrap_code & level == 2,]
    d3_deel <- ekr_scores2[EKR==min(EKR,na.rm=T),]

    ## calculate score per deelmaatlat from EKRset2
    ekr_scores <- ppr_tabelPerWL3jaargemEAG_incl2022(EKRset = EKRset2)

    # subset 1, en zoek laagste score (old: ekr_scores_sel2_deel)
    ekr_scores3 <- ekr_scores[wbmethode =="Maatlatten2018 Ov. waterflora" &
                              level == 3 & !(GHPR %in% c('Bedekking Grote drijfbladplanten','Bedekking Kruidlaag')),]
    ## make neat titles, alleen hoofdmaatlatten
    ekr_scores3[GHPR == 'Waterdiepte',GHPR := as.factor(gsub('Waterdiepte',"Vestigingsdiepte waterplanten",GHPR))]

    d3_deelptn <- ekr_scores3[EKR==min(EKR,na.rm=T),]

    # create plt met ekr scores en doelen
    # loopje voor alle figuren als EKRset1 = EKRset 
    # NL11 is ongewogen
    # for(i in unique(ekr_scores1$id)){
    # mapEKR <- ppr_ekrplot2(ekr_scores1[ekr_scores1$id == i,])
    # ggplot2::ggsave(mapEKR,file=paste0('factsheets/output/',i,'mapEKR.png'),
    #                 width = 14, height = 10,units='cm',dpi=1000)
    # }
    
    if(nrow(ekr_scores1)> 0){
      # plot figure EKR
      mapEKR <- ppr_ekrplot2(ekr_scores1)
    } else {
      # plot figure EKR when no data is available
      db <- data.frame()
      mapEKR = plotEmpty(db = db, type='plotekrplot')
    }
    
    # save plot, and location where map is saved
    if(splot){ggplot2::ggsave(mapEKR,file=paste0('factsheets/routput/',my_title2,'/mapEKR.png'),
                              width = 14, height = 10,units='cm',dpi=1000)}
    mapEKR <- paste0('routput/',my_title2,'/mapEKR.png')
    
    # create plt met ekr scores in de tijd
    # ongewogen scores obv koppeling locatie en EAG
    z <- rbind(EKRset1,EKRset2[EKRset2$level == 3,])
    plotEKRlijn <- plotEKRlijnfs(z)

    # --- Ecologische SleutelFactoren (ESF tabel) ------

    # ESF is doubled in columns: one string and one number, detect via lenght of the first row
    cols <- which(grepl('ESF',colnames(ESF)))
    cols_nchar <- ESF[1,lapply(.SD,function(x) nchar(x)),.SDcols = cols]

    # make data.table
    ESFtab = data.table(esf = paste0('ESF',1:8),
                        kleur = as.numeric(ESF[,.SD,.SDcols=cols[cols_nchar==1]]),
                        oms = as.factor(ESF[,.SD,.SDcols=cols[cols_nchar>1|cols_nchar==0]]))

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

    # adapt text to latex format
    ESFtab[,oms_latex := gsub('%','\\%',oms,fixed=TRUE)]
    ESFtab[,oms_latex := gsub('  ',' ',gsub('\r\n','',oms_latex))]

    # --- uitgevoerde maatregelen ----------
    
    # uitgevoerd in SGBP 1 en 2, in planvorming of in fasering dan wel ingetrokken of vervangen
    maatregelen1[,Uitgevoerd1 := pmax(0,as.numeric(Uitgevoerd),na.rm=T)+pmax(0,as.numeric(Uitvoering),na.rm=T)]
    maatregelen1[,Plan := as.numeric(Plan)]
    maatregelen1[,Gefaseerd := as.numeric(Gefaseerd)]

    # percentage per type
    periode <- c("SGBP1 2009-2015", "SGBP2 2015-2021", "SGBP1 2006-2015") # hier mis een periode (niet in sgbp)
    rates <- list(
      rate_uit = nrow(maatregelen1[SGBPPeriode.omschrijving %in% periode & Uitgevoerd1 > 0,]),
      rate_max = nrow(maatregelen1[SGBPPeriode.omschrijving %in% periode]),
      rate_plan = nrow(maatregelen1[Plan > 0 & SGBPPeriode.omschrijving %in% periode,]),
      rate_fase = nrow(maatregelen1[Gefaseerd > 0 & SGBPPeriode.omschrijving %in% periode,]),
      rate_del = nrow(maatregelen1[Vervangen > 0 | Ingetrokken > 0  & SGBPPeriode.omschrijving %in% periode,]),
      rate_nieuw = nrow(maatregelen1[SGBPPeriode.omschrijving %in% c("SGBP3 2021-2027"),]))

    # add gauges (its not yet possible to save as png)
    if(FALSE){
    m_gaug_uit <-  gauge(rates$rate_uit, min = 0, max = rates$rate_max, symbol = '', gaugeSectors(
                         success = c(80, 100), warning = c(40, 79), danger = c(0, 39)))
    m_gaug_plan <- gauge(rates$rate_plan, min = 0, max = rates$rate_max, symbol = '', gaugeSectors(
                         success = c(80, 100), warning = c(40, 79), danger = c(0, 39)))
    m_gaug_fase <- gauge(rates$rate_fase, min = 0, max = rates$rate_max, symbol = '', gaugeSectors(
                         success = c(80, 100), warning = c(40, 79), danger = c(0, 39)))
    m_gaug_del <-  gauge(rates$rate_del, min = 0, max = rates$rate_max, symbol = '', gaugeSectors(
                         success = c(80, 100), warning = c(40, 79), danger = c(0, 39)))
    m_gaug_new <-  gauge(rates$rate_nieuw, min = 0, max = length(maatregelen1$Naam), symbol = '', gaugeSectors(
                         success = c(80, 100), warning = c(40, 79), danger = c(0, 39)))
    }

    # make widgets in ggplot, and add color in relation to sucess/ warning / danger criteria
    m_gauges <- data.table(title = c('Maatregelen SGBP 1 en 2\n uitgevoerd of in uitvoering',
                                     'Maatregelen SGBP 1 en 2\n in planvorming',
                                     'Maatregelen SGBP 1 en 2\n gefaseerd',
                                     'Maatregelen SGBP 1 en 2\n vervangen of ingetrokken',
                                     'Maatregelen SGBP3\n nieuw t.o.v. totaal'),
                      percentage = c(as.numeric(rates[c(1,3,4,5)])/rates$rate_max,rates$rate_nieuw/length(maatregelen1$Naam))
    )
    m_gauges[,label := paste0(round(percentage*100),'%')]
    m_gauges[,group := fifelse(percentage < 0.4,'red',fifelse(percentage < 0.8,'orange','green'))]

    mapGauge <- ggplot2::ggplot(m_gauges, aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
                  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="gray85") +
                  geom_rect() +
                  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
                  geom_text(aes(x = 0, y = 0, label = label, colour=group), size=5.5) +
                  geom_text(aes(x=1.5, y=1.5, label=title), size=3.2) +
                  facet_wrap(~title, ncol = 5) +
                  theme_void() +
                  scale_fill_manual(values = c("red"="#CC0000", "orange"="#FF8800", "green"="#007E33")) +
                  scale_colour_manual(values = c("red"="#CC0000", "orange"="#FF8800", "green"="#007E33")) +
                  theme(strip.background = element_blank(),
                        strip.text.x = element_blank()) +
                  guides(fill=FALSE) +
                  guides(colour=FALSE)

    # save plot, and location where map is saved
    if(splot){ggplot2::ggsave(mapGauge,file=paste0('factsheets/routput/',my_title2,'/mapGauge.png'),width = 24,height = 5,units='cm',dpi=500)}
    mapGauge <- paste0('routput/',my_title2,'/mapGauge.png')

    # add nieuw information to maatregelen
    maatregelen1[,Gebiedspartner := `gebiedspartner (gesprekspartner bij financiering, uitvoering of beleid)`]
    maatregelen1[,SGBPPeriode := SGBPPeriode.omschrijving]
    maatregelen1[,Initiatiefnemer := Initiatiefnemer.naam]
    maatregelen1[,BeoogdInitiatiefnemer := Initiatiefnemer.waternet]
    maatregelen1[,esffrst := substr(esf,1,4)]
    maatregelen1[nchar(esffrst)==0, esffrst := NA]

    # join measures with ESF-tabel
    cols <- c('Naam','Toelichting','SGBPPeriode','esffrst','Initiatiefnemer','BeoogdInitiatiefnemer',
              'Gebiedspartner','UitvoeringIn',"afweging")
    maatregelen2 <- merge.data.table(ESFtab, maatregelen1[,mget(cols)],by.x = 'esf', by.y = 'esffrst', all.y = T)
    
    # als meerdere esf aan een maatregel gekoppeld zijn dan wordt de eerste geselecteerd
    cols <- c('ESFoordeel','ESFoordeel_latex','SGBPPeriode','Naam','Toelichting','Initiatiefnemer','BeoogdInitiatiefnemer','Gebiedspartner','UitvoeringIn','afweging')
    maatregelen2[,ESFoordeel := OORDEEL]
    maatregelen2[,ESFoordeel_latex := piclatex]
    maatregelen2[is.na(ESFoordeel), ESFoordeel := '![esficon](esf/9grijsnummer.jpg ){width=50px}']
    #maatregelen2[is.na(ESFoordeel), ESFoordeel_latex := 'esf/9grijsnummer.jpg']
    maatregelen2 <- maatregelen2[,mget(cols)]

    # sorteer met oplopend jaar en ESF
    setorder(maatregelen2,ESFoordeel,-SGBPPeriode)

    # formatting (avoid characters that conflict with latex)
    hp_latex <- "\\T\\includegraphics[height = 0.8cm,keepaspectratio,valign=t]{"
    maatregelen2[, Toelichting := gsub('%','\\%',Toelichting,fixed=TRUE)]
    maatregelen2[,Toelichting_latex := gsub('  ',' ',gsub('\r\n','',Toelichting))]
    maatregelen2[,Toelichting_latex := gsub('\\.nl/','.nl',gsub('\\https://','www.',Toelichting_latex))]
    maatregelen2[,ESFoordeel_latex := sprintf(paste0(hp_latex,ESFoordeel_latex,"}"))]
    maatregelen2[,Gebiedspartner := gsub('\\?','onbekend',Gebiedspartner)]
    maatregelen2[, Naam := gsub('%','\\%',Naam,fixed=TRUE)]

    # make empty table to be used in rnw file
    if(nrow(maatregelen2)==0){

      maatregelen2 <- data.table(ESFoordeel = '', ESFoordeel_latex = '', SGBPPeriode = '',
                                 Naam = '',Toelichting = '',Initiatiefnemer = 'waterschap',
                                 BeoogdInitiatiefnemer = '', Gebiedspartner = '',UitvoeringIn = '',
                                 afweging = '',Toelichting_latex ='')

    }

    # --- plot ESF1: productiviteit ----
    plotPwbal.ref <- paste0('routput/',my_title2,'/plotPwbal.png')
    if(sum(is.na(pvskpsel$naam)) != length(pvskpsel$naam)){

      # plot figure ESF1
      plotPwbal = ppr_pvskpplot(pvskpsel)
      class(plotPwbal.ref) <- 'plotref'

    } else {

      # plot figure ESF1 when no data is available
      plotLeegDB = data.frame(GAF = eagwl$GAFIDENT)
      plotLeegDB$pload <- 0
      plotPwbal = plotEmpty(db = plotLeegDB, type='Pwbal')
      class(plotPwbal.ref) <- 'plotempty'
    }

    # save plot
    if(splot){ggplot2::ggsave(plotPwbal,file=paste0('factsheets/routput/',my_title2,'/plotPwbal.png'),
                              width = 13,height = 8,units='cm',dpi=600)}


    # --- plot ESF 2: lichtklimaat ----
    plotLichtklimaat.ref <- paste0('routput/',my_title2,'/plotLichtklimaat.png')
    if(nrow(wq1[fewsparameter == 'VEC' & jaar > '2015',]) > 0) {

      # plot ESF 2
      plotLichtklimaat = ppr_extinctie1(wq = wq1, hybi = hybi1, parameter = c('VEC','WATDTE_m'))
      class(plotLichtklimaat.ref) <- 'plotref'
    } else {

      # plot figure ESF2 when no data is available
      plotLeegDB = data.frame(GAF = eagwl$GAFIDENT, Lext = 0)
      plotLichtklimaat = plotEmpty(db = plotLeegDB,type='plotLichtklimaat')
      class(plotLichtklimaat.ref) <- 'plotempty'
    }

    # save plot is saved
    if(splot){ggplot2::ggsave(plotLichtklimaat,file=paste0('factsheets/routput/',my_title2,'/plotLichtklimaat.png'),
                              width = 13,height = 8,units='cm',dpi=600)}


    # --- plot ESF 4: waterdiepte ----
    plotWaterdiepte.ref <- paste0('routput/',my_title2,'/plotWaterdiepte.png')

    if(nrow(hybi1[fewsparameter == 'WATDTE_m',])>0){

      # plot ESF 4
      hybi2 <- hybi1[!is.na(fewsparameter == 'WATDTE_m'),]
      plotWaterdiepte = ppr_waterdieptesloot(hybi2, parameter = c('WATDTE_m'))
      class(plotWaterdiepte.ref) <- 'plotref'
    } else {

      # plot figure ESF4 when no data is available
      plotLeegDB = data.frame(GAF = eagwl$GAFIDENT, wd = 0,krwwt ='onbekend')
      plotWaterdiepte = plotEmpty(db = plotLeegDB,type='plotWaterdiepte')
      class(plotWaterdiepte.ref) <- 'plotempty'

    }

    # save plot
    if(splot){ggplot2::ggsave(plotWaterdiepte,file=paste0('factsheets/routput/',my_title2,'/plotWaterdiepte.png'),
                              width = 13,height = 8,units='cm',dpi=600)}


    # --- plot ESF3 : waterbodem ----
    plotbodFW.ref <- paste0('routput/',my_title2,'/plotWaterbodem_FW.png')
    plotqPW.ref <- paste0('routput/',my_title2,'/plotWaterbodem_qPW.png')
    plotWaterbodem.ref <- paste0('routput/',my_title2,'/plotWaterbodem.png')

    if(nrow(bod1) > 0) {

      # plot ESF 4
      plotbodFW = ppr_plotbod(bod1,type = 'plotFW')
      plotqPW = ppr_plotbod(bod1,type = 'plotqPW')
      plotWaterbodem = ppr_plotbod(bod1,type='grid')
      class(plotbodFW.ref) <- 'plotref'
      class(plotqPW.ref) <- 'plotref'
      class(plotWaterbodem.ref) <- 'plotref'

    } else {

      # plot figure ESF3 when no data is available
      plotLeegDB = data.frame(GAF = eagwl$GAFIDENT, plv = 0,ijzerval ='onbekend')
      plotbodFW = plotEmpty(db = plotLeegDB,type='plotbodFW')
      plotqPW = plotEmpty(db = plotLeegDB,type='plotqPW')
      plotWaterbodem = plotbodFW
      class(plotbodFW.ref) <- 'plotempty'
      class(plotqPW.ref) <- 'plotempty'
      class(plotWaterbodem.ref) <- 'plotempty'

    }

    # save plots
    if(splot){
      ggplot2::ggsave(plotbodFW,file=paste0('factsheets/routput/',my_title2,'/plotWaterbodem_FW.png'),width = 13,height = 8,
                      units='cm',dpi=600)
      ggplot2::ggsave(plotqPW,file=paste0('factsheets/routput/',my_title2,'/plotWaterbodem_qPW.png'),width = 13,height = 8,
                      units='cm',dpi=600)
      ggplot2::ggsave(plotWaterbodem,file=paste0('factsheets/routput/',my_title2,'/plotWaterbodem.png'),
                      width = 14, height = 10,units='cm',dpi=600)
      }

    # make a list to store the output
    out <- list(waterlichamenwl = waterlichamenwl,
                wlname = wlname,
                my_title2 = my_title2,
                my_title = my_title,
                eagwl = eagwl,
                deelgebieden = deelgebieden,
                ESTnaam = ESTnaam,
                mapEAG = mapEAG,
                mapDEELGEBIED = mapDEELGEBIED,
                mapEKR = mapEKR,
                mapGauge = mapGauge,
                plotPwbal = plotPwbal.ref,
                plotLichtklimaat = plotLichtklimaat.ref,
                plotWaterdiepte = plotWaterdiepte.ref,
                plotWaterbodem = plotWaterbodem.ref,
                plotbodFW = plotbodFW.ref,
                plotqPW = plotqPW.ref,
                ESFtab = ESFtab,
                maatregelen1 = maatregelen1,
                maatregelen2 = maatregelen2,
                rates = rates,
                d3 = d3,
                d3_deel = d3_deel,
                d3_deelptn = d3_deelptn,
                plotEKRlijn = plotEKRlijn
                )

    # return list with relevant properties
    return(out)})
  }


  # remove all data except brondata en factsheetExtract
  rm(list=setdiff(ls(), c("brondata","factsheetExtract")))
