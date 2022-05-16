
#i <- unique(macft$monsterident)[1]
# i <- 'WP556666' w3, w6, w7
# i <- 'WP527261'
# i<- "WP527185"
# i <- macft$monsterident[macft$compartiment == 'OR'][1]


monstextract <- function(i, macft, soortenlijst_submers, soortenlijst_kroos, soortenlijst_oever, grenswaarden_EST){
  # monsters zijn slechts 1 compartiment en komen samen op loc niveau, dus niet te veel filteren op dit niveau, later wordt er geaggregeerd naar loc
  sel <- unique(macft[macft$monsterident == i,])
  # parameters water---------  
  doorz_diep <- ifelse(length(sel$meetwaarde[sel$parametercode %in% "ZICHT"])>0 &
                         length(sel$meetwaarde[sel$parametercode %in% "WATDTE"])>0,
                       sel$meetwaarde[sel$parametercode %in% "ZICHT"]/sel$meetwaarde[sel$parametercode %in% "WATDTE"], NA)
  
  if(length(doorz_diep)==0){out <- NULL}else{
  
  diepte <- ifelse(length(sel$meetwaarde[sel$parametercode %in% "WATDTE"])>0, sel$meetwaarde[sel$parametercode %in% "WATDTE"], NA)
  slib <- ifelse(length(sel$meetwaarde[sel$parametercode %in% "SLIBDTE"])>0, sel$meetwaarde[sel$parametercode %in% "SLIBDTE"], NA)
  talud <- ifelse(length(sel$meetwaarde[sel$fewsparameter %in% "TALBVWTR_graad"])>0, sel$meetwaarde[sel$fewsparameter %in% "TALBVWTR_graad" ], NA)
   
  n_soort <- nrow(sel[sel$parametercode %in% "" & sel$parameterfractie %in% "" & sel$biotaxonnaam %in% soortenlijst_submers,]) #LET OP: DIT IS VOOR W5 en verder
  
  woeker <- ifelse(length(sel$meetwaarde[sel$parametercode %in% "" & sel$biotaxonnaam %in% soortenlijst_submers])==0,
                   0, max(sel$meetwaarde[sel$parametercode %in% "" & sel$biotaxonnaam %in% soortenlijst_submers])) # max bedekking van 1 submers soort
  
  SUBMS <- sel$meetwaarde[sel$parametercode %in% "SUBMSPTN"]
  if(!length(SUBMS)>0){
    SUBMS <- min(100, sum(sel$meetwaarde[sel$biotaxonnaam %in% soortenlijst_submers])) #als groeivormmeting ontbreekt, dan bedekkingen optellen
  }
  
  KROOS <- sel$meetwaarde[sel$parametercode %in% "KROOS"]
  if(!length(KROOS)>0){
    KROOS <- min(100,sum(sel$meetwaarde[sel$biotaxonnaam %in% soortenlijst_kroos])) #als groeivormmeting ontbreekt, dan bedekkingen optellen
  }
  
  FLAB <- sel$meetwaarde[sel$parametercode %in% "FLAB" & sel$parameterfractie %in% "DRIJVD"]
  if(!length(FLAB)>0){
    FLAB <- 0.001 # als ontbreekt dan is er geen flab
  }
  
  # parameters oever----------  
  sel2 <- sel[sel$compartiment %in% c("OR", "EZ")]
  beschoeid <- if(length(sel2$meetwaarde[sel2$parametercode %in% "OEVBSIG"])==0){ 
    #als OEVBSIG ontbreekt wordt beschoeing op "nee" gezet!
    "nee"}else{if(sel2$meetwaarde[sel2$parametercode %in% "OEVBSIG"] %in% c('31','32','41','52','35')){'ja'}else{"nee"}}
  
  n_emsoort <- nrow(sel2[sel2$biotaxonnaam %in% soortenlijst_emers & sel2$compartiment == 'EZ',])
  emers <- sel$meetwaarde[sel$parametercode %in% "EMSPTN"]
  
  n_oevsoort <- nrow(sel2[sel2$biotaxonnaam %in% soortenlijst_oever & sel2$compartiment == 'OR',])
  oever <- sel$meetwaarde[sel$parametercode %in% "OEVPTN"]
  
  if(!length(emers)>0){
    emers <- min(100,sum(sel$meetwaarde[sel$biotaxonnaam %in% soortenlijst_oever])) #als groeivormmeting ontbreekt, dan bedekkingen optellen
  }
  riet <- sel2$meetwaarde[sel2$biotaxonnaam %in% "Phragmites australis"]
  if(length(riet)<1){riet <- 0}
  
  #W1 -----------
  #water met flab/draadalgen, weinig soorten en geen woekerende planten
  W1 <- NA
  if(FLAB >= grens_flab & n_soort < grens_n_soort & woeker < grens_woeker){W1 <- 1}else{W1  <- 0}
  
  
  #W2----
  #water met kroos en weinig soorten (en wel of geen woekerende/drijfblad planten)
  W2 <- NA
  if(KROOS >= grens_kroos & n_soort < grens_n_soort){W2 <- 1}else{W2  <- 0}
  
  #W3-----
  #water met drijfbladplanten
  w3_sel <- sel[sel$parametercode %in% "DRIJFBPTN", ]
  W3 <- NA
  if(nrow(w3_sel)==0){W3  <- 0}else{
  if(w3_sel$meetwaarde >= grens_drijf & n_soort < grens_n_soort & woeker < grens_woeker){W3 <- 1} #1=ja, 0=nee, 99=onbekend
  if(w3_sel$meetwaarde < grens_drijf){W3 <- 0}
  }
  
  #W6----
  #water met veel woekerende waterplanten (en weinig soorten)
  if(n_soort < grens_n_soort & woeker >= grens_woeker){W6<-1}
  if(!(n_soort < grens_n_soort & woeker >= grens_woeker)){W6 <-0}
  
  if(!is.na(doorz_diep)){
  #W4----
  # troebel, weinig planten
  if(doorz_diep < grens_zicht & SUBMS < grens_submers){W4 <- 1}else{W4 <- 0}
  
  #W4a----
  # troebel, veel planten: hier is doorzicht/diepte dus geen goede indicator
  if(doorz_diep < grens_zicht & SUBMS >= grens_submers & woeker < grens_woeker){W4a <- 1}else{W4a <- 0}
  
  #W5----
  #helder water met veel waterplanten in hoge bedekking (en meer dan 5 soorten)
  if(doorz_diep >= grens_zicht & n_soort >= grens_n_soort & SUBMS >= grens_submers ){W5 <- 1}
  if(!(doorz_diep >= grens_zicht & n_soort >= grens_n_soort & SUBMS >= grens_submers )){W5 <- 0}

  #W7-----
  #helder water met weinig soorten (1 en 5) niet woekerende, ondergedoken waterplanten 
  if(doorz_diep >= grens_zicht & n_soort <= grens_n_soort & n_soort >= 1 & woeker < grens_woeker){W7<-1}
  if(!(doorz_diep >= grens_zicht & n_soort <= grens_n_soort & n_soort >= 1 & woeker < grens_woeker )){W7<-0}
  
  #W8----
  #helder water met veel soorten ondergedoken waterplanten in lage dichtheid
  # waterdiepte toevoegen?
  W8 <- NA
  if(doorz_diep >= grens_zicht & n_soort > grens_n_soort & SUBMS < grens_submers ){W8 <- 1}
  if(!(doorz_diep >= grens_zicht & n_soort > grens_n_soort & SUBMS < grens_submers )){W8 <- 0}

  #W9----
  #helder water zonder waterplanten
  if(doorz_diep >= grens_zicht & n_soort < 1 & FLAB < grens_flab & KROOS < grens_kroos){W9<-1}else{W9<-0}
  }else{W4<-NA ;W4a<-NA;W5<-NA;W7<-NA;W8<-NA;W9<-NA}
 
  if(beschoeid %in% "ja" & n_emsoort < gr_soorten & riet < gr_riet){O1 <- 1}
  if(!(beschoeid %in% "ja" & n_emsoort < gr_soorten & riet < gr_riet)){O1 <- 0}
  
  if(beschoeid %in% "ja" & n_soort >= gr_soorten & riet < gr_riet){O2 <- 1}
  if(!(beschoeid %in% "ja" & n_soort >= gr_soorten & riet < gr_riet)){O2 <- 0}
  
  if(beschoeid %in% "ja" & n_soort < gr_soorten & riet >= gr_riet){O3 <- 1}
  if(!(beschoeid %in% "ja" & n_soort < gr_soorten & riet >= gr_riet)){O3 <- 0}
  
  if(beschoeid %in% "ja" & n_soort >= gr_soorten & riet >= gr_riet){O4 <- 1}
  if(!(beschoeid %in% "ja" & n_soort >= gr_soorten & riet >= gr_riet)){O4 <- 0}
  
  if(beschoeid %in% "nee" &  n_soort < gr_soorten & riet < gr_riet){O5 <- 1}
  if(!(beschoeid %in% "nee" &  n_soort < gr_soorten & riet < gr_riet)){O5 <- 0}
  
  if(beschoeid %in% "nee" & n_soort >= gr_soorten & riet < gr_riet){O6 <- 1}
  if(!(beschoeid %in% "nee" & n_soort >= gr_soorten & riet < gr_riet)){O6 <- 0}
  
  if(beschoeid %in% "nee" & n_soort < gr_soorten & riet >= gr_riet){O7 <- 1}
  if(!(beschoeid %in% "nee" & n_soort < gr_soorten & riet >= gr_riet)){O7 <- 0}
  
  if(beschoeid %in% "nee" & n_soort >= gr_soorten & riet >= gr_riet){O8 <- 1}
  if(!(beschoeid %in% "nee" & n_soort >= gr_soorten & riet >= gr_riet)){O8 <- 0}
  
  # make a list to store the output
  out <- data.table(locatie.EAG= unique(sel$locatie.EAG), 
              jaar = unique(sel$jaar), 
              locatiecode = unique(sel$locatiecode),
              watertype = unique(sel$locatie.KRW.watertype),
              monsterident= unique(sel$monsterident),
              compartiment= unique(sel$compartiment),
              doorz_diep,
              diepte,
              slib,
              talud,
              FLAB,
              KROOS,
              SUBMS,
              emers,
              woeker,
              oever,
              n_soort,
              n_emsoort,
              n_oevsoort,
              beschoeid,
              W1,W2,W3,W4,W4a,W5,W6,W7,W8,W9,O1,O2,O3,O4,O5,O6,O7,O8
          
  )
  }
  # return list with relevant properties
  return(out)
}

EST_aggloc <- function(est){
  cols <- c('compartiment','monsterident','doorz_diep','diepte','slib','talud','FLAB','KROOS','SUBMS','emers','woeker','n_soort','oever','n_emsoort','n_oevsoort','beschoeid')
  estloc <- estout[,lapply(.SD, sum, na.rm=TRUE), by=c('locatie.EAG','locatiecode','jaar','watertype'),.SDcols = -cols]
  cols2 <- c('compartiment','monsterident',"W1","W2","W3","W4","W4a","W5","W6","W7","W8","W9","O1","O2","O3","O4","O5","O6","O7","O8")
  estloc2 <- estout[,lapply(.SD, median, na.rm=TRUE), by=c('locatie.EAG','locatiecode','jaar','watertype'),.SDcols = -cols2]
  estloc <- merge(estloc,estloc2, by=c('locatie.EAG','locatiecode','jaar','watertype'))
  write.table(estloc, paste0("output/estlocatie_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(estloc)
}

EST_aggeag <- function(estloc){
  cols <- c('locatiecode','doorz_diep','diepte','slib','talud','FLAB','KROOS','SUBMS','emers','woeker','n_soort','oever','n_emsoort','n_oevsoort','beschoeid')
  esteag <- estloc[, lapply(.SD, sum, na.rm=TRUE), by=c('locatie.EAG','jaar','watertype'),.SDcols = -cols]
  cols2 <- c('locatiecode',"W1","W2","W3","W4","W4a","W5","W6","W7","W8","W9","O1","O2","O3","O4","O5","O6","O7","O8")
  esteag2 <- estloc[,lapply(.SD, median, na.rm=TRUE), by=c('locatie.EAG','jaar','watertype'),.SDcols = -cols2]
  esteag <- merge(esteag,esteag2, by=c('locatie.EAG','jaar','watertype'))
  write.table(esteag, paste0("output/esteag_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(esteag)
}

EST_addnameeag <- function(esteag, EKRset, eag_wl){
  esteag <- esteag[rowSums(esteag[,4:13]) > 0,]
  esteag$W <- colnames(esteag[,4:13])[max.col(esteag[,4:13],ties.method="first")]
  esteag$O <- colnames(esteag[,14:21])[max.col(esteag[,14:21],ties.method="first")]
  estmerg <- merge(esteag[,-'watertype'], eag_wl[,c('GAFIDENT', "type","StedelijkLandelijk","watertype")], by.x = c('locatie.EAG'), by.y = c('GAFIDENT'))
  estmerg$ESTnaam2[estmerg$watertype == 'M20'] <-  'DM'
  estmerg$ESTnaam2[estmerg$watertype %in% c('M14','M27',"M25","M11")] <-  'OM'
  estmerg$ESTnaam2[estmerg$watertype %in% c('M1a','M1b','M8',"M10","M3")] <- 'Sl'
  estmerg$ESTnaam2[estmerg$watertype %in% c("M6b",'M30',"M7b", "M6a")] <- 'K'
  estmerg$ESTnaam3[estmerg$StedelijkLandelijk == 'Stedelijk'] <- 'St'
  estmerg$ESTnaam3[estmerg$StedelijkLandelijk == 'Landelijk'] <- 'L'
  estmerg$estnaam <- paste0(estmerg$W,'_',estmerg$O,'_',estmerg$ESTnaam2,'_', estmerg$ESTnaam3)
  estmerg <- merge(grenswaarden_EST[,c('omschrijving','type')], estmerg, by.y = 'W', by.x = 'type', all.y = T, allow.cartesian =T)
  estmerg <- merge(grenswaarden_EST[,c('omschrijving','type')], estmerg, by.y = 'O', by.x = 'type', all.y = T, allow.cartesian =T)
  estmerg$type <- NULL; estmerg$type.y <-NULL
  estmerg$estnaamvol <- paste0(estmerg$estnaam,'_',estmerg$omschrijving.y,'_', estmerg$omschrijving.x)
  write.table(estmerg, paste0("output/esteagnaam_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(estmerg)
}  

EST_addnameloc <- function(estloc, EKRset, eag_wl){  
  estloc <- estloc[rowSums(estloc[,5:14]) > 0,]
  estloc$W <- colnames(estloc[,5:14])[max.col(estloc[,5:14],ties.method="first")]
  estloc$O <- colnames(estloc[,15:22])[max.col(estloc[,15:22],ties.method="first")]
  estmergl <- merge(estloc[,-'watertype'], eag_wl[,c('GAFIDENT', "type","StedelijkLandelijk","watertype")], by.x = c('locatie.EAG'), by.y = c('GAFIDENT'))
  estmergl$ESTnaam2[estmergl$watertype == 'M20'] <-  'DM'
  estmergl$ESTnaam2[estmergl$watertype %in% c('M14','M27',"M25","M11")] <-  'OM'
  estmergl$ESTnaam2[estmergl$watertype %in% c('M1a','M1b','M8',"M10","M3")] <- 'Sl'
  estmergl$ESTnaam2[estmergl$watertype %in% c("M6b",'M30',"M7b", "M6a")] <- 'K'
  estmergl$ESTnaam3[estmergl$StedelijkLandelijk == 'Stedelijk'] <- 'St'
  estmergl$ESTnaam3[estmergl$StedelijkLandelijk == 'Landelijk'] <- 'L'
  estmergl$estnaam <- paste0(estmergl$W,'_',estmergl$O,'_',estmergl$ESTnaam2,'_', estmergl$ESTnaam3)
  
  write.table(estmergl, paste0("output/estlocnaam_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(estmergl)
}

EST_koppeleag <- function(esteagname, EKRset){
  ekragg <- krw[krw$wbmethode == 'ml_2018_ov.wflora', ]
  estekr <- merge(esteagname, ekragg, by.x=c('locatie.EAG','jaar'), by.y = c('EAGIDENT','jaar'))
  write.table(estekr, paste0("output/estekr_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(estekr)
}

EST_koppelloc <- function(estmergl, krwloc){  
  ekrsel <- dcast(krwloc, id+mpid2+jaar~GPHRnew, fun = median, value.var = 'EKR')
  estekrloc <- merge(estmergl, ekrsel, by.x=c('locatiecode','jaar'), by.y = c('mpid2','jaar'))
  write.table(estekrloc, paste0("output/estekrloc_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(estekrloc)
}

estekrplot <- function(estekrloc){
  #Numeriekewaarde of EKR
  p<- ggplot(estekrloc, aes(x= reorder(W, ss_macrofyten, na.rm = TRUE), y= ss_macrofyten))+ #,"_",O
    geom_boxplot() +
    facet_grid(ESTnaam2~., scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), 
      strip.text.y = element_text(size = 5), 
      axis.text.x = element_text(size= 8, angle=90,hjust=1),
      axis.text.y = element_text(size= 8, hjust=2),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank()
    )+
    ggtitle('') +
    labs(x= 'ecosysteem toestand' , y= 'ekr flora')
  ggplotly(p=p)
  
  p<- ggplot(estekrloc[estekrloc$ESTnaam2 == "Sl",], aes(x= estnaam, y= ss_macrofyten, label = paste0(locatie.EAG, jaar)))+
    geom_boxplot() +
    #facet_grid(~jaar, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), 
      strip.text.y = element_text(size = 5), 
      axis.text.x = element_text(size= 8, angle=90,hjust=1),
      axis.text.y = element_text(size= 8, hjust=2),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      
    )+
    ggtitle('') +
    labs(x= 'ecosysteem toestand' , y= 'ekr flora')
  ggplotly(p=p)
  
  #simpele correlaties/ pairs per watertype
  p<- ggplot(estekrloc[estekrloc$diepte  & estekrloc$ESTnaam2 == "Sl",], aes(x= diepte, y= oever, label = paste0(locatie.EAG, jaar),  col = watertype))+
    geom_jitter() +
    #facet_grid(~jaar, scales = 'free')+
    # scale_fill_manual(values = c("1" =  "green",
    #                              "0" = "red")) +
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), 
      strip.text.y = element_text(size = 5), 
      axis.text.x = element_text(size= 8, angle=90,hjust=1),
      axis.text.y = element_text(size= 8, hjust=2),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank()
    )+
    ggtitle('') +
    labs(x= 'slib' , y= 'nsoort')
  ggplotly(p=p)

  # grenswaarden per EST
  meltest <- melt(estlocname, id.vars= c('locatie.EAG','locatiecode','jaar','watertype','W','O','estnaam'), measure.vars=c('doorz_diep','diepte','slib','talud','FLAB','KROOS','SUBMS','emers','woeker','n_soort','n_ovsoort','beschoeid'))
  meltest$vars <- as.character(meltest$variable)
  p<- ggplot(meltest, aes(x= W, y= value))+
    geom_boxplot() +
    facet_wrap(~vars)+
    coord_flip()+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), 
      strip.text.y = element_text(size = 5), 
      axis.text.x = element_text(size= 8, angle=90,hjust=1),
      axis.text.y = element_text(size= 8, hjust=2),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank()
    )+
    ggtitle('') +
    labs(x= '' , y= '')
  ggplotly(p=p)  
  
  }

printestplots <- function(estekr){
  
  for(i in unique(paste0(estekrloc$W,estekrloc$ESTnaam2,estekrloc$ESTnaam3))){
    p<- ggplot(estekrloc[paste0(estekrloc$W,estekrloc$ESTnaam2,estekrloc$ESTnaam3) == i,], aes(x= estnaam, y= Numeriekewaarde, label = paste0(locatie.EAG, jaar)))+
      geom_boxplot() +
      #facet_grid(~jaar, scales = 'free')+
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), 
        strip.text.y = element_text(size = 5), 
        axis.text.x = element_text(size= 8, angle=90,hjust=1),
        axis.text.y = element_text(size= 8, hjust=2),
        axis.ticks =  element_line(colour = "black"),
        panel.background = element_blank(),
        plot.background = element_blank()
      )+
      ggtitle('') +
      labs(x= 'ecosysteem toestand' , y= 'ekr flora')
    ggsave(paste0("output/ekrest_W",i,".png"))
  }
}

kaartEST <- function(){
  library(sp)
  library(plotGoogleMaps)
  estmap <- merge(gEAG,estmergl, by.x = "GAFIDENT" , by.y = "locatie.EAG" )
  # coordinates(estmap)<-~x+y
  
  #using plotGoogleMaps::pieSP to generate the spatial data.frame for pie-chart
  pies <- pieSP(estmap,zcol=unique(estmap$estnaam), max.radius=50)
  pies$pie=rep(unique(estmap$estnaam),155)
  
  # m=plotGoogleMaps(pies, zcol='pie') #run this to show the java-based output of piechart on map
  
  #Extract spatial polygon data.frame 
  library(broom)
  library(ggplot2)
  
  names(pies@polygons)<-pies$pie
  pi<-tidy(pies)
  
  ggplot() +
    geom_polygon(data=pi, aes(x=long, y=lat, group=id, fill=.id))
}

kaartEKRmp <- function(dt = dt,
                       EAGsel = EAGsel,
                       mlocs = locaties,
                       nyears = 3,
                       ekr_col = c("red", "orange", "yellow", "green"),
                       ekr_labels = c("slecht","ontoereikend","matig","goed"), 
                       ekr_breaks = c(0, 0.2, 0.4, 0.6, 1)){
  
  # add year number and take only nyears most recent years (selection per EAG)
  dt <- dt[,yearid := frank(-jaar, ties.method = 'dense'), by = c('EAGIDENT','GPHRnew')][yearid <= nyears]
  dt <- dt[!is.na(EKR),  cat1 := as.integer(cut(EKR, ekr_breaks, labels = 1:4, include.lowest = T))]
  
  pl <- merge(dt, mlocs[,c('CODE','XCOORD','YCOORD')], by.x ='mpid2', by.y = 'CODE')
  pl <- st_as_sf(pl, coords = c('XCOORD','YCOORD'), crs = proj4.rd)
  sort(pl$jaar, decreasing = T)
 
  bboxEAG <- st_bbox(EAGsel)
  # gEAGsel %>%st_transform(crs=proj4.google) 
  # plaats <- st_crop(grenzen, bboxEAG)
  # location <- as.vector(c(left= bboxEAGv[1], bottom = bboxEAGv[2],right = bboxEAGv[3],top= bboxEAGv[4]))
  # map <- ggmap::get_map(location,  source = "stamen", maptype = "toner-background")
  basemap <- tmaptools::read_osm(bboxEAG, type = "esri-topo", zoom = 15, ext = 1.2)

  p <- 
    ggplot()+
    layer_spatial(basemap)+
    # basemap_gglayer(bboxEAG, map_type = "watercolor")+
    # geom_sf(data= watersel, color = NA, fill = '#3498DB')+
    geom_sf(data = EAGsel, color = 'black', fill = NA, size = 0.75, inherit.aes = F) +
    geom_sf(data = pl, aes(fill = as.factor(cat1), color = as.factor(cat1), size = jaar), 
            colour="black", pch=21,
            show.legend = TRUE, inherit.aes = F) +
    scale_size(range= c(2,9))+
    scale_fill_manual(values = c("1" =  ekr_col[1],
                                 "2" = ekr_col[2],
                                 "3" = ekr_col[3],
                                 "4" = ekr_col[4]), drop =T, labels = ekr_labels) +
    scale_colour_manual(values = c("1" =  ekr_col[1],
                                   "2" = ekr_col[2],
                                   "3" = ekr_col[3],
                                   "4" = ekr_col[4]), drop =T, labels = ekr_labels, guide = "none") +
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks =  element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title=element_text(size=10), 
      legend.text=element_text(size=9)
    )+
    guides(fill = guide_legend(title = unique(pl$GHPR)), size = guide_legend(title = 'Jaar'))+
    ggtitle(unique(pl$waterlichaam), subtitle= unique(pl$GPHRnew))+
    # add north
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5,  text_col = "black", pad_y = unit(0.1, "cm")) +
    # add scale bar
    ggspatial::annotation_north_arrow(location = "bl",which_north = "true",
                                      pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                                      height = unit(0.5, 'in'), width = unit(0.5, 'in'),
                                      style = north_arrow_fancy_orienteering(text_col = "black"))
    ##coord_sf(xlim = c(bboxEAG$xmin,bboxEAG$xmax), ylim = c(bboxEAG$ymin,bboxEAG$ymax), datum = NA)
    
    ggsave(p, file = paste0('output/ekrstippen/', unique(pl$gebied),unique(pl$GPHRnew),'.png'), units='cm',dpi=1000)
    print(paste0(unique(pl$EAGIDENT),unique(pl$GPHRnew)))
    }

EST_koppeleagestwq <- function(esteagname, wqmeanEAG){
  estmeanhybi <- merge(estmeanhybi, wqmeanEAG, by=c('locatie.EAG','jaar'))
  return(estmeanhybi)
  write.table(estmeanhybi, file = paste(getwd(),"/output/esthybimeanEAG",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
}

toestandbeschrijving <- function(estmeanhybi){
hybiest$toestandb <- paste0("In ",hybiest$locatie.EAG,": ",hybiest$omschrijving.y,
                            ". De gemiddelde en mediane bedekking met waterplanten is respectievelijk ", round(hybiest$bedsubmers.mean, 1)," en ",round(hybiest$bedsubmers.median, 1),
                            ". De gemiddelde en mediane bedekking met drijvende draadalgen is respectievelijk ", round(hybiest$FLAB.mean,1)," en ",round(hybiest$FLAB.median,1),
                            ". De gemiddelde en mediane bedekking met kroos is respectievelijk ", round(hybiest$kroos.mean,1)," en ",round(hybiest$kroos.median,1),
                            ". Het mediane aanal soorten onderwaterplanten per meetlocatie is ", as.integer(hybiest$n_soort),
                            ifelse(!is.na(hybiest$mean_CHLFA),
                            ifelse(hybiest$mean_CHLFA > 25 & !is.na(hybiest$mean_FLUOBLAU) & hybiest$mean_FLUOBLAU > 12,". Er bevinden zich veel blauwalgen in het water. ",
                            ifelse(hybiest$mean_CHLFA > 25 & !is.na(hybiest$mean_FLUOGROE) & hybiest$mean_FLUOGROE > 15,". Er bevinden zich veel groenalgen in het water. ",
                            ifelse(hybiest$mean_CHLFA > 25, ". Er bevinden zich veel algen in het water. ",". Er bevinden zich weinig algen in het water. " ))),". "),
                            hybiest$omschrijving.x,
                            ". De gemiddelde en mediane bedekking met emerse planten is respectievelijk ", round(hybiest$bedemers.mean,1)," en ",round(hybiest$bedemers.median,1),
                            ".")
}


