
#i <- unique(macft$monsterident)[1]
# i <- 'WP556666' w3, w6, w7
# i <- 'WP527261'
# i<- "WP527185"


monstextract <- function(i, macft, soortenlijst_submers, soortenlijst_kroos, soortenlijst_oever, grenswaarden_EST){
  sel <- unique(macft[macft$monsterident == i,])
# parameters water---------  
  doorz_diep <- ifelse(length(sel$meetwaarde[sel$parametercode %in% "ZICHT"])>0 &
                         length(sel$meetwaarde[sel$parametercode %in% "WATDTE"])>0,
                       sel$meetwaarde[sel$parametercode %in% "ZICHT"]/sel$meetwaarde[sel$parametercode %in% "WATDTE"], NA)
  
  if(length(doorz_diep)==0|is.na(doorz_diep)|doorz_diep < 0) {out <- NULL}else{
  
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
  
  n_ovsoort <- nrow(sel2[sel2$biotaxonnaam %in% soortenlijst_oever,])
  emers <- sel$meetwaarde[sel$parametercode %in% "EMSPTN"]
  if(!length(emers)>0){
    emers <- min(100,sum(sel$meetwaarde[sel$biotaxonnaam %in% soortenlijst_oever])) #als groeivormmeting ontbreekt, dan bedekkingen optellen
  }
  riet <- sel2$meetwaarde[sel2$biotaxonnaam %in% "Phragmites australis"]
  if(length(riet)<1){riet <- 0}
  
  #W1 -----------
  #water met flab/draadalgen
  W1 <- NA
  if(FLAB >= grens_flab){W1 <- 1}else{W1  <- 0}
  
  
  #W2----
  #water met kroos
  W2 <- NA
  if(KROOS >= grens_kroos){W2 <- 1}else{W2  <- 0}
  
  #W3-----
  #water met drijfbladplanten
  w3_sel <- sel[sel$parametercode %in% "DRIJFBPTN", ]
  W3 <- NA
  if(nrow(w3_sel)==0){W3  <- 0}else{
  if(w3_sel$meetwaarde >= grens_drijf){W3 <- 1} #1=ja, 0=nee, 99=onbekend
  if(w3_sel$meetwaarde < grens_drijf){W3 <- 0}
  }
  
  #W4----
  # troebel, weinig planten
  if(doorz_diep < grens_zicht & SUBMS < grens_submers){W4 <- 1}else{W4 <- 0}
  
  #W4a----
  # troebel, veel planten: hier is doorzicht/diepte dus geen goede indicator
  if(doorz_diep < grens_zicht & SUBMS >= grens_submers){W4a <- 1}else{W4a <- 0}
  
    #W5----
  #helder water met veel waterplanten in hoge bedekking (en meer dan 5 soorten)
  if(doorz_diep >= grens_zicht & n_soort >= grens_n_soort & SUBMS >= grens_submers ){W5 <- 1}
  if(!(doorz_diep >= grens_zicht & n_soort >= grens_n_soort & SUBMS >= grens_submers )){W5 <- 0}
  
  #W6----
  #helder water met veel woekerende waterplanten (en weinig soorten)
  if(doorz_diep >= grens_zicht & n_soort < grens_n_soort & woeker >= grens_woeker){W6<-1}
  if(!(doorz_diep >= grens_zicht & n_soort < grens_n_soort & woeker >= grens_woeker)){W6 <-0}
    
  #W7-----
  #helder water met weinig soorten (1 en 5) niet woekerende, ondergedoken waterplanten 
  if(doorz_diep >= grens_zicht & n_soort <= grens_n_soort & n_soort >= 1 & woeker < grens_woeker){W7<-1}
  if(!(doorz_diep >= grens_zicht & n_soort <= grens_n_soort & n_soort >= 1 & woeker < grens_woeker )){W7<-0}
  
  #W7a-----
  #helder water met weinig soorten (1 en 5) niet woekerende, ondergedoken waterplanten in lage dichtheid
  if(doorz_diep >= grens_zicht & n_soort <= grens_n_soort & n_soort >= 1 & SUBMS < grens_submers ){W7a<-1; W7 <-0}
  if(!(doorz_diep >= grens_zicht & n_soort <= grens_n_soort & n_soort >= 1 & SUBMS < grens_submers)){W7a<-0}
  
  #W8----
  #helder water met veel soorten ondergedoken waterplanten in lage dichtheid
  W8 <- NA
  if(doorz_diep >= grens_zicht & n_soort > grens_n_soort & SUBMS < grens_submers ){W8 <- 1}
  if(!(doorz_diep >= grens_zicht & n_soort > grens_n_soort & SUBMS < grens_submers )){W8 <- 0}

  #W9----
  #helder water zonder waterplanten
  if(doorz_diep >= grens_zicht & n_soort < 1 & FLAB < grens_flab & KROOS < grens_kroos){W9<-1}else{W9<-0}
 
  #W5c combinatie W3 en W5 of 8 drijfblad met veel waterplanten----
  W5c <- 0
  if(W3 %in% 1 & W5 %in% 1|W3 %in% 1 & W8 %in% 1){W5c <- 1; W3 <- 0; W5 <- 0; W8 <- 0; W1 <- 0; W2 <- 0}
  
  #W5a combinatie W1 en W5 of W8 flab en veel waterplanten----
  W5a <- 0
  if(W1 %in% 1 & W5 %in% 1|W3 %in% 1 & W8 %in% 1){W5a <- 1; W1 <- 0; W5 <- 0; W8 <- 0; W2 <- 0}
  
  #W5b combinatie W2 en W5 of 8 kroos met veel waterplanten----
  W5b <- 0
  if(W2 %in% 1 & W5 %in% 1|W2 %in% 1 & W8 %in% 1){W5b <- 1; W2 <- 0; W5 <- 0; W8 <- 0}
  
  #W6b toevoegen als combinatie W3 en W6 drijfblad met woekerende waterplanten----
  W6b <- 0
  if(W3 %in% 1 & W6 %in% 1){W6b <- 1; W3 <- 0; W6 <- 0; W1 <- 0; W2 <- 0}
  
  #W6a toevoegen als combinatie W2 of W1 en W6 kroos of flab met woekerende waterplanten----
  W6a <- 0
  if(W1 %in% 1 & W6 %in% 1|W2 %in% 1 & W6 %in% 1){W6a <- 1; W1 <- 0; W2 <- 0; W6 <- 0}
  
  #W7d toevoegen als combinatie W3 en W7 drijfblad met weinig soorten onderwaterplanten----
  W7d <- 0
  if(W3 == 1 & W7 ==1|W3 == 1 & W7a ==1){W7d <- 1; W3 <- 0; W7 <- 0; W7a <- 0; W1 <- 0; W2 <- 0}
  
  #W7b combinatie W1 en W7 of 7a flab met weinig soorten onderwaterplanten----
  W7b <- 0
  if(W1 %in% 1 & W7 %in% 1|W1 %in% 1 & W7a %in% 1){W7b <- 1; W1 <- 0; W7 <- 0; W7a <- 0; W2 <- 0}
  
  #W7c toevoegen als combinatie W2 en W7 of 7a kroos met weinig soorten onderwaterplanten----
  W7c <- 0
  if(W2 %in% 1 & W7 %in% 1|W1 %in% 1 & W7a %in% 1){W7c <- 1; W2 <- 0; W7 <- 0; W7a <- 0}
  
  #W2a combinatie W1 W2 flab en kroos----
  W2a <- 0
  if(W1 %in% 1 & W2 %in%1){W2a <- 1; W1 <- 0; W2 <- 0}
  
  if(beschoeid %in% "ja" & n_ovsoort < gr_soorten & riet < gr_riet){O1 <- 1}
  if(!(beschoeid %in% "ja" & n_ovsoort < gr_soorten & riet < gr_riet)){O1 <- 0}
  
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
              n_soort,
              n_ovsoort,
              W1,W2,W2a,W3,W4,W4a,W5,W5a,W5b,W5c,W6,W6a,W6b,W7,W7a,W7b,W7c,W7d,W8,W9,O1,O2,O3,O4,O5,O6,O7,O8
          
  )
  }
  # return list with relevant properties
  return(out)
}

EST_aggloc <- function(est){
  cols <- c('compartiment','monsterident','doorz_diep','diepte','slib','talud','FLAB','KROOS','SUBMS','emers','woeker','n_soort','n_ovsoort')
  estloc <- est[,lapply(.SD, sum, na.rm=TRUE), by=c('locatie.EAG','locatiecode','jaar','watertype'),.SDcols = -cols]
  cols2 <- c('compartiment','monsterident',"W1","W2","W2a","W3","W4","W4a","W5","W5a","W5b","W5c","W6","W6a","W6b","W7","W7a","W7b","W7b","W7c","W7d","W8","W9","O1","O2","O3","O4","O5","O6","O7","O8")
  estloc2 <- est[,lapply(.SD, median, na.rm=TRUE), by=c('locatie.EAG','locatiecode','jaar','watertype'),.SDcols = -cols2]
  estloc <- merge(estloc,estloc2, by=c('locatie.EAG','locatiecode','jaar','watertype'))
  write.table(estloc, paste0("output/estlocatie_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(estloc)
}

EST_aggeag <- function(estloc){
  cols <- c('locatiecode','doorz_diep','diepte','slib','talud','FLAB','KROOS','SUBMS','emers','woeker','n_soort','n_ovsoort')
  esteag <- estloc[, lapply(.SD, sum, na.rm=TRUE), by=c('locatie.EAG','jaar','watertype'),.SDcols = -cols]
  cols2 <- c('locatiecode',"W1","W2","W2a","W3","W4","W4a","W5","W5a","W5b","W5c","W6","W6a","W6b","W7","W7a","W7b","W7b","W7c","W7d","W8","W9","O1","O2","O3","O4","O5","O6","O7","O8")
  esteag2 <- estloc[,lapply(.SD, median, na.rm=TRUE), by=c('locatie.EAG','jaar','watertype'),.SDcols = -cols2]
  esteag <- merge(esteag,esteag2, by=c('locatie.EAG','jaar','watertype'))
  write.table(esteag, paste0("output/esteag_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(esteag)
}

EST_addnameeag <- function(esteag, EKRset, eag_wl){
  esteag <- esteag[rowSums(esteag[,4:23]) > 0,]
  esteag$W <- colnames(esteag[,4:23])[max.col(esteag[,4:23],ties.method="first")]
  esteag$O <- colnames(esteag[,24:31])[max.col(esteag[,24:31],ties.method="first")]
  estmerg <- merge(esteag[,-'watertype'], eag_wl, by.x = c('locatie.EAG'), by.y = c('GAFIDENT'))
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
  estloc <- estloc[rowSums(estloc[,5:24]) > 0,]
  estloc$W <- colnames(estloc[,5:24])[max.col(estloc[,5:24],ties.method="first")]
  estloc$O <- colnames(estloc[,25:32])[max.col(estloc[,25:32],ties.method="first")]
  estmergl <- merge(estloc[,-'watertype'], eag_wl, by.x = c('locatie.EAG'), by.y = c('GAFIDENT'))
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
  p<- ggplot(estekrloc, aes(x= paste0(W,"_",O), y= Numeriekewaarde))+
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
  ggplotly(p=p)
  
  p<- ggplot(estekrloc[estekrloc$ESTnaam2 == "S",], aes(x= estnaam, y= Numeriekewaarde, label = paste0(locatie.EAG, jaar)))+
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
  ggplotly(p=p)
  
  p<- ggplot(estekrloc[estekrloc$diepte > 0,], aes(x= talud, y= emers, label = paste0(locatie.EAG, jaar), col = watertype))+
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
    labs(x= 'talud' , y= 'emers')
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
                       waterpereag = waterpereag,
                       EAG = EAG,
                       mlocs = locaties,
                       nyears = 3,
                       ekr_col = c("red", "orange", "yellow", "green"),
                       ekr_labels = c("slecht","ontoereikend","matig","goed"), 
                       ekr_breaks = c(-0.01, 0.2, 0.4, 0.6, 1)){
  
  # add year number and take only nyears most recent years (selection per EAG)
  dt <- dt[,yearid := frank(-jaar, ties.method = 'dense'), by = c('EAGIDENT','GPHRnew')][yearid <= nyears]
  dt <- dt[!is.na(EKR),  cat1 := as.integer(cut(EKR, ekr_breaks, labels = 1:4, include.lowest = T))]
  
  pl <- merge(dt, mlocs[,c('CODE','XCOORD','YCOORD')], by.x ='mpid2', by.y = 'CODE')
  pl <- st_as_sf(pl, coords = c('XCOORD','YCOORD'), crs = proj4.rd)

  # bounding box needed
  bboxEAG <- st_bbox(EAG)
  bm <- loadbasemap(EAG, "hybrid",1.2,13)
  
  p <- ggplot() + 
    geom_sf(data = waterpereag, color = '#3498DB', fill = '#3498DB') +
    geom_sf(data = EAG, color = 'grey25', fill = "white", size = 0.2) +
    geom_sf(data = pl, aes(fill = as.factor(cat1), color = as.factor(cat1), size = jaar), 
            show.legend = TRUE) +
    scale_fill_manual(values = c("1" =  ekr_col[1],
                                 "2" = ekr_col[2],
                                 "3" = ekr_col[3],
                                 "4" = ekr_col[4]), drop =T, labels = ekr_labels, guide = "none") +
    scale_colour_manual(values = c("1" =  ekr_col[1],
                                   "2" = ekr_col[2],
                                   "3" = ekr_col[3],
                                   "4" = ekr_col[4]), drop =T, labels = ekr_labels) +
    theme_void() +
    guides(colour = guide_legend(title = paste0("EKR ",ke)), size = guide_legend(title = 'Jaar'))+
    coord_sf(xlim = c(bboxEAG$xmin,bboxEAG$xmax) + c(-250,250),
             ylim = c(bboxEAG$ymin,bboxEAG$ymax) + c(-250,250), datum = NA)+
    # add north
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
    # add scale bar
    ggspatial::annotation_north_arrow(location = "bl",which_north = "true", 
                                      pad_x = unit(0, "in"), pad_y = unit(0.3, "in"),
                                      height = unit(0.5, 'in'), width = unit(0.5, 'in'), 
                                      style = north_arrow_fancy_orienteering) 
  
    ggsave(p, file = paste0('output/ekrstippen/', unique(pl$EAGIDENT),unique(pl$GPHRnew),'.png'), units='cm',dpi=600)
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

clusteranalyse <- function() {
  
  
}
