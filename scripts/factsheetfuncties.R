# helpers-----------------------
importCSV <- function(x, path = y){
  read.csv(file= paste(path, x, sep="/"), header=T, sep=";", dec=".", stringsAsFactors=F, quote="\"", na.strings = "-99")
}

simplifyShapefile <- function(shapefilename=eags, dirGIS, proj4.rd){
  # shapefilename <- "Watervlakken.shp"
  shapefile <- importOGR(shapefilename, dirGIS, proj4.rd)
  shapefile_s <- rmapshaper::ms_simplify(shapefile)
  writeOGR(obj = shapefile_s, dsn = paste(dirGIS, paste(gsub(".shp", "", shapefilename), "_simplified.shp", sep = ""), sep = ""), layer = shapefilename, driver="ESRI Shapefile")
}

waterpereag <- function(water,gEAG){
  packages(rgeos)
  watpereag <- gIntersection(water, gEAG, byid = TRUE, drop_lower_td = TRUE)
}

importOGR <- function(x, dirGIS, CRSobj = proj4.rd){
  proj4.rd <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.87740,4.0725 +units=m +no_defs")
  eags <- readOGR(paste(getwd(),dirGIS, x, sep ='/')) #import
  eags <- spTransform(eags, CRSobj = proj4.rd)
  eags <- spTransform(eags, CRSobj = CRSobj)
  return(eags)
}

check1 <-  function(EST, eag_wl){
  dtEST2 <- merge(EST, eag_wl, by.x = "EAG", by.y  = "GAFIDENT")
  dtEST2[,3:19] <- apply(dtEST2[,3:19], 2, as.numeric)
  dtEST2 <- dtEST2[dtEST2$OWMIDENT %in% ESFoordelen$OWL|dtEST2$EAG %in% ESFoordelen$OWL,]
  
  myimages2 <- NULL
  for(i in unique(dtEST2$EAG)){
    dtEST3 <- dtEST2[dtEST2$EAG == i,]
  ## gather O en W
    dtEST4 <- dtEST3 %>%
      group_by(jaar)%>%
      top_n(1, wt = jaar) %>%
      dplyr::select(-EAG)%>%
      gather(key = "typeEST", value = "waarde", 2:18) %>%
      separate(typeEST, c("locatietype", "nummer"), 1) %>%
      group_by(locatietype) %>%
      top_n(1, wt = waarde) %>%
      top_n(1, wt = nummer) %>%
      arrange(desc(locatietype))
    dtEST4$type2 <- ifelse(dtEST4$watertype == 'M20','DM',ifelse(dtEST4$type %in% c('plas','kleine plas'),'OM', ifelse(dtEST4$type %in% c('sloot'),'Sl','K')))
    dtEST4$stland <- ifelse(dtEST4$StedelijkLandelijk == 'Stedelijk','St','L')
    
    myimages <- paste0(dtEST4$locatietype[1], dtEST4$nummer[1],"_",dtEST4$locatietype[2],
             dtEST4$nummer[2],"_",dtEST4$type2[1],"_",dtEST4$stland[1],".jpg")
    myimages <- as.data.frame(myimages, dtEST4$GAFNAAM[1])
    myimages2 <- rbind(myimages, myimages2)
  }
  
  myimages2$gebied <- row.names(myimages2)
  files  <- list.files("esticon/")
  files  <- as.data.frame(files)
  misttekening <- myimages2[!(myimages2$myimages %in% files$files),]
  #write.table(misttekening, file = paste(getwd(),"/output/tektoevoegen",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  
}

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# functies voor plot P belasting versus kP ---------------------
# helper om voor alle balansdata P vs kP matrix te maken (deze niet in iteratie, maar als input vanuit preprocessing gebruiken)
makePmaps <- function(dat, Overzicht_kp, hybi, nomogram){
  #hier tabel van sven overnemen voor kP
  # Selecteer data en summarise -------------------------------------------
  dat$bodem <- dat$i_bt1
  dat$bodem[is.na(dat$i_bt1)& dat$watertype %in% c('M8','M10','M27','M25')] <- "VEEN"
  dat$bodem[is.na(dat$i_bt1)& dat$watertype %in% c("M3", "M1a","M30","M14","M11","M6a","M7b","M6b","M7a")] <- "KLEI"  
  
  # dat1<- dat[!is.na(dat$KRW),]
  dg <- dat %>%
    # toegepaste filters
    filter(jaar %in% c(2010:2018)) %>%          
    # groeperingsfactor
    group_by(pol, EAG, GAF, KRW, naam, namen, watertype, bodem, 
             a_inlaat1,a_inlaat2,a_inlaat3,a_inlaat4,a_inlaat5,a_uitlaat1,a_uitlaat2,a_uitlaat3,a_uitlaat4) %>% 
    # neem gemiddelde
    summarise_all(mean, na.rm=T)
  
  dg <- dg %>%
    dplyr::select(-starts_with("i_"))%>%
    dplyr::select(-starts_with("b_"))
  
  dg$wp_tot_sum <- dg$wp_min_sum+dg$wp_inc_sum
  
  # gemiddelde waterdiepte per EAG
  mdPtb <- hybi %>%
    filter(jaar %in% c(2010:2017)) %>% 
    filter(fewsparameter %in% 'WATDTE_m') %>%               
    group_by(locatie.EAG) %>% 
    summarise_at(c('meetwaarde'), median, na.rm=T) 
  mdPtb$watdteF <- cut(mdPtb$meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))
  
  # gemiddelde waterdiepte per GAF
  mdPtbG <- hybi %>%
    filter(jaar %in% c(2010:2017)) %>% 
    filter(fewsparameter %in% 'WATDTE_m') %>%               
    group_by(locatie.afaanvoergebied) %>% 
    summarise_at(c('meetwaarde'), median, na.rm=T) 
  mdPtbG$watdteF <- cut(mdPtbG$meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))
  
  mdPtbK <- hybi %>%
    filter(jaar %in% c(2010:2017)) %>% 
    filter(fewsparameter %in% 'WATDTE_m') %>%               
    group_by(locatie.KRWmeetpuntlocatie) %>% 
    summarise_at(c('meetwaarde'), median, na.rm=T) 
  mdPtbG$watdteF <- cut(mdPtbG$meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))
  
  # merge met kP ----------------------------------------------------------
  # koppel waterdiepte per eag en afvoergebied aan water en stoffenbalans
  dgwatdte <- merge(dg[is.na(dg$GAF),], mdPtb, by.x = 'EAG', by.y = 'locatie.EAG', all.x = F)
  dgwatdteG <- merge(dg[is.na(dg$EAG),], mdPtbG, by.x = 'GAF', by.y = 'locatie.afaanvoergebied', all.x = F)
  dgwatdteK <- merge(dg[is.na(dg$EAG) & is.na(dg$GAF),], mdPtbK, by.x = 'KRW', by.y = 'locatie.KRWmeetpuntlocatie', all.x = T)
  dgwatdte <- smartbind(dgwatdte,dgwatdteG,dgwatdteK)  # mis 1 balans
  dgwatdte$watdte <- dgwatdte$meetwaarde; dgwatdte$meetwaarde = NULL
  dgwatdte$watdteF[dgwatdte$watdte > 0.7] <- '(0.5,0.7]'
  nomogram$watdteF <- cut(nomogram$watdte_m, breaks = c('0','0.3','0.5','0.7'))
  
  ditchkP <- NULL
  # i <- unique(nomogram$bodemtype)[1]
  for (i in unique(nomogram$bodemtype)){
    nomogram_bt <- nomogram[nomogram$bodemtype == i,]
    dgwatdte_bt <- dgwatdte[dgwatdte$bodem == toupper(i),]
    dgwatdte_bt <- dgwatdte_bt[!is.na(dgwatdte_bt$bodem),]
    for(j in unique(nomogram_bt$watdteF)){
      nomogram_btdp <- nomogram_bt[nomogram_bt$watdteF == j,]
      dgwatdte_btdp <- dgwatdte_bt[dgwatdte_bt$watdteF == j,]
      dgwatdte_btdp <- dgwatdte_btdp[!is.na(dgwatdte_btdp$watdteF),]
      ditch <- merge(dgwatdte_btdp, nomogram_btdp[,c('debiet..mm.dag.','kP')], by.x = 'w_debiet', by.y = 'debiet..mm.dag.', all = TRUE)
      ditch <- ditch[order(ditch$w_debiet),]
      ditch$kP2 <- na.approx(ditch$kP, method = "linear", rule = 2) # interpolatie kP tussen debieten in nomogram
      ditch$w_debiet <- as.numeric(ditch$w_debiet)
      ditchkP <- rbind(ditch[!is.na(ditch$pol),], ditchkP)
    }
  }
  
  #koppel kritische fosforgrenzen obv waterdiepte, hydraulische belasting en bodemtype aan metamodel PCditch
  PvskP <- merge(dgwatdte, ditchkP[,c('kP2','pol','EAG')], by = c('pol','EAG'), all.x = TRUE, all.y =FALSE); PvskP$kPDitch <- PvskP$kP2; PvskP$kP2 = NULL
  # bereken kP ------------------------------------------------------------
  PvskP$PvskPDitch<- PvskP$wp_min_sum/PvskP$kP # >1 is te veel
  
  # koppel kp plassen obv invoertabel per EAG
  Overzicht_kP_plas <- Overzicht_kP
  
  sel <- dgwatdte$watertype %in% c('M20','M27','M25',"M14")  # selectie van alleen plassen
  PvskPplas <-  merge(dgwatdte[sel & !is.na(dgwatdte$EAG) ,], Overzicht_kP_plas[!is.na(Overzicht_kP_plas$EAG),c('EAG',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.', 
                                                                                                                'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                      by.x = 'EAG', by.y = 'EAG', all.x = F, all.y = T)
  PvskPplas1 <-  merge(dgwatdte[sel& !is.na(dgwatdte$GAF),], Overzicht_kP_plas[!is.na(Overzicht_kP_plas$afvoergebied),c('afvoergebied','EAG',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.', 
                                                                                                                        'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                       by.x = 'GAF', by.y = 'afvoergebied', all.x = F, all.y = T)
  PvskPplas2 <-  merge(dgwatdte[sel,], Overzicht_kP_plas[,c('EAG',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.',
                                                            'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                       by.x = 'KRW', by.y = 'EAG', all.x = F, all.y = T)
  
  pvskp <- smartbind(PvskPplas, PvskPplas1) # PvskPplas2 zit er niet bij 
  
  PvskP <- merge(PvskP, pvskp[,c('pol','EAG','Troebel.naar.helder..mg.P.m2.d.', 'P.load_year..mgP.m2.d.', 
                                 'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')], by = c('pol','EAG'), all = TRUE)
  
  PvskP$wp_min_sum[!is.na(PvskP$P.load_year..mgP.m2.d.)]  <- PvskP$P.load_year..mgP.m2.d.[!is.na(PvskP$P.load_year..mgP.m2.d.)] 
  PvskP$PvskPLake <- PvskP$wp_min_sum/PvskP$Helder.naar.troebel..mg.P.m2.d. # >1 is te veel
  
  PvskP <- PvskP[!is.na(PvskP$wp_min_sum),]
  return(PvskP)
  # write.table(PvskP, file = paste(getwd(),"/pbelasting/output/PvskPditchlakeALL",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  # schrijf data weg voor controle ------------------------------------------------
  # geen p belasting in ouderkerk, gaasperplas, loenderveen en terra nova, boezem toevoegen: toevoegen tabel
  
}
# maak plot van p VS Kp
pvskpplot <- function(pvskpsel){
    
    PvskP <- pvskpsel %>%
      group_by(naam) %>%
      summarise_all(mean)
    
    PvskP <- PvskP[!is.na(PvskP$naam),]
    if(is.na(PvskP$wp_meting_mgm2d)){
      PvskP$wp_meting_mgm2d <- 0 }
    
    colWat1 <-  c("green4","darkorange", "darkred", "red", "red1",  "red2","red3", "brown",  "blue", "black","darkgreen", "grey")  
    colWat2 <- adjustcolor(colWat1, alpha.f = 0.2)
    colWat <- paste(c(colWat2,"yellow", colWat1))
    
    d <- PvskP %>%
      dplyr::select(naam, wp_meting_mgm2d, kPDitch, Helder.naar.troebel..mg.P.m2.d.,
                    starts_with('wp_'),
                    -one_of(c('wp_min_sum', 'wp_tot_sum', 'wp_inc_sum','wp_meting_gm3'))) %>%
      mutate(wp_meting_mgm2d = -wp_meting_mgm2d)
    
    d2 <- gather(d,
                   d %>% dplyr::select( -naam, -kPDitch, -Helder.naar.troebel..mg.P.m2.d.) %>% 
                   names(),
                   key = 'source',
                   value = 'value') 
    
    plot <- ggplot(d2) +
      geom_bar(aes(x = naam, y = value, fill = source), stat = 'identity') +
      geom_point(aes(x = naam, y= kPDitch), shape = 20, size = 6, fill = "red", colour = "red")+
      geom_point(aes(x = naam, y= Helder.naar.troebel..mg.P.m2.d.), shape = 20, size = 6, fill = "salmon",
                 colour = "salmon")+
      xlab('') + ylab('mg P/m2/dag') +
      #ggtitle("Fosfor- en kritische belasting per deelgebied")+
      theme_classic() +
      guides(shape = guide_legend(override.aes = list(size = 4)),
             color = guide_legend(override.aes = list(size = 4))) +
      theme(legend.title = element_blank(), 
            legend.text  = element_text(size = 6),
            legend.key.size = unit(0.9, "lines"),
            legend.position = "right")+
      theme(axis.text.x = element_text(angle = 30, hjust =1))+
      scale_fill_manual(values = colWat)
    #ggsave(plot = plot, file = paste(getwd(),naamplot, format(Sys.time(),"%Y%m%d%H%M"),".png", sep= ""))
    
    
    return(plot)
}

legendplot <- function(PvskP){
  if(nrow(PvskP) > 0) {
    naamplot <- unique(PvskP$KRW)
    PvskP <- PvskP %>%
      group_by(pol) %>%
      summarise_all(mean)
    PvskP <- PvskP[!is.na(PvskP$pol),]
    
    if(is.na(PvskP$wp_meting_mgm2d)){
      PvskP$wp_meting_mgm2d <- 0 }
    
    colWat1 <-  c("green4","darkorange", "darkred", "red", "red1",  "red2","red3", "brown",  "blue", "black","darkgreen", "grey")  
    colWat2 <- adjustcolor(colWat1, alpha.f = 0.2)
    colWat <- paste(c(colWat2,"yellow", colWat1))
    
    d <- PvskP %>%
      dplyr::select(date, pol, wp_meting_mgm2d, kPDitch, Helder.naar.troebel..mg.P.m2.d.,
                    starts_with('wp_'),
                    -one_of(c('wp_min_sum', 'wp_tot_sum', 'wp_inc_sum','wp_meting_gm3'))) %>%
      mutate(wp_meting_mgm2d = -wp_meting_mgm2d)
    
    d2 <- gather(d,
                 d %>% dplyr::select(-date, -pol, -kPDitch, -Helder.naar.troebel..mg.P.m2.d.) %>% names(),
                 key = 'source',
                 value = 'value') 
    
    plot <- ggplot(d2) +
      geom_bar(aes(x = pol, y = value, fill = source), stat = 'identity') +
      geom_point(aes(x = pol, y= kPDitch), shape = 20, size = 6, fill = "red", colour = "red")+
      geom_point(aes(x = pol, y= Helder.naar.troebel..mg.P.m2.d.), shape = 20, size = 6, fill = "salmon",
                 colour = "salmon")+
      xlab('') + ylab('P Belasting [mg P/m2/dag]') +
      theme_classic() +
      theme(legend.title = element_blank(), legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_fill_manual(values = colWat)
    #ggsave(plot = plot, file = paste(getwd(),naamplot, format(Sys.time(),"%Y%m%d%H%M"),".png", sep= ""))
    
    labels <-  d %>% dplyr::select(-date, -pol, -kPDitch, -Helder.naar.troebel..mg.P.m2.d., -starts_with('wp_inc')) %>% names()
    
    legendpl <- plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1);legend("topleft", legend = labels, pch=16, pt.cex=2, cex=1.2, bty='n', col = c(colWat1,'yellow')); mtext("", at=0.2, cex=2)
    
    arragep <- ggarrange(plot, legendpl,
                         ncol = 2, nrow = 1)
    return(legendpl)
    
    #ggsave(plot = plot, file = paste(getwd(),naamplot, format(Sys.time(),"%Y%m%d%H%M"),".png", sep= ""))
  }
}

# functie vor lichtklimaat en waterdiepte-----------
extinctie1 <- function(wq, hybi, parameter = c('VEC', 'WATDTE_m')){
  # diepte4licht <- log(25)/2.25 #extinctie
  # diepte20licht <- log(5)/2.25
  # ext4licht <- log(25)/2.4 #waterdiepte
  # ext20licht <- log(5)/2.4
  # percentagelicht <- 100*exp((diepteVoorjaarGem * vecVoorjaar)) 
  
  hybi2 <- hybi1[hybi1$fewsparameter %in% parameter,]
  wq1<- wq[wq$fewsparameter %in% parameter,]
  wq1 <- wq1[!is.na(wq1$locatie.KRW.watertype) & !is.na(wq1$locatie.EAG) & !wq1$locatie.EAG == '',]
  wq1 <- wq1[wq1$jaar > 2015,]
  wq1 <- wq1[wq1$meetwaarde > 0,]
  medianewd <- as.character(median(hybi2$meetwaarde))
  meanext <- mean(wq1$meetwaarde)
  
  p<- ggplot(wq1, aes(x= locatie.EAG, y= meetwaarde))+
    geom_boxplot() +
    geom_hline(aes(yintercept = 3.22, col = '1 meter'), show.legend = T)+ #vec voor 1 meter >4%
    geom_hline(aes(yintercept = log(25)/median(hybi$meetwaarde), col = paste0(medianewd, ' meter (mediane diepte)')), show.legend = T)+ #vec voor 4 meter >4%
    geom_hline(aes(yintercept = 0.46, col = '7 meter'), show.legend = T)+ #vec+ voor 7 meter 4%
    guides(col=guide_legend(title="4 % licht voor waterplanten op"))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 9, angle=40),
      axis.text.y = element_text(size= 9, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank(),
      axis.title=element_text(size=9) )+
    theme(legend.title = element_text(size = 10, face = 'bold'), 
          legend.text  = element_text(size = 10),
          legend.key.size = unit(0.9, "lines"),
          legend.position = "right")+
    ggtitle('') +
    labs(x= 'Ecologisch analysegebied', y = 'Verticale extinctie')
  p
  
}
waterdieptesloot <- function(hybi, parameter = c('WATDTE_m')){
  # diepte4licht <- log(25)/1.2
  hybi2 <- hybi1[hybi1$fewsparameter %in% parameter,]
  
  p<- ggplot(hybi2, aes(x= locatie.EAG, y= meetwaarde, col = hybi2$locatie.KRW.watertype))+
    geom_boxplot() +
    theme_minimal()+
    guides(col=guide_legend(title="KRW watertype"))+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=40),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle('') +
    labs(x= 'Ecologisch analysegebied', y = 'waterdiepte (m)')
  p
  
}
plotbod <- function(bod1){
  
  # dcast slootbodem 
  selb <- dcast(bod1, loc.eag+loc.code+loc.oms+loc.x+loc.y+loc.z+datum+jaar ~ parm.fews+parm.compartiment, value.var = "meetwaarde", fun.aggregate = mean)
  
  # calculate relevant ratios
  selb[,FESPFWratio := (Fe_mg_l_ng_BS/55.845 - Stot_mg_l_ng_BS/32065)/(Ptot_mgP_l_ng_BS/30.974)]
  selb[,FESPDWratio := (Fe_mg_kg_dg_BS/55.845-Stot_mg_kg_dg_BS/32.065)/(Ptot_gP_kg_dg_BS*1000/30.974)]
  
  # add SP-ratio
  if(is.null(selb$Stot_mg_l_PW & selb$Stot_mg_l_nf_PW)){
  selb[!is.na(SO4_mg_l_PW),FESPPWratio := (Fe_ug_l_nf_PW*0.001/55.845 - SO4_mg_l_PW/96.06)/(Ptot_mgP_l_nf_PW/30.974)]
  }
  if(is.null(selb$Stot_mg_l_nf_PW)){
  selb[!is.na(Stot_mg_l_PW),FESPPWratio := (Fe_ug_l_nf_PW*0.001/55.845 - Stot_mg_l_PW/32.06)/(Ptot_mgP_l_nf_PW/30.974)]
  }
  if(!is.null(selb$Stot_mg_l_nf_PW)){
  selb[!is.na(Stot_mg_l_nf_PW),FESPPWratio := (Fe_ug_l_nf_PW*0.001/55.845 - Stot_mg_l_nf_PW/32.065)/(Ptot_mgP_l_nf_PW/30.974)]
  }
  
  # filter only op samples where FESPFWratio, FESPDWratio and FESPPWratio are present
  selb <- selb[!(is.na(FESPFWratio)|is.na(FESPDWratio)|is.na(FESPPWratio))]
  
  # calculate nalevering
  selb[,nlvrFW := 0.0247 * Ptot_mgP_l_ng_BS - 1.6035]
  selb[,nlvrDW := 0.0077 * Ptot_gP_kg_dg_BS * 1000 - 4.7259]
  selb[,nlvrPW := 0.8095 * Ptot_mgP_l_nf_PW - 0.2905]
  
  # add categories
  selb[,classFESPFWratio := cut(FESPFWratio, breaks = c((min(FESPFWratio)-1), 1.4, 4, max(FESPFWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  selb[,classFESPDWratio := cut(FESPDWratio, breaks = c((min(FESPDWratio)-1), 1.4, 4, max(FESPDWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  selb[,classFESPPWratio := cut(FESPPWratio, breaks = c((min(FESPPWratio)-1), 1.4, 4, max(FESPPWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  
  plotFW <- ggplot(selb, aes(x= reorder(loc.eag, -nlvrFW), y= nlvrFW, fill = classFESPFWratio, group = classFESPFWratio))+
    geom_boxplot() +
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle = 90),
      axis.text.y = element_text(size= 5),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank()
    )+
    scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
    ggtitle( "Potentiele nalevering") +
    labs(x="",y="P mg/m2/dag", fill = '')
  
  if(!is.null(selb$FESPPWratio)){
  qPW <- ggplot(selb, aes(x= reorder(loc.eag, -nlvrPW), y= nlvrPW, fill = classFESPPWratio))+
      geom_boxplot() +
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 5, angle = 90),
        axis.text.y = element_text(size= 5, hjust=2),
        axis.ticks =  element_line(colour = "black"), 
        panel.background = element_blank(), 
        plot.background = element_blank()
      )+
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Actuele nalevering uit de waterbodem obv poriewatermetingen") +
      labs(x="",y="P mg/m2/dag", fill = '')
  }
  if(is.null(selb$FESPPWratio)){plotFW} 
  if(!is.null(selb$FESPPWratio)){grid.arrange(plotFW, qPW)}
}

# functie EKRplot -------------
tabelPerWL3jaargemEAG <- function (EKRset1, doelen = doelen){
  d1 <- dcast(EKRset1[EKRset$jaar > 2006,], HoortBijGeoobject.identificatie+KRWwatertype.code.y+
                Waardebepalingsmethode.code+GHPR+level+jaar ~ .,
              value.var = "Numeriekewaarde", fun.aggregate = mean)
  
  d3 <- d1 %>% 
    dplyr::arrange(HoortBijGeoobject.identificatie,KRWwatertype.code.y,GHPR,level, Waardebepalingsmethode.code,desc(jaar)) %>% 
    dplyr::group_by(HoortBijGeoobject.identificatie,KRWwatertype.code.y,GHPR,level, Waardebepalingsmethode.code) %>% 
    dplyr::top_n(3, wt = jaar) 
  
  d4 = d3 %>%
    dplyr::group_by(HoortBijGeoobject.identificatie,KRWwatertype.code.y,GHPR,level, Waardebepalingsmethode.code) %>% 
    dplyr::summarize_all(mean)
  d4$EKR <- d4$.; d4$. <- NULL
  
  doelgeb <- dcast(doelen, HoortBijGeoobject.identificatie+KRWwatertype.code.y+bronddoel+GHPR ~ ., value.var = "Doel", fun.aggregate = mean)
  doelgeb$GEP <- doelgeb$. ; doelgeb$. <- NULL
  
  tabset <- merge(d4, doelgeb, by.x = c('HoortBijGeoobject.identificatie','KRWwatertype.code.y','GHPR'),
                  by.y = c('HoortBijGeoobject.identificatie','KRWwatertype.code.y','GHPR'))
  tabset$oordeel <- ifelse(tabset$EKR < tabset$GEP/3, 'slecht',
                           ifelse(tabset$EKR < 2*(tabset$GEP/3), 'ontoereikend',
                                  ifelse(tabset$EKR < tabset$GEP, 'matig', 'goed')))
  
  # wat wil ik: set start niet met NL11
  tabset$waterlichaam <- sapply(strsplit(tabset$HoortBijGeoobject.identificatie, '_'), `[`, 2)
  tabset$waterlichaam[is.na(tabset$waterlichaam)] <- paste0('gewogen_',tabset$HoortBijGeoobject.identificatie[is.na(tabset$waterlichaam)])
  tabset$waterlichaam[tabset$waterlichaam == 'OvWa'] <- sapply(strsplit(tabset$HoortBijGeoobject.identificatie[tabset$waterlichaam == 'OvWa'], '_'), `[`, 3)  
  tabset$waterlichaam <- as.character(tabset$waterlichaam)
 
  return(tabset)
  
  #write.table(tabset, file = paste(getwd(),"/EKR3jaarOordeel",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
}
tabelPerWL3jaargemEAG_incl2022 <- function (EKRset, doelen){
  
  d1 <- dcast(EKRset[EKRset$jaar > 2006,], HoortBijGeoobject.identificatie+KRWwatertype.code.y+
                Waardebepalingsmethode.code+GHPR+level+jaar+Doel+bronddoel ~ .,
              value.var = "Numeriekewaarde", fun.aggregate = mean)
  
  d3 <- d1 %>% 
    dplyr::arrange(HoortBijGeoobject.identificatie,KRWwatertype.code.y,GHPR,level, Waardebepalingsmethode.code,desc(jaar)) %>% 
    dplyr::group_by(HoortBijGeoobject.identificatie,KRWwatertype.code.y,GHPR,level, Waardebepalingsmethode.code) %>% 
    dplyr::top_n(3, wt = jaar) 
  
  d4 = d3 %>%
    dplyr::group_by(HoortBijGeoobject.identificatie,KRWwatertype.code.y,GHPR,level, Waardebepalingsmethode.code) %>% 
    dplyr::summarize_all(mean)
  d4$EKR <- d4$.; d4$. <- NULL
  d4$GHPR <- gsub(' $','',d4$GHPR)
  
  doelgeb <- dcast(doelen, HoortBijGeoobject.identificatie+bronddoel+GHPR ~ ., value.var = c("Doel", "Doel_2022"), fun.aggregate = mean)
  # doelgeb <- dcast(doelen, HoortBijGeoobject.identificatie+bronddoel+GHPR ~ ., value.var = "Doel", fun.aggregate = mean)
  # doelgeb <- as.data.frame(doelgeb)
  doelgeb$GEP <- doelgeb$Doel ; doelgeb$Doel <- NULL
  doelgeb$GEP_2022 <- doelgeb$Doel_2022 ; doelgeb$Doel_2022 <- NULL
  
  tabset <- merge(d4, 
                  doelgeb, 
                  by.x = c('HoortBijGeoobject.identificatie', 'GHPR'),
                  by.y = c('HoortBijGeoobject.identificatie', 'GHPR'),
                  all.x = TRUE)
  
  tabset$oordeel <- ifelse(tabset$EKR < tabset$GEP/3, 'slecht',
                           ifelse(tabset$EKR < 2*(tabset$GEP/3), 'ontoereikend',
                                  ifelse(tabset$EKR < tabset$GEP, 'matig', 'goed')))
  tabset$oordeel_2022 <- ifelse(tabset$EKR < tabset$GEP_2022/3, 'slecht',
                                ifelse(tabset$EKR < 2*(tabset$GEP_2022/3), 'ontoereikend',
                                       ifelse(tabset$EKR < tabset$GEP_2022, 'matig', 'goed')))
  
  
  # wat wil ik: set start niet met NL11
  tabset$waterlichaam <- sapply(strsplit(tabset$HoortBijGeoobject.identificatie, '_'), `[`, 2)
  tabset$waterlichaam[is.na(tabset$waterlichaam)] <- paste0('gewogen_',tabset$HoortBijGeoobject.identificatie[is.na(tabset$waterlichaam)])
  tabset$waterlichaam[tabset$waterlichaam == 'OvWa'] <- sapply(strsplit(tabset$HoortBijGeoobject.identificatie[tabset$waterlichaam == 'OvWa'], '_'), `[`, 3)  
  tabset$waterlichaam <- as.character(tabset$waterlichaam)

  return(tabset)
  
  #write.table(tabset, file = paste(getwd(),"/EKR3jaarOordeel",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
}

ekrplot <- function(ekr_scores_sel2){
  ## build background [Kan eleganter..]
  es_sel_background <- ekr_scores_sel2[, c("HoortBijGeoobject.identificatie", "GEP", "GEP_2022", "facet_wrap_code")]
  
  # es_sel_background$GEP_new <- es_sel_background$GEP - 0.05
  
  es_sel_background$goed_ymin_old <- es_sel_background$GEP
  es_sel_background$goed_ymax_old <- Inf
  es_sel_background$matig_ymin_old <- es_sel_background$GEP / 3 * 2
  es_sel_background$matig_ymax_old <- es_sel_background$GEP 
  es_sel_background$ontoereikend_ymin_old <- es_sel_background$GEP / 3
  es_sel_background$ontoereikend_ymax_old <- es_sel_background$GEP / 3 * 2 
  es_sel_background$slecht_ymin_old <- 0
  es_sel_background$slecht_ymax_old <- es_sel_background$GEP / 3
  
  es_sel_background$goed_ymin_new <- es_sel_background$GEP_2022
  es_sel_background$goed_ymax_new <- Inf
  es_sel_background$matig_ymin_new <- es_sel_background$GEP_2022 / 3 * 2
  es_sel_background$matig_ymax_new <- es_sel_background$GEP_2022 
  es_sel_background$ontoereikend_ymin_new <- es_sel_background$GEP_2022 / 3
  es_sel_background$ontoereikend_ymax_new <- es_sel_background$GEP_2022 / 3 * 2 
  es_sel_background$slecht_ymin_new <- 0
  es_sel_background$slecht_ymax_new <- es_sel_background$GEP_2022 / 3
  
  es_sel_background_gather <- gather(es_sel_background, key = "doelen", value = "waarde", goed_ymin_old, goed_ymax_old, matig_ymin_old, matig_ymax_old, ontoereikend_ymin_old, ontoereikend_ymax_old, slecht_ymin_old, slecht_ymax_old,
                                     goed_ymin_new, goed_ymax_new, matig_ymin_new, matig_ymax_new, ontoereikend_ymin_new, ontoereikend_ymax_new, slecht_ymin_new, slecht_ymax_new)
  es_sel_background_gather <- separate(es_sel_background_gather, doelen, into = c("doelen", "type", "sgbp_version"), sep = "_")
  es_sel_background_spr    <- spread(es_sel_background_gather, key = type, value = waarde)
  es_sel_background_spr[es_sel_background_spr$sgbp_version %in% "old", "xmin"] <- 0
  es_sel_background_spr[es_sel_background_spr$sgbp_version %in% "old", "xmax"] <- 0.5
  es_sel_background_spr[es_sel_background_spr$sgbp_version %in% "new", "xmin"] <- 0.5
  es_sel_background_spr[es_sel_background_spr$sgbp_version %in% "new", "xmax"] <- 1
  es_sel_background_spr$sgbp_version[es_sel_background_spr$sgbp_version %in% "new"]<- "SGBP3"
  es_sel_background_spr$sgbp_version[es_sel_background_spr$sgbp_version %in% "old"]<- "SGBP2"
  
  es_sel_background_spr$Oordeel <- as.factor(es_sel_background_spr$doelen)
  
  #Create a custom color scale
  myColors <- c("#00FF00", "#FFFF33", "#FF8000", "#FF0000")
  names(myColors) <- levels(es_sel_background_spr$doelen)
  
  ## make plot
  plot <- ggplot(ekr_scores_sel2, aes(x = HoortBijGeoobject.identificatie, y = EKR)) +
    geom_rect(data = es_sel_background_spr, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                  group = HoortBijGeoobject.identificatie, fill = Oordeel), alpha = 0.3) +
    scale_fill_manual(values = myColors) +
    geom_text(
      aes(x = xmin+0.25, y = 0.9, label = sgbp_version),
      data = es_sel_background_spr, check_overlap = TRUE,size = 3)+ 
    geom_vline(xintercept = 0.5, color = "lightgrey") +
    # geom_point() +
    geom_segment(aes(x = 0, xend = 1, 
                     y = EKR, yend = EKR, linetype = "Huidige toestand"), 
                 col = "black", cex = 1.2) + # linetype = 2 -> dashed
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_linetype_manual("",values= c("Huidige toestand" = 1))+
    facet_grid(cols = vars(facet_wrap_code)) +
    theme_minimal()+ 
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom")
  return(plot)
}

trend <- function(z, detail = "hoofd"){
  #EKRset$jaar <- as.numeric(EKRset$jaar)
  #z <- EKRset[EKRset$jaar > 2005 & EKRset$jaar < 2019, ]
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  
  if(detail == "hoofd"){
    z<- z[z$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),] #alleen hoofdmaatlatten
  }
  
  tabset2 <- dcast(z, HoortBijGeoobject.identificatie+KRWwatertype.code.y+Waardebepalingsmethode.code+GHPR_level ~ jaar, 
                   value.var = "Numeriekewaarde", fun.aggregate = mean)
  tb <- melt(tabset2, variable.name = "jaar", na.rm =TRUE, value.name = "gemEKRscore") # omgekeerde draaitabel voor correct format lm
  tb$jaar <- as.numeric(tb$jaar) # met factor doet lm een regressie voor ieder jaar tov min
  
  fitted_models = tb %>%  # lineaire regressie over jaren per parameter en EAGIDENT
    group_by(HoortBijGeoobject.identificatie, Waardebepalingsmethode.code, KRWwatertype.code.y, GHPR_level) %>% 
    filter(length(unique(jaar)) > 1) %>% 
    do(model = lm(gemEKRscore ~ jaar, data = ., na.action = NULL)) #lineaire regressie EKRscores over jaren per EAGIDENT
  
  COF <- fitted_models %>% tidy(model) 
  R2 <- fitted_models %>% glance(model) 
  trtabset <- merge(COF, tabset2, by =
                      c('HoortBijGeoobject.identificatie','Waardebepalingsmethode.code','KRWwatertype.code.y','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData  <- merge(trtabset, R2, by = c('HoortBijGeoobject.identificatie','Waardebepalingsmethode.code','KRWwatertype.code.y','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData<- gebiedData[gebiedData$term == 'jaar' ,]
  gebiedData$group <- 'grey'# 1 jaar data
  return(gebiedData)
} 
# deze functie werkt alleen als alle 4 de maatlatten significante trend wordt berekend
# trendkrw <- trend(EKRset[EKRset$jaar > 2005 & !EKRset$Waardebepalingsmethode.code %in% c("Maatlatten2012 Vis", "Maatlatten2012 Ov. waterflora"),], detail = "deel") # juist trend per waterlichaam berekenen
# write.table(trendkrw, file = paste(getwd(),"/EAGTrend",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE,               na = "", sep =';', row.names = FALSE) # wegschrijven als csv
