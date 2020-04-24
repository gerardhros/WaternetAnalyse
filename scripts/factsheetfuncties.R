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
  
  sel <- dgwatdte$watertype %in% c('M20','M27','M25',"M14")  
  PvskPplas <-  merge(dgwatdte[sel & !is.na(dgwatdte$EAG) ,], Overzicht_kP_plas[!is.na(Overzicht_kP_plas$gebied),c('gebied',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.', 
                                                                                                                   'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                      by.x = 'EAG', by.y = 'gebied', all.x = F, all.y = T)
  
  PvskPplas1 <-  merge(dgwatdte[sel& !is.na(dgwatdte$GAF),], Overzicht_kP_plas[!is.na(Overzicht_kP_plas$gebied),c('gebied',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.', 
                                                                                                                  'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                       by.x = 'GAF', by.y = 'gebied', all.x = F, all.y = T)
  
  PvskPplas2 <-  merge(dgwatdte[sel,], Overzicht_kP_plas[,c('gebied',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.',
                                                            'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                       by.x = 'KRW', by.y = 'gebied', all.x = F, all.y = T)
  
  pvskp <- smartbind(PvskPplas, PvskPplas1)
  
  PvskP <- merge(PvskP, pvskp[,c('pol','EAG','Troebel.naar.helder..mg.P.m2.d.', 'P.load_year..mgP.m2.d.', 
                                 'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')], by = c('pol',"EAG"), all = TRUE)
  
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
      dplyr::group_by(naam) %>%
      dplyr::summarise_all(mean)
    
    PvskP <- PvskP[!is.na(PvskP$naam),]
    if(is.na(PvskP$wp_meting_mgm2d)){
      PvskP$wp_meting_mgm2d <- 0 }
    
    colWat1 <-  c("green4","darkorange", "darkred", "red", "red1",  "red2","red3", "brown",  "blue", "black","darkgreen", "grey")  
    colWat2 <- adjustcolor(colWat1, alpha.f = 0.2)
    colWat <- paste(c(colWat2,"yellow", colWat1))
    
    d <- PvskP %>%
      dplyr::select(naam, wp_meting_mgm2d, kPDitch, Helder.naar.troebel..mg.P.m2.d.,
                    dplyr::starts_with('wp_'),
                    -dplyr::one_of(c('wp_min_sum', 'wp_tot_sum', 'wp_inc_sum','wp_meting_gm3'))) %>%
      mutate(wp_meting_mgm2d = -wp_meting_mgm2d)
    
    d2 <- tidyr::gather(d,
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

plotEmpty <-function(db,type){
  
  # plot Pwbal
  if(type=='Pwbal'){
    plot <-  ggplot(db) +
      geom_bar(aes(x = GAF, y = pload), stat = 'identity') +
      xlab('') + ylab('mg P/m2/dag')+
      theme_classic() + ylim(0,2) +
      theme(legend.title = element_blank(), 
            legend.text  = element_text(size = 6),
            legend.key.size = unit(0.9, "lines"),
            legend.position = "right")+
      theme(axis.text.x = element_text(angle = 30, hjust =1)) +
      annotate("text", x = nrow(db) * 0.6 , y=1, 
               label = "P-belasting en bronnen\nzijn (nog) niet bekend.",
               hjust = 'middle',size=8,color='blue') +
      theme(axis.text =element_text(colour="black"))
  }
  
  if(type=='plotLichtklimaat'){
    
    plot <-  ggplot(db) +
      geom_bar(aes(x = GAF, y = Lext), stat = 'identity') +
      xlab('') + ylab('Vertical extinctie')+
      theme_classic() + ylim(0,4) +
      theme(legend.title = element_blank(), 
            legend.text  = element_text(size = 6),
            legend.key.size = unit(0.9, "lines"),
            legend.position = "right")+
      theme(axis.text.x = element_text(angle = 30, hjust =1)) +
      annotate("text", x = nrow(db) * 0.6 , y=2, 
               label = "Gegevens over het lichtklimaat\nzijn voor deze EAGs\n(nog) niet bekend.",
               hjust = 'middle',size=6,color='blue') +
      theme(axis.text =element_text(colour="black"))
  }
  
  if(type == 'plotWaterdiepte'){
    
    plot <- ggplot(db, aes(x= GAF, y= wd, col = krwwt))+
      geom_boxplot() +
      theme_minimal()+ scale_y_reverse(limits=c(3.5,0)) + 
      guides(col=guide_legend(title="KRW watertype"))+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle=0,colour = 'black'),
        axis.text.y = element_text(size= 7, hjust=2,colour = 'black'),
        axis.ticks =  element_line(colour = "black"), 
        axis.line = element_line(colour='black'),
        panel.background = element_blank(), 
        plot.background = element_blank() )+ 
      annotate("text", x = nrow(db) * 0.6 , y=1.6, 
               label = "Metingen waterdiepte \nzijn (nog) niet bekend.",
               hjust = 'middle',size=6,color='blue') +
      ggtitle('') +
      labs(x= '', y = 'waterdiepte (m)\n')
  }
    
  if(type=='plotbodFW'){
    
    plot <- ggplot(db, aes(x= GAF, y= plv, fill = ijzerval))+
      geom_boxplot() +
      theme_minimal()+ ylim(-0.5,0.5)+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle = 0,colour = 'black'),
        axis.text.y = element_text(size= 7,colour = 'black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank())+
      annotate("text", x = nrow(db) * 0.6 , y=0, 
               label = "Potentiele nalevering \nis (nog) niet bekend.",
               hjust = 'middle',size=6,color='blue') +
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), 
                        labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Potentiele nalevering") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
    
  if(type=='plotqPW'){
    
    plot <- ggplot(db, aes(x= GAF, y= plv, fill = ijzerval))+
      geom_boxplot() + ylim(-0.5,1.5)+
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle = 0,colour='black'),
        axis.text.y = element_text(size= 7, colour='black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(), 
        plot.background = element_blank())+
      annotate("text", x = nrow(db) * 0.6 , y=0.8, 
               label = "Actuele nalevering \nis (nog) niet bekend.",
               hjust = 'middle',size=6,color='blue') +
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Actuele nalevering uit de waterbodem\nobv poriewatermetingen") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
  
  
  # return plot
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
  
  hybi2 <- hybi[fewsparameter %in% parameter,]
  wq1<- wq[wq$fewsparameter %in% parameter,]
  wq1 <- wq1[!is.na(wq1$locatie.KRW.watertype) & !is.na(wq1$locatie.EAG) & !wq1$locatie.EAG == '',]
  wq1 <- wq1[wq1$jaar > 2015,]
  wq1 <- wq1[wq1$meetwaarde > 0,]
  medianewd <- as.character(median(hybi2$meetwaarde))
  meanext <- mean(wq1$meetwaarde)
  
  p<- ggplot(wq1, aes(x= locatie.EAG, y= meetwaarde))+
    geom_boxplot() +
    geom_hline(aes(yintercept = log(25)/0.5, col = '0.5 meter'), show.legend = T)+
    geom_hline(aes(yintercept = log(25), col = '1 meter'), show.legend = T)+ #vec voor 1 meter >4%
    geom_hline(aes(yintercept = log(25)/median(hybi$meetwaarde), col = paste0(medianewd, ' meter (mediane diepte)')), show.legend = T)+ #vec voor 4 meter >4%
    geom_hline(aes(yintercept = log(25)/7, col = '7 meter'), show.legend = T)+ #vec+ voor 7 meter 4%
    guides(col=guide_legend(title="4 % licht voor waterplanten op"))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 7, angle=0, colour = 'black'),
      axis.text.y = element_text(size= 7, hjust=0, colour = 'black'),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank(),
      axis.title=element_text(size=9),
      axis.line = element_line(colour='black'))+
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
  hybi2 <- hybi[hybi$fewsparameter %in% parameter & hybi$jaar == max(hybi$jaar),]
  
  # remove values that cannot exists (negative depths)
  hybi2[meetwaarde < 0, meetwaarde := NA]
  
  p<- ggplot(hybi2, aes(x= locatie.EAG, y= meetwaarde, col = hybi2$locatie.KRW.watertype))+
    geom_boxplot() +
    theme_minimal()+ scale_y_reverse(limits=c(max(hybi2$meetwaarde)+0.1,0)) + 
    guides(col=guide_legend(title="KRW watertype"))+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 7, angle=0, colour = 'black'),
      axis.text.y = element_text(size= 7, hjust=2, colour = 'black'),
      axis.ticks =  element_line(colour = "black"), 
      axis.line = element_line(colour='black'),
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+ 
    ggtitle('') +
    labs(x= '', y = 'waterdiepte (m)\n')
  p
  
}

plotbod <- function(bod1, type='grid'){
  
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
  
  plotFW <- ggplot(selb, aes(x= loc.eag, y= nlvrFW, fill = classFESPFWratio))+
    geom_boxplot() +
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 7, angle = 0, colour='black'),
      axis.text.y = element_text(size= 7, colour='black'),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      panel.background = element_blank(),
      plot.background = element_blank()
    )+
    scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
    ggtitle( "Potentiele nalevering") +
    labs(x="",y="P mg/m2/dag\n", fill = '')
  
  if(!is.null(selb$FESPPWratio)){
  qPW <- ggplot(selb, aes(x= loc.eag, y= nlvrPW, fill = classFESPPWratio))+
      geom_boxplot() +
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle = 0,colour='black'),
        axis.text.y = element_text(size= 7,colour='black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(), 
        plot.background = element_blank()
      )+
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Actuele nalevering uit de waterbodem\nobv poriewatermetingen") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
  if(is.null(selb$FESPPWratio)){out = plotFW} 
  if(!is.null(selb$FESPPWratio)){out = arrangeGrob(plotFW, qPW)}

  if(type=='plotFW'){out = plotFW}
  if(type=='plotqPW'){out = qPW}
  
  return(grid.draw(out))
  }


# functie EKRplot -------------
tabelPerWL3jaargemEAG <- function (EKRset,eag_wl,doelen){
  
  # make local copy (only within this function)
  doelen1 <- copy(doelen)
  
  # calculate mean per groep
  colgroup <-c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code',
               'Waardebepalingsmethode.code','GHPR_level','GHPR','level','jaar')
  d1 <- EKRset[,.(waarde = mean(Numeriekewaarde, na.rm=TRUE)),by=colgroup]
  
  # rename columns and order data.table
  setnames(d1,colgroup,c('id','EAGIDENT','watertype','wbmethode','GHPR_level','GHPR','level','jaar'))
  setorder(d1,EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode,-jaar)
  
  # add year number (given ordered set), and take only three most recent years
  d1 <- d1[,yearid := seq_len(.N),by=.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode)][yearid < 4]
  
  # calculate mean EKR per group over the three years
  d1 <- d1[,.(EKR = mean(waarde,na.rm=T)),by =.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode)]
  
  # remove empty spaces in GHPR needed for joining later
  d1[,GHPR := gsub(' $','',GHPR)]
  
  # merge with doelen
  
  # rename columns doelen object
  setnames(doelen1,c('HoortBijGeoobject.identificatie'),c('id'))
  
  # mean GEP per object: zou eigenlijk niet moeten als er dubbelen instaan
  doelgeb <- doelen1[,.(GEP = mean(Doel,na.rm=TRUE), GEP_2022 = mean(Doel_2022,na.rm=TRUE)),by =.(id,bronddoel,GHPR)]
  doelgeb2 <- doelgeb
  doelgeb2$id <- sapply(strsplit(doelgeb2$id, '_'), `[`, 2)
  doelgeb <- smartbind(doelgeb,doelgeb2)
  
  # merge with doelen
  # zowel met als zonder NL11_
  d2 <- merge(d1, doelgeb, by = c('id','GHPR'), all.x = TRUE)
  
  # add classification for EKR
  d2[EKR < GEP/3,oordeel := 'slecht']
  d2[EKR >= GEP/3 & EKR < 2 * GEP / 3,oordeel := 'ontoereikend']
  d2[EKR >= 2 * GEP / 3,oordeel := 'matig']
  d2[EKR >= GEP, oordeel := 'goed']
  
  # add type water body
  eag_wl[,waterlichaam := sapply(strsplit(KRWmonitoringslocatie_SGBP3, '_'), `[`, 2)]
  d3 <- merge(d2[!is.na(d2$EAGIDENT),], eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], by.x = c('EAGIDENT'),
              by.y = c('GAFIDENT'), all.x = TRUE)
  eag_wl2 <- dcast(eag_wl, KRW_SGBP3+KRWmonitoringslocatie_SGBP3+SGBP3_NAAM+waterlichaam~., fun.aggregate = mean)
  d4 <- merge(d2[is.na(d2$EAGIDENT),], eag_wl2[,c('KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM','waterlichaam')], by.x = c('id'),
              by.y = c('waterlichaam'), all.x = TRUE)
  d3 <- smartbind(d3,d4)
  
  write.table(d3, file = paste(getwd(),"/output/EKROordeelPerGebied3JaarGem",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  # return the object
  return(d3)
}

tabelPerWL3jaargemEAG_incl2022 <- function (EKRset,eag_wl, doelen){
  
  # make local copy (only within this function)
  d1 <- copy(EKRset)
  
  # calculate mean per groep
  colgroup <-c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code',
               'Waardebepalingsmethode.code','facet_wrap_code','GHPR_level','GHPR','level','jaar','GEP','GEP_2022','waterlichaam','KRW_SGBP3')
  d1 <- d1[jaar > 2008,.(waarde = mean(Numeriekewaarde,na.rm=TRUE)),by=colgroup]
  
  # rename columns and order data.table
  setnames(d1,colgroup,c('id','EAGIDENT','watertype','wbmethode','facet_wrap_code','GHPR_level','GHPR','level','jaar','GEP','GEP_2022','waterlichaam','KRW_SGBP3'))
  setorder(d1,EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode,facet_wrap_code,-jaar, GEP, GEP_2022, waterlichaam, KRW_SGBP3)
  
  # add year number (given ordered set), and take only three most recent years
  d1 <- d1[,yearid := seq_len(.N),by=.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode,facet_wrap_code, GEP, GEP_2022, waterlichaam, KRW_SGBP3)][yearid < 4]
  
  # calculate mean EKR per group over the three years
  d1 <- d1[,.(EKR = mean(waarde,na.rm=T)),by =.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode,facet_wrap_code, GEP, GEP_2022, waterlichaam, KRW_SGBP3)]
  
  # remove empty spaces in GHPR needed for joining later
  # LM: bij mij gaan joins hierdoor juist mis
  d1[,GHPR := gsub(' $','',GHPR)]
  
  # add classification for EKR
  d1[EKR < GEP/3,oordeel := 'slecht']
  d1[EKR >= GEP/3 & EKR < 2 * GEP / 3,oordeel := 'ontoereikend']
  d1[EKR >= 2 * GEP / 3,oordeel := 'matig']
  d1[EKR >= GEP, oordeel := 'goed']
  
  # add classification for EKR
  d1[EKR < GEP_2022/3,oordeel_2022 := 'slecht']
  d1[EKR >= GEP_2022/3 & EKR < 2 * GEP_2022 / 3, oordeel_2022 := 'ontoereikend']
  d1[EKR >= 2 * GEP_2022 / 3,oordeel_2022 := 'matig']
  d1[EKR >= GEP_2022, oordeel_2022 := 'goed']
  
  #write.table(d2, file = paste(getwd(),"./output/EKROordeelPerGebied3JaarGem",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  # return the object
  return(d1)
}

ekrplot <- function(ekr_scores_sel2){
  ## build background [Kan eleganter..]
  es_sel_background <- unique(ekr_scores_sel2[, c("id", "GEP", "GEP_2022", "facet_wrap_code")])
  
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
  es_sel_background$goed_ymax_new <- 1
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
  plot <- ggplot(ekr_scores_sel2, aes(x = id, y = EKR)) +
    geom_rect(data = es_sel_background_spr, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                  group = id, fill = Oordeel), alpha = 0.3) +
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
ekrplot2 <- function(ekr_scores_sel2){
  # facet per maatlat en EAG (als FS niveau is GAF en meerdere EAGs)
  NA -> ekr_scores_sel2$KRW_SGBP3[ekr_scores_sel2$KRW_SGBP3 == ""]
  ekr_scores_sel2$facet_wrap_code <- as.character(ekr_scores_sel2$facet_wrap_code)
  ekr_scores_sel2$wlmt <- ifelse(is.na(ekr_scores_sel2$KRW_SGBP3), paste0(ekr_scores_sel2$EAGIDENT," ",ekr_scores_sel2$facet_wrap_code), ekr_scores_sel2$facet_wrap_code)
  
  ## build background [Kan eleganter..]
  es_sel_background <- unique(ekr_scores_sel2[, c("id", "GEP_2022", "wlmt")])
  
  # es_sel_background$GEP_new <- es_sel_background$GEP - 0.05
  
  es_sel_background$goed_ymin_new <- es_sel_background$GEP_2022
  es_sel_background$goed_ymax_new <- 1
  es_sel_background$matig_ymin_new <- es_sel_background$GEP_2022 / 3 * 2
  es_sel_background$matig_ymax_new <- es_sel_background$GEP_2022 
  es_sel_background$ontoereikend_ymin_new <- es_sel_background$GEP_2022 / 3
  es_sel_background$ontoereikend_ymax_new <- es_sel_background$GEP_2022 / 3 * 2 
  es_sel_background$slecht_ymin_new <- 0
  es_sel_background$slecht_ymax_new <- es_sel_background$GEP_2022 / 3
  
  es_sel_background_gather <- gather(es_sel_background, key = "doelen", value = "waarde", 
                                     goed_ymin_new, goed_ymax_new, matig_ymin_new, matig_ymax_new, ontoereikend_ymin_new, ontoereikend_ymax_new, slecht_ymin_new, slecht_ymax_new)
  es_sel_background_gather <- separate(es_sel_background_gather, doelen, into = c("doelen", "type", "sgbp_version"), sep = "_")
  es_sel_background_spr    <- spread(es_sel_background_gather, key = type, value = waarde)
  es_sel_background_spr$sgbp_version[es_sel_background_spr$sgbp_version %in% "new"] <- "SGBP3"
  
  es_sel_background_spr$Oordeel <- as.factor(es_sel_background_spr$doelen)
  
  #Create a custom color scale
  myColors <- c("#00FF00", "#FFFF33", "#FF8000", "#FF0000")
  names(myColors) <- levels(es_sel_background_spr$doelen)
  
  ## make plot
  plot <- ggplot(ekr_scores_sel2, aes(x = id, y = EKR)) +
    geom_rect(data = es_sel_background_spr, inherit.aes = FALSE,
              aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, 
                  fill = Oordeel), alpha = 0.3) +
    scale_fill_manual(values = myColors) +
    geom_segment(aes(x = 0, xend = 1, 
                     y = EKR, yend = EKR, linetype = "Huidige toestand"), 
                 col = "black", cex = 1.4) + # linetype = 2 -> dashed
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    scale_linetype_manual("",values= c("Huidige toestand" = 1))+
    facet_grid(cols = vars(wlmt)) +
    theme_minimal()+ 
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          strip.text.x = element_text(size = 11), # maatlat
          strip.text.y = element_text(size = 9), #
          axis.text.x = element_blank(), #
          axis.text.y = element_text(size= 9), # ekrscores
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_line(),
          panel.ontop = F,
          legend.title = element_text(size = 11), 
          legend.text  = element_text(size = 10),
          legend.position = "bottom")
  return(plot)
}

plotEKRlijnfs <- function(z){

  z <- z %>%
    arrange(GHPR_level) %>%               # sort your dataframe
    mutate(GHPR = factor(GHPR, unique(GHPR))) # reset your factor-column based on that order
  z$jaar <- as.numeric(z$jaar)
  z$Numeriekewaarde <- as.numeric(z$Numeriekewaarde)
  z$facet_wrap_code <- as.character(z$facet_wrap_code)
  z$wlmt <- ifelse(is.na(z$KRW_SGBP3)|z$KRW_SGBP3 == "", paste0(z$EAGIDENT," ",z$facet_wrap_code), z$facet_wrap_code)
  
  z <- z %>%
    group_by(waterlichaam, wlmt ,GHPR , GHPR_level, level, jaar)%>%
    summarise_at(c('Numeriekewaarde'),mean) 
  
  z <- z %>%
    ungroup(waterlichaam)
  
 ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col = GHPR, group = GHPR))+
    stat_summary(fun.y = "mean", geom = "point") + 
    stat_summary(fun.y = "mean", geom = "line") +
    scale_y_continuous(limits= c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    scale_x_continuous(limits= c(2006, 2020), breaks=c(2006, 2008, 2010, 2012, 2014,2016,2018))+
    facet_grid(vars(level),vars(wlmt))+
    ylab('')+xlab('')+
    guides(col=guide_legend(title=""))+
    ggtitle("", subtitle = z$waterlichaam)+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7), #waardebepmethode
      strip.text.y = element_text(size = 2), #level
      axis.text.x = element_text(size= 5, angle=90), #jaar
      axis.text.y = element_text(size= 5),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank(),
      legend.title = element_text(size = 7), 
      legend.text  = element_text(size = 6),
      legend.key.size = unit(0.9, "lines"),
      legend.position = "right")
}

trend <- function(z, detail = "hoofd"){
  #EKRset$jaar <- as.numeric(EKRset$jaar)
  z <- EKRset[EKRset$jaar > 2005 & EKRset$jaar < 2019, ]
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  setnames(z,c('HoortBijGeoobject.identificatie'),c('id'))
  
  if(detail == "hoofd"){
    z<- z[z$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),] #alleen hoofdmaatlatten
  }
  
  tabset2 <- dcast(z, id+KRWwatertype.code+Waardebepalingsmethode.code+facet_wrap_code+GHPR_level ~ jaar, 
                   value.var = "Numeriekewaarde", fun.aggregate = mean)
  tb <- melt(tabset2, variable.name = "jaar", na.rm =TRUE, value.name = "gemEKRscore") # omgekeerde draaitabel voor correct format lm
  tb$jaar <- as.numeric(tb$jaar) # met factor doet lm een regressie voor ieder jaar tov min
  
  fitted_models = tb %>%  # lineaire regressie over jaren per parameter en EAGIDENT
    group_by(id, Waardebepalingsmethode.code, KRWwatertype.code, GHPR_level) %>% 
    filter(length(unique(jaar)) > 1) %>% 
    do(model = lm(gemEKRscore ~ jaar, data = ., na.action = NULL)) #lineaire regressie EKRscores over jaren per EAGIDENT
  
  COF <- fitted_models %>% tidy(model) 
  R2 <- fitted_models %>% glance(model) 
  trtabset <- merge(COF[,c('id','Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level','estimate',"term")], 
                    tabset2, by = c('id','Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData  <- merge(trtabset, R2, by = c('id','Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData<- gebiedData[gebiedData$term == 'jaar' ,]
  gebiedData$group <- 'grey'# 1 jaar data
  return(gebiedData)
} 

# subset locaties voor KRW selecteren: moet kolom CODE bevatten met locatiecodes
# locKRW <- fread('./atlasKRW/input/NL11_relatieKRWmonitoringslocatiesEnWerkelijkeMeetpunten.csv')
# wqmeanK <- trendfychem(wq, param = c("P","N","NH3","ZICHT","PH","ZVP","T","CL","CHLFa"), locset = locaties)
trendfychem <- function(wq1, param = c("P","N","NH3","ZICHT","PH","ZVP","T","CL","CHLFa"),locset = locKRW){
  # subset locaties voor KRW selecteren: moet kolom CODE bevatten met locatiecodes
  z<- wq1[wq1$locatiecode %in% locset$CODE,]
  z<- z[z$fewsparameter %in% param ,]
  z<- z[grep('*VAST*',z$locatie.meetnethistorie),]
  
  tabset2 <- dcast(z, fewsparametercode+locatie.KRWmeetpuntlocatie ~ jaar, 
                   value.var = "meetwaarde", fun.aggregate = mean)
  tb <- melt(tabset2, variable.name = "jaar", na.rm = TRUE, value.name = "JGM") # omgekeerde draaitabel voor correct format lm
  tb$jaar <- as.numeric(tb$jaar) # met factor doet lm een regressie voor ieder jaar tov min
  
  fitted_models = tb %>%  # lineaire regressie over jaren per parameter en gebied
    group_by(fewsparametercode, locatie.KRWmeetpuntlocatie) %>% 
    filter(length(unique(jaar)) > 1) %>% 
    do(model = lm(JGM ~ jaar, data = ., na.action = NULL)) #lineaire regressie EKRscores over jaren per gebied
  
  COF <- fitted_models %>% tidy(model) 
  R2 <- fitted_models %>% glance(model) 
  trtabset <- merge(COF[,c("locatie.KRWmeetpuntlocatie",'fewsparametercode','estimate',"term")], tabset2, by =
                      c('fewsparametercode','locatie.KRWmeetpuntlocatie')) # data trend samenvoegen met resultaten
  gebiedData  <- merge(trtabset, R2, by = c('fewsparametercode','locatie.KRWmeetpuntlocatie')) # data trend samenvoegen met resultaten
  gebiedData <- gebiedData[!is.na(gebiedData$locatie.KRWmeetpuntlocatie),]
  gebiedData<- gebiedData[gebiedData$term == 'jaar' ,]
  write.table(gebiedData, file = paste(getwd(),"KRWTrendfychem",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE,               na = "", sep =';', row.names = FALSE) # wegschrijven als csv
  return(gebiedData)
}





# deze functie werkt alleen als alle 4 de maatlatten significante trend wordt berekend
# trendkrw <- trendEAG(EKRset[EKRset$jaar > 2005 & !EKRset$Waardebepalingsmethode.code %in% c("Maatlatten2012 Vis", "Maatlatten2012 Ov. waterflora"),], detail = "deel") # juist trend per waterlichaam berekenen
# write.table(trendkrw, file = paste(getwd(),"/EAGTrend",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE) # wegschrijven als csv
