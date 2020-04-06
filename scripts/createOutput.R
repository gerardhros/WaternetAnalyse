# helpers voor app -------------------------
layout_ggplotly <- function(gg, x = -0.08, y = -0.04){
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
  gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
  gg
}

#fractieplot
plotFractiePerMaatlatPerEAG <- function(l){
  l <- l[!is.na(l$CODE),] # geen totaal scores per toetsgeied meenemen
  l<- l[is.na(l$Monster.lokaalID),] # alleen scores per meetCODE per jaar
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  titel = paste(unique(l[ ,c('EAGIDENT','KRWwatertype.code')]),sep="",collapse=" ")
  ggplot(l, aes(x = GHPR_level, fill = klasse)) + 
    geom_bar(position = "fill") + 
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red"), 
                      labels = c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2"))+
    guides(fill=guide_legend(title='EKR score'))+
    coord_flip()+
    facet_grid(jaar~.)+
    ggtitle(titel, subtitle = "Fractie meetlocaties per EKR klasse ") +
    labs(x="",y="") 
} 
plotFractiePerMaatlatPerFacetEAG <- function(l){
  #l <- MCFT
  l<- l[is.na(l$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  titel = paste(unique(l[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code')]),sep="",collapse=" ")
  ggplot(l, aes(x = GHPR_level, fill = klasse)) + 
    geom_bar(position = "fill") + 
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= col, labels = labels,element_text(size = 6))+
    guides(fill=guide_legend(title='EKR score'), element_text(size = 6))+
    coord_flip()+
    facet_grid(jaar~EAGIDENT, space="free", scale="free")+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6, angle = 40), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, hjust=1, angle = 90),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      #panel.border =element_blank(), 
      #panel.grid.major = element_blank(), 
      #panel.grid.minor = element_blank(), 
      #panel.margin = unit(0.20, "lines"), 
      plot.background = element_blank(), 
      plot.margin = unit(c(0.25,0.25, 0.5, 0.5), "lines")
    )+
    #ggtitle("Fractie meetlocaties per EKR klasse ") +
    labs(x="",y="") 
} 
plotFractiePerToetsgebied <- function(l){
  l <- l[!is.na(l$CODE),] # geen totaal scores per toetsgeied meenemen
  l<- l[is.na(l$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  l<- l[l$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),]#alleen hoofdmaatlatten
  titel = paste(unique(l[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code')]),sep="",collapse=" ")
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  #l$Classificatie <- as.factor(l$Classificatie)
  ggplot(l, aes(x = Waardebepalingsmethode.code, fill = klasse)) + 
    geom_bar(position = "fill") +
    #geom_text(stat='count', aes(label=..count..), vjust=3)+
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red"), 
                      labels = c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2"))+
    guides(fill=guide_legend(title='EKR score'))+
    coord_flip()+
    facet_grid(jaar~.)+ 
    ggtitle(titel, subtitle = "Fractie meetlocaties per EKR klasse ") +
    labs(x="",y="") 
} 
plotFractiePerToetsgebied2 <- function(l){
  l <- l[!is.na(l$CODE),] # geen totaal scores per toetsgebied meenemen
  l <- l[is.na(l$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  #l<- l[l$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),]#alleen hoofdmaatlatten
  titel = paste(unique(l[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code')]),sep="",collapse=" ")
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  #l$Classificatie <- as.factor(l$Classificatie)
  ggplot(l, aes(x = GHPR_level, fill = klasse)) + 
    geom_bar(position = "fill") +
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red"), 
                      labels = c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2"))+
    guides(fill=guide_legend(title='EKR score'))+
    coord_flip()+
    facet_grid(jaar~.)+ 
    ggtitle(titel, subtitle = "Fractie meetlocaties per EKR klasse ") +
    labs(x="",y="") 
} 

# lijnplot
plotEKRlijn <- function(z){
  #z<- EKRset1 z2 <-
  z <- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  # z <- z[!is.na(z$CODE),] # geen totaal scores per toetsgeied meenemen
  # wat wil ik: set start niet met NL11
  z$waterlichaam <- sapply(strsplit(z$HoortBijGeoobject.identificatie, '_'), `[`, 2)
  z$waterlichaam[is.na(z$waterlichaam)] <- paste0('gewogen_',z$HoortBijGeoobject.identificatie[is.na(z$waterlichaam)])
  z$waterlichaam[z$waterlichaam == 'OvWa'] <- sapply(strsplit(z$HoortBijGeoobject.identificatie[z$waterlichaam == 'OvWa'], '_'), `[`, 3)  
  z$waterlichaam <- as.character(z$waterlichaam)
  z$GHPR <- with(z,reorder(GHPR, z$level))
  
  z$jaar <- as.numeric(z$jaar)
  
  plot <- ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col= waterlichaam, group = waterlichaam))+
    stat_summary(fun.y = "mean", geom = "point") + 
    stat_summary(fun.y = "mean", geom = "line") +
    scale_y_continuous(limits= c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    facet_wrap(.~ GHPR)+
    ylab('')+xlab('')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90),
      axis.text.y = element_text(size= 5),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )
  ggplotly(plot)
}
plotEKRlijn2 <- function(z){
  z<- EKRset1
  z <- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  #z <- z[!is.na(z$CODE),] # geen totaal scores per toetsgeied meenemen
  # wat wil ik: set start niet met NL11
  z$waterlichaam <- sapply(strsplit(z$HoortBijGeoobject.identificatie, '_'), `[`, 2)
  z$waterlichaam[z$waterlichaam == 'OvWa'] <- sapply(strsplit(z$HoortBijGeoobject.identificatie[z$waterlichaam == 'OvWa'], '_'), `[`, 3)  
  z$waterlichaam <- as.character(z$waterlichaam)
  z$GHPR <- with(z,reorder(GHPR, z$level))
  z$jaar <- as.numeric(z$jaar)
  
  plot <- ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col= GHPR, group = GHPR))+
    stat_summary(fun.y = "mean", geom = "point") + 
    stat_summary(fun.y = "mean", geom = "line") +
    scale_y_continuous(limits= c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    facet_wrap(.~waterlichaam)+
    ylab('gebiedsgemiddelde EKR score')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90, hjust =1, vjust =1),
      axis.text.y = element_text(size= 5, hjust = 1),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank())
  ggplotly(plot)
}

#alleen hoofdmaatlatten per toetgebied
plotEKRlijnToetsgebied_hoofd <- function(z){
  #z<- EKRset[EKRset$EAGIDENT == '2000-EAG-2',]
  #z<- EKRset[EKRset$HoortBijGeoobject.identificatie == "NL11_Sterenzodden",]
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  z<- z[z$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),] #alleen hoofdmaatlatten
  #titel2 = paste(unique(z[ ,c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code')]),sep="",collapse=" ")
  plot <- ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col= Waardebepalingsmethode.code, group = Waardebepalingsmethode.code))+
    stat_summary(fun.y = "mean", geom = "point") + stat_summary(fun.y = "mean", geom = "line") +
    scale_x_continuous(breaks=c(2005,2006, 2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,2016,2017,2018))+
    scale_y_continuous(limits= c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    facet_wrap(.~HoortBijGeoobject.identificatie)+
    theme_minimal()+
    ggtitle("")+ ylab('gemiddelde EKR score')+
    theme (axis.text.x =element_text(angle=90, vjust=1))
  ggplotly(plot)
}
#alleen hoofdmaatlatten per toetgebied
plotEKRlijn2Toetsgebied_hoofd <- function(z){
  #z<- EKRset[EKRset$EAGIDENT == '2000-EAG-2',]
  #z<- EKRset[EKRset$HoortBijGeoobject.identificatie == "NL11_Sterenzodden",]
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  z<- z[z$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),] #alleen hoofdmaatlatten
  # z<- z[z$Grootheid.code %in% c('OVWFLORA'),] 
  z$waterlichaam <- sapply(strsplit(z$HoortBijGeoobject.identificatie, '_'), `[`, 2)
  z$waterlichaam[is.na(z$waterlichaam)] <- paste0('gewogen_',z$HoortBijGeoobject.identificatie[is.na(z$waterlichaam)])
  z$waterlichaam[z$waterlichaam == 'OvWa'] <- sapply(strsplit(z$HoortBijGeoobject.identificatie[z$waterlichaam == 'OvWa'], '_'), `[`, 3)  
  z$waterlichaam <- as.character(z$waterlichaam)
  z$GHPR <- reorder(z$GHPR, z$GHPR_level)
  #titel2 = paste(unique(z[ ,c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code')]),sep="",collapse=" ")
  plot <- ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col= waterlichaam, group = waterlichaam))+
    stat_summary(fun.y = "mean", geom = "point") + stat_summary(fun.y = "mean", geom = "line") +
    scale_x_continuous(breaks=c(2005,2006, 2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,2016,2017,2018))+
    scale_y_continuous(limits= c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    #facet_wrap(.~facet_wrap_code)+
    ggtitle("")+ ylab('')+ xlab('')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90, vjust =1.5, hjust =1),
      axis.text.y = element_text(size= 5),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank())
    
  ggplotly(plot)
  #plot
}

#alleen hoofdmaatlatten per eag
plotEKRlijnEAG_hoofd <- function(z){
  #z<- EKRset[EKRset$EAGIDENT == '2000-EAG-2',]
  #z<- EKRset[EKRset$HoortBijGeoobject.identificatie == "NL11_Sterenzodden",]
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  z<- z[z$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),] #alleen hoofdmaatlatten
  #titel2 = paste(unique(z[ ,c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code')]),sep="",collapse=" ")
  plot <- ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col= Waardebepalingsmethode.code, group = Waardebepalingsmethode.code))+
    stat_summary(fun.y = "mean", geom = "point") + stat_summary(fun.y = "mean", geom = "line") +
    scale_x_continuous(breaks=c(2005,2006, 2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,2016,2017,2018))+
    scale_y_continuous(limits= c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    facet_wrap(.~EAGIDENT)+
    theme_minimal()+
    ggtitle("")+ ylab('gemiddelde EKR score')+
    theme (axis.text.x =element_text(angle=90, vjust=1))
  ggplotly(plot)
}

# overzicht
plotChangeAW <- function(l){
  l<- l[!l$Grootheid.code %in% c("AANTPVLME", "SOORTRDM"),] #alleen hoofdmaatlatten
  l <- dcast(l, EAGIDENT+KRWwatertype.code+jaar+GHPR_level ~ ., 
             value.var = "Numeriekewaarde", fun.aggregate = mean, na.rm =TRUE, drop = TRUE)
  '7' -> l$klasse[l$. < 0.2];  '6' -> l$klasse[l$. >= 0.2 & l$. < 0.4];  '5' -> l$klasse[l$. >= 0.4 & l$. < 0.6]
  '4' -> l$klasse[l$. >= 0.6 & l$. < 0.8];  '3' -> l$klasse[l$. >= 0.8]; '1'-> l$klasse[l$. == -99]
  l$klasse <- as.factor(l$klasse)
  l <- l[!is.na(l$EAGIDENT),] # geen totaal scores per toetsgeied meenemen
  ggplot(l, aes(x = GHPR_level, fill = klasse)) + 
    geom_bar(position = "fill") + 
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red"), 
                      labels = c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2"))+
    guides(fill=guide_legend(title='EKR score'))+
    coord_flip()+
    facet_grid(jaar~.)+
    labs(x="",y="") 
}

# overzicht KRW --------------------------------------------------------
plotEKRlijnToetsgebied <- function(z, detail = "hoofd"){
  # z <- EKRset
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  z<- z[is.na(z$CODE),] # alleen gewogen scores per waterlichaam
  if(detail == "hoofd"){
    z<- z[z$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),] #alleen hoofdmaatlatten
  }
  titel2 = paste(unique(z[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code')]),sep="",collapse=" ")
  p <- plot_ly(z, x=z$jaar, y=z$Numeriekewaarde,
               color = z$GHPR_level, mode = 'lines+markers')
  return(p)
}

## functie om ekr scores in tabellen ------------------------------
# long format hoofdmtlt, eag, waterlichaam
tabelOordeelPerGebiedPerJaar <- function (EKRset, doelen){
  
  b = filter(EKRset, Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS')) %>% # alleen hoofdmaatlatten
    dplyr::select(id = HoortBijGeoobject.identificatie,
                  waterlichaamcode = KRW_SGBP3,
                  waterlichaam,
                  eag = EAGIDENT,
                  eagnaam = GAFNAAM,
                  watertype = KRWwatertype.code,
                  maatlat = GHPR,
                  maatlatversie = Waardebepalingsmethode.code,
                  jaar,
                  EKR = Numeriekewaarde,
                  GEP,
                  GEP_2022)%>%
    group_by(id,waterlichaam,waterlichaamcode,eag,eagnaam,watertype,maatlat,maatlatversie,jaar, GEP, GEP_2022)%>%
    summarise_all(mean)
  b$maatlat <- gsub(' $','',b$maatlat)
  
  #deze kan ook als aparte functie
  tabset$oordeel <- ifelse(b$EKR < b$GEP_2022/3, 'slecht',
                           ifelse(b$EKR < 2*(b$GEP_2022/3), 'ontoereikend',
                                  ifelse(b$EKR < b$GEP_2022, 'matig', 'goed')))
  
  write.table(tabset, file = paste(getwd(),"/output/EKROordeelPerGebiedJaarLong",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  return(tabset)
}

tabelOordeelPerGebiedPerJaarDeel <- function (EKRset, eag_wl, doelen){
  
  b = EKRset %>% 
    dplyr::select(waterlichaam = HoortBijGeoobject.identificatie,
                  waterlichaamcode = GeoObject.code,
                  eag = EAGIDENT,
                  watertype = KRWwatertype.code,
                  maatlat = GHPR,
                  maatlatversie = Waardebepalingsmethode.code,
                  jaar,
                  EKR = Numeriekewaarde)%>%
    group_by(waterlichaam,waterlichaamcode,eag,watertype,maatlat,maatlatversie,jaar)%>%
    summarise_all(mean)
  b$maatlat <- gsub(' $','',b$maatlat)
  
  doelgeb <- dcast(doelen, HoortBijGeoobject.identificatie+bronddoel+GHPR ~ ., value.var = "Doel", fun.aggregate = mean)
  doelgeb$GEP <- doelgeb$. ; doelgeb$. <- NULL
  tabset <- merge(b, doelgeb, by.x = c('waterlichaam','maatlat'),
                  by.y = c('HoortBijGeoobject.identificatie', 'GHPR'), all.x = T)
  tabset$oordeel <- ifelse(tabset$EKR < tabset$GEP/3, 'slecht',
                           ifelse(tabset$EKR < 2*(tabset$GEP/3), 'ontoereikend',
                                  ifelse(tabset$EKR < tabset$GEP, 'matig', 'goed')))
  
  # add type water body
  eag_wl[,waterlichaam := sapply(strsplit(KRWmonitoringslocatie_SGBP3, '_'), `[`, 2)]
  tabset1 <- merge(tabset[!is.na(tabset$eag),], eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], by.x = c('eag'),
                   by.y = c('GAFIDENT'), all.x = TRUE)
  
  eag_wl2 <- dcast(eag_wl, KRW_SGBP3+KRWmonitoringslocatie_SGBP3+SGBP3_NAAM+waterlichaam~., fun.aggregate = mean)
  tabset2 <- merge(tabset[is.na(tabset$eag),], eag_wl2[,c('KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM','waterlichaam')], by.x = c('waterlichaam'),
                   by.y = c('waterlichaam'), all.x = TRUE, all.y =F)
  tabset <- smartbind(tabset1,tabset2)
  
  write.table(tabset, file = paste(getwd(),"/output/EKROordeelPerGebiedJaarLongDeelMtlt",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  return(tabset)
}
# long format hoofdmtlt, eag, waterlichaam, locatie
tabelOordeelPerMeetlocatiePerJaar <- function (EKRset, doelen){
  
  b = filter(EKRset, Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS')) %>% # alleen hoofdmaatlatten
    dplyr::select(waterlichaam = HoortBijGeoobject.identificatie,
                  waterlichaamcode = GeoObject.code,
                  locatie = CODE,
                  eag = EAGIDENT,
                  watertype = KRWwatertype.code,
                  maatlat = GHPR,
                  maatlatversie = Waardebepalingsmethode.code,
                  jaar,
                  EKR = Numeriekewaarde)
  b$maatlat <- gsub(' $','',b$maatlat)
  
  doelgeb <- dcast(doelen, HoortBijGeoobject.identificatie+bronddoel+GHPR ~ ., value.var = "Doel", fun.aggregate = mean)
  doelgeb$GEP <- doelgeb$. ; doelgeb$. <- NULL
  tabset <- merge(b, doelgeb, by.x = c('waterlichaam','maatlat'),
                  by.y = c('HoortBijGeoobject.identificatie', 'GHPR'), all.x = T)
  tabset$oordeel <- ifelse(tabset$EKR < tabset$GEP/3, 'slecht',
                           ifelse(tabset$EKR < 2*(tabset$GEP/3), 'ontoereikend',
                                  ifelse(tabset$EKR < tabset$GEP, 'matig', 'goed')))
  
  # add type water body
  eag_wl[,waterlichaam := sapply(strsplit(KRWmonitoringslocatie_SGBP3, '_'), `[`, 2)]
  tabset1 <- merge(tabset[!is.na(tabset$eag),], eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], by.x = c('eag'),
                   by.y = c('GAFIDENT'), all.x = TRUE)
  eag_wl2 <- dcast(eag_wl, KRW_SGBP3+KRWmonitoringslocatie_SGBP3+SGBP3_NAAM+waterlichaam~., fun.aggregate = mean)
  tabset2 <- merge(tabset[is.na(tabset$eag),], eag_wl2[,c('KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM','waterlichaam')], by.x = c('waterlichaam'),
                   by.y = c('waterlichaam'), all.x = TRUE, all.y =F)
  tabset <- smartbind(tabset1,tabset2)
  
  write.table(tabset, file = paste(getwd(),"/output/EKROordeelPerLocatieJaarLong",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  return(tabset)
}
# wide format hoofdmtlt, eag, waterlichaam
tabelEKRPerWLEnEAGPerJaar <- function (EKRset, doelen){
  b = filter(EKRset, Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS')) %>% # alleen hoofdmaatlatten
    dplyr::select(waterlichaam = HoortBijGeoobject.identificatie,
           eag = EAGIDENT,
           watertype = KRWwatertype.code,
           maatlat = GHPR,
           maatlatversie = Waardebepalingsmethode.code,
           jaar,
           EKR = Numeriekewaarde)
  b$maatlat <- gsub(' $','',b$maatlat)
  tabset <- dcast(b, waterlichaam+eag+watertype+maatlat+maatlatversie ~ jaar, value.var = "EKR", fun.aggregate = mean)
  doelgeb <- dcast(doelen, HoortBijGeoobject.identificatie+bronddoel+GHPR ~ ., value.var = "Doel", fun.aggregate = mean)
  doelgeb$GEP <- doelgeb$. ; doelgeb$. <- NULL
  tabset <- merge(tabset, doelgeb, by.x = c('waterlichaam','maatlat'),
                  by.y = c('HoortBijGeoobject.identificatie', 'GHPR'))
  
  # add type water body
  eag_wl[,waterlichaam := sapply(strsplit(KRWmonitoringslocatie_SGBP3, '_'), `[`, 2)]
  tabset1 <- merge(tabset[!is.na(tabset$eag),], eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], by.x = c('eag'),
                   by.y = c('GAFIDENT'), all.x = TRUE)
  eag_wl2 <- dcast(eag_wl, KRW_SGBP3+KRWmonitoringslocatie_SGBP3+SGBP3_NAAM+waterlichaam~., fun.aggregate = mean)
  tabset2 <- merge(tabset[is.na(tabset$eag),], eag_wl2[,c('KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM','waterlichaam')], by.x = c('waterlichaam'),
                   by.y = c('waterlichaam'), all.x = TRUE, all.y =F)
  tabset <- smartbind(tabset1,tabset2)
  write.table(tabset, file = paste(getwd(),"/output/EKROordeelPerGebiedJaarWide",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
}
# deelmtlt, eag, waterlichaam
tabelPerWLEnEAGPerJaarDeel <- function (EKRset, doelen, eag_wl){
  b = filter(EKRset) %>% 
    dplyr::select(waterlichaam = HoortBijGeoobject.identificatie,
           eag = EAGIDENT,
           watertype = KRWwatertype.code,
           maatlat = GHPR,
           maatlatsort = GHPR_level,
           maatlatversie = Waardebepalingsmethode.code,
           jaar,
           EKR = Numeriekewaarde)
  b$maatlat <- gsub(' $','',b$maatlat)
  tabset <- dcast(b, waterlichaam+eag+watertype+maatlat+maatlatsort+maatlatversie ~ jaar, value.var = "EKR", fun.aggregate = mean)
 
  # mean GEP per object
  doelgeb <- doelen[,.(GEP = mean(Doel,na.rm=TRUE)),by =.(HoortBijGeoobject.identificatie,bronddoel,GHPR)]
  
  # merge with doelen
  tabset <- merge(tabset, doelgeb, by.x = c('waterlichaam','maatlat'),
                  by.y = c('HoortBijGeoobject.identificatie', 'GHPR'), all.x = TRUE)
  
  # add type water body
  eag_wl[,waterlichaam := sapply(strsplit(KRWmonitoringslocatie_SGBP3, '_'), `[`, 2)]
  tabset1 <- merge(tabset[!is.na(tabset$eag),], eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], by.x = c('eag'),
                   by.y = c('GAFIDENT'), all.x = TRUE)
  eag_wl2 <- dcast(eag_wl, KRW_SGBP3+KRWmonitoringslocatie_SGBP3+SGBP3_NAAM+waterlichaam~., fun.aggregate = mean)
  tabset2 <- merge(tabset[is.na(tabset$eag),], eag_wl2[,c('KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM','waterlichaam')], by.x = c('waterlichaam'),
                   by.y = c('waterlichaam'), all.x = TRUE, all.y =F)
  tabset <- smartbind(tabset1,tabset2)
  
  write.table(tabset, file = paste(getwd(),"/output/EKROordeelPerGebiedJaarWideDeelmtlt",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  return(tabset)
  }

# taxa
draaitaxa <- function (EKRlijst1){
  selectedData3 <- EKRlijst1[EKRlijst1$Analysecompartiment.code %in% 'OW',
                          c('CODE','HoortBijGeoobject.identificatie','EAGIDENT','Biotaxon.naam',
                            'Biotaxon.naam.nl', 'KRWwatertype.code','Grootheid.code','jaar',
                            'Numeriekewaarde', 'Waardebepalingsmethode.code')]

  selectedData2 <- selectedData3[selectedData3$Grootheid.code %in% c('AANWZHD','BEDKG','SOORTRDM') & !is.na(selectedData3$Biotaxon.naam),]
  
  '1_somScorePlanten' -> selectedData2$Biotaxon.naam[selectedData2$Biotaxon.naam == 'Macrofyten' & selectedData2$Grootheid.code == 'AANWZHD']
  '2_gemiddeldeSoortenrijkdomPlanten' -> selectedData2$Biotaxon.naam[selectedData2$Biotaxon.naam == 'Macrofyten' & selectedData2$Grootheid.code == 'SOORTRDM']
  
  rpivotTable(
    selectedData2,
    rows = c("Grootheid.code","Biotaxon.naam", "Waardebepalingsmethode.code"),
    cols = c("jaar"),
    aggregatorName = "Average",
    inclusions = list(HoortBijGeoobject.identificatie = list("NL11_Botshol")),
    exclusions= list( Grootheid.code = list( "BEDKG")),
    vals = "Numeriekewaarde",
    rendererName = "Heatmap"
)
}

vispivot <- function (EKRlijst){
  
  selKol <- c("Numeriekewaarde","Biotaxon.naam.nl","Identificatie","Begindatum","Resultaatdatum","HoortBijGeoObjectCode","Eenheid.code")
  
  sel <- is.na(EKRlijst$HoortBijGeoObjectCode)
  sel <- sel & !is.na(EKRlijst$Biotaxon.naam.nl)#als geen nederlandse naam dan gaat aggregate fout. En ook niet relevant.
  sel <- sel & EKRlijst$Eenheid.code %in% "kg/ha"
  EKRlijst1 <- EKRlijst[sel,]
  EKRlijst1$toetsjaar <- EKRlijst1$Waardebepalingsmethode.code
  
  if(nrow(EKRlijst1)>0){
  xagg <- aggregate(Numeriekewaarde ~ Biotaxon.naam.nl+jaar+Identificatie+toetsjaar,EKRlijst1,FUN=sum)
  
  
  #sel <- xagg$Biotaxon.naam.nl%in%c("Giebel")#eruit halen, want deze komt alleen voor in toetsjaar 2019 en daardoor wordt kleuring van de staafjes anders per soort.
  xagg <- xagg[!sel,] 
  
  #rpivotTable(xagg[xagg$toetsjaar%in%2018,],rows=c("Meetobject.lokaalID"),cols="Biotaxon.naam.nl",vals="Numeriekewaarde",aggregatorName= "Sum",rendererName="Horizontal Stacked Bar Chart", width="100%", height="4000px")
  rpivotTable(
    xagg,
    rows=c("Identificatie","jaar","toetsjaar"),
    cols="Biotaxon.naam.nl",
    aggregatorName = "Sum",
    inclusions = list(HoortBijGeoobject.identificatie = list("NL11_Botshol")),
    exclusions= list( Grootheid.code = list( "BEDKG")),
    vals = "Numeriekewaarde",
    rendererName = "Horizontal Stacked Bar Chart",
    width="100%"
  )
}
}

# geo ------------------------------------------------
geoplot3 <- function(l, wsa){
  #l <- mcft2
  titel = paste(unique(l[ ,c("GHPR")]),sep="",collapse=" ")
  # titel = paste(unique(l[ ,c("Waardebepalingsmethode.code",'HoortBijGeoobject.identificatie','KRWwatertype.code', 'GHPR')]),sep="",collapse=" ")
  subtitel = paste(unique(l[ ,c("Waardebepalingsmethode.code")]),sep="",collapse="")
  subtitel = sapply(strsplit(as.character(subtitel), ' '), `[`, 1)
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  # coord naar projectie osm omzetten
  l <- l[!is.na(l$XCOORD),]
  l <- spTransform(SpatialPointsDataFrame(coords=l[,c("XCOORD", "YCOORD")], data=l, proj4string=proj4.rd), CRSobj=proj4.google)
  l <- as.data.frame(l)
  l$jaar <- as.integer(l$jaar)
  
  if(nrow(l)>0){
    gebieden <- unique(l$EAGIDENT) # selectie te plotten eags, bbox
    gebied <- gEAG[gEAG$GAFIDENT %in% c(gebieden),]
    grens <- bbox(gebied)
    map <- get_map(location = grens, maptype = "toner-background", source = "stamen", color = 'bw',force = FALSE)
    
    if(nrow(map)>0){
      p <-ggmap(map)+
        geom_point(data=l,aes(x=XCOORD.1,y=YCOORD.1,colour=klasse), size = 6)+
        guides(colour=guide_legend(title='EKR score'), size=guide_legend(title='EKR score'))+
        scale_colour_manual('klasse',values=as.character(col),labels=labels,drop=FALSE)+
        #facet_wrap(~jaar)+
        theme_bw()+
        theme(title =element_text(size=10), strip.text.y = element_text(size = 7, angle = 0), 
              strip.text.x = element_text(angle = 0), axis.text.x=element_blank(), axis.text.y=element_blank()) +
        labs(x="", y="")+ 
        ggtitle(titel, 
                subtitle= subtitel)
      
      anim <- p + transition_time(jaar) +
          labs(title = "Jaar: {frame_time}")+
          ease_aes('linear')
      gganimate::animate(anim, nframes = 24, fps = 0.5, renderer = gifski_renderer(paste0(wsa,"mafy.gif")))
        
     }
  }
}

geoplot <- function(lijst){
  l <- as.data.frame(do.call('rbind', lijst)) # dataframe voor ggplot
  titel = paste(unique(l[ ,c('HoortBijGeoobject.identificatie',"GHPR")]),sep="",collapse=" ")
  # titel = paste(unique(l[ ,c("Waardebepalingsmethode.code",'HoortBijGeoobject.identificatie','KRWwatertype.code', 'GHPR')]),sep="",collapse=" ")
  subtitel = paste(unique(l[ ,c("Waardebepalingsmethode.code")]),sep="",collapse="")
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  # coord naar projectie osm omzetten
  l <- spTransform(SpatialPointsDataFrame(coords=l[,c("XCOORD", "YCOORD")], data=l, proj4string=proj4.rd), CRSobj=proj4.google)
  l <- as.data.frame(l)
  l$jaar <- as.integer(l$jaar)
  
  if(nrow(l)>0){
  gebieden <- unique(l$EAGIDENT) # selectie te plotten eags, bbox
  gebied <- gEAG[gEAG$GAFIDENT %in% c(gebieden),]
  grens <- gebied@bbox
  map <- get_map(location = grens, maptype = "toner", source = "stamen", color = 'bw',force = FALSE)
  
  if(nrow(map)>0){
  p <-ggmap(map)+
    geom_point(data=l,aes(x=XCOORD.1,y=YCOORD.1,colour=klasse), size = 6)+
    guides(colour=guide_legend(title='EKR score'), size=guide_legend(title='EKR score'))+
    scale_colour_manual('klasse',values=as.character(col),labels=labels,drop=FALSE)+
  facet_wrap(~jaar)+
    theme_tufte(14,"Avenir")+
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y=element_blank())+
    ggtitle(titel, 
            subtitle= subtitel)+
    labs(x="", y="")
  ggsave(plot = p, file = paste(getwd(),"/krtTtsgbdJr",titel, subtitel, format(Sys.time(),"%Y%m%d%H%M"),".png", sep= ""))
  
  anim <- p + transition_time(jaar) +
    labs(title = "Jaar: {frame_time}")+
    ease_aes('linear')
  animate(anim, nframes = 24, fps = 3, renderer = gifski_renderer(paste(getwd(),"/gifkrtTtsgbd",titel, subtitel, format(Sys.time(),"%Y%m%d%H%M"),".gif", sep= "")))
  }
}
}
gGeofiguurPerindicatorPerToetsgebiedFacetJaar <- function (EKRset){
  b = EKRset[!EKRset$Grootheid.code %in% c("AANTPVLME", "SOORTRDM") & !(is.na(EKRset$XCOORD)) 
             & !is.na(EKRset$CODE) & 
               !(is.na(EKRset$YCOORD)) & !(is.na(EKRset$Waardebepalingsmethode.code)) & 
               !(is.na(EKRset$GHPR)) & !(is.na(EKRset$KRWwatertype.code))& 
               !(is.na(EKRset$HoortBijGeoobject.identificatie)),]
  #b <- b[grep('*NL11_*',b$HoortBijGeoobject.identificatie),]
  '7' -> b$klasse[b$Numeriekewaarde < 0.2]
  '6' -> b$klasse[b$Numeriekewaarde >= 0.2 & b$Numeriekewaarde < 0.4]
  '5' -> b$klasse[b$Numeriekewaarde >= 0.4 & b$Numeriekewaarde < 0.6]
  '4' -> b$klasse[b$Numeriekewaarde >= 0.6 & b$Numeriekewaarde < 0.8]
  '3' -> b$klasse[b$Numeriekewaarde >= 0.8 & b$Numeriekewaarde <= 1.0]
  # b$klasse <- as.factor(b$klasse)
  b = b[!(is.na(b$klasse)),]
  a = split(b,list(b$GHPR, b$KRWwatertype.code, b$HoortBijGeoobject.identificatie, b$Waardebepalingsmethode.code),drop=TRUE)
  lapply(1:length(a), function(i) geoplot(a[i]))
  print(i)
}
geoplot2 <- function(lijst){
  l <- as.data.frame(do.call('rbind', lijst)) # dataframe voor ggplot
  titel = paste(unique(l[ ,c('GAFIDENT','KRWwatertype.code',"GHPR",'jaar')]),sep="",collapse=" ")
  subtitel = unique(l$GHPR_level)
  l <- spTransform(SpatialPointsDataFrame(coords=l[,c("XCOORD", "YCOORD")], data=l, proj4string=proj4.rd), CRSobj=proj4.google)
  l$klasse <- as.factor(l$klasse)
  pal <- colorFactor(palette = col,  domain = EKRset$klasse)
  
  gebieden <- unique(l$gebiedData) # selectie te plotten eags, bbox
  gebied <- gEAG[gEAG$GAFIDENT %in% c(gebieden),]
  #grens <- gebied@bbox
 
  m <- leaflet() %>%
          # addProviderTiles(providers$OpenStreetMap,) %>% 
          addTiles("http://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png") %>%
          # addPolygons(data = gebied, layerId = gebied$GAFIDENT,
          #             stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
          #             fill=F, fillOpacity = 0.6)%>%
          addCircleMarkers(data=l, layerId=l$CODE, popup= paste("EAG naam", l$gebiednaam, "<br>", 
                                                                                       "CODE: ", l$CODE,"<br>", 
                                                                                       "EKR:", l$Numeriekewaarde, "<br>",
                                                                                       "jaar:", l$jaar),
                           weight = 3, fill=T, fillOpacity = 0.5, fillColor = ~pal(l$klasse)) %>%
          addLegend("bottomright", colors=col, labels=labels, title = titel)
  mapshot(m, file = paste0("kaart",titel,".png")) 
} 
gGeofiguurPerIndicatorPerAfvoergebiedFacetJaar <- function (EKRset){ 
  b = EKRset[!EKRset$Grootheid.code %in% c("AANTPVLME", "SOORTRDM")
             & !(is.na(EKRset$XCOORD)) & !is.na(EKRset$EAGIDENT) & !is.na(EKRset$CODE) & 
               !(is.na(EKRset$YCOORD)) & !(is.na(EKRset$Waardebepalingsmethode.code)) & 
               !(is.na(EKRset$GHPR)) & !(is.na(EKRset$GAFIDENT)) & !(is.na(EKRset$KRWwatertype.code))& 
               !(is.na(EKRset$HoortBijGeoobject.identificatie)),]
 # b = b[b$GAFIDENT %in% c('2503'),]
  b = b[b$Waardebepalingsmethode.code == "Maatlatten2012 Ov. waterflora",]
  '7' -> b$klasse[b$Numeriekewaarde >= 0.0 & b$Numeriekewaarde < 0.2]
  '6' -> b$klasse[b$Numeriekewaarde >= 0.2 & b$Numeriekewaarde < 0.4]
  '5' -> b$klasse[b$Numeriekewaarde >= 0.4 & b$Numeriekewaarde < 0.6]
  '4' -> b$klasse[b$Numeriekewaarde >= 0.6 & b$Numeriekewaarde < 0.8]
  '3' -> b$klasse[b$Numeriekewaarde >= 0.8 & b$Numeriekewaarde <= 1.000000000000]
  b = b[!(is.na(b$klasse)),]; b$klasse <- as.character(b$klasse)
  a = split(b,list(b$GHPR, b$KRWwatertype.code, b$GAFIDENT, b$jaar),drop=TRUE)
  lapply(1:length(a), function(i) geoplot2(a[i]))
}

# kaart ekr per mp ---------------
ekrmap <- function(EKRset2, maatlat = "2V1 Overige waterflora"){
  #EKRset[EKRset$Waardebepalingsmethode.code == "Maatlatten2012 Vis",], maatlat = "4VI1 Vis-kwaliteit"
  # gebiedData <- krwchem[!(is.na(krwchem$XCOORD)) & !(is.na(krwchem$YCOORD))
  #                    & !is.na(krwchem$CODE),]
  gebiedData <- EKRset2[!(is.na(EKRset2$XCOORD)) & !(is.na(EKRset2$YCOORD))
                       & !is.na(EKRset2$CODE),]
  gebiedData <- gebiedData[gebiedData$GHPR_level == maatlat,]
  #gebiedData <- gebiedData[gebiedData$jaar > 2015,]
  #gebiedData$EKR <- gebiedData$.
  #gebiedData$EKR <- gebiedData$Numeriekewaarde
  #gebiedData <- gebiedData[gebiedData$Waardebepalingsmethode.code == "Maatlatten2012 Ov. waterflora",]
  gebiedData <- gebiedData[!(is.na(gebiedData$XCOORD)),]
  
  if(nrow(gebiedData)>1){
  
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=gebiedData[,c("XCOORD","YCOORD")],
                                       data=gebiedData, proj4string=proj4.rd),
                CRSobj=proj4.google)
  
  gebiedData <-as.data.frame(gebiedData)
  
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  gebiedData <- gebiedData[order(gebiedData$jaar),]
  x <- as.character(sort(unique(gebiedData$jaar)))
  x <- paste0(x,' ', collapse = '')
  titel1 <- paste0(x)

  leaflet() %>% 
    addCircles(data = gebiedData, ~XCOORD.1, ~YCOORD.1, popup = paste("CODE", as.character(gebiedData$CODE), "<br>",
                                                                      "EKR score:", as.character(gebiedData$Numeriekewaarde), "<br>",
                                                                      "Meetjaar:", as.character(gebiedData$jaar)),
               weight = 3, radius=40, color= ~pal(klasse), fillOpacity = 0.8) %>%
    addLegend("bottomright", colors=col, labels=labels, title = titel1) %>%
    addTiles() 
  }
}

# ekr per eag
ekrmap2 <- function(EKRset, maatlat = "2V1 Overige waterflora", col=col, labels=labels){
    gebiedData <- EKRset[EKRset$GHPR_level == maatlat,]
    gebiedData <- gebiedData[!is.na(gebiedData$EAGIDENT),]
    
    gebiedData1 <- gebiedData[!(is.na(gebiedData$XCOORD)) & !(is.na(gebiedData$YCOORD))
                              & !is.na(gebiedData$CODE),]
    gebiedData1 <-
      spTransform(SpatialPointsDataFrame(coords=gebiedData1[,c("XCOORD","YCOORD")],
                                         data=gebiedData1, proj4string=proj4.rd),
                  CRSobj=proj4.google, match.ID = FALSE)
    gebiedData1 <-as.data.frame(gebiedData1)
    
    
    gebiedData <- dcast(gebiedData, EAGIDENT+KRWwatertype.code+
                          Waardebepalingsmethode.code+GHPR_level+jaar+Doel+bronddoel ~ .,
                        value.var = "Numeriekewaarde", fun.aggregate = mean)
    gebiedData$EKR <- gebiedData$.
    '7' -> gebiedData$klasse[gebiedData$EKR < 0.2]
    '6' -> gebiedData$klasse[gebiedData$EKR >= 0.2 & gebiedData$EKR < 0.4]
    '5' -> gebiedData$klasse[gebiedData$EKR >= 0.4 & gebiedData$EKR < 0.6]
    '4' -> gebiedData$klasse[gebiedData$EKR >= 0.6 & gebiedData$EKR < 0.8]
    '3' -> gebiedData$klasse[gebiedData$EKR >= 0.8]
    gebiedData$klasse <- as.factor(gebiedData$klasse)
    gebiedData$klasse = factor(gebiedData$klasse, levels = c("3", "4", "5", "6","7"))
    gebiedData <- gebiedData[!is.na(gebiedData$klasse)& !is.na(gebiedData$EKR) & !is.na(gebiedData$jaar),]    
    
    gebiedData1$klasse = factor(gebiedData1$klasse, levels = c("3", "4", "5", "6","7"))
    pal1 <- colorFactor(palette = col,  domain = gebiedData1$klasse)
    x <- as.character(sort(unique(gebiedData$jaar)))
    x <- paste0(x,' ', collapse = '')
    titel1 <- paste0("EKR scores in ",x)
    pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
    
    map <- sp::merge(gEAG, gebiedData[, c('.','klasse','jaar','EAGIDENT','Doel','bronddoel', 'KRWwatertype.code',
                                          'GHPR_level')], by.x = 'GAFIDENT', by.y =
                       'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
    map <- map[order(map$jaar),]
    
    leaflet() %>%
      
      addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                      "EKR score:", map$., "<br>",
                                                      "EKR klasse:", map$klasse, "<br>",
                                                      "Meetjaar:", map$jaar, "<br>",
                                                      "Doel:", map$Doel),
                  stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                  fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
      addCircles(data = gebiedData1, ~XCOORD.1, ~YCOORD.1, popup = paste("CODE", as.character(gebiedData1$CODE), "<br>",
                                                                       "EKR score:", as.character(gebiedData1$Numeriekewaarde), "<br>",
                                                                       "Meetjaar:", as.character(gebiedData1$jaar)),
                 weight = 3, radius=40, color= ~pal1(klasse), fillOpacity = 0.8) %>%
      addLegend("bottomright", colors=col, labels=labels, title = titel1) %>%
      addTiles()
}
# 3 jaar per eag
ekrmap3jrEAGOvWa <- function(tabset, maatlat = "2V21 Soortensamenstelling macrofyten"){
  
  gebiedData <- tabset[!is.na(tabset$EAGIDENT),]
  #alleen overig water
  gebiedData <- gebiedData[grep('*OvWa_*',gebiedData$HoortBijGeoobject.identificatie),]
  
  gebiedData <- gebiedData[gebiedData$GHPR_level == maatlat,]
  
  '7' -> gebiedData$klasse[gebiedData$EKR < 0.2]
  '6' -> gebiedData$klasse[gebiedData$EKR >= 0.2 & gebiedData$EKR < 0.4]
  '5' -> gebiedData$klasse[gebiedData$EKR >= 0.4 & gebiedData$EKR < 0.6]
  '4' -> gebiedData$klasse[gebiedData$EKR >= 0.6 & gebiedData$EKR < 0.8]
  '3' -> gebiedData$klasse[gebiedData$EKR >= 0.8]
  gebiedData$klasse <- as.factor(gebiedData$klasse)
  gebiedData$klasse = factor(gebiedData$klasse, levels = c("3", "4", "5", "6","7"))
  gebiedData <- gebiedData[!is.na(gebiedData$klasse),]
  
  
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  map <- sp::merge(gEAG, gebiedData[, c('.','klasse','jaar','EAGIDENT','Doel','bronddoel', 'KRWwatertype.code',
                                        'GHPR_level')], by.x = 'GAFIDENT', by.y =
                     'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
  map <- map[order(map$jaar),]
  
  leaflet(map) %>%
    addPolygons(layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                     "EKR score:", map$., "<br>",
                                                     "EKR klasse:", map$klasse, "<br>",
                                                     "Gemiddelde getoetste meetjaren:", map$jaar, "<br>",
                                                     "Doel:", map$Doel, "<br>",
                                                     "Maatlat:", maatlat ),
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors=col, labels=labels, title = "")%>%
    addTiles()
}
ekrmap3jrEAG <- function(tabset, maatlat = "2V1 Overige waterflora" ){
  #maatlat = c("2V1 Soortensamenstelling macrofyten Waterplanten", "2V21 Soortensamenstelling hydrofyten")
  gebiedData <- tabset[!is.na(tabset$EAGIDENT),]
  gebiedData <- gebiedData[gebiedData$GHPR_level %in% maatlat,]
  
  '7' -> gebiedData$klasse[gebiedData$EKR < 0.2]
  '6' -> gebiedData$klasse[gebiedData$EKR >= 0.2 & gebiedData$EKR < 0.4]
  '5' -> gebiedData$klasse[gebiedData$EKR >= 0.4 & gebiedData$EKR < 0.6]
  '4' -> gebiedData$klasse[gebiedData$EKR >= 0.6 & gebiedData$EKR < 0.8]
  '3' -> gebiedData$klasse[gebiedData$EKR >= 0.8]
  gebiedData$klasse <- as.factor(gebiedData$klasse)
  gebiedData$klasse = factor(gebiedData$klasse, levels = c("3", "4", "5", "6","7"))
  gebiedData <- gebiedData[!is.na(gebiedData$klasse),]
  
  
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  map <- sp::merge(gEAG, gebiedData[, c('EKR','klasse','EAGIDENT','GEP_2022','bronddoel', 'watertype',
                                        'GHPR_level')], by.x = 'GAFIDENT', by.y =
                     'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
  
  map2 <- map[map$GAFIDENT %in% c('3000-EAG-3','3000-EAG-4','3000-EAG-2','2000-EAG-7','2000-EAG-2','2000-EAG-3','2000-EAG-4','2000-EAG-5','2000-EAG-6'),] 
  
  # map <- map[order(map$jaar),]
  
  leaflet() %>%
    addPolygons(data = map,layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                     "EKR score:", map$., "<br>",
                                                     "EKR klasse:", map$klasse, "<br>",
                                                     "Gemiddelde getoetste meetjaren:", map$jaar, "<br>",
                                                     "Doel:", map$GEP_2022, "<br>",
                                                     "Maatlat:", maatlat ),
                stroke = F, color= 'green', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    addPolygons(data= map2,layerId = map2$GAFIDENT,
                stroke = T, color= ~pal(klasse), opacity=0.8, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(klasse), fillOpacity = 1) %>%
    addLegend("bottomright", colors=col, labels=labels, title = "")%>%
    addTiles()
}
ekrmap3jrWL <- function(tabset, maatlat = "2V21 Soortensamenstelling macrofyten"){
  #gebiedData <- EKRset[EKRset$jaar > '2013'& EKRset$jaar < '2018',]
  #maatlat = "4VI1 Vis-kwaliteit"
  gebiedData <- tabset[!(is.na(tabset$GeoObject.code) & !(tabset$GeoObject.code == 'nietNodig')),]
  gebiedData <- gebiedData[gebiedData$GHPR_level == maatlat,]
  # koppel identKRW aan gebieddata
  '7' -> gebiedData$klasse[gebiedData$EKR < 0.2]
  '6' -> gebiedData$klasse[gebiedData$EKR >= 0.2 & gebiedData$EKR < 0.4]
  '5' -> gebiedData$klasse[gebiedData$EKR >= 0.4 & gebiedData$EKR < 0.6]
  '4' -> gebiedData$klasse[gebiedData$EKR >= 0.6 & gebiedData$EKR < 0.8]
  '3' -> gebiedData$klasse[gebiedData$EKR >= 0.8]
  gebiedData$klasse <- as.factor(gebiedData$klasse)
  gebiedData$klasse = factor(gebiedData$klasse, levels = c("3", "4", "5", "6","7"))
  gebiedData <- gebiedData[!is.na(gebiedData$klasse),]
  
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  map <- sp::merge(gKRW, gebiedData, by.x = 'OWMIDENT', by.y =
                     'GeoObject.code', all.x = TRUE, duplicateGeoms = T)
  
  map <- map[order(map$jaar),]
  #map2 <- map[map$OWMIDENT %in% c('NL11_4_1','NL11_1_2','NL11_3_4'),]
  
  leaflet() %>%
    addPolygons(data = map,layerId = map$GAFIDENT, popup= paste("Waterlichaam", map$HoortBijGeoobject.identificatie, "<br>",
                                                     "EKR score:", map$., "<br>",
                                                     "EKR klasse:", map$klasse, "<br>",
                                                     "jaar:", map$jaar, "<br>",
                                                     "Doel:", map$Doel),
                stroke = T, color= 'green', opacity=0.8, weight = 0.2, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    # addPolygons(data= map2,layerId = map2$GAFIDENT, popup= paste("Waterlichaam", map2$HoortBijGeoobject.identificatie, "<br>",
    #                                                  "EKR score:", map2$., "<br>",
    #                                                  "EKR klasse:", map2$klasse, "<br>",
    #                                                  "jaar:", map2$jaar, "<br>",
    #                                                  "Doel:", map2$Doel),
    #             stroke = T, color= 'red', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
    #             fill=F, fillOpacity = 0.6) %>%
    # addLegend("bottomright", colors=col, labels=labels, title = "")%>%
    addTiles()
}
# kaart ekr scores per krw waterlichaam
ekrmap3 <- function(gebiedData, maatlat = "4VI1 Vis-kwaliteit"){
  #gebiedData <- EKRset[EKRset$jaar >= '2006' & EKRset$jaar < '2019' & EKRset$Waardebepalingsmethode.code == "Maatlatten2018 Vis",]
  #maatlat = "4VI1 Vis-kwaliteit"
  gebiedData <- gebiedData[gebiedData$HoortBijGeoobject.identificatie %in% gebiedData$Identificatie,]
  gebiedData <- gebiedData[gebiedData$GHPR_level == maatlat,]
  # koppel identKRW aan gebieddata
  
  gebiedData$klasse = factor(gebiedData$klasse, levels = c("3", "4", "5", "6","7"))
  
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  map <- sp::merge(gKRW, gebiedData, by.x = 'OWMIDENT', by.y =
                     'GeoObject.code', all.x = TRUE, duplicateGeoms = T)
 
  map <- map[order(map$jaar),]
    leaflet(map) %>%
    addPolygons(layerId = map$GAFIDENT, popup= paste("Waterlichaam", map$HoortBijGeoobject.identificatie, "<br>",
                                                     "EKR score:", map$Numeriekewaarde, "<br>",
                                                     "EKR klasse:", map$klasse, "<br>",
                                                     "jaar:", map$jaar, "<br>",
                                                     "Doel:", map$Doel),
                stroke = T, color= ~pal(klasse), opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors=col, labels=labels, title = "")%>%
    addTiles()
}

oordeelmap3jrEAG <- function(tabset, maatlat = "2V1 Overige waterflora" ){
  #maatlat = c("2V1 Soortensamenstelling macrofyten Waterplanten", "2V21 Soortensamenstelling hydrofyten")
  gebiedData <- tabset[!is.na(tabset$EAGIDENT),]
  gebiedData <- gebiedData[gebiedData$GHPR_level %in% maatlat,]
  
  gebiedData$oordeel <- as.factor(gebiedData$oordeel_2022)
  #gebiedData$oordeel = factor(gebiedData$oordeel, levels = c( "4", "5", "6","7"))
  gebiedData <- gebiedData[!is.na(gebiedData$oordeel),]
  
  colordl <- c('goed'="green",'matig'="yellow",'ontoereikend'="orange",'slecht'="red")
  labelsordl <- c('goed'='goed','matig'='matig','ontoereikend'='ontoereikend','slecht'='slecht')
  pal <- colorFactor(palette = colordl,  domain = gebiedData$oordeel)
  map <- sp::merge(gEAG, gebiedData[, c('EKR','oordeel','oordeel_2022','EAGIDENT','GEP_2022','bronddoel', 'watertype',
                                        'GHPR_level')], by.x = 'GAFIDENT', by.y =
                     'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
  
  map2 <- map[map$GAFIDENT %in% c('3000-EAG-3','3000-EAG-4','3000-EAG-2','2000-EAG-7','2000-EAG-2','2000-EAG-3','2000-EAG-4','2000-EAG-5','2000-EAG-6'),] 
  
  # map <- map[order(map$jaar),]
  
  leaflet() %>%
    addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                                "EKR score:", map$EKR, "<br>",
                                                                "Oordeel:", map$oordeel_2022, "<br>",
                                                                "Doel:", map$GEP_2022, "<br>",
                                                                "Maatlat:", maatlat ),
                stroke = F, color= 'green', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(oordeel), fillOpacity = 0.6) %>%
    addPolygons(data= map2, layerId = map2$GAFIDENT,
                stroke = T, color= ~pal(oordeel), opacity=0.8, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(oordeel), fillOpacity = 1) %>%
    addLegend("bottomright", colors=colordl, labels=labelsordl, title = "")%>%
    addTiles()
}

## trend ----------------------------------------------------------------------------------------
trendEAG <- function(z, detail = "deel"){
  #EKRset$jaar <- as.numeric(EKRset$jaar)
  # z <- EKRset[EKRset$jaar > 2005 & EKRset$jaar < 2019, ]
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  setnames(z,c('HoortBijGeoobject.identificatie'),c('id'))
  
  if(detail == "hoofd"){
    z<- z[z$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),] #alleen hoofdmaatlatten
  }
  
  tabset2 <- dcast(z, id+KRWwatertype.code+Waardebepalingsmethode.code+GHPR_level+EAGIDENT ~ jaar, 
                   value.var = "Numeriekewaarde", fun.aggregate = mean)
  tb <- melt(tabset2, variable.name = "jaar", na.rm =TRUE, value.name = "gemEKRscore") # omgekeerde draaitabel voor correct format lm
  tb$jaar <- as.numeric(tb$jaar) # met factor doet lm een regressie voor ieder jaar tov min
  
  fitted_models = tb %>%  # lineaire regressie over jaren per parameter en EAGIDENT
    group_by(id, Waardebepalingsmethode.code, KRWwatertype.code, GHPR_level,EAGIDENT) %>% 
    filter(length(unique(jaar)) > 1) %>% 
    do(model = lm(gemEKRscore ~ jaar, data = ., na.action = NULL)) #lineaire regressie EKRscores over jaren per EAGIDENT
  
  COF <- fitted_models %>% tidy(model) 
  R2 <- fitted_models %>% glance(model) 
  trtabset <- merge(COF[,c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level','estimate',"term")], 
                    tabset2, by =
                      c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData  <- merge(trtabset, R2, by = c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData  <- gebiedData[gebiedData$term == 'jaar',]
  gebiedData$group <- 'grey'# 1 jaar data
  return(gebiedData)
}

trendEAGverschil <- function(z, detail = "deel"){
  #EKRset$jaar <- as.numeric(EKRset$jaar)
  z <- EKRset[EKRset$jaar > 2005 & EKRset$jaar < 2020, ]
  z<- z[is.na(z$Monster.lokaalID),] # alleen scores per meetlocatie per jaar
  setnames(z,c('HoortBijGeoobject.identificatie'),c('id'))
  
  if(detail == "hoofd"){
    z<- z[z$Grootheid.code %in% c('FYTOPL','OVWFLORA',"MAFAUNA",'VIS'),] #alleen hoofdmaatlatten
  }
  
  tabset2 <- dcast(z, id+KRWwatertype.code+Waardebepalingsmethode.code+GHPR_level+EAGIDENT ~ jaar, 
                   value.var = "Numeriekewaarde", fun.aggregate = mean)
  tb <- melt(tabset2, variable.name = "jaar", na.rm =TRUE, value.name = "gemEKRscore") # omgekeerde draaitabel voor correct format lm
  tb$jaar <- as.numeric(tb$jaar) # met factor doet lm een regressie voor ieder jaar tov min
  
  fitted_models = tb %>%  # lineaire regressie over jaren per parameter en EAGIDENT
    group_by(id, Waardebepalingsmethode.code, KRWwatertype.code, GHPR_level,EAGIDENT) %>% 
    filter(length(unique(jaar)) > 1) %>% 
    do(model = lm(gemEKRscore ~ jaar, data = ., na.action = NULL)) #lineaire regressie EKRscores over jaren per EAGIDENT
  
  COF <- fitted_models %>% tidy(model) 
  R2 <- fitted_models %>% glance(model) 
  trtabset <- merge(COF[,c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level','estimate',"term")], 
                    tabset2, by =
                      c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData  <- merge(trtabset, R2, by = c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData  <- gebiedData[gebiedData$term == 'jaar',]
  
  # trend 2018
  tabset2 <- dcast(z[z$jaar < 2019,], id+KRWwatertype.code+Waardebepalingsmethode.code+GHPR_level+EAGIDENT ~ jaar, 
                   value.var = "Numeriekewaarde", fun.aggregate = mean)
  tb <- melt(tabset2, variable.name = "jaar", na.rm =TRUE, value.name = "gemEKRscore") # omgekeerde draaitabel voor correct format lm
  tb$jaar <- as.numeric(tb$jaar) # met factor doet lm een regressie voor ieder jaar tov min
  
  fitted_models = tb %>%  # lineaire regressie over jaren per parameter en EAGIDENT
    group_by(id, Waardebepalingsmethode.code, KRWwatertype.code, GHPR_level,EAGIDENT) %>% 
    filter(length(unique(jaar)) > 1) %>% 
    do(model = lm(gemEKRscore ~ jaar, data = ., na.action = NULL)) #lineaire regressie EKRscores over jaren per EAGIDENT
  
  COF <- fitted_models %>% tidy(model) 
  R2 <- fitted_models %>% glance(model) 
  trtabset <- merge(COF[,c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level','estimate',"term")], tabset2, by =
                      c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData1  <- merge(trtabset, R2, by = c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData1  <- gebiedData1[gebiedData1$term == 'jaar' ,]
  
  gebiedData <- merge(gebiedData, gebiedData1[,c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level','estimate','p.value','r.squared')], by =c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level'))
  colnames(gebiedData) <- gsub(x = colnames(gebiedData), pattern = "\\.x", replacement = ".2019")
  colnames(gebiedData) <- gsub(x = colnames(gebiedData), pattern = "\\.y", replacement = ".2018") 
  
  
  # trend 2017
  tabset2 <- dcast(z[z$jaar < 2018,], id+KRWwatertype.code+Waardebepalingsmethode.code+GHPR_level+EAGIDENT ~ jaar, 
                   value.var = "Numeriekewaarde", fun.aggregate = mean)
  tb <- melt(tabset2, variable.name = "jaar", na.rm =TRUE, value.name = "gemEKRscore") # omgekeerde draaitabel voor correct format lm
  tb$jaar <- as.numeric(tb$jaar) # met factor doet lm een regressie voor ieder jaar tov min
  
  fitted_models = tb %>%  # lineaire regressie over jaren per parameter en EAGIDENT
    group_by(id, Waardebepalingsmethode.code, KRWwatertype.code, GHPR_level,EAGIDENT) %>% 
    filter(length(unique(jaar)) > 1) %>% 
    do(model = lm(gemEKRscore ~ jaar, data = ., na.action = NULL)) #lineaire regressie EKRscores over jaren per EAGIDENT
  
  COF <- fitted_models %>% tidy(model) 
  R2 <- fitted_models %>% glance(model) 
  trtabset <- merge(COF[,c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level','estimate',"term")], tabset2, by =
                      c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData1  <- merge(trtabset, R2, by = c('id',"EAGIDENT",'Waardebepalingsmethode.code','KRWwatertype.code','GHPR_level')) # data trend samenvoegen met resultaten
  gebiedData1  <- gebiedData1[gebiedData1$term == 'jaar' ,]
  
  gebiedData <- merge(gebiedData, gebiedData1[,c('id',"EAGIDENT",'Waardebepalingsmethode.code','GHPR_level','estimate','p.value','r.squared')], by =c('id',"EAGIDENT",'Waardebepalingsmethode.code','GHPR_level'))
  
  write.table(gebiedData, file = paste(getwd(),"/output/EAGTrendVerschil",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE,               na = "", sep =';', row.names = FALSE) # wegschrijven als csv
  
  return(gebiedData)
} 

trendfychem <- function(z, param = c("P","N","NH3")){
  z <- wq
  z<- z[z$fewsparameter %in% param ,]
  z<- z[grep('*VAST*',z$locatie.meetnethistorie),]
  
  tabset2 <- dcast(z, fewsparametercode+EAGIDENT ~ jaar, 
                   value.var = "meetwaarde", fun.aggregate = mean)
  tb <- melt(tabset2, variable.name = "jaar", na.rm = TRUE, value.name = "JGM") # omgekeerde draaitabel voor correct format lm
  tb$jaar <- as.numeric(tb$jaar) # met factor doet lm een regressie voor ieder jaar tov min
  
  fitted_models = tb %>%  # lineaire regressie over jaren per parameter en gebied
    group_by(fewsparametercode, EAGIDENT) %>% 
    filter(length(unique(jaar)) > 1) %>% 
    do(model = lm(JGM ~ jaar, data = ., na.action = NULL)) #lineaire regressie EKRscores over jaren per gebied
  
  COF <- fitted_models %>% tidy(model) 
  R2 <- fitted_models %>% glance(model) 
  trtabset <- merge(COF[,c("EAGIDENT",'fewsparametercode','estimate',"term")], tabset2, by =
                      c('fewsparametercode','EAGIDENT')) # data trend samenvoegen met resultaten
  gebiedData  <- merge(trtabset, R2, by = c('fewsparametercode','EAGIDENT')) # data trend samenvoegen met resultaten
  gebiedData <- gebiedData[!is.na(gebiedData$EAGIDENT),]
  gebiedData<- gebiedData[gebiedData$term == 'jaar' ,]
  write.table(gebiedData, file = paste(getwd(),"/output/EAGTrendfychem",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE,               na = "", sep =';', row.names = FALSE) # wegschrijven als csv
  return(gebiedData)
} 

plottrend <- function(gebiedData){  
  zw <- 
    ggplot(gebiedData[gebiedData$term == 'jaar' ,], aes(x = reorder(EAGIDENT, -estimate), y = estimate, fill = group)) +
    geom_bar(stat ='identity')+
    scale_fill_manual(values=c('grey','red'))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5,angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    theme(legend.position='none')+
    ggtitle(paste0( "Trend in EKR scores op de maatlat ",unique(gebiedData$GHPR_level))) +
    labs(x = "", y="")
  ggplotly(p=zw) 
}
plottrend2 <- function(gebiedData){
  gebiedData <- gebiedData[!is.na(gebiedData$GHPR_level),]
  gebiedData <- with(gebiedData, gebiedData[order(GHPR_level), ])
  
  zw <- 
    ggplot(gebiedData[gebiedData$term == 'jaar' ,], aes(x = GHPR_level, y = estimate, fill = group)) +
    geom_bar(stat ='identity')+
    facet_wrap(.~id)+
    scale_fill_manual(values=c('grey','red'))+
    coord_flip()+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5,angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    theme(legend.position='none')+
    ggtitle(paste0( "Trend in EKR scores per maatlat ")) +
    labs(x = "", y="")
  
  ggplotly(p=zw) 
}
plottrend3 <- function(mfatrend){
  # gebiedData <- vegtrend
  gebiedData<- mfatrend[!mfatrend$EAGIDENT %in% c('2220-EAG-1','1000-EAG-1','2000-EAG-1'),] 
  # Gaasperplas verkeerde trend berekend door fout in monitoringslocatie emerse zone,
  # grachten fout in monsterlocaties 2012. Te weinig data voor trend.
  # ams zuid, de schommeling zit in drijfbladplanten en die zijn niet eenduidig bemonsterd, terwijl klassegrenzen smal zijn.
  
  #gebiedData$klasse <- gebiedData[order(gebiedData$klasse),]
  '1' -> gebiedData$klasse[gebiedData$estimate < -0.15]
  '2' -> gebiedData$klasse[gebiedData$estimate >= -0.15 & gebiedData$estimate < -0.1]
  '3' -> gebiedData$klasse[gebiedData$estimate >= -0.1 & gebiedData$estimate < 0]
  '4' -> gebiedData$klasse[gebiedData$estimate >= 0 & gebiedData$estimate < 0.1]
  '5' -> gebiedData$klasse[gebiedData$estimate >= 0.1 & gebiedData$estimate < 0.15]
  '6' -> gebiedData$klasse[gebiedData$estimate >= 0.2]
  gebiedData$klasse[is.na(gebiedData$p.value)] <- '8'# slechts 1 of 2 jaar data
  gebiedData$klasse[gebiedData$p.value > 0.4] <- "7" # geen trend
  gebiedData$klasse[gebiedData$r.squared == 1] <- "8" 
  gebiedData$klasse <- factor(gebiedData$klasse, levels = c("1", "2", "3", "4","5","6","7","8"))
  
  #KRW <- as.data.frame(gKRW)
  #KRW$eag <- sapply(strsplit(as.character(KRW$OWAIDENT), '_'), `[`, 2)
  #gebiedData <- sp::merge(KRW, gebiedData, by.x = 'eag', by.y =
  #            'EAGIDENT', all.y = TRUE)
  #write.table(gebiedData, file = paste(getwd(),"/output/EAGTrend",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE,               na = "", sep =';', row.names = FALSE) # wegschrijven als csv
  
  col <- c('1'= 'darkred','2'="red", '3'="salmon",'4'="lightgreen",'5'="green",'6'="darkgreen", 
           '7' ="yellow", '8' ="grey")
  labels <- c('1'="-0.02 - -0.15",'2'="-0.15 - -0.1" ,'3'="-0.1 - 0",'4'="0 - 0.15",'5'="0.15 - 0.02",
              '6'="0.2 - 0.5", '7' = 'geen trend', '8' = 'berekening niet mogelijk')
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  map <- sp::merge(gEAG, gebiedData[, c('klasse','estimate','p.value','r.squared','group','EAGIDENT', 'KRWwatertype.code',
                                        'GHPR_level')], by.x = 'GAFIDENT', by.y =
                     'EAGIDENT', duplicateGeoms = T)
  
  leaflet(map) %>%
    addPolygons(layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                     "EAG code", map$GAFIDENT, "<br>",
                                                     "EKR trend:", map$estimate, "<br>",
                                                     "trend significantie:", map$p.value, "<br>",
                                                     "R2:", map$r.squared, "<br>"
                                                     ),
                stroke = T, color= 'black', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors = col, labels = labels, title = "Lineaire trend EKR score")%>%
    addTiles()
}

# biologie ------------------------------------------------------------------------
# kranswieren en fonteinkruiden 
plotmcftchara <- function(hybi1){
  ptbLocSubms <- dcast(hybi1,locatie.EAG+jaar ~ .,lengthUnique,value.var=c("locatiecode"), subset = .(parametercode == 'SUBMSPTN'))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
  ptbLocSubms$nLoc <- ptbLocSubms$.# aantal vegetatielocatie in EAG
  
  ptbFreqChara2 <- rbind(hybi1[grep('^Chara',hybi1$TWN.naam) ,], hybi1[grep('^Nitellopsis',hybi1$TWN.naam) ,],
                               hybi1[grep('^Nitella',hybi1$TWN.naam) ,])
  if(nrow(ptbFreqChara2)>0){
  ptbFreqChara2<- dcast(ptbFreqChara2, locatie.EAG+jaar~ .,
                         lengthUnique, value.var = "locatiecode")
  ptbFreqChara2$Freq <- ptbFreqChara2$. # aantal UNIEKE locatie met taxa X in EAG
  ptbAbunC2 <- merge(ptbFreqChara2,ptbLocSubms[,c('locatie.EAG','jaar','nLoc')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  ptbAbunC2$percentageLocatiesWaarAangetroffen <- 100*ptbAbunC2$Freq/ptbAbunC2$nLoc
  # aantal soorten
  ptbNChara <- dcast(rbind(hybi1[grep('^Chara',hybi1$TWN.naam) ,], hybi1[grep('^Nitellopsis',hybi1$TWN.naam) ,],
                           hybi1[grep('^Nitella',hybi1$TWN.naam) ,]), locatie.EAG+jaar~ .,
                     lengthUnique, value.var = "TWN.naam")
  ptbNChara$Ntaxa <- ptbNChara$. # aantal UNIEKE soorten met taxa X in EAG
  ptbAbunC3 <- merge(ptbAbunC2,ptbNChara[,c('locatie.EAG','jaar','Ntaxa')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  
  
 plot<- ggplot(ptbAbunC3, aes(x= jaar, y= percentageLocatiesWaarAangetroffen, fill = Ntaxa))+
    geom_col(position = 'dodge') +
    facet_grid(~locatie.EAG)+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    #coord_flip()+
    ggtitle( "Abundantie kranswieren") +
    labs(x="",y="% van locaties gevonden")
  chara <- ggplotly(plot)
  return(chara)
  }
  
  if(!nrow(ptbFreqChara2)>0){ggtexttable('Geen kranswieren gevonden.')}
}
kaartmcftchara <- function(hybi1){  
  chara <- hybi1[hybi1$analysecode == 'PTN' ,]
  chara <- rbind(chara[grep('^Chara',chara$TWN.naam) ,], chara[grep('^Nitellopsis',chara$TWN.naam) ,],
                 chara[grep('^Nitella',chara$TWN.naam),])
  chara <- dcast(chara, locatiecode+jaar+locatie.x+locatie.y+eenheid+eenheidequivalent ~ ., sum, value.var = "meetwaarde")
  chara$bedekkingspercentageKranswieren <- chara$.; chara$. <- NULL
  #write.table(chara, file = paste(getwd(),"/output/kranswierenSomBedekking",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  ptn <- dcast(hybi1[hybi1$analysecode == 'PTN',],locatiecode+jaar+locatie.x+locatie.y~.,
               sum, value.var = "meetwaarde")
  
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=chara[,c("locatie.x","locatie.y")],
                                       data=chara, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(gebiedData)
  
  gebiedData1 <-
    spTransform(SpatialPointsDataFrame(coords=ptn[,c("locatie.x","locatie.y")],
                                       data=ptn, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData1 <-as.data.frame(gebiedData1)
  
  gebiedData$klasse <- cut(gebiedData$bedekkingspercentageKranswieren, 
                           breaks= c(min(gebiedData$bedekkingspercentageKranswieren)-0.1, 5, 10,15,25,50, 80,max(gebiedData$bedekkingspercentageKranswieren)+0.1))
  
  col <- c('7'= 'darkred','6'="red", '5'="salmon",'4'="orange",'3'="yellow",'2'="deepskyblue",'1'="blue")
  labels <- c('1'="0.1-5",'2'="5-10" ,'3'="10-15",'4'="15-25",'5'="25-50",'6'="50-80",'7'='80-100')
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  chara <- leaflet() %>% 
    addCircles(data= gebiedData1, ~locatie.x.1, ~locatie.y.1, popup = c(as.character(gebiedData1$locatiecode)),
               weight = 3, radius=40, fillOpacity = 0.8, color = 'grey') %>%
    addCircles(data= gebiedData, ~locatie.x.1, ~locatie.y.1, popup =  paste("locatiecode", as.character(gebiedData$locatiecode), "<br>",
                                                                            "Bedekking:", as.character(gebiedData$bedekkingspercentageKranswieren), gebiedData$eenheidequivalent, gebiedData$eenheid, "<br>",
                                                                            "Meetjaar:", as.character(gebiedData$jaar)),
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klasse)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Bedekking som kranswieren (%)') %>%
    #addControl(paste0('Meetjaar: ',unique(gebiedData$jaar)), position = "bottomright")%>%
    addTiles()
  
  return(chara)
}
kaartfontein <- function(hybi1){  
  fontein <- hybi1[!(hybi1$TWN.naam == 'Potamogeton pectinatus') & hybi1$analysecode == 'PTN',]
  fontein <- dcast(fontein[grep('^Potamogeton', fontein$TWN.naam),],locatiecode+jaar+locatie.x+locatie.y+eenheid+eenheidequivalent~.,
                   sum, value.var = "meetwaarde")
  fontein$bedekkingspercentagefonteinkruiden <- fontein$.; fontein$. <- NULL
  #write.table(fontein, file = paste(getwd(),"/output/fonteinkruidenSomBedekking",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  ptn <- dcast(hybi1[hybi1$analysecode == 'PTN',],locatiecode+jaar+locatie.x+locatie.y~.,
               sum, value.var = "meetwaarde")
  
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=fontein[,c("locatie.x","locatie.y")],
                                       data=fontein, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(gebiedData)
  
  gebiedData1 <-
    spTransform(SpatialPointsDataFrame(coords=ptn[,c("locatie.x","locatie.y")],
                                       data=ptn, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData1 <-as.data.frame(gebiedData1)
  
  gebiedData$klasse <- cut(gebiedData$bedekkingspercentagefonteinkruiden, breaks= c(0, 5, 10,15,25,50, 80,100))
  
  col <- c('7'= 'darkred','6'="red", '5'="salmon",'4'="orange",'3'="yellow",'2'="deepskyblue",'1'="blue")
  labels <- c('1'="0.1-5",'2'="5-10" ,'3'="10-15",'4'="15-25",'5'="25-50",'6'="50-80",'7'='80-100')
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  ktPbod <- leaflet() %>% 
    addCircles(data= gebiedData1, ~locatie.x.1, ~locatie.y.1, popup = c(as.character(gebiedData1$locatiecode)),
               weight = 3, radius=40, fillOpacity = 0.8, color = 'grey') %>%
    addCircles(data= gebiedData, ~locatie.x.1, ~locatie.y.1, popup =  paste("locatiecode", as.character(gebiedData$locatiecode), "<br>",
                                                                            "Bedekking:", as.character(gebiedData$bedekkingspercentagefonteinkruiden),gebiedData$eenheidequivalant, gebiedData$eenheid,"<br>",
                                                                            "Meetjaar:", as.character(gebiedData$jaar)),
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klasse)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Bedekking som fonteinkruiden (%)') %>%
    #addControl(paste0('Meetjaar: ',unique(gebiedData$jaar)), position = "bottomright")%>%
    addTiles()
  
  return(ktPbod)
}
plotmcftftn <- function(hybi1){    
  ptbLocSubms <- dcast(hybi1,locatie.EAG+jaar ~ .,lengthUnique,value.var=c("locatiecode"), subset = .(parametercode == 'SUBMSPTN'))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
  ptbLocSubms$nLoc <- ptbLocSubms$.# aantal vegetatielocatie in EAG
  
  if(nrow(hybi1[grep('^Potamogeton',hybi1$TWN.naam) ,])>0){
  ptbFreqfontein2 <- dcast(hybi1[grep('^Potamogeton',hybi1$TWN.naam) ,],
                           locatie.EAG+jaar~.,
                           lengthUnique, value.var = "locatiecode")
  
  ptbFreqfontein2$Freq <- ptbFreqfontein2$.# aantal UNIEKE locatie met taxa X in EAG
  ptbAbunF2 <- merge(ptbFreqfontein2,ptbLocSubms[,c('locatie.EAG','jaar','nLoc')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  if(nrow(ptbAbunF2)>0){
  ptbAbunF2$percentageLocatiesWaarAangetroffen <- 100*ptbAbunF2$Freq/ptbAbunF2$nLoc
  # aantal soorten
  ptbNF <- dcast(hybi1[grep('^Potamogeton',hybi1$TWN.naam) ,],
                 locatie.EAG+jaar~.,
                 lengthUnique, value.var = "TWN.naam")
  ptbNF$Ntaxa <- ptbNF$. # aantal UNIEKE soorten met taxa X in EAG
  ptbAbunF3 <- merge(ptbAbunF2,ptbNF[,c('locatie.EAG','jaar','Ntaxa')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  
  plot <- ggplot(ptbAbunF3, aes(x= jaar, y= percentageLocatiesWaarAangetroffen, fill = Ntaxa))+
    geom_col(position = 'dodge') +
    facet_grid(~locatie.EAG)+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    #coord_flip()+
    ggtitle( "Abundantie fonteinkruiden") +
    labs(x="",y="% van locaties gevonden")
  fontein <- ggplotly(plot)
  return(fontein)}}
  
  if(!nrow(ptbAbunF2)>0){{ggtexttable('Geen fonteinkruiden gevonden.')}}
}
plotmcftkrab <- function(hybi1){    
  ptbLocSubms <- dcast(hybi1,locatie.EAG+jaar ~ .,lengthUnique,value.var=c("locatiecode"), subset = .(parametercode == 'SUBMSPTN'))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
  ptbLocSubms$nLoc <- ptbLocSubms$.# aantal vegetatielocatie in EAG
  
  ptbFreqfontein2 <- dcast(hybi1[hybi1$TWN.naam == "Stratiotes aloides",],
                           locatie.EAG+jaar~.,
                           lengthUnique, value.var = "locatiecode")
  ptbFreqfontein2$Freq <- ptbFreqfontein2$.# aantal UNIEKE locatie met taxa X in EAG
  ptbAbunF2 <- merge(ptbFreqfontein2,ptbLocSubms[,c('locatie.EAG','jaar','nLoc')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  ptbAbunF2$percentageLocatiesWaarAangetroffen <- 100*ptbAbunF2$Freq/ptbAbunF2$nLoc

  plot <- ggplot(ptbAbunF2, aes(x= jaar, y= percentageLocatiesWaarAangetroffen))+
    geom_col(position = 'dodge') +
    facet_grid(~locatie.EAG)+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    #coord_flip()+
    ggtitle( "Abundantie Krabbenscheer") +
    labs(x="",y="% van locaties gevonden")
  ggplotly(plot)
}
kaartstrat <- function(hybi1){  
  kr <- dcast(hybi1[hybi1$TWN.naam == "Stratiotes aloides" & hybi1$parameterfractie == "" & hybi1$analysecode == 'PTN',],
              locatiecode+jaar+locatie.x+locatie.y+eenheid+eenheidequivalent~.,
              sum, value.var = "meetwaarde")
  kr$bedekkingspercentageKrabbenscheer <- kr$.; kr$. <- NULL
  #write.table(kr, file = paste(getwd(),"/output/krabbenscheerSomBedekking",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  ptn <- dcast(hybi1[hybi1$analysecode == 'PTN',],locatiecode+jaar+locatie.x+locatie.y~.,
               sum, value.var = "meetwaarde")
  
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=kr[,c("locatie.x","locatie.y")],
                                       data=kr, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(gebiedData)
  
  gebiedData1 <-
    spTransform(SpatialPointsDataFrame(coords=ptn[,c("locatie.x","locatie.y")],
                                       data=ptn, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData1 <-as.data.frame(gebiedData1)
  
  gebiedData$klasse <- cut(gebiedData$bedekkingspercentageKrabbenscheer, 
                           breaks= c(min(gebiedData$bedekkingspercentageKrabbenscheer)-0.1, 5, 10,15,25,50, 80,max(gebiedData$bedekkingspercentageKrabbenscheer)+0.1))
  
  col <- c('7'= 'darkred','6'="red", '5'="salmon",'4'="orange",'3'="yellow",'2'="deepskyblue",'1'="blue")
  labels <- c('1'="0.1-5",'2'="5-10" ,'3'="10-15",'4'="15-25",'5'="25-50",'6'="50-80",'7'='80-100')
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  ktPbod <- leaflet() %>% 
    addCircles(data= gebiedData1, ~locatie.x.1, ~locatie.y.1, popup = c(as.character(gebiedData1$locatiecode)),
               weight = 3, radius=40, fillOpacity = 0.8, color = 'grey') %>%
    addCircles(data= gebiedData, ~locatie.x.1, ~locatie.y.1, popup =  paste("locatiecode", as.character(gebiedData$locatiecode), "<br>",
                                                                            "Bedekking:", as.character(gebiedData$bedekkingspercentageKrabbenscheer),gebiedData$eenheidequivalant, gebiedData$eenheid,"<br>",
                                                                            "Meetjaar:", as.character(gebiedData$jaar)),
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klasse)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Bedekking Krabbenscheer (%)') %>%
    #addControl(paste0('Meetjaar: ',unique(gebiedData$jaar)), position = "bottomright")%>%
    addTiles()
  
  return(ktPbod)
}
kaartutr <- function(hybi1){  
  kr <- dcast(hybi1[hybi1$TWN.naam == "Utricularia vulgaris" & hybi1$analysecode == 'PTN',],locatiecode+jaar+locatie.x+locatie.y+eenheidequivalent+eenheid~.,
              sum, value.var = "meetwaarde")
  
  kr$bedekkingspercentageBlaasjeskruid <- kr$.; kr$. <- NULL
  #write.table(kr, file = paste(getwd(),"/output/grblaasjeskruidSomBedekking",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  ptn <- dcast(hybi1[hybi1$analysecode == 'PTN',],locatiecode+jaar+locatie.x+locatie.y~.,
               sum, value.var = "meetwaarde")
  
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=kr[,c("locatie.x","locatie.y")],
                                       data=kr, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(gebiedData)
  
  gebiedData1 <-
    spTransform(SpatialPointsDataFrame(coords=ptn[,c("locatie.x","locatie.y")],
                                       data=ptn, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData1 <-as.data.frame(gebiedData1)
  
  gebiedData$klasse <- cut(gebiedData$bedekkingspercentageBlaasjeskruid, breaks= c(min(gebiedData$bedekkingspercentageBlaasjeskruid)- 0.1, 5, 10,15,25,50, 80,max(gebiedData$bedekkingspercentageBlaasjeskruid)+0.1))
  
  col <- c('7'= 'darkred','6'="red", '5'="salmon",'4'="orange",'3'="yellow",'2'="deepskyblue",'1'="blue")
  labels <- c('1'="0.1-5",'2'="5-10" ,'3'="10-15",'4'="15-25",'5'="25-50",'6'="50-80",'7'='80-100')
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  ktPbod <- leaflet() %>% 
    addCircles(data= gebiedData1, ~locatie.x.1, ~locatie.y.1, popup =  paste("locatiecode", as.character(gebiedData1$locatiecode), "<br>",
                                                                             "Bedekking:", as.character(gebiedData1$bedekkingspercentageBlaasjeskruid),gebiedData$eenheidequivalent,gebiedData$eenheid,"<br>",
                                                                             "Meetjaar:", as.character(gebiedData1$jaar)),
               weight = 3, radius=40, fillOpacity = 0.8, color = 'grey') %>%
    addCircles(data= gebiedData, ~locatie.x.1, ~locatie.y.1, popup =  paste("locatiecode", as.character(gebiedData$locatiecode), "<br>",
                                                                            "Bedekking:", as.character(gebiedData$bedekkingspercentageBlaasjeskruid),gebiedData$eenheidequivalent,gebiedData$eenheid,"<br>",
                                                                            "Meetjaar:", as.character(gebiedData$jaar)),
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klasse)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Bedekking Groot blaasjeskruid (%)') %>%
    #addControl(paste0('Meetjaar: ',unique(gebiedData$jaar)), position = "bottomright")%>%
    addTiles()
  
  return(ktPbod)
}
# exoten
kaartkreeft <- function(hybi1){  
  kr <-  rbind(hybi1[grep('^Procambarus', hybi1$TWN.naam),], hybi1[grep('^Oronectus', hybi1$TWN.naam),], hybi1[grep('^Oronectes', hybi1$TWN.naam),])
  kreeft <- dcast(kr,locatiecode+jaar+locatie.x+locatie.y+eenheid+eenheidequivalent~.,
                  sum, value.var = "meetwaarde")
  kreeft$aantalKreeften <- kreeft$.; kreeft$. <- NULL
  #write.table(kreeft, file = paste(getwd(),"/2018/output/kreeft",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=kreeft[,c("locatie.x","locatie.y")],
                                       data=kreeft, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(gebiedData)
  
  gebiedData$klasse <- cut(gebiedData$aantalKreeften, breaks= c(min(gebiedData$aantalKreeften)-0.1, 5, 10,15,25,50,max(gebiedData$aantalKreeften))+0.1)
  
  col <- c('7'= 'darkred','6'="red", '5'="salmon",'4'="orange",'3'="yellow",'2'="deepskyblue")
  labels <- c('1'="0-5",'2'="5-10" ,'3'="10-15",'4'="15-25",'5'="25-50",'6'="50-80")
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  ktPbod <- leaflet() %>% 
    addCircles(data= gebiedData, ~locatie.x.1, ~locatie.y.1, popup =  paste("locatiecode", as.character(gebiedData$locatiecode), "<br>",
                                                                            "Bedekking:", as.character(gebiedData$aantalKreeften),gebiedData$eenheid,"<br>",
                                                                            "Meetjaar:", as.character(gebiedData$jaar)),
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klasse)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Aantal kreeften (bijvangst)') %>%
    #addControl(paste0('Meetjaar: ',unique(gebiedData$jaar)), position = "bottomright")%>%
    addTiles()
  
  return(ktPbod)
}
#cabomba
kaartcam <- function(hybi1){  
  kr <-  rbind(hybi1[grep('^Cabomba', hybi1$TWN.naam),])
  cabomba <- dcast(kr,locatiecode+jaar+locatie.x+locatie.y+eenheid+eenheidequivalent~.,
                   sum, value.var = "meetwaarde")
  cabomba$bedCabomba <- cabomba$.; cabomba$. <- NULL
  #write.table(cabomba, file = paste(getwd(),"/2018/output/cabomba",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=cabomba[,c("locatie.x","locatie.y")],
                                       data=cabomba, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(gebiedData)
  
  gebiedData$klasse <- cut(gebiedData$bedCabomba, breaks= c(min(gebiedData$bedCabomba)-0.1, 5, 10,15,25,50,80,max(gebiedData$bedCabomba)+0.1))
  
  col <- c('7'= 'darkred','6'="red", '5'="salmon",'4'="orange",'3'="yellow",'2'="deepskyblue", "1"='blue')
  labels <- c('1'="0-5",'2'="5-10" ,'3'="10-15",'4'="15-25",'5'="25-50",'6'="50-80", '7'='80-100')
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  ktPbod <- leaflet() %>% 
    addCircles(data= gebiedData, ~locatie.x.1, ~locatie.y.1, popup =  paste("locatiecode", as.character(gebiedData$locatiecode), "<br>",
                                                                            "Bedekking:", as.character(gebiedData$bedCabomba),gebiedData$eenheidequivalent, gebiedData$eenheid,"<br>",
                                                                            "Meetjaar:", as.character(gebiedData$jaar)),
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klasse)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Bedekking Cabomba') %>%
    #addControl(paste0('Meetjaar: ',unique(gebiedData$jaar)), position = "bottomright")%>%
    addTiles()
  
  return(ktPbod)
}
kaartved <- function(hybi1){  
  kr <-  rbind(hybi1[grep('^Myriophyllum heterophyllum', hybi1$TWN.naam),])
  cabomba <- dcast(kr,locatiecode+jaar+locatie.x+locatie.y+eenheid+eenheidequivalent~.,
                   sum, value.var = "meetwaarde")
  cabomba$bedVederkruid <- cabomba$.; cabomba$. <- NULL
  #write.table(cabomba, file = paste(getwd(),"/2018/output/vederkruid",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=cabomba[,c("locatie.x","locatie.y")],
                                       data=cabomba, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(gebiedData)
  
  gebiedData$klasse <- cut(gebiedData$bedVederkruid, breaks= c(min(gebiedData$bedVederkruid)-0.1, 5, 10,15,25,50,80,max(gebiedData$bedVederkruid)+0.1))
  
  col <- c('7'= 'darkred','6'="red", '5'="salmon",'4'="orange",'3'="yellow",'2'="deepskyblue", "1"='blue')
  labels <- c('1'="0-5",'2'="5-10" ,'3'="10-15",'4'="15-25",'5'="25-50",'6'="50-80", '7'='80-100')
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  ktPbod <- leaflet() %>% 
    addCircles(data= gebiedData, ~locatie.x.1, ~locatie.y.1, popup =  paste("locatiecode", as.character(gebiedData$locatiecode), "<br>",
                                                                            "Bedekking:", as.character(gebiedData$bedVederkruid),gebiedData$eenheidequivalent, gebiedData$eenheid,"<br>",
                                                                            "Meetjaar:", as.character(gebiedData$jaar)),
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klasse)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Bedekking Ongelijkbladig vederkruid') %>%
    #addControl(paste0('Meetjaar: ',unique(gebiedData$jaar)), position = "bottomright")%>%
    addTiles()
  
  return(ktPbod)
}
#est
est <- function (hybi1, hybi1parameter = c('SUBMSPTN','FLAB', 'WATDTE',"ZICHT_m","PTN_BEDKG_%","OEVBSIG_SOORT","KROOS", "DRIJF")){
  hybi1_grenswaarden <- read.csv("./hydrobiologie/grenswaarden_est.csv", header = TRUE, na.strings = " ", sep=";", dec =".", 
                                stringsAsFactors = F)
}

# fytosoorten
plotfytpl <- function(fyt){
  fyt$meetwaarde[fyt$limietsymbool == '<'] <- fyt$meetwaarde[fyt$limietsymbool == '<']/2 # meetwaarden detectiegrens/ halve detectiegrens meenemen
  fytwrn <- fyt[fyt$eenheidequivalent =="waarneming" & fyt$jaar > 2005 ,]
  fytwrn$groep <- paste0(fytwrn$WNA.fytoplankton.groep, ': ', fytwrn$WNA.fytoplankton.subgroep)
  fytwrn <- fytwrn[!is.na(fytwrn$groep) & !is.na(fytwrn$jaar) &!is.na(fytwrn$meetwaarde),]
  fytwrn <- dcast(fytwrn, groep+locatie.EAG+datum+jaar~ ., value.var = "meetwaarde", fun.aggregate = sum, drop = TRUE)# som aantallen per groep
  ft <- dcast(fytwrn, locatie.EAG+groep+jaar~ ., value.var = ".", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  
  
  fyto <- 
    ggplot(ft)+
    geom_bar(aes(x = ft$jaar, y = ft$., fill = ft$groep), 
             width = 0.9, stat = 'identity', position = "stack") +
    guides(fill=guide_legend(title='Groepen Fytoplankton'), element_text(size = 4))+
    facet_grid(~locatie.EAG)+
    scale_y_continuous()+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), 
      strip.text.y = element_text(size = 5), 
      axis.text.x = element_text(size= 5, hjust=2, angle = 90),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle(paste0("Aantal waarnemingen fytoplankton in ")) +
    labs(x="jaar",y="aantal/ml")
  
  ggplotly(p=fyto)
}

lengthUnique <- function(x){return(length(unique(x)))}#by default telt dcast ALLE value.var, dus ook als deze dubbel voorkomt (meerdere waarnemingen op 1 locatie). Daarom deze functie: alleen UNIEKE value.var

# diversiteit 
div<- function(hybi1){
  ptb <- hybi1[hybi1$parameterfractie == '',]
  ptb <- ptb[ptb$fewsparameter %in% c('PTN_BEDKG_%','PTN_BEDKG_BraunBS','PTN_BEDKG_NatS',
                                      'PTN_BEDKG_TansleyS'),]
  ptb<- ptb[ptb$monsterident %in% unique(ptb$monsterident[ptb$parametercode == 'SUBMSPTN']),] # alleen monst waar submers is gemeten
  
  # gemiddelde submerse bedekking
  ptbSubms <- dcast(ptb,locatie.EAG+TWN.naam+WNA.nederlandse.soortnaam+parametercode+jaar ~ ., mean,
                    value.var = "meetwaarde", subset = .(parametercode == 'SUBMSPTN'))
  ptbSubms$AvBedkgSums <- ptbSubms$.;ptbSubms$.<-NULL # gemiddelde submerse bedekking in EAG
  
  # aantal unieke locaties
  ptbLoc <- dcast(ptb,locatie.EAG+jaar+compartiment ~ .,lengthUnique,value.var=c("locatiecode")) #aantal unieke locatiecodes per EAG+jaar
  ptbLoc$nLoc <- ptbLoc$. ;  ptbLoc$. <- NULL# aantal vegetatielocatie in EAG
  
  # gamma
  ptbGamma <- dcast(ptb[ptb$WNA.onderwaterplantensoorten == '1',],locatie.EAG+jaar+compartiment ~ ., lengthUnique, value.var = "TWN.naam",subset = .(WNA.onderwaterplantensoorten == 1)) # aantal UNIEKE TWN.naam per EAG+jaar.
  ptbGamma$GammaDiv <- ptbGamma$.; ptbGamma$. <- NULL# aantal UNIEKE locatie met taxa X in EAG
  
  # alfa
  ptbAlfaSum <- dcast(ptb[ptb$WNA.onderwaterplantensoorten == '1',],locatiecode+locatie.EAG+jaar+compartiment ~ ., lengthUnique, value.var="TWN.naam",subset = .(WNA.onderwaterplantensoorten == 1)) #aantal UNIEKE taxa per locatie per EAG+jaar (controle >> altijd >= gammaDiv)
  ptbAlfaSum$AlfaSum <- ptbAlfaSum$.;ptbAlfaSum$. <- NULL
  ptbalfa <- dcast(ptbAlfaSum,locatie.EAG+jaar+compartiment ~ ., mean, value.var = "AlfaSum") # gem soorten onderwater/ jaar gaat 0 goed?
  ptbalfa$AlfaDiv <- ptbalfa$.;ptbalfa$. <-NULL
  
  ptbABC <-  merge(ptbalfa,ptbGamma,by = c('locatie.EAG','jaar','compartiment'), all.x = FALSE, all.y = TRUE) 
  ptbABC$BetaDiv <- ptbABC$GammaDiv/ptbABC$AlfaDiv
  ptbABC <-  merge(ptbLoc,ptbABC,by = c('locatie.EAG','jaar','compartiment'), all.x = TRUE, all.y = FALSE) 
  
  x<- melt(ptbABC, id.vars= c("locatie.EAG","jaar","compartiment" ))
  x$diversiteitsindex <- x$variable
  
  div <- 
    ggplot(x[!is.na(x$locatie.EAG),], aes(x= jaar, y = value, fill = locatie.EAG))+
    geom_col(position = 'dodge') +
    facet_grid(.~diversiteitsindex)+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, vjust=2),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    #coord_flip()+
    ggtitle( "Biodiversiteit aan de hand van soortensamenstelling vegetatie") +
    labs(x="",y="")
  ggplotly(p=div)
  
  # ptbABC<- ptbABC[grep('^3100', ptbABC$locatie.EAG),]
  # ggplot(ptbABC, aes(x= nLoc, y = GammaDiv))+
  #   geom_jitter() +
  #   #facet_grid(.~diversiteitsindex)+
  #   theme_minimal()+
  #   theme(
  #     strip.background = element_blank(),
  #     strip.text.x = element_text(size = 6), #EAG
  #     strip.text.y = element_text(size = 5), #EKR
  #     axis.text.x = element_text(size= 5, vjust=2),
  #     axis.text.y = element_text(size= 5, hjust=2),
  #     axis.ticks =  element_line(colour = "black"), 
  #     panel.background = element_blank(), 
  #     plot.background = element_blank()
  #   )+
  #   #coord_flip()+
  #   ggtitle( "Biodiversiteit aan de hand van soortensamenstelling vegetatie") +
  #   labs(x="",y="")
}
plotmcftsrt <- function(hybi1){
  ptbLocSubms <- dcast(hybi1,locatie.EAG+jaar ~ .,lengthUnique,value.var=c("locatiecode"), subset = .(parametercode == 'SUBMSPTN'))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
  ptbLocSubms$nLoc <- ptbLocSubms$.# aantal vegetatielocatie in EAG
  
  ptbFreq <- dcast(hybi1,locatie.EAG+jaar+TWN.naam+TWN.taxongroup+WNA.nederlandse.soortnaam ~ .,lengthUnique,value.var = "locatiecode", subset = .(WNA.onderwaterplantensoorten == 1))
  ptbbed <- dcast(hybi1,locatie.EAG+jaar+TWN.naam+TWN.taxongroup+WNA.nederlandse.soortnaam ~ .,mean,value.var = "meetwaarde", subset = .(WNA.onderwaterplantensoorten == 1 & grootheid == 'BEDKG'))
  
  ptbFreq$Freq <- ptbFreq$.# aantal UNIEKE locatie met taxa X in EAG
  ptbbed$Bed <- ptbbed$.
  
  ptbAbun <- merge(ptbFreq,ptbLocSubms[,c('locatie.EAG','jaar','nLoc')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = TRUE) 
  ptbAbun$percentageLocatiesWaarAangetroffen <- 100*ptbAbun$Freq/ptbAbun$nLoc
  ptbAbun$percentageLocatiesWaarAangetroffen[is.na(ptbAbun$percentageLocatiesWaarAangetroffen)] <- 0
    
  ptbAbun <- merge(ptbAbun,ptbbed[,c('locatie.EAG','jaar','Bed', 'TWN.naam')], by = c('locatie.EAG','jaar', 'TWN.naam'), all.x = TRUE, all.y = FALSE) 
  ptbAbun %>%
    arrange(desc(TWN.naam))%>%
    arrange(TWN.taxongroup) 
  
  ptbAbun$WNA.nederlandse.soortnaam[ptbAbun$WNA.nederlandse.soortnaam == ""] <-
    ptbAbun$TWN.naam[ptbAbun$WNA.nederlandse.soortnaam == ""]
  
  abn <- 
    ggplot(ptbAbun, aes(x=reorder(WNA.nederlandse.soortnaam, Bed), y= percentageLocatiesWaarAangetroffen, fill = locatie.EAG ))+
    geom_col(position = 'dodge') +
    facet_grid(~jaar, scales = 'fixed')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 5), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90, hjust=0),
      axis.text.y = element_text(size= 5, hjust=1),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    coord_flip()+
    ggtitle( "Soortensamenstelling onderwaterplanten") +
    labs(x="",y="% van locaties gevonden")
  ggplotly(p=abn)
  
}
# macrofauna
plotmafa <- function(mafa,TWNev){
 # mafa <-hybi
  mafa <- mafa[mafa$analysecode == 'MEA',]
  mafa <- merge(TWNev, mafa, by.x = 'taxonname', by.y ='TWN.naam', all.x = FALSE, all.y = TRUE)
  #mafa <- dcast(mafa, TWN.taxongroup+locatie.EAG+datum+jaar~ ., value.var = "meetwaarde", fun.aggregate = sum, drop = TRUE)# som aantallen per groep
  mafa <- dcast(mafa, locatie.EAG+taxongroup+jaar~ ., value.var = "meetwaarde", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  mafa<- mafa[!is.na(mafa$taxongroup) & !mafa$taxongroup == "" & !is.na(mafa$.),]
  
  mafaplot <- ggplot(mafa)+
    geom_bar(aes(x = jaar, y = ., fill = taxongroup), stat= "identity", position = "stack") +
    guides(fill= guide_legend(title='Groepen macrofauna', label.theme = element_text(size = 5), ncol = 2))+
    #scale_x_discrete(position = "bottom") +
    facet_wrap(.~locatie.EAG, scales = 'fixed')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle = 90),
      axis.text.y = element_text(size= 5),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      #plot.background = element_blank()
    )+
    ggtitle( "Aantal waarnemingen per macrofauna taxongroep") +
    labs(x="jaar",y="n")
  return(mafaplot)
}

plotmafa2 <- function(mafa,TWNev){
  mafa <- hybi
  mafa <- mafa[mafa$analysecode == 'MEA',]
  mafa <- merge.data.frame(TWNev, mafa, by.x = 'taxonname', by.y ='TWN.naam', all.x = FALSE, all.y = TRUE)
  # mafa <- dcast(mafa, taxongroup+locatie.EAG+datum+jaar~ ., value.var = "meetwaarde", fun.aggregate = sum, drop = TRUE)# som aantallen per groep
  mafa <- dcast(mafa, monsterident+locatie.KRW.watertype+locatie.KRWmeetpuntlocatie +taxongroup+jaar~ ., value.var = "meetwaarde", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  #mafa <- dcast(mafa, taxongroup+jaar~ ., value.var = "locatie.EAG", fun.aggregate = lengthUnique, drop = TRUE) # gemiddelde van locaties en jaren
  mafa<- mafa[!is.na(mafa$taxongroup) & !mafa$taxongroup == "" & !is.na(mafa$.),]
  #mafa <- dcast(mafa, taxongroup+jaar~ ., value.var = ".", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  mafa <-as.data.table(mafa)
  mafasel <- mafa[,length(unique(jaar))>3, by=locatie.KRWmeetpuntlocatie]
  # mafasel <- mafa[,length(unique(jaar))>3, by=locatie.EAG]
  # mafa <- mafa[ocatie.EAG %in% mafasel$locatie.EAG[mafasel$V1 == TRUE]]
  mafa <- mafa[locatie.KRWmeetpuntlocatie %in% mafasel$locatie.KRWmeetpuntlocatie[mafasel$V1 == TRUE]]
  mafa <- mafa[mafa$locatie.KRW.watertype %in% c('M10','M27','M14','M20'),]
  mafa$meetwaarde <- mafa$.
  mafa <- dcast(mafa, locatie.KRW.watertype+taxongroup+jaar~ ., value.var = "meetwaarde", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  eptindex <- mafa[mafa$taxongroup %in% c("Insecta - Ephemeroptera","Insecta - Remaining","Insecta - Trichoptera"),]
  
  mafaplot <- ggplot(mafa)+
    geom_bar(aes(x = jaar, y = ., fill = taxongroup), stat= "identity", position = "stack") +
    guides(fill= guide_legend(title='Groepen macrofauna', label.theme = element_text(size = 7), ncol = 2))+
    #scale_x_discrete(position = "bottom") +
    # facet_wrap(.~locatie.KRW.watertype, scales = 'fixed')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle = 90),
      axis.text.y = element_text(size= 5),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      #plot.background = element_blank()
    )+
    ggtitle( "Gemiddeld aantal individuen macrofauna per monster") +
    labs(x="jaar",y="n")
  return(mafaplot)
}

# vis
plotvisstand <- function(hybi1){
  #hybi11 <- hybi1[grep('NL11', hybi1$locatiecode),]
  hybi1 <- hybi1[hybi1$eenheid == "kg/ha",]
  if(nrow(hybi1)>1){
  vissum <- dcast(hybi1, jaar+TWN.naam+WNA.nederlandse.soortnaam+locatie.EAG ~ ., sum, value.var = "meetwaarde")
  
  ggplot(vissum[!is.na(vissum$WNA.nederlandse.soortnaam),], aes(x=reorder(WNA.nederlandse.soortnaam, .), y= ., fill = locatie.EAG))+
    geom_col(position = 'dodge') +
    facet_grid(~jaar, scales = 'fixed')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7,hjust=1, vjust = 1), 
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, hjust=1),
      axis.text.y = element_text(size= 5, hjust=1, vjust = 1),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    coord_flip()+
    ggtitle( "Soortensamenstelling vis") +
    labs(x="",y="kg/ha")
  }  
}

# esf1----------
plotPO4veg <- function(hybi1, wq, wqparameter = c('PO4', 'VEC'), hybiparameter = c('SUBMSPTN','FLAB','PTN')){
  hybi1<- hybi1[hybi1$parametercode %in% hybiparameter| hybi1$TWN.naam == 'Nuphar lutea',]
  hybi2 <- dcast(hybi1,locatie.EAG+locatiecode+jaar+locatie.KRW.watertype ~ 
                   parametercode+parameterfractie+TWN.naam, mean, 
                 value.var = c("meetwaarde")) #gemiddelde per EAG+jaar
  hybi2$subms <- hybi2$SUBMSPTN__ - hybi2$FLAB_SUBMS_
  hybi2$subms <-  hybi2$subms -  hybi2$`_SUBMS_Nuphar lutea`
  hybi3 <- hybi2 %>%
    filter(jaar > 2008) %>%
    group_by(locatie.EAG,jaar, locatie.KRW.watertype) %>%
    summarise_at(c('SUBMSPTN__','subms'), mean, na.rm = TRUE)
  
  wq1 <- wq %>%
    filter(fewsparameter %in% wqparameter)%>%
    filter(maand %in% c(4:9) & jaar > 2008) %>%  
    group_by(locatie.EAG,jaar,fewsparameter) %>%
    summarise_at(c('meetwaarde'), mean, na.rm = TRUE)
  
  wq2 <- wq %>%
    filter(fewsparameter %in% wqparameter)%>%
    filter(maand %in% c(4:9) & jaar > 2008) %>%  
    group_by(locatie.EAG, jaar, fewsparameter, locatie.KRW.watertype) %>%
    summarise_at(c('meetwaarde'), median, na.rm = TRUE)
  
  selEAG <- wq2$locatie.EAG[wq2$meetwaarde < 2.22 & wq2$fewsparameter == 'VEC']
  wq2 <- wq2[wq2$locatie.EAG %in% c(selEAG,'4000-EAG-3','4000-EAG-7') ,]
  
  # merge met PO4 gemiddeld zomerhalfjaar per EAG
  wqhybi <- merge(hybi3, wq2, by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  #wqhybi$meetwaarde <- cut(wqhybi$meetwaarde, breaks = quantile(wqhybi$meetwaarde, probs = c(0, 0.05, 0.1, 0.2,0.3,0.4, 0.5,0.6, 0.7, 0.95, 1)))
  wqhybi <- wqhybi[!is.na(wqhybi$locatie.EAG) & !wqhybi$locatie.EAG == "" & !is.na(wqhybi$subms) & !is.na(wqhybi$meetwaarde),]
  wqhybi <- wqhybi[which(wqhybi$subms >= 0),]
  
  p<- ggplot(wqhybi, aes(x= meetwaarde, y= subms, col = locatie.KRW.watertype.x, label = paste0(locatie.EAG, jaar)))+
    geom_jitter() +
    #facet_grid(~jaar, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle(paste0(wqparameter, " versus ", hybiparameter)) +
    labs(x= 'PO4', y= '% submers')
  ggplotly(p=p)  
  
  p<- ggplot(wqhybi, aes(x= meetwaarde, y= subms, col = locatie.KRW.watertype.x, label = paste0(locatie.EAG, jaar)))+
    geom_boxplot() +
    #facet_grid(~jaar, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle(paste0(wqparameter, " versus ", hybiparameter)) +
    labs(x= wqparameter, y= hybiparameter)
  ggplotly(p=p)
  
  wqhybi <- merge(hybi3, wq1, by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  wqhybi <- wqhybi[!is.na(wqhybi$locatie.EAG) & !wqhybi$locatie.EAG == "" & !is.na(wqhybi$subms) & !is.na(wqhybi$meetwaarde),]
  wqhybi <- wqhybi[which(wqhybi$subms >= 0),]
  wqhybi <- wqhybi[which(wqhybi$meetwaarde >= 0),]
  
}
plotPO4vegPerc <- function(hybi, wq, wqparameter = 'PO4', hybiparameter = c('SUBMSPTN','FLAB')){
  hybi1<- hybi[hybi$parametercode %in% hybiparameter,]
  hybi2 <- dcast(hybi1,locatie.EAG+locatiecode+jaar+locatie.KRW.watertype ~ parametercode+parameterfractie, mean, 
                 value.var = c("meetwaarde")) #gemiddelde per EAG+jaar
  hybi2$subms <- hybi2$SUBMSPTN_ - hybi2$FLAB_SUBMS
  hybi2 <- hybi2[!is.na(hybi2$locatie.EAG) & !hybi2$locatie.EAG == "" & !is.na(hybi2$subms),]
  hybi3 <- hybi2 %>%  
    group_by(locatie.EAG, jaar, locatie.KRW.watertype) %>%
    summarise(percentiel95 = quantile(subms, 0.95))
  
  wq1 <- wq %>%
    filter(fewsparameter %in% wqparameter)%>%
    filter(maand %in% c(4:9)) %>%  
    group_by(locatie.EAG,jaar) %>%
    summarise_at(c('meetwaarde'), mean, na.rm = TRUE)
  
  # merge met PO4 gemiddeld zomerhalfjaar per EAG
  wqhybi <- merge(hybi3, wq1, by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  wqhybi$meetwaarde <- cut(wqhybi$meetwaarde, breaks = quantile(wqhybi$meetwaarde, probs = c(0, 0.05, 0.1, 0.2,0.3,0.4, 0.5,0.6, 0.7, 0.95, 1)))
  wqhybi <- wqhybi[!is.na(wqhybi$locatie.EAG) & !wqhybi$locatie.EAG == "" & !is.na(wqhybi$percentiel95) & !is.na(wqhybi$meetwaarde),]
  wqhybi <- wqhybi[which(wqhybi$percentiel95 >= 0),]
  
  p<- ggplot(wqhybi, aes(x= meetwaarde, y= percentiel95, col = locatie.KRW.watertype, 
                         text = sprintf("EAG: %s, <br> %s",locatie.EAG, jaar)))+
    geom_jitter() +
    #facet_grid(~jaar, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle(paste0(wqparameter, " versus ", hybiparameter)) +
    labs(x= wqparameter, y= hybiparameter)
  ggplotly(p=p)  
  
  p<- ggplot(wqhybi, aes(x= meetwaarde, y= subms, col = locatie.KRW.watertype,  text = sprintf("EAG: %s, <br> %s",locatie.EAG, jaar)))+
    geom_boxplot() +
    #facet_grid(~jaar, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle(paste0(wqparameter, " versus ", hybiparameter)) +
    labs(x= wqparameter, y= hybiparameter)
  ggplotly(p=p)
  
  wqhybi <- merge(hybi3, wq1, by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  wqhybi <- wqhybi[!is.na(wqhybi$locatie.EAG) & !wqhybi$locatie.EAG == "" & !is.na(wqhybi$subms) & !is.na(wqhybi$meetwaarde),]
  wqhybi <- wqhybi[which(wqhybi$subms >= 0),]
  wqhybi <- wqhybi[which(wqhybi$meetwaarde >= 0),]
  
  #HOF curve
  #unique(wqhybi$locatie.KRW.watertype))
  wt <- 'M6a'
  wqhybi1 <- wqhybi[wqhybi$locatie.KRW.watertype == wt,]
  mh2 <- HOF(as.vector(wqhybi1$subms), as.vector(wqhybi1$meetwaarde), y.name="hoi", family=gaussian, lim=100,
             bootstrap=100, test=c('AICc', 'BIC', 'AIC','Dev'),
             modeltypes=eHOF.modelnames)
  plot(mh2, para=TRUE, xlab="PO4", xlim=c(0,1),
       ylab="Total submerged vegetation coverage (%)", main = wt)
  #abline(h = 30)
  
}

waterbalansmaand <- function(dat){
  if(nrow(dat) > 0) {
    waterbalansplot <- function(dat2){
      dat2 <- as.data.frame(do.call('rbind', dat2))
      d <- dat2 %>%
        dplyr::select(jaar, pol, w_maalstaat, w_sluitfout, starts_with('w_i_'), starts_with('w_o_')) %>%
        dplyr::select(-w_i_inlaat5, -w_o_uitlaat5) %>%
        mutate(
          w_o_intrek     = -w_o_intrek,
          w_o_uitlaat1   = -w_o_uitlaat1,
          w_o_uitlaat2   = -w_o_uitlaat2,
          w_o_uitlaat3   = -w_o_uitlaat3,
          w_o_uitlaat4   = -w_o_uitlaat4,
          w_o_verdamping = -w_o_verdamping,
          w_o_wegzijging   = -w_o_wegzijging
        )
      
     colPP <-  c("green4","darkorange", "darkred", "red", "red1",  "red2", "brown",  "blue", "darkgrey",  "darkgreen", "grey","yellow", "green4", "orchid1", "cyan", "deeppink1", "darkturquoise", "blue","brown", "black" )    
      
     plot <- tidyr::gather(d,
                            d %>% dplyr::select(-jaar, -pol) %>% names(),
                            key = 'source',
                            value = 'value') %>%
        
        ggplot(aes(x = jaar, y = value, fill = source)) +
        geom_bar(stat = 'identity') +
        xlab('Datum') + ylab('Debiet [m3/dag]') +
        ggtitle(paste0('Water in- en uitstroming in ', d$pol)) +
        theme_classic() +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values = colPP)
    
    ggplotly(plot)  
    }
    
    dat2 <- dat %>% group_by(pol, jaar) %>%
      summarise_all(mean)
    
    a = split(dat2,list(dat2$pol),drop=TRUE)
    samples <- lapply(1:length(a), function(i) waterbalansplot(a[i]))
    
    combineWidgets(
      nrow = length(samples),
      list = samples
    )
  }
}

pbalansjaar <- function(dat){
  if(nrow(dat) > 0) {
    fosforbalansplot <- function(dat1){
      dat1 <- as.data.frame(do.call('rbind', dat1))
      if(is.na(dat1$wp_meting_mgm2d)){
        dat1$wp_meting_mgm2d <- 0 }
      
      colP <-  c("green4","darkorange", "darkred", "red", "red1",  "red2","red3", "brown",  "blue", "black","darkgreen", "grey") 
      colP1 <- adjustcolor(colP, alpha.f = 0.2)
      colP <- paste(c(colP1, "yellow",colP))
      
      d <- dat1 %>%
        dplyr::select(jaar, pol, wp_meting_mgm2d,
                      starts_with('wp_'),
                      -one_of(c('wp_min_sum', 'wp_tot_sum', 'wp_inc_sum','wp_meting_gm3'))) %>%
        mutate(wp_meting_mgm2d = -wp_meting_mgm2d)
      
      plot <- gather(d,
                     d %>% dplyr::select(-jaar, -pol) %>% names(),
                     key = 'source',
                     value = 'value') %>%
        ggplot(aes(x = jaar, y = value, fill = source)) +
        geom_bar(stat = 'identity') +
        xlab('Datum') + ylab('P Belasting [mg P/m2/dag]') +
        ggtitle(paste0('Fosfaat belasting watersysteem',d$pol)) +
        theme_classic() +
        theme(legend.title = element_blank()) +
        theme(axis.text.x = element_text(angle = 45))+
        scale_fill_manual(values =colP)
      
      ggplotly(plot)  
    }
    
    pbalansinv <- dat %>% group_by(pol, jaar) %>%
      summarise_all(mean)
    a = split(pbalansinv,list(pbalansinv$pol),drop=TRUE)
    samples <- lapply(1:length(a), function(i) fosforbalansplot(a[i]))
    
    combineWidgets(
      nrow = length(samples),
      list = samples
    )
  }
}

# esf waterdiepte----------------------------
diepte<- function (hybi){
  # hybi2 <-hybi1[hybi1$jaar >= '2018' & hybi1$jaar < '2021',]
  if(nrow(hybi)>0){
  b = dcast(hybi,locatie.EAG+jaar ~ fewsparameter, 
            value.var = "meetwaarde", fun.aggregate = median, na.rm =TRUE, fill = NaN)
  #median(b$WATDTE_m[b$jaar > 2015 & b$locatie.KRW.watertype %in% c('M1a','')], na.rm =T)
  if(!is.null(b$WATDTE_m)){
  c = dcast(hybi,locatiecode+jaar+xcoormonster+ycoormonster ~ fewsparameter, 
            value.var = "meetwaarde", fun.aggregate = median, na.rm =TRUE, fill = NaN)
   
  
  b$watdtefac <- cut(b$WATDTE_m, breaks = c('0','0.1','0.2','0.3','0.4','0.6','3.0','20'))
  c$watdtefac <- cut(c$WATDTE_m, breaks = c('0','0.1','0.2','0.3','0.4','0.6','3.0','20'))
  col <- c('1'="darkred",'2'="red", '3'="orange",'4'="yellow",'5'="deepskyblue", '6'= 'blue', '7'='darkblue')
  labels <- c('1'="0-0.1",'2'="0.1-0.2" ,'3'="0.2-0.3",'4'="0.3-0.4",'5'="0.4-0.6",'6' = '0.6-3.0','7'='3.0-20')
  
  
  map <- sp::merge(gEAG, b[, c('watdtefac','WATDTE_m','jaar','locatie.EAG')], 
                   by.x = 'GAFIDENT', by.y =
                     'locatie.EAG', all.x = FALSE, duplicateGeoms = T)
  pal <- colorFactor(palette = col,  domain = map$watdtefac)
  
  c<- c[!is.na(c$xcoormonster) & !c$xcoormonster == '' & !c$xcoormonster == 0,]
  selc <-
    spTransform(SpatialPointsDataFrame(coords=c[,c("xcoormonster","ycoormonster")],
                                       data=c, proj4string=proj4.rd),
                CRSobj=proj4.google)
  selc <-as.data.frame(selc)
  pal1 <- colorFactor(palette = col,  domain = c$watdtefac)
  
  selc <- selc[order(selc$jaar),]
  map <- map[order(map$jaar),]
  
  leaflet() %>%
    addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("Naam ecologisch analyse gebied", map$GAFNAAM, "<br>",
                                                                 "Diepte in m:", map$WATDTE_m, "<br>",
                                                                 "Meetjaar:", map$jaar), 
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(watdtefac), fillOpacity = 0.6) %>%
    addCircles(data = selc, ~xcoormonster.1, ~ycoormonster.1, popup= paste("Meetlocatiecode", selc$locatiecode, "<br>",
                                                                           "Diepte in meter:", selc$WATDTE_m, "<br>",
                                                                            "Meetjaar:", selc$jaar), 
               weight = 3, radius=40, fillOpacity = 0.8, color = ~pal1(watdtefac)) %>%
    addLegend("bottomright", colors=col, labels=labels, title = unique(map$jaar))%>%
    addTiles()
}}
}
diepte1<- function (hybi){
    if(nrow(hybi)>0){ 
    b = dcast(hybi,locatie.EAG+jaar ~ fewsparameter, 
              value.var = "meetwaarde", fun.aggregate = median, na.rm =TRUE, fill = NaN)
    if(!is.null(b$WATDTE_m)){
    b$watdtefac <- cut(b$WATDTE_m, breaks = c('0','0.1','0.2','0.3','0.4','0.6','3.0','20'))
    col <- c('1'="darkred",'2'="red", '3'="orange",'4'="yellow",'5'="deepskyblue", '6'= 'blue', '7'='darkblue')
    labels <- c('1'="0-0.1",'2'="0.1-0.2" ,'3'="0.2-0.3",'4'="0.3-0.4",'5'="0.4-0.6",'6' = '0.6-3.0','7'='3.0-20')
    
    map <- sp::merge(gEAG, b[, c('watdtefac','WATDTE_m','jaar','locatie.EAG')], 
                     by.x = 'GAFIDENT', by.y =
                       'locatie.EAG', all.x = FALSE, duplicateGeoms = T)
    pal <- colorFactor(palette = col,  domain = map$watdtefac)
    map <- map[order(map$jaar),]
    
    leaflet() %>%
      addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                                   "Diepte:", map$WATDTE_m, "<br>",
                                                                   "jaar:", map$jaar), 
                  stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                  fill=T, fillColor = ~pal(map$watdtefac), fillOpacity = 0.6) %>%
      addLegend("bottomright", colors=col, labels=labels, title = "")%>%
      addTiles()
    }}
}
diepteVegetatie <- function (hybi, hybiparameter = c('SUBMSPTN','FLAB', 'WATDTE','ZICHT')){
  #boxplotje plot maken
  hybi1<- hybi[hybi$parametercode %in% hybiparameter| hybi$TWN.naam == 'Nuphar lutea',]
  hybi2 <- dcast.data.table(hybi1,locatie.EAG+locatiecode+jaar+locatie.KRW.watertype ~ 
                   parametercode+parameterfractie+TWN.naam, mean, 
                 value.var = c("meetwaarde")) #gemiddelde per EAG+jaar
 
  hybi2$subms <- hybi2$SUBMSPTN__ - hybi2$FLAB_SUBMS_
  hybi2$subms <- hybi2$subms -  hybi2$`_SUBMS_Nuphar lutea`
  hybi2$subms[hybi2$subms < 0] = 0
  
  hybi2$DTEZICHT <- ifelse(hybi2$ZICHT__NA/hybi2$WATDTE__NA > 1, NaN, hybi2$ZICHT__NA/hybi2$WATDTE__NA)
  hybi2$DTEZICHT <- as.numeric(hybi2$DTEZICHT)
  hybi2$DTEZICHTfac <- cut(hybi2$DTEZICHT, breaks = c('0.1','0.2','0.3','0.4','0.6', '0.8','1.0'))
  hybi3 <- hybi2[!is.na(hybi2$locatie.EAG) & !is.na(hybi2$locatie.KRW.watertype) & !is.na(hybi2$subms) & !is.na(hybi2$DTEZICHTfac),]
  
  hybi4 <- hybi3 %>%  
    group_by(locatie.EAG, jaar, locatie.KRW.watertype) %>%
    summarise(waterdiepte = mean(WATDTE__NA), submers = mean(subms))
  
  hybi4 <- hybi4[!is.na(hybi4$locatie.KRW.watertype) & !hybi4$locatie.KRW.watertype == "" & !is.na(hybi4$submers) & !is.na(hybi4$waterdiepte),]
  #hybi4$waterdiepte <- cut(hybi4$waterdiepte, breaks = quantile(hybi4$waterdiepte, probs = c(0, 0.05, 0.1, 0.2,0.3,0.4, 0.5,0.6, 0.7, 0.95, 1)), na.rm = T)
  #hybi4 <- hybi4[which(hybi4$submers >= 0),]
  
  p<- ggplot(hybi4, aes(x= waterdiepte, y= submers, col = locatie.KRW.watertype, 
                        text = sprintf("EAG: %s, <br> %s",locatie.EAG, jaar)))+
    geom_jitter() +
    scale_x_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,1,1.5,2.0,2.5,3.0))+
    #facet_grid(~jaar, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle(paste0("Gemiddeld gemeten waterdiepte versus bedekking onderwaterplanten")) +
    labs(x= 'waterdiepte' , y= 'submerse bedekking - submerse fractie draadalgen en gele plomp')
  
  ggplotly(p=p)
} 

diepteVegetatiemp <- function (hybi, hybiparameter = c('SUBMSPTN','FLAB', 'WATDTE','ZICHT'), watertype = c('M10','M1a','M8','M3')){
  #boxplotje plot maken
  hybi1<- hybi[hybi$parametercode %in% hybiparameter| hybi$TWN.naam == 'Nuphar lutea',]
  hybi1 <- hybi[hybi$locatie.KRW.watertype %in% watertype,]
  hybi2 <- dcast.data.table(hybi1,locatie.EAG+locatiecode+jaar+locatie.KRW.watertype ~ 
                              parametercode+parameterfractie+TWN.naam, mean, 
                            value.var = c("meetwaarde")) #gemiddelde per EAG+jaar
  
  hybi2$subms <- hybi2$SUBMSPTN__ - hybi2$FLAB_SUBMS_
  hybi2$subms <- hybi2$subms -  hybi2$`_SUBMS_Nuphar lutea`
  hybi2$subms[hybi2$subms < 0] = 0
  
  hybi2$DTEZICHT <- ifelse(hybi2$ZICHT__NA/hybi2$WATDTE__NA > 1, NaN, hybi2$ZICHT__NA/hybi2$WATDTE__NA)
  hybi2$DTEZICHT <- as.numeric(hybi2$DTEZICHT)
  hybi2$DTEZICHTfac <- cut(hybi2$DTEZICHT, breaks = c('0.1','0.2','0.3','0.4','0.6', '0.8','1.0'))
  hybi3 <- hybi2[!is.na(hybi2$locatie.EAG) & !is.na(hybi2$locatie.KRW.watertype) & !is.na(hybi2$subms) & !is.na(hybi2$DTEZICHTfac),]
  
  p<- ggplot(hybi3, aes(x= WATDTE__NA, y= subms, col = DTEZICHTfac, 
                        text = sprintf("EAG: %s, <br> %s",locatie.EAG, locatiecode, jaar)))+
    geom_jitter() +
    scale_x_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,1,1.5,2.0,2.5,3.0))+
    facet_grid(~locatie.KRW.watertype, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 6), #EKR
      axis.text.x = element_text(size= 6, angle = 90),
      axis.text.y = element_text(size= 6),
      axis.title = element_text(size=8),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    guides(col=guide_legend(title='zicht/ diepte'), size = "legend")+
    ggtitle(paste0("Gemiddeld gemeten waterdiepte versus bedekking onderwaterplanten")) +
    labs(x= 'waterdiepte' , y= 'submerse bedekking')
  
  
  ggplotly(p=p)%>% 
    layout_ggplotly
}

slibdikte <- function (hybi){
  if(nrow(hybi)>0){ 
  c = dcast(hybi,locatiecode+jaar+xcoormonster+ycoormonster ~ fewsparameter, 
            value.var = "meetwaarde", fun.aggregate = mean, na.rm =TRUE, fill = NaN)
  if(!is.null(c$SLIBDTE_m)){
  c$slibdtefac <- cut(c$SLIBDTE_m, breaks = c('0','0.05','0.1','0.15','0.2','0.25','0.3','0.4','0.5','2'))
  
  colsl <- c('1'='darkblue','2'= 'blue','3'="deepskyblue",'4'="yellow",'5'="orange",'6'='salmon','7'="red",'8'="darkred",'9'='black')
  labelssl <- c('1'="0-0.05",'2'="0.05-0.1" ,'3'="0.1-0.15",'4'="0.15-0.2",'5'="0.2-0.25",'6' = '0.25-0.30','7'='0.3-0.4','8'='0.4-0.5','9'='>0.5')
 
  c<- c[!is.na(c$xcoormonster) & !c$xcoormonster == '' & !c$xcoormonster == 0,]
  selc <-
    spTransform(SpatialPointsDataFrame(coords=c[,c("xcoormonster","ycoormonster")],
                                       data=c, proj4string=proj4.rd),
                CRSobj=proj4.google)
  selc <-as.data.frame(selc)
  pal1 <- colorFactor(palette = colsl,  domain = c$slibdtefac)
  
  selc <- selc[order(selc$jaar),]
  
  leaflet() %>%
    
    addCircles(data = selc, ~xcoormonster.1, ~ycoormonster.1, popup= paste("Meetlocatiecode", selc$locatiecode, "<br>",
                                                                           "Slibdikte in meter:", selc$SLIBDTE_m, "<br>",
                                                                           "Meetjaar:", selc$jaar), 
               weight = 3, radius=40, fillOpacity = 0.8, color = ~pal1(slibdtefac)) %>%
    addLegend("bottomright", colors=colsl, labels=labelssl, title = unique(selc$jaar))%>%
    addTiles()
  }}
}

# esf 2-----------------------------
diepteZicht<- function (hybi){
  b = dcast(hybi, locatiecode+locatie.EAG+jaar+locatie.KRW.watertype+xcoormonster+ycoormonster ~ fewsparameter, 
            value.var = "meetwaarde", fun.aggregate = mean, na.rm =TRUE, fill = NaN)
  b = b[!b$locatie.KRW.watertype %in% c('M20'),]
  b$DTEZICHT <- ifelse(b$ZICHT_m/b$WATDTE_m > 1, NaN, b$ZICHT_m/b$WATDTE_m)
  b$DTEZICHTfac <- cut(b$DTEZICHT, breaks = c('0.1','0.2','0.3','0.4','0.6', '0.8','1.0'))
  b <- b[!is.na(b$DTEZICHTfac) & !is.na(b$jaar) &!is.na(b$locatie.EAG),]
 
  b <-
    spTransform(SpatialPointsDataFrame(coords=b[,c("xcoormonster","ycoormonster")],
                                       data=b, proj4string=proj4.rd), CRSobj=proj4.google)
  b <-as.data.frame(b)
  b <- b[order(b$jaar),]
 
  c = b %>%
    group_by(locatie.EAG,locatie.KRW.watertype,jaar) %>% 
    summarize_if(is.numeric,median,na.rm =TRUE)
  c$DTEZICHTfac <- cut(c$DTEZICHT, breaks = c('0.1','0.2','0.3','0.4','0.6', '0.8','1.0'))
  #return(c)
  col <- c('1'="darkred",'2'="red", '3'="orange",'4'="yellow",'5'="deepskyblue", '6'= 'blue')
  labels <- c('1'="0-0.1",'2'="0.1-0.2" ,'3'="0.2-0.4",'4'="0.4-0.6",'5'="0.6-0.8",'6' = '0.8-1.0')
  #pal <- colorFactor(palette = col,  domain = b$DTEZICHTfac)
  pal1 <- colorFactor(palette = col,  domain = b$DTEZICHTfac)
  pal2 <- colorFactor(palette = col,  domain = c$DTEZICHTfac)
  
  map <- sp::merge(gEAG, c[, c('DTEZICHT','DTEZICHTfac','ZICHT_m','WATDTE_m','jaar','locatie.EAG')], 
                   by.x = 'GAFIDENT', by.y =
                     'locatie.EAG', all.x = FALSE, all.y = TRUE, duplicateGeoms = T)
  
  leaflet(map) %>%
    addPolygons(layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                     "Diepte:", map$WATDTE_m, "<br>",
                                                     "Doorzicht:", map$ZICHT_m, "<br>",
                                                     "jaar:", map$jaar, "<br>",
                                                     "Diepte/doorzicht:", map$DTEZICHT),
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal2(DTEZICHTfac), fillOpacity = 0.6) %>%
    addCircles(data = b, ~xcoormonster.1, ~ycoormonster.1, popup= paste("Meetlocatiecode", b$locatiecode, "<br>",
                                                                         "Diepte in meter:", b$WATDTE_m, "<br>",
                                                                        "Doorzicht:", b$ZICHT_m, "<br>",
                                                                         "Meetjaar:", b$jaar), 
               weight = 3, radius=40, fillOpacity = 0.8, color = ~pal1(DTEZICHTfac)) %>%
    addLegend("bottomright", colors=col, labels=labels, title = "")%>%
    addTiles()
  
  # zw <- 
  #   ggplot(b, aes(x = reorder(locatie.EAG, -DTEZICHT), y = DTEZICHT)) +
  #   geom_bar(stat ='identity')+
  #   facet_wrap(.~jaar)+
  #   theme_minimal()+
  #   theme(
  #     strip.background = element_blank(),
  #     strip.text.x = element_text(size = 6), #EAG
  #     strip.text.y = element_text(size = 5), #EKR
  #     axis.text.x = element_text(size= 5,angle=90,hjust=1),
  #     axis.text.y = element_text(size= 5, hjust=2),
  #     axis.ticks =  element_line(colour = "black"), 
  #     panel.background = element_blank(), 
  #     plot.background = element_blank()
  #   )+
  #   theme(legend.position='none')+
  #   ggtitle(paste0( "diepte zicht")) +
  #   labs(x = "", y="")
  # ggplotly(p=zw) 
  
}
extinctie1 <- function(wq, parameter = c('VEC')){
  # diepte4licht <- log(25)/1.2
  wq1<- wq[wq$fewsparameter %in% parameter,]
  wq1 <- wq1[!is.na(wq1$locatie.KRW.watertype) & !is.na(wq1$locatie.EAG) & !wq1$locatie.EAG == '',]
  wq1 <- wq1[wq1$meetwaarde > 0,]
  
  p<- ggplot(wq1, aes(x= locatie.EAG, y= meetwaarde, col = locatie.KRW.watertype,  text = sprintf("EAG: %s, <br> %s",locatie.EAG, jaar)))+
    geom_boxplot() +
    geom_hline(aes(yintercept = 3.22, col = '4% licht op 1 meter'), show.legend = T)+ #vec voor 1 meter >4%
    geom_hline(aes(yintercept = 0.805, col = '4% licht op 4 meter'), show.legend = T)+ #vec voor 4 meter >4%
    geom_hline(aes(yintercept = 0.46, col = '4% licht op 7 meter'), show.legend = T)+ #vec voor 7 meter 4%
    #facet_grid(~jaar, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle('') +
    labs(x= 'Ecologisch analysegebied', y = parameter)
  ggplotly(p=p)
  
}

lichtopbodem <- function (r, wq, waterpeil){
 
    diepteVoorjaarGem <- r - mean(waterpeil$Meetwaarde[waterpeil$voorjaar == 'T']) # gemiddeld peil in voorjaar
    
    extinctie <- wq[wq$fewsparameter == 'VEC' & wq$jaar > 2014,]
    extinctie <- extinctie[extinctie$meetwaarde > 0, ] # meetwaarde -1 verwijderen
    #extinctie$voorjaar <- "" # seizoenen toevoegen
    extinctie$voorjaar[as.numeric(format(extinctie$datum, "%m")) > 3 & as.numeric(format(extinctie$datum, "%m")) < 6] <- 'T'
    vecVoorjaar <- mean(extinctie$meetwaarde[extinctie$voorjaar == 'T'], na.rm = TRUE) #gemiddelde vec
    
    percentageOppervlakteLicht <-  100*exp((diepteVoorjaarGem * vecVoorjaar)) # gemiddeld scenario
    
    # omzetten voor ggplotbest
    maplp <- rasterToPoints(percentageOppervlakteLicht)
    dfb <- data.frame(maplp)
    dfb[,3][dfb[,3]>20] = 20
    dfb[,3][dfb[,3]< 0] = NA
    
    ggplot(data=dfb, aes(y=y, x=x), col = dfb[,3]) +
      geom_raster(aes(fill=dfb[,3])) +
      theme_bw() +
      coord_equal() +
      scale_fill_gradientn(name = "0 - >20%", limits= c(0, 20), 
                           breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20),
                           values = NULL, space = "Lab",na.value = "grey50", 
                           guide = "colourbar", colors = rev(rainbow(12))) +
      ggtitle(paste("%licht op de bodem")) +
      labs(x=" ",y=" ") +
      theme(
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(0.5, "cm") )
  
}

dieptegrid <- function(r, waterpeil){
  
    ### waterpeil inlezen # benodigde kolommen: Datum, Meetwaarde, Meetpunt.ID
    
    diepteVoorjaarGem <- r - mean(waterpeil$Meetwaarde[waterpeil$voorjaar == 'T' ]) # gemiddeld peil in voorjaar
    meddiepte <- quantile(diepteVoorjaarGem,0.5)
    
    # omzetten voor ggplotbest
    maplp <- rasterToPoints(diepteVoorjaarGem)
    dfb <- data.frame(maplp)
    dfb[,3][dfb[,3]< -30] = -30
    dfb <- dfb[!dfb[,3]> 0,]
    dfb <- dfb[!is.na(dfb[,3]),]
    
    #histogram waterdiepte per eag
    ggplot(data=dfb, aes(dfb[,3])) +
      geom_histogram(bins = 30, na.rm = TRUE) +
      ggtitle(paste("Distributie van Waterdiepten", names(r))) +
      labs(x="Waterdiepte (m)",y="oppervlak (m2)") 
    ggsave(file = paste(names(r), "waterdiepte_histogram", ".png"))
    
    
    breaks <- as.vector(round(quantile(dfb[,3], c(1,0.95, 0.85,0.75,0.50,0.25,0.10,0)), digits = 2))
    
    ggplot(data=dfb, aes(y=y, x=x), col = dfb[,3]) +
      geom_raster(aes(fill=dfb[,3])) +
      theme_bw() +
      coord_equal() +
      scale_fill_gradientn(name = paste0(round(max(dfb[,3]), digits = 0)," - ",round(min(dfb[,3]))," m"), 
                           limits= c(round(min(dfb[,3]), digits = 0), round(max(dfb[,3]))),
                           breaks= breaks,
                           values = NULL, space = "Lab", na.value = "grey50", 
                           guide = "colourbar", colours = rainbow(12)) +
      ggtitle(paste("waterdiepte in meter")) +
      labs(x=" ",y=" ") +
      theme(
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(0.5, "cm"))
    
    #ggsave(file = paste(names(r), "waterdiepte_kaart", ".png"))  
}

# esf3 bodem ----------------------------------------------------
bodsam <- function(bod){
  selb <- dcast(bod, locatie.EAG+locatiecode+locatie.omschrijving+locatie.x+locatie.y+locatie.z+datum+jaar ~ fewsparameter+compartiment, value.var = "meetwaarde", fun.aggregate = mean)
  selb$FESPFWratio <-((selb$`Fe_mg/l_ng_BS`/55.845)-(selb$`Stot_mg/l_ng_BS`/32.065))/(selb$`Ptot_mgP/l_ng_BS`/30.974)
  selb$FESPDWratio <-((selb$`Fe_mg/kg_dg_BS`/55.845)-(selb$`Stot_mg/kg_dg_BS`/32.065))/((selb$`Ptot_gP/kg_dg_BS`*1000)/30.974)
  #selb$`Ptot_mgP/l_nf_PW`<- selb$`Ptot_mgP/l_PW`
  
  if(is.null(selb$`Stot_mg/l_nf_PW`)){
    if(!is.null(selb$`SO4_mg/l_PW`)){
      selb$FESPPWratio <-(((selb$`Fe_ug/l_nf_PW`/1000)/55.845)-(selb$`SO4_mg/l_PW`/96.06))/(selb$`Ptot_mgP/l_nf_PW`/30.974)
    }
    if(!is.null(selb$`Stot_mg/l_PW`)){
      selb$FESPPWratio <-(((selb$`Fe_ug/l_nf_PW`/1000)/55.845)-(selb$`Stot_mg/l_PW`/32.06))/(selb$`Ptot_mgP/l_nf_PW`/30.974)
    }}
  if(!is.null(selb$`Stot_mg/l_nf_PW`)){  
    selb$FESPPWratio <-(((selb$`Fe_ug/l_nf_PW`/1000)/55.845)-(selb$`Stot_mg/l_nf_PW`/32.065))/(selb$`Ptot_mgP/l_nf_PW`/30.974)
  }
  selb$nlvrFW <- 0.0247*selb$`Ptot_mgP/l_ng_BS`-1.6035
  selb$nlvrDW <- 0.0077*(selb$`Ptot_gP/kg_dg_BS`*1000)-4.7259
  
  selb <- selb[!is.na(selb$FESPFWratio) ,]
  selb$FESPFWratio <-cut(selb$FESPFWratio, breaks= c((min(selb$FESPFWratio)-1), 1.4, 4, max(selb$FESPFWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))
  selb <- selb[!is.na(selb$FESPDWratio) ,]
  selb$FESPDWratio <-cut(selb$FESPDWratio, breaks= c((min(selb$FESPDWratio)-1), 1.4, 4, max(selb$FESPDWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))
  if(!is.null(selb$FESPPWratio)){
    selb$nlvrPW <- 0.8095*selb$`Ptot_mgP/l_nf_PW`-0.2905
    #write.table(selb, file = paste(getwd(),"baggernutQuickscan",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
    selb <- selb[!is.na(selb$FESPPWratio) ,]
    selb$FESPPWratio <-cut(selb$FESPPWratio, breaks= c((min(selb$FESPPWratio)-1), 1.4, 4, max(selb$FESPPWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))
    }
  return(selb)
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
  
  plotFW <- ggplot(selb, aes(x= loc.eag, y= nlvrFW, fill = classFESPFWratio))+
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
    qPW <- ggplot(selb, aes(x= loc.eag, y= nlvrPW, fill = classFESPPWratio))+
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
zwavel <- function(bod1){
  S <- bod1[bod1$parm.fews %in% c('Stot_mg_l_nf','Stot_mg/l'),] %>%
    group_by(loc.eag) %>%
    summarise(s=mean(meetwaarde))
  S$group <- 'grey'
  S$group[S$loc.eag %in% eag] <- "red" 
  
  zw<- ggplot(S, aes(x = reorder(loc.eag, -s), y = s, fill = group)) +
    geom_bar(stat ='identity')+
    scale_fill_manual(values=c('grey','red'))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5,angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    theme(legend.position='none')+
    ggtitle( "Zwavel in poriewater") +
    labs(x = "", y="mg/l")
  ggplotly(p=zw) 
}

bodmap <- function(selb){
  selb <-
    spTransform(SpatialPointsDataFrame(coords=selb[,c("loc.x","loc.y")],
                                       data=selb, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(selb)
  
  col <- c( '1'='blue','2'='deepskyblue','3'='yellow','4'='orange','5'='salmon','6'='red','7'='darkred','8'='grey')
  labels <- c('1'="< 500",'2'="500 - 1000" ,'3'="1000 - 1500",'4'="1500 - 2500",'5'="2500-5000",'6'="5000-10000",'7'="> 10000", '8'= 'niet beschikbaar')
  pal <- colorFactor(palette = col,  domain = gebiedData$klasseP)
  
  
  ktPbod <- 
    leaflet(gebiedData) %>% 
    addCircles(~loc.x.1, ~loc.y.1, popup = paste("locatie", gebiedData$locatiecode, "<br>",
                                                  "Ptot bodem:", gebiedData$Ptot_gP_kg_dg_BS, "g/kgP dg", "<br>",
                                                 "jaar:", gebiedData$jaar),
               weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(klasseP)) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'Ptot mgP/kg DG') %>%
    addTiles()
  
  return(ktPbod)
}

bodmap2 <- function(bod1){
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
  
    
    selb <-
      spTransform(SpatialPointsDataFrame(coords=selb[,c("loc.x","loc.y")],
                                         data=selb, proj4string=proj4.rd),
                  CRSobj=proj4.google)
    selb <-as.data.frame(selb)
    col <- c('3'="blue",'4'="deepskyblue",'5'="yellow",'6'="orange",'7'="salmon",'1'='grey')
    labels <- c('3'="< 0.5",'4'="0.5 - 2.5",'5'="2.5-5",'6'="5-10",'7'="> 10", '1' = 'niet beschikbaar')
    pal <- colorFactor(palette = col,  domain = bod$nlvrPW)
    
    ktnlvP <- leaflet(selb) %>% 
      addCircles(~loc.x.1, ~loc.y.1, popup = paste("locatie", selb$locatiecode, "<br>",
                                                           "nalevering:", selb$nlvrPW, "mg/m2/dag", "<br>", "jaar:", selb$jaar, "<br>",
                                                           "IJzer-Zwavel/Fosfor-ratio:", selb$FESPPWratio),
                 weight = 3, radius=40, fillOpacity = 0.8, color= ~pal(nlvrPW)) %>%
      addLegend("bottomright", colors= col, labels=labels, title = 'Nalevering P obv poriewater mg/m2/dag') %>%
      addTiles()
    return(ktnlvP)
}

# esf 4 --------------------
# functie taxa voorkomen per SO4, Ca, Cl, HCO3
plotkoolstoftax <- function(hybi, taxa = 'Stratiotes aloides'){
  ptbLocSubms <- dcast(hybi,locEAG+jaar ~ .,lengthUnique,value.var=c("locatiecode"), subset = .(parametercode == 'SUBMSPTN'))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
  ptbLocSubms$nLoc <- ptbLocSubms$.# aantal vegetatielocatie in EAG
  
  ptbFreqhot <- dcast(hybi[hybi$TWN.naam == taxa & hybi$parameterfractie == "" & hybi$analysecode == 'PTN',], locEAG+jaar~ .,
                      lengthUnique, value.var = "locatiecode")
  ptbFreqhot$Freq <- ptbFreqhot$. # aantal UNIEKE locatie met taxa X in EAG
  ptbAbunC2 <- merge(ptbFreqhot,ptbLocSubms[,c('locatie.EAG','jaar','nLoc')], by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  ptbAbunC2$percentageLocatiesWaarAangetroffen <- 100*ptbAbunC2$Freq/ptbAbunC2$nLoc
  # namen zijn niet goed weergegeven en legenda met symboolgrootte ontbreekt
  # merge met HCO3 gemiddeld jaar
  car <- dcast(wq[wq$fewsparameter %in% c('HCO3','HCO3-1'),],locatie.EAG+jaar ~ .,mean,value.var=c("meetwaarde"))#aantal unieke locatiecodes per EAG+jaar waar SUBMSPTN is gerapporteerd.
  carhot <- merge(ptbAbunC2,car, by = c('locatie.EAG','jaar'), all.x = FALSE, all.y = FALSE) 
  
  carhot %>%
    arrange(percentageLocatiesWaarAangetroffen)
  
  ggplot(carhot, aes(x= ..y, y= percentageLocatiesWaarAangetroffen))+
    geom_jitter() +
    #facet_grid(~jaar, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    #coord_flip()+
    ggtitle(paste0(taxa, " versus koolstof")) +
    labs(x="bicarbonaat",y="% van locaties gevonden")
  
}
plotbasen <- function(hybi, wq, param = 'CL', grens = 150){
  taxa <- dcast(hybi[hybi$parameterfractie == "" & hybi$analysecode == 'PTN' & 
                       hybi$WNA.onderwaterplantensoorten == '1',], locatie.EAG+jaar+TWN.naam~ .,
                      lengthUnique, value.var = "locatiecode")
  taxabase <- merge(taxa, wq[wq$fewsparameter %in% c('HCO3','HCO3-1','CA-1','CL','SO4'),], by = c('locatie.EAG','jaar'), all = TRUE) 
  
  
  cl <- taxabase[taxabase$fewsparameter == param & !is.na(taxabase$TWN.naam) ,]
  cl <- rbind(cl[grep('^Chara',cl$TWN.naam) ,], cl[grep('^Nitellopsis',cl$TWN.naam) ,],
                               cl[grep('^Nitella',cl$TWN.naam) ,],cl[grep('^Potamogeton',cl$TWN.naam) ,],
              cl[grep('^Tolypella',cl$TWN.naam) ,],cl[grep('^Stratiotes',cl$TWN.naam) ,],cl[grep('^Utricularia',cl$TWN.naam) ,])
  cl %>%
    arrange(meetwaarde)

 
  # alleen kranswieren, fonteinkruiden
  abn <- ggplot(cl, aes(x= reorder(TWN.naam, meetwaarde), y= meetwaarde, text = sprintf("EAG: %s, <br> %s",locatie.EAG, jaar)))+
          geom_boxplot() +
          geom_hline(yintercept = grens)+
          #scale_x_discrete(limits = (levels(cl$TWN.naam)))+
          coord_flip()+
          theme_minimal()+
          theme(
            strip.background = element_blank(),
            strip.text.x = element_text(size = 6), #EAG
            strip.text.y = element_text(size = 5), #EKR
            axis.text.x = element_text(size= 5, angle=90,hjust=1),
            axis.text.y = element_text(size= 5, hjust=2),
            axis.ticks =  element_line(colour = "black"), 
            panel.background = element_blank(), 
            plot.background = element_blank()
          )+
          ggtitle(paste0("Concentraties ", param, "waarbij verschillende taxa waterplanten voorkomen")) +
          labs(x="",y="mg/l")
  ggplotly(p=abn)
}
esf4ph <- function(wq, parameter = c('PH'), grens = 120){
  # diepte4licht <- log(25)/1.2
  wq1<- wq[wq$fewsparameter %in% parameter,]
  wq1 <- wq1[!is.na(wq1$locatie.KRW.watertype) & !is.na(wq$locatie.afaanvoergebied) & !wq1$locatie.afaanvoergebied == '',]
  wq1 <- wq1[wq1$meetwaarde > 0,]
  
  p<- ggplot(wq1, aes(x= reorder(locatie.afaanvoergebied, meetwaarde), y= meetwaarde,  col = locatie.KRW.watertype, text = sprintf("EAG: %s, <br> %s",locatie.EAG, jaar)))+
    geom_boxplot() +
    geom_hline(aes(yintercept = grens, col = 'grenswaarde'), show.legend = T)+ 
    #facet_grid(~locatie.KRW.watertype, scales = 'free')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle=90,hjust=1),
      axis.text.y = element_text(size= 5, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    ggtitle('') +
    labs(x= 'Ecologisch analysegebied', y = parameter)
  ggplotly(p=p)
  
}

# chemie----------------------------
plot_lijn2 <- function(so4, kw, titel = 'Sulfaat en waterpeil in Botshol'){
  so4$datum <- as.Date(so4$datum, format = "%Y-%m-%d %H:%M")
  so4$jaar <- format(so4$datum, '%Y')
  so4$jaar <- as.numeric(so4$jaar)
  
  p1 <- ggplot()+
    geom_point(data= so4, aes(x=datum, y=meetwaarde, col= locatiecode, group = locatiecode))+
    geom_line(data= so4, aes(x=datum, y=meetwaarde, col= locatiecode, group = locatiecode))+
    facet_wrap(.~fewsparameter)+
    theme_minimal()+
    ggtitle(titel)+ ylab('ug/l')+
    theme (axis.text.x =element_text(angle=0, vjust=1)) 
  
  kw1 <- kw[kw$Parameternaam == 'Gemeten waterhoogte',]
  p2 <- ggplot()+
    geom_line(data = kw1, aes(x=datum, y=Meetwaarde))+
    theme_minimal()+
    ggtitle('')+ ylab('m NAP')+
    theme (axis.text.x =element_text(angle=0, vjust=1))
  
    subplot(p1,p2, nrows = 2, widths = NULL, heights = NULL, margin = 0.02,
          shareX = TRUE, shareY = FALSE, which_layout = "merge")
}

plot_lijn3 <- function(wq, kw, titel = 'Inlaatkwaliteit in Botshol'){
kw2 <- kw[kw$Parameternaam == 'Gemeten debiet' & kw$Meetwaarde < 9000,]
kw2$maand <- format(kw2$datum, '%m')
kw2$maand <- as.numeric(kw2$maand)
kw2$jaar <- as.numeric(kw2$jaar)

p <- p[p$Element.Taxon %in% c('P','Ptot') & 
p$Monsterpunt.omschrijving %in% c('RWZI Botshol, effluent','effluent DFI Botshol'),]
p$datum <- as.Date(p$Bem.datum, format = "%Y-%m-%d %H:%M")
p$jaar <- format(p$datum, '%Y')
p$jaar <- as.numeric(p$jaar)
p$maand <- format(p$datum, '%m')
p$maand <- as.numeric(p$maand)


kw3 <- merge(kw2, p, by = c('jaar','maand'), all.x =TRUE, all.y = FALSE)
kw3$Resultaat <- as.numeric(kw3$Resultaat)
kw3$Meetwaarde <- as.numeric(kw3$Meetwaarde)
kw3$vracht <- kw3$Resultaat*kw3$Meetwaarde # g/l mg/100 en m3*1000

p1 <- ggplot()+
  geom_line(data = kw3, aes(x=datum.x, y=Meetwaarde))+
  theme_minimal()+
  ggtitle('Inlaatdebiet, P concentraties effluent en inlaatvracht')+ ylab('m3/dag')+
  theme (axis.text.x =element_text(angle=0, vjust=1))

p2 <- ggplot()+
  geom_point(data = kw3, aes(x=datum.x, y=Resultaat))+
  theme_minimal()+
  ggtitle('')+ ylab('mg/l')+ xlab('datum')+
  theme (axis.text.x =element_text(angle=0, vjust=1))

p3 <- ggplot()+
  geom_line(data = kw3, aes(x=datum.x, y=vracht))+
  theme_minimal()+
  ggtitle('')+ ylab('g/dag')+
  theme (axis.text.x =element_text(angle=0, vjust=1))

subplot(p1,p2,p3, nrows = 3, widths = NULL, heights = NULL, margin = 0.02,
        shareX = TRUE, shareY = FALSE, which_layout = "merge")
}

plot_lijn <- function(z){
  z$datum <- as.Date(z$datum, format = "%Y-%m-%d %H:%M")
  z$jaar <- format(z$datum, '%Y')
  z$jaar <- as.numeric(z$jaar)
 
  # eenheid
  plot<- ggplot(data= z, aes(x=datum, y=meetwaarde, col= locatiecode, group = locatiecode))+
    geom_point()+
    geom_line()+
    scale_x_date(date_breaks = "1 year")+
    facet_wrap(.~fewsparameter)+
    theme_minimal()+
    ggtitle('')+ ylab(unique(z$eenheid))+ xlab('')+
    theme (axis.text.x =element_text(angle=90, vjust=1))   
  ggplotly(p=plot)
}

toxiciteit <- function(simoni){
  
  gebiedData <- simoni
  gebiedData <-
    spTransform(SpatialPointsDataFrame(coords=gebiedData[,c("X","Y")],
                                       data=gebiedData, proj4string=proj4.rd),
                CRSobj=proj4.google)
  gebiedData <-as.data.frame(gebiedData)
  
  '6' -> gebiedData$SIMONIkl[gebiedData$SRI.1.2 > 2]
  '5' -> gebiedData$SIMONIkl[gebiedData$SRI.1.2 <= 2 & gebiedData$SRI.1.2 > 1]
  '4' -> gebiedData$SIMONIkl[gebiedData$SRI.1.2 <= 1 & gebiedData$SRI.1.2 > 0.5]
  '3' -> gebiedData$SIMONIkl[gebiedData$SRI.1.2 <= 0.5 & gebiedData$SRI.1.2 > 0.25]
  '2' -> gebiedData$SIMONIkl[gebiedData$SRI.1.2 <= 0.25 & gebiedData$SRI.1.2 >= 0]
  gebiedData$SIMONIkl <- as.factor(gebiedData$SIMONIkl)
  
  colt <- c('2'="deepskyblue",'3'="blue",'4'="yellow",'5'="red",'6'="darkred")
  labelst <- c('2'="<0.25",'3'="0.25-0.5",'4'="0.5-1.0",'5'="1.0-2.0",'6'="> 2.0")
  pal <- colorFactor(palette = colt,  domain = gebiedData$SIMONIkl)
  rwziData <- gebiedData[gebiedData$bron == 'RWZI',]
  
  gebiedData <- gebiedData[order(gebiedData$jaar),]
  gebiedData$jaar <- as.character(gebiedData$jaar)
  
  toxiciteit <- leaflet() %>% 
    addCircles(data= gebiedData, ~X.1, ~Y.1, popup = paste("locatie:", gebiedData$Locatie, "<br>",
                                          "SRI:", gebiedData$SRI.1.2, "<br>", 
                                          "potentiele bron:", gebiedData$bron, "<br>",
                                          "meetjaar:", gebiedData$jaar),
               weight = 20, radius=80, fillOpacity = 0.8, color= ~pal(gebiedData$SIMONIkl),
               popupOptions = (closeButton = FALSE)) %>%
    addCircleMarkers(data= rwziData, ~X.1, ~Y.1, popup = paste("locatie:", rwziData$Locatie, "<br>",
                                                           "SRI:", rwziData$SRI.1.2, "<br>", 
                                                           "potentiele bron:", rwziData$bron),
               weight = 3, radius=20, fillOpacity = 0.5,   
               stroke = T, fillColor= ~pal(rwziData$SIMONIkl)) %>%
    
    addLegend("bottomright", colors= colt, labels=labelst, title = 'Simoni Risico Index') %>%
    addTiles()
  
  return(toxiciteit)
}

toxiciteit2 <- function(simoni){
  
  gebiedData <- simoni %>%
    # groeperingsfactor
    group_by(EAG) %>% 
    # neem gemiddelde
    summarise_all(mean, na.rm=T)
  
  gebiedData <- sp::merge(gebiedData, gEAG[,c('GAFIDENT', 'GAFNAAM')], 
                   by.y = 'GAFIDENT', by.x =
                     'EAG', all.x = TRUE, all.y = FALSE)
  
  gebiedData <- as.data.frame(gebiedData)
  gebiedData <- gebiedData[!is.na(gebiedData$GAFNAAM),]
  gebiedData$group <- 'grey'
  gebiedData$group[gebiedData$SRI.1.2 > 1] <- "red" 
  
  zw <- 
  ggplot(gebiedData, aes(x = reorder(GAFNAAM, -SRI.1.2), y = SRI.1.2, fill = group)) +
  geom_bar(stat ='identity')+
  scale_fill_manual(values=c('grey','red'))+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 6), #EAG
    strip.text.y = element_text(size = 5), #EKR
    axis.text.x = element_text(size= 5,angle=90,hjust=1),
    axis.text.y = element_text(size= 5, hjust=2),
    axis.ticks =  element_line(colour = "black"), 
    panel.background = element_blank(), 
    plot.background = element_blank()
  )+
  theme(legend.position='none')+
  ggtitle(paste0( "Simoni scores per EAG")) +
  labs(x = "", y="")

  ggplotly(p=zw) 
}

toxiciteit3 <- function(simoni){
  
  gebiedData <- simoni %>%
    # groeperingsfactor
    group_by(WS.code,EAG, jaar) %>% 
    # neem gemiddelde
    summarise_all(mean, na.rm=T)
  
  #gebiedData <- gebiedData[gebiedData$EAG %in% c('2130-EAG-1','2130-EAG-2','2150-EAG-1','2150-EAG-2','2150-EAG-3'),]
  gebiedData$group <- 'grey'
  gebiedData$group[gebiedData$SRI.1.2 > 1] <- "red" 
  
  zw <- 
    ggplot(gebiedData, aes(x = reorder(paste0(WS.code,"-",jaar), -SRI.1.2), y = SRI.1.2, fill = group)) +
    geom_bar(stat ='identity')+
    scale_fill_manual(values=c('grey','red'))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 8), #EAG
      strip.text.y = element_text(size = 8), #EKR
      axis.text.x = element_text(size= 8,angle=90,hjust=1),
      axis.text.y = element_text(size= 8, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    theme(legend.position='none')+
    ggtitle(paste0( "Simoni scores per meetpunt")) +
    labs(x = "", y="")
  
  zw <- 
    ggplot(gebiedData, aes(x = jaar, y = SRI.1.2, col = WS.code)) +
    geom_point()+
    geom_line()+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 8), #EAG
      strip.text.y = element_text(size = 8), #EKR
      axis.text.x = element_text(size= 8,angle=90,hjust=1),
      axis.text.y = element_text(size= 8, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      plot.background = element_blank()
    )+
    theme()+
    ggtitle(paste0( "Simoni scores per meetpunt")) +
    labs(x = "", y="")
  
  ggplotly(p=zw) 
}

chlorofylA <- function (wq){
  b = dcast(wq, locatie.EAG+jaar+locatie.KRW.watertype ~ fewsparameter, 
            value.var = "meetwaarde", fun.aggregate = mean, na.rm =TRUE, fill = NaN, subset = .(fewsparameter == 'CHLFA'))
  b <- b[!is.na(b$CHLFA)& !is.na(b$jaar) & !is.na(b$locatie.EAG) & !b$locatie.EAG == '', ]
  
  '1' -> b$CHLFAkl[b$CHLFA > 200]
  '2' -> b$CHLFAkl[b$CHLFA <= 200 & b$CHLFA > 100]
  '3' -> b$CHLFAkl[b$CHLFA <= 100 & b$CHLFA > 50]
  '4' -> b$CHLFAkl[b$CHLFA <= 50 & b$CHLFA > 25]
  '5' -> b$CHLFAkl[b$CHLFA <= 25 & b$CHLFA > 10]
  '6' -> b$CHLFAkl[b$CHLFA <= 10]
  b$CHLFAkl <- as.factor(b$CHLFAkl)
  
  colc <- c('1'="darkred",'2'="red", '3'="orange",'4'="yellow",'5'="deepskyblue", '6'= 'blue')
  labelsc <- c('1'=">200",'2'="200-100" ,'3'="100-50",'4'="50-25",'5'="25-10",'6' = '<10')
  pal <- colorFactor(palette = colc,  domain = b$CHLFAkl)
  map <- sp::merge(gEAG, b[, c('CHLFAkl','CHLFA','jaar','locatie.EAG')], 
                   by.x = 'GAFIDENT', by.y =
                     'locatie.EAG', all.x = TRUE, duplicateGeoms = T)
  
  map <- map[order(map$jaar),]
  leaflet(map) %>%
    addPolygons(layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                     "Chlorofyl:", map$CHLFA, "<br>",
                                                     "jaar:", map$jaar, "<br>"),
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(CHLFAkl), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors=colc, labels=labelsc, title = "")%>%
    addTiles()
}


# matrix -----------------------------------------------------------------
makeMatrix <- function(EKRset, bod, wq, hybi, dat){
  #laatste 3 meetjaren EKR scores ---------
  krw <- tabelPerWL3jaargemEAG(EKRset[!EKRset$Grootheid.code %in% c("AANTPVLME", "SOORTRDM"),])
  # dcst om maatlatten aprt te beschouwen en relaties tussen maatlatten te leggen
  krw <- dcast(krw, EAGIDENT+HoortBijGeoobject.identificatie+KRWwatertype.code ~ GHPR+Waardebepalingsmethode.code, value.var = "EKR", fun.aggregate = mean)
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

# waterstromen---------------
calculateIRRatio <- function(myData) {
  ### slecteer de correcte parameters uit bestand
  funcData.CA <- myData[myData$fewsparameter=='CA-1',]
  funcData.CL <- myData[myData$fewsparameter=='CL',]
  ### converteer calcium en chloride naar correcte eenheid
  funcData.CA$meetwaarde <- funcData.CA$meetwaarde/(40.078/2) # Eenheid is nu meq/l
  funcData.CL$meetwaarde <- funcData.CL$meetwaarde/(35.45/1)  # Eenheid is nu meq/l
  funcData.IR <- base::merge(funcData.CA, funcData.CL, by=c("locatiecode", "datum"))# Eenheid is geen mg/l maar meq/l
  ### bereken IR 
  funcData.IR$meetwaarde <- funcData.IR$meetwaarde.x / (funcData.IR$meetwaarde.x + funcData.IR$meetwaarde.y)
  funcData.IR$fewsparameter <- "IR"
  funcData.IR$eenheid <- "DIMSLS"
  funcData.IR$locatie.EAG <- funcData.IR$locatie.EAG.x
  
  return(funcData.IR[,c("datum", "locatiecode", "fewsparameter", "meetwaarde", "eenheid", "locatie.EAG")])
}

createIR_EGV_Graph <- function(theData, colir) {
  #theData <- wq
  # calculate correct values for IR and EGV met functies hierboven
  IR <- calculateIRRatio(theData)
  EGV <- theData[theData$fewsparameter == 'GELDHD',]
  
  # Combine values on Date, Point and area
  data <- base::merge(EGV, IR, by=c("datum", "locatiecode", "locatie.EAG"))
  data <- data[!is.na(data$locatiecode) &!is.na(data$meetwaarde.x)&!is.na(data$meetwaarde.y),]
  
  p <- ggplot2::ggplot()+
    geom_point(data = data, aes(x= data$meetwaarde.x*10, y = data$meetwaarde.y, col = data$locatiecode))+
    scale_colour_manual(values=colir) + 
    geom_path(data = LATframework, aes(x= LATframework$EC25*10, y = LATframework$IR/100), size = 0.1, linetype = 'dotdash')+
    geom_text(data = referencepoints, aes(x = referencepoints$EC25*10, y = referencepoints$IR/100, label = referencepoints$Name))+
    scale_x_log10()+
    guides(col=guide_legend(title='Locatiecode'))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 10), 
      strip.text.y = element_text(size = 10), 
      axis.text.x = element_text(size= 10, angle=90,hjust=1),
      axis.text.y = element_text(size= 10, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      #panel.border =element_blank(), 
      #panel.grid.major = element_blank(), 
      #panel.grid.minor = element_blank(), 
      #panel.margin = unit(0.20, "lines"), 
      plot.background = element_blank(), 
      plot.margin = unit(c(0.25,0.25, 0.5, 0.5), "lines")
    )+
    ggtitle("IR EGV diagram") +
    labs(x="EGV (uS/cm)",y="IR-Ratio (Ca/(Ca+Cl))") 
  ggplotly(p)
}
iregvplotmap <- function(wq){
wqir <- wq[wq$fewsparameter %in% c('CA-1','GELDHD','CL'),]
b = dcast(wqir, locatiecode+locatie.x+locatie.y ~ fewsparameter, 
          value.var = "meetwaarde", fun.aggregate = lengthUnique)
b <- b[!is.na(b$locatiecode) & !is.na(b$locatie.x) & b$`CA-1` > 0, ]
b<- spTransform(SpatialPointsDataFrame(coords=b[,c("locatie.x","locatie.y")],
                                       data=b, proj4string=proj4.rd),
                CRSobj=proj4.google)
b <-as.data.frame(b)

colir <- rainbow(length(b$locatiecode))
wqir <- wqir[wqir$locatiecode %in% b$locatiecode,]
pal <- colorFactor(palette = colir,  domain = b$locatiecode)

combineWidgets(
  ncol = 2, colsize = c(1.5,1),
  createIR_EGV_Graph(wqir, colir),
  leaflet(b) %>%
    addCircles(~locatie.x.1, ~locatie.y.1, 
               label = b$locatiecode, labelOptions = labelOptions(interactive = T, clickable = T , 
                                                                  noHide = T, direction = "auto"),                                               stroke = T, color = ~pal(locatiecode), opacity=0.8, weight = 10, 
               fillOpacity = 0.6) %>%
    addTiles()
)
}

# overzichtskaarten-------------------------------------
krwmap <- function(gKRW){
  leaflet(gKRW) %>%
    addPolygons(layerId = gKRW$OBJECTID, popup= paste("naam", gKRW$OWMNAAM, "<br>",
                                                      "Ident:", gKRW$OWMIDENT, "<br>",
                                                      "watertype:", gKRW$OWMTYPE),
                stroke = T, color= 'green', fillColor = brewer.pal(length(gKRW$OWMIDENT), "Spectral"), opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillOpacity = 0.6) %>%
    addTiles()
}

eagoverzicht <- function(gEAG){
  wtgeb <- dcast(EKRset, EAGIDENT+KRWwatertype.code~ ., value.var = "Doel", fun.aggregate = mean)
  wtgeb$EAGIDENT <- as.factor(wtgeb$EAGIDENT)
  tabset <- sp::merge(gEAG, wtgeb, by.x = 'GAFIDENT', by.y = 'EAGIDENT',  all.x = TRUE, duplicateGeoms = T)
  
  leaflet(tabset) %>%
    addPolygons(layerId = tabset$GAFIDENT, popup= paste("naam", tabset$GAFNAAM, "<br>",
                                                      "Ident:", tabset$GAFIDENT,"<br>",
                                                      "watertype:", tabset$KRWwatertype.code),
                stroke = T, color= 'green', fillColor = brewer.pal(length(gEAG$GAFNAAM), "Spectral"), opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillOpacity = 0.6) %>%
    addTiles()
}

mpwaterkwaliteit <- function (wq){
  b = dcast(wq, locatiecode+locatie.EAG+locatie.KRW.watertype+locatie.meetnethistorie+locatie.meetprogrammahistorie+locatie.x+locatie.y ~ fewsparameter, 
            value.var = "jaar", fun.aggregate = lengthUnique)
  b <- b[!is.na(b$locatie.EAG) & !is.na(b$locatiecode) & !is.na(b$locatie.x) & !b$locatiecode == 'test', ]
  
  b<- spTransform(SpatialPointsDataFrame(coords=b[,c("locatie.x","locatie.y")],
                                     data=b, proj4string=proj4.rd),
              CRSobj=proj4.google)
  b <-as.data.frame(b)
  
  leaflet(b) %>%
    addCircles(~locatie.x.1, ~locatie.y.1, popup= paste("locatienaam", b$locatiecode, "<br>",
                                                     "meetnet:", b$locatie.meetnethistorie, "<br>",
                                                     "meetprogramma:", b$locatie.meetnethistorie, "<br>",
                                                     "aantal jaar bemonsterd:", b$GELDHD),
               label = b$locatiecode,
               labelOptions = labelOptions(interactive = T, clickable = T , noHide = T, direction = "auto"),                                                 
                stroke = T, color= 'green', opacity=0.8,  weight = 3, radius=40, fillOpacity = 0.6) %>%
    addTiles()
}

mphybi <- function (hybi){
  b = dcast(hybi, locatiecode+locatie.EAG+locatie.KRW.watertype+locatie.meetnethistorie+locatie.meetprogrammahistorie+locatie.x+locatie.y ~ fewsparameter, 
            value.var = "jaar", fun.aggregate = lengthUnique)
  b <- b[!is.na(b$locatie.EAG) & !is.na(b$locatiecode) & !is.na(b$locatie.x) & !b$locatiecode == 'test', ]
  
  b<- spTransform(SpatialPointsDataFrame(coords=b[,c("locatie.x","locatie.y")],
                                         data=b, proj4string=proj4.rd),
                  CRSobj=proj4.google)
  b <-as.data.frame(b)
  
  leaflet(b) %>%
    addCircles(~locatie.x.1, ~locatie.y.1, popup= paste("locatienaam", b$locatiecode, "<br>",
                                                        "meetnet:", b$locatie.meetnethistorie, "<br>",
                                                        "meetprogramma:", b$locatie.meetprogrammahistorie, "<br>",
                                                        "aantal jaar bemonsterd:", b$WATDTE_m),
               label = b$locatiecode,
               labelOptions = labelOptions(interactive = T, clickable = T , noHide = T, direction = "auto"),                                                 
               stroke = T, color= 'green', opacity=0.8,  weight = 3, radius=40, fillOpacity = 0.6) %>%
    addTiles()
}


# invulblad doelen ----------------------------

# hybi indicatoren matrix maken van laatste drie meetjaren
hybisam <- function(hybi){
  b2 = dcast(hybi, locatie.KRWmeetpuntlocatie+jaar+fewsparameter+parametercode+parameterfractie~., 
           value.var = "meetwaarde", fun.aggregate = mean)

  d3 <- b2 %>% 
  arrange(locatie.KRWmeetpuntlocatie,fewsparameter,parametercode,parameterfractie,desc(jaar)) %>% 
  group_by(locatie.KRWmeetpuntlocatie,fewsparameter,parametercode,parameterfractie) %>% 
  top_n(3, wt = jaar) 

  d4 = d3 %>%
  group_by(locatie.KRWmeetpuntlocatie,fewsparameter,parametercode,parameterfractie) %>% 
  summarize_all(mean)

  hybiind <- dcast(d4, fewsparameter+parametercode+parameterfractie ~ locatie.KRWmeetpuntlocatie, 
                 value.var = ".", fun.aggregate = mean)
  write.table(hybiind, file = paste(getwd(),"./hybi3jaarWL",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  
}

calcMeanHybiY <- function(hybi, eenheid = 'KRW'){
  
  # make local copy
  b = copy(hybi) 
  
  # adjust fews parameter names
  b[,fewsparameter := gsub("/","_",fewsparameter)]
  
  # dcast table
  if(eenheid == "KRW"){
  b <- dcast(b, locatiecode+locatie.KRWmeetpuntlocatie+locatie.KRW.watertype+compartiment+jaar~fewsparameter+parametercode+parameterfractie, 
               value.var = "meetwaarde", fun.aggregate = mean)  
  }
  if(eenheid == "EAG"){
  b <- dcast(b, locatiecode+locatie.EAG+locatie.KRW.watertype+compartiment+jaar~fewsparameter+parametercode+parameterfractie, 
             value.var = "meetwaarde", fun.aggregate = mean)
  }
  # calculate and classify zichtdiepte
  b[,DTEZICHT := ZICHT_m_ZICHT_/WATDTE_m_WATDTE_]
  b[DTEZICHT > 1, DTZICHT := NaN]
  b[,DTEZICHTfac := cut(DTEZICHT, breaks = c('0.1','0.2','0.3','0.4','0.6', '0.8','1.0'))]
  
  # filter and sort database
  b <- b[!is.na(DTEZICHTfac) & !is.na(jaar),]
  setorder(b,jaar)
  
  # rename relevant columns # toevoegen kroos
  cols <- c('PTN_BEDKG_%_SUBMSPTN_','PTN_BEDKG_%_FLAB_SUBMS','PTN_BEDKG_%_FLAB_DRIJVD','PTN_BEDKG_%_EMSPTN_',
            'TALBVWTR_graad_TALBVWTR_','ZICHT_m_ZICHT_','WATDTE_m_WATDTE_',"WATERBTE_m_WATERBTE_","SLIBDTE_m_SLIBDTE_",'DTEZICHT','DTEZICHTfac','CHLFa_ug_l_CHLFa_')
  colsn <- c('bedsubmers','draadwieren','FLAB','bedemers','taludhoek','doorzicht',
             'waterdiepte','waterbreedte','slibdikte','dieptedoorzicht','dieptedoorzichtfac','chlorofyla')
  setnames(b,cols,colsn, skip_absent = T)
  
  # select those columns
  if(eenheid == "KRW"){
    b <- b[,mget(c('locatie.KRWmeetpuntlocatie','locatie.KRW.watertype','jaar',colsn))]
  }
  if(eenheid == "EAG"){
  b <- b[,mget(c('locatie.EAG','locatie.KRW.watertype','jaar',colsn))]
  }
  
  # calculate median value per EAG, watertype and year
  cols <- colnames(b)[sapply(b, is.numeric)]
  if(eenheid == "KRW"){
    b <- b[,lapply(.SD,median),.SDcols = cols[!cols=='jaar'],by=.(locatie.KRWmeetpuntlocatie,locatie.KRW.watertype,jaar)]
  }
  if(eenheid == "EAG"){
  b <- b[,lapply(.SD,median),.SDcols = cols[!cols=='jaar'],by=.(locatie.EAG,locatie.KRW.watertype,jaar)]
  }
  # return database
  return(b)
}

# waterkwaliteit
wqsamKRW <- function(wq, locset = locKRW){
  wq1<- wq[wq$fewsparameter %in% c("PO4","O2","P","N","NH3","NO3", "CHLFa"),]

  # subset locaties voor KRW selecteren: moet kolom CODE bevatten met locatiecodes
  wq1<- wq1[wq1$locatiecode %in% locset$CODE,]
  
  d4 <-  wq1 %>%
    filter(maand %in% c(04:09)) %>%  
    filter(jaar %in% c(2000:2020)) %>%
    group_by(locatie.KRWmeetpuntlocatie,fewsparameter,jaar) %>%
    summarise_at(c('meetwaarde'), mean, na.rm = TRUE)
  d4$mean <- d4$meetwaarde
  
  d5 <-  wq1 %>%
    filter(maand %in% c(04:09)) %>%  
    filter(jaar %in% c(2010:2020)) %>%
    group_by(locatie.KRWmeetpuntlocatie,fewsparameter,jaar) %>%
    summarise_at(c('meetwaarde'), median, na.rm = TRUE)
  d5$median <- d5$meetwaarde
  
  d6 <-  wq1 %>%
    filter(maand %in% c(04:09)) %>%  
    filter(jaar %in% c(2000:2020)) %>%
    group_by(locatie.KRWmeetpuntlocatie,fewsparameter,jaar) %>%
    summarise_at(c('meetwaarde'), min, na.rm = TRUE)
  d6$min <- d6$meetwaarde
  
  d7 <- merge(d4,d5, by = c('locatie.KRWmeetpuntlocatie','fewsparameter','jaar'))
  d7 <- merge(d7,d6, by = c('locatie.KRWmeetpuntlocatie','fewsparameter','jaar'))
  
  wqsam <- dcast(setDT(d7),  locatie.KRWmeetpuntlocatie+jaar ~ fewsparameter , 
                 value.var = c("median","mean","min"))
  
  write.table(wqsam, file = paste(getwd(),"/output/chemKRW",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  return(wqsam)
}
wqsamEAG <- function(wq, locset = locKRW){
  wq1<- wq[wq$fewsparameter %in% c("PO4","O2","P","N","NH3","NO3", "CHLFa"),]
  #selecteer alleen KRW locaties
  wq1<- wq1[wq1$locatiecode %in% locset$CODE,]
  
  d4 <-  wq1 %>%
    filter(maand %in% c(04:09)) %>%  
    filter(jaar %in% c(2000:2020)) %>%
    group_by(locatie.EAG,fewsparameter,jaar) %>%
    summarise_at(c('meetwaarde'), mean, na.rm = TRUE)
  d4$mean <- d4$meetwaarde
  
  d5 <-  wq1 %>%
    filter(maand %in% c(04:09)) %>%  
    filter(jaar %in% c(2010:2020)) %>%
    group_by(locatie.EAG,fewsparameter,jaar) %>%
    summarise_at(c('meetwaarde'), median, na.rm = TRUE)
  d5$median <- d5$meetwaarde
  
  d6 <-  wq1 %>%
    filter(maand %in% c(04:09)) %>%  
    filter(jaar %in% c(2000:2020)) %>%
    group_by(locatie.EAG,fewsparameter,jaar) %>%
    summarise_at(c('meetwaarde'), min, na.rm = TRUE)
  d6$min <- d6$meetwaarde
  
  d7 <- merge(d4,d5, by = c('locatie.EAG','fewsparameter','jaar'))
  d7 <- merge(d7,d6, by = c('locatie.EAG','fewsparameter','jaar'))
  
  wqsam <- dcast(setDT(d7),  locatie.EAG+jaar ~ fewsparameter , 
                 value.var = c("median","mean","min"))
  
  write.table(wqsam, file = paste(getwd(),"/output/chemEAG",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  return(wqsam)
}



