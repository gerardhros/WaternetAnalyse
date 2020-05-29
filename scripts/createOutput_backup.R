
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