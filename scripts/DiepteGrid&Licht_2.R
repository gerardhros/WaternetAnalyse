# Gemaakt door Laura Moria april 2016 #
# Met dit script wordt lichtklimaat in plassen gevisualiseerd op kaart
# To do: Noorderijplas & breukeleveen mbp012, terra nova

################################
#### algemene settings    ######
################################

rm(list=ls())             #maakt geheugen leeg voor aanvang
memory.limit(size=8000)   #zet geheugen groot genoeg
#dev.off()                 # om plotjes te kunnen zien
#setwd("T:/WS/AFD WP&BEST/Prog KRW/06 Waterlichamen/0 Gebiedsbreed/Vegetatie gebiedsbreed 2016-2021/Dataanalyse_laura/Licht/")
setwd("C:/Users/WNet/Desktop/directorieswaternet/Dataanalyse_laura/ESF2/raster") #moet plek zijn waar grids staan
path = "../lichtopdebodem/" #plek waar plaatjes lichtklimaat worden weggeschreven
pathWD = "../dieptekaarten/" #plek waar plaatjes waterdiepte worden weggeschreven
  
################################
######## laad packages #########
################################

# data processing
#library(foreign) # for reading dbfs
library(plyr) # for joining
#library(magrittr)
#library(tidyr) 
library(ggplot2) # plotjes
#library(gridExtra) # to arrange grid plots
library(lattice)

# spatial
library("sp")
library("grid")
library(raster)
#library(rasterVis)
library(rgdal) # inlezen shape
library("RColorBrewer")
#library(dismo) #map raster on Google Map
library("maptools") # conversie shp to points

################################
######## DATA INLEZEN  #########
################################

# grid data inlezen
files <- list.files(path=getwd(), pattern="*.tif$")
# ruimtelijk projectie rijksdriehoek toevoegen
proj4.rd_new <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

for(fi in files) { 
  r <- raster(fi) 
  setMinMax(r)
  projection(r) <- proj4.rd_new
  plas.name <- c(unlist(lapply(strsplit(fi,"[.]"), FUN=function(x) { x[1] })))
  assign(plas.name, r)
}

### waterpeil inlezen # benodigde kolommen: Datum, Meetwaarde, Meetpunt.ID
waterpeil <- read.csv("../peilen/plassen.csv", header = TRUE, na.strings = "-999999", sep=";", dec =".")
waterpeil$Datum <- strptime(waterpeil$Datum, format = "%Y-%m-%d", tz = "GMT") # datumformat waterpeilen instellen
waterpeil <- waterpeil[!waterpeil$Meetwaarde > 0, ] #ouliers verwijderen
waterpeil$seizoen <- "" # seizoenen toevoegen
waterpeilzomer <- waterpeil[(as.numeric(format(waterpeil$Datum, "%m")) > 3 & as.numeric(format(waterpeil$Datum, "%m")) < 10),]
waterpeilwinter <- waterpeil[(as.numeric(format(waterpeil$Datum, "%m")) < 4 | as.numeric(format(waterpeil$Datum, "%m")) > 9),]
waterpeilzomer$seizoen <- "zomer"
waterpeilwinter$seizoen <- "winter"
waterpeil <- rbind(waterpeilzomer, waterpeilwinter)

# extinctiegegevens benodigde kolommen: Datum, Meetwaarde, Meetlocatie.omschrijving
extinctie <- read.csv("../extinctie/VEC_plassen.csv", header = TRUE, na.strings = "-999999", sep=";", dec =".")
extinctie$Datum <- strptime(extinctie$Datum, format = "%d-%m-%Y", tz = "GMT") #datumformat instellen
extinctie <- extinctie[extinctie$Meetwaarde > 0, ] # meetwaarde -1 verwijderen 
extinctiezomer <- extinctie[(as.numeric(format(extinctie$Datum, "%m")) > 3 & as.numeric(format(extinctie$Datum, "%m")) < 10),]
extinctiezomer <- extinctiezomer[(as.numeric(format(extinctiezomer$Datum, "%Y"))) > 2010,] #alleen recente jaren
#extgebiedsgemiddelde <- aggregate(extinctie, by = )

koppelT <- read.csv("../lichtopdebodem/RelatiePeilEXTGrid.csv", header = TRUE, na.strings = "-999999", sep=";", dec =".")# koppelbestand
eags <- shapefile("../shape/ecologischeanalysegebieden.shp")
projection(eags) <- proj4.rd_new

################################
######## BEWERKINGEN  #########
################################

# grid converteren naar percentage licht op de bodem bij verschillende scenarios
# lichtformule bij 5 % reflectie op het water: %oppervlaktelicht = (100-5)* e (-vec*diepte)
# percentageOppervlakteLicht <- exp(-vecZomer*WaterpeilZomer) # uit artikel midelboe 1997
# procentLichtOpBodem <- log(100*(1/diepte))/ vec # uit script eva de ruiter

plas.names <- c(unlist(lapply(strsplit(files,"[.]"), FUN=function(x) { x[1] })))
# i <- "Wijde Blik1"
# i<- "Kortenhoefse Plassen1" 
stats_raster <- NULL

for(i in plas.names){
gebied <- koppelT[koppelT$Raster %in% i, "gebiedsnaam"]
MPpeil <- koppelT[koppelT$Raster %in% i, "MP_P"]
wpzomer <- waterpeilzomer[waterpeilzomer$Meetpunt.ID %in% MPpeil, ]  
diepteZomerGem <- get(i) - mean(wpzomer$Meetwaarde) # gemiddeld peil
diepteZomerMax <- get(i) - max(wpzomer$Meetwaarde) # ongunstig lichtklimaat
diepteZomerMin <- get(i) - min(wpzomer$Meetwaarde) # gunstig lichtklimaat

MPext <- koppelT[koppelT$Raster %in% i, "MP_VEC"] # als extinctiewaarden van meetpunten wil middelen moet 3lettercode in koppeltabel staan
if(nchar(as.character(MPext)) == 3){ 
  MPext <- koppelT[koppelT$Raster %in% MPext, "MP_VEC"]} # hier wordt 3 lettercode aan voorgedefineerde meetpunten gekoppeld
extzmr <- extinctiezomer[extinctiezomer$Omschrijving.Meetpunt %in% MPext,] 
vecZomer <- mean(extzmr$Meetwaarde) #gemiddelde vec
vecZomer95P <- quantile(extzmr$Meetwaarde, probs = c(0.95)) # ongunstig lichtklimaat
vecZomer5P <- quantile(extzmr$Meetwaarde, probs = c(0.05)) # gunstig lichtklimaat
MPext <- paste0(MPext[1]," ",MPext[2])

#percentageOppervlakteLichtlaagpeil <- 100*exp(diepteZomerMin * vecZomer) # peil uitzakken, gunstig lichtklimaat
percentageOppervlakteLicht <-  100*exp((diepteZomerGem * vecZomer)) # gemiddeld scenario
percentageOppervlakteLichtworst <- 100*exp(diepteZomerMax * vecZomer95P) # ongunstig lichtklimaat
percentageOppervlakteLichtbest <- 100*exp(diepteZomerMin * vecZomer5P) # gunstig lichtklimaat

############################### stats berekenen ############################
#log vecZomer/100 #diepte met 4% licht 
vol<- cellStats(diepteZomerGem, 'sum') #volume plassen
a <- freq(diepteZomerGem) # hoe worden frequentieklasse gemaakt? lijkt mis te gaan
a[,2] <- a[,2] * prod(res(diepteZomerGem))
a <- as.data.frame(a)
a <- a[!is.na(a[,1]),]
area <- sum(a[,2])
a3<- a[(a[,1]) >= -3,]; area3 <- sum(a3[,2])
a4<- a[(a[,1]) >= -4,]; area4 <- sum(a4[,2])
a7<- a[(a[,1]) >= -7,]; area7 <- sum(a7[,2])
watdte <- cellStats(diepteZomerGem, 'mean') #gemdiepte
watdtemin <- cellStats(diepteZomerGem, 'min') #mindiepte
watdtemax <- cellStats(diepteZomerGem, 'max') #maxdiepte
watdteperc <- quantile(diepteZomerGem, probs = c(0.05,0.10,0.30, 0.50, 0.70, 0.95)) #diepteperc alleen NP+ ZP
lichtperc <- quantile(percentageOppervlakteLicht, probs = c(0.05,0.10,0.30, 0.50, 0.70, 0.95)) 
diepte4licht <- log(25)/vecZomer  #ln(100%/4%)/ Z = E
f<- freq(percentageOppervlakteLicht, digits = 4)
f <- as.data.frame(f)
f<- f[!is.na(f[,1]),]
f<- f[(f[,1])>4,]
opp4 <- sum(f[,2]) # oppervlak waar 4 % licht valt
br <- brick(diepteZomerGem, percentageOppervlakteLicht)
map <- rasterToPoints(br)
df <- data.frame(map)
opplicht3meter <- nrow(df[!is.na(df[,3]) & df[,3]>= -3 & df[,4]> 4,])
opplicht4meter <- nrow(df[!is.na(df[,3]) & df[,3]>= -4 & df[,4]> 4,])
opplicht7meter <- nrow(df[!is.na(df[,3]) & df[,3]>= -7 & df[,4]> 4,])
# percentage oppervlak < 3 meter < 4 <6 <7.5 met meer dan 4% licht
opp4_3meter <- opplicht3meter/area3
opp4_4meter <- opplicht4meter/area4
opp4_7meter <- opplicht7meter/area7
stats_df <- data.frame(loc = gebied, locnaam = i, opp = area, volume = vol, opp4procentlicht = opp4, 
                       dieptewaar4procentlichtvalt = diepte4licht,
                       opp4procentlichtbegroeibaar3meter = opplicht3meter, fractie4procentlichtbegroeibaar3meter = opp4_3meter, 
                       opp4procentlichtbegroeibaar3meter = opplicht4meter, fractie4procentlichtbegroeibaar4meter =opp4_4meter, 
                       opp4procentlichtbegroeibaar3meter = opplicht7meter, fractie4procentlichtbegroeibaar7meter = opp4_7meter,
                       watdteperc95 = watdteperc[6], watdteperc70 = watdteperc[5], 
                       watdteperc50 = watdteperc[4], watdteperc30 = watdteperc[3],
                       watdteperc10 = watdteperc[2], watdteperc5 = watdteperc[1],
                       lichtperc95 = lichtperc[6], lichtperc70 = lichtperc[5], 
                       lichtperc50 = lichtperc[4], lichtperc30 = lichtperc[3],
                       lichtperc10 = lichtperc[2], lichtperc5 = lichtperc[1],
                       nrow(wpzomer),nrow(extzmr), mppeil = MPpeil, mpextinctie = MPext)

stats_raster <- rbind(stats_raster, stats_df)
write.table(stats_raster, file = "../statestiek/dieptelichtstats_rasters20102016.csv", quote = FALSE, sep = ";",row.names = FALSE)

################################
####### plaatjes maken #########
################################

# omzetten voor ggplot
map <- rasterToPoints(diepteZomerGem)
df <- data.frame(map)

#histogram waterdiepte per eag
ggplot(data=df, aes(df[,3])) +
  geom_histogram(bins = 20, na.rm = TRUE) +
  ggtitle(paste("Distributie van Waterdiepten", gebied)) +
  labs(x="Waterdiepte (m)",y="oppervlak (m2)") 
ggsave(file = paste(i, "waterdiepte_histogram", ".png"), path = pathWD)

#Now make the map waterdiepte per eag
ggplot(data=df, aes(y=y, x=x), col = df[,3]) +
  geom_raster(aes(fill=df[,3])) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradientn(name = "Waterdiepte (m)", values = NULL, space = "Lab",
                       na.value = "grey50", guide = "colourbar", colors = rainbow(20)) +
  ggtitle(paste("Waterdiepte", gebied)) +
  labs(x=" ",y=" ") +
  theme(
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) 
ggsave(file = paste(i, "waterdiepte", ".png"), path = pathWD)

# omzetten voor ggplot
map <- rasterToPoints(percentageOppervlakteLicht)
df <- data.frame(map)

# gghistogram 
ggplot(data=df, aes(df[,3])) +
  geom_histogram(bins = 20, na.rm = TRUE) +
  ggtitle(paste("%licht op de bodem", gebied, "gemiddeld scenario")) +
  labs(x="% licht ",y="oppervlak (m2)") 
ggsave(file = paste(i, "lichtopdebodem_gem_his", ".png"), path = path)

#Now make the map voor gemiddeld scenario
df[,3][df[,3]>20] = 20
df[,3][df[,3]< 0] = NA
ggplot(data=df, aes(y=y, x=x), col = df[,3]) +
  geom_raster(aes(fill=df[,3])) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradientn(name = "0 - >20%", limits= c(0, 20), 
                       breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20),
                       values = NULL, space = "Lab",na.value = "grey50", 
                       guide = "colourbar", colors = (rainbow(12))) +
  ggtitle(paste("% licht op de bodem", gebied)) +
  labs(x=" ",y=" ") +
  theme(
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.position="right",
    legend.title= element_blank()) +
  annotate(geom = "text", label = "WK meetpunt ZGW:", 
                x = (max(df$x)-(max(df$x)-min(df$x))/10), y = max(df$y)+(max(df$y)-min(df$y))/30,size = 3)+
  annotate(geom = "text", label = paste(as.character(MPext)), 
         x = (max(df$x)-(max(df$x)-min(df$x))/10), y = max(df$y),size = 3)+
  annotate(geom = "text", label = "WP meetpunt GW:", 
           x = (max(df$x)-(max(df$x)-min(df$x))/10), y = max(df$y) - (max(df$y)-min(df$y))/30,size = 3)+
  annotate(geom = "text", label = paste(as.character(MPpeil)), 
           x = (max(df$x)-(max(df$x)-min(df$x))/10), y = (max(df$y) - (max(df$y)-min(df$y))/15),size = 3)
ggsave(file = paste(i, "lichtopdebodem_gem_rb", ".png"), path = path)

#geom_text(aes(label = paste("Gebruikte peilmeetpunt GW:", as.character(MPpeil)), x = 2.5, y = 0), hjust = -2, vjust = 6, color="#a0a0a0", size=3.5)
# } # kan hier loop laten eindigen met resultaten per raster

#raster knippen op EAG grenzen 
clip1 <- crop(eags, get(i)) #crops eags to the extent of een raster 
# verwijder NaN/ Na uit raster?

# loop om stats ook per eag te berekenen
# j <- "3230-EAG-1" # het hol
for(j in unique(clip1$GAFIDENT)){
    eag <- koppelT[koppelT$Raster %in% j, "gebiedsnaam"]
    r <- clip1[clip1@data$GAFIDENT == j,] 
    lr <- mask(x= get(i), mask=r) # maskeerd (dimensies blijven hetzelfde) diepteraster per eag
    if (is.na(maxValue(lr))){
      warning(paste("Alleen NA waarden in eag", j))
      next # stopt de loop als er alleen na waarden in raster zitten
      } 
  
    MPpeil <- koppelT[koppelT$Raster %in% j, "MP_P"]
    wpzomer <- waterpeilzomer[waterpeilzomer$Meetpunt.ID %in% MPpeil, ]  
    if (nrow(wpzomer) <= 0){
      warning(paste("Geen waterpeilen gekoppeld voor eag of beschikbaar bij meetpunt", j, MPpeil))
      next} # stopt de loop als er geen peilgegevens zijn
    diepteZomerGem <- lr - mean(wpzomer$Meetwaarde) # gemiddeld peil
    diepteZomerMax <- lr - max(wpzomer$Meetwaarde) # ongunstig lichtklimaat
    diepteZomerMin <- lr - min(wpzomer$Meetwaarde) # gunstig lichtklimaat
  
    MPext <- koppelT[koppelT$Raster %in% j, "MP_VEC"]
    extzmr <- extinctiezomer[extinctiezomer$Omschrijving.Meetpunt %in% MPext,] 
    if (nrow(extzmr) <= 0){
      warning(paste("Geen extinctiemetingen gekoppeld voor eag of beschikbaar bij meetpunt", j, MPext))
      next} # stopt de loop als er geen extinctiegegevens zijn
    vecZomer <- mean(extzmr$Meetwaarde) #gemiddelde vec
    vecZomer95P <- quantile(extzmr$Meetwaarde, probs = c(0.95)) # ongunstig lichtklimaat
    vecZomer5P <- quantile(extzmr$Meetwaarde, probs = c(0.05)) # gunstig lichtklimaat
  
    percentageOppervlakteLichtlaagpeil <- 100*exp(diepteZomerMin * vecZomer) # peil uitzakken, gunstig lichtklimaat
    percentageOppervlakteLicht <-  100*exp((diepteZomerGem * vecZomer)) # gemiddeld scenario
    percentageOppervlakteLichtworst <- 100*exp(diepteZomerMax * vecZomer95P) # ongunstig lichtklimaat
    percentageOppervlakteLichtbest <- 100*exp(diepteZomerMin * vecZomer5P) # gunstig lichtklimaat
    
    vol<- cellStats(diepteZomerGem, 'sum') #volume plassen
    a <- freq(diepteZomerGem) # hoe worden frequentieklasse gemaakt? lijkt mis te gaan
    a[,2] <- a[,2] * prod(res(diepteZomerGem))
    a <- as.data.frame(a)
    a <- a[!is.na(a[,1]),]
    area <- sum(a[,2])
    a3<- a[(a[,1]) >= -3,]; area3 <- sum(a3[,2])
    a4<- a[(a[,1]) >= -4,]; area4 <- sum(a4[,2])
    a7<- a[(a[,1]) >= -7,]; area7 <- sum(a7[,2])
    watdte <- cellStats(diepteZomerGem, 'mean') #gemdiepte
    watdtemin <- cellStats(diepteZomerGem, 'min') #mindiepte
    watdtemax <- cellStats(diepteZomerGem, 'max') #maxdiepte
    watdteperc <- quantile(diepteZomerGem, probs = c(0.05,0.10,0.30, 0.50, 0.70, 0.95)) #diepteperc alleen NP+ ZP
    lichtperc <- quantile(percentageOppervlakteLicht, probs = c(0.05,0.10,0.30, 0.50, 0.70, 0.95)) 
    diepte4licht <- log(25)/vecZomer  #ln(100%/4%)/ Z = E
    f<- freq(percentageOppervlakteLicht, digits = 4)
    f <- as.data.frame(f)
    f<- f[!is.na(f[,1]),]
    f<- f[(f[,1])>4,]
    opp4 <- sum(f[,2]) # oppervlak waar 4 % licht valt
    br <- brick(diepteZomerGem, percentageOppervlakteLicht)
    map <- rasterToPoints(br)
    df <- data.frame(map)
    opplicht3meter <- nrow(df[!is.na(df[,3]) & df[,3]>= -3 & df[,4]> 4,])
    opplicht4meter <- nrow(df[!is.na(df[,3]) & df[,3]>= -4 & df[,4]> 4,])
    opplicht7meter <- nrow(df[!is.na(df[,3]) & df[,3]>= -7 & df[,4]> 4,])
    # percentage oppervlak < 3 meter < 4 <6 <7.5 met meer dan 4% licht
    opp4_3meter <- opplicht3meter/area3
    opp4_4meter <- opplicht4meter/area4
    opp4_7meter <- opplicht7meter/area7
    stats_df <- data.frame(loc = j, locnaam = eag, opp = area, volume = vol, opp4procentlicht = opp4, 
                           dieptewaar4procentlichtvalt = diepte4licht,
                           opp4procentlichtbegroeibaar3meter = opplicht3meter, fractie4procentlichtbegroeibaar3meter = opp4_3meter, 
                           opp4procentlichtbegroeibaar3meter = opplicht4meter, fractie4procentlichtbegroeibaar4meter =opp4_4meter, 
                           opp4procentlichtbegroeibaar3meter = opplicht7meter, fractie4procentlichtbegroeibaar7meter = opp4_7meter,
                           watdteperc95 = watdteperc[6], watdteperc70 = watdteperc[5], 
                           watdteperc50 = watdteperc[4], watdteperc30 = watdteperc[3],
                           watdteperc10 = watdteperc[2], watdteperc5 = watdteperc[1],
                           lichtperc95 = lichtperc[6], lichtperc70 = lichtperc[5], 
                           lichtperc50 = lichtperc[4], lichtperc30 = lichtperc[3],
                           lichtperc10 = lichtperc[2], lichtperc5 = lichtperc[1],
                           nrow(wpzomer),nrow(extzmr), mppeil = MPpeil, mpextinctie = MPext)
    
        stats_raster <- rbind(stats_raster, stats_df)
    write.table(stats_raster, file = "../statestiek/dieptelichtstats_rasterseag20102016.csv", quote = FALSE, sep = ";",row.names = FALSE)

    ################################
    ####### plaatjes maken #########
    ################################
    
    # omzetten voor ggplot
    map <- rasterToPoints(diepteZomerGem)
    df <- data.frame(map)
    
    #histogram waterdiepte per eag
    ggplot(data=df, aes(df[,3])) +
      geom_histogram(bins = 20, na.rm = TRUE) +
      ggtitle(paste("Distributie van Waterdiepten", eag)) +
      labs(x="Waterdiepte (m)",y="oppervlak (m2)") 
    ggsave(file = paste(j, "waterdiepte_histogram", ".png"), path = pathWD)
    
    #Now make the map waterdiepte per eag
    ggplot(data=df, aes(y=y, x=x), col = df[,3]) +
      geom_raster(aes(fill=df[,3])) +
      theme_bw() +
      coord_equal() +
      scale_fill_gradientn(name = "Waterdiepte (m)", values = NULL, space = "Lab",
                           na.value = "grey50", guide = "colourbar", colors = rainbow(20)) +
      ggtitle(paste("Waterdiepte", eag)) +
      labs(x=" ",y=" ") +
      theme(
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(0.5, "cm")
      )
    ggsave(file = paste(j, "waterdiepte", ".png"), path = pathWD)
    
    # omzetten voor ggplot
    map <- rasterToPoints(percentageOppervlakteLicht)
    df <- data.frame(map)
    
    # gghistogram 
    ggplot(data=df, aes(df[,3])) +
      geom_histogram(bins = 20, na.rm = TRUE) +
      ggtitle(paste("%licht op de bodem", eag, "gemiddelde waterdiepte, gemiddelde extinctie op", MPext)) +
      labs(x="% licht ",y="oppervlak (m2)") 
    ggsave(file = paste(j, "lichtopdebodem_gem_his", ".png"), path = path)
    
    #Now make the map voor gemiddeld scenario
    df[,3][df[,3]>20] = 20
    df[,3][df[,3]< 0] = NA
    ggplot(data=df, aes(y=y, x=x), col = df[,3]) +
      geom_raster(aes(fill=df[,3])) +
      theme_bw() +
      coord_equal() +
      scale_fill_gradientn(name = "0 - >20%", limits= c(0, 20), 
                           breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20),
                                    values = NULL, space = "Lab",na.value = "grey50", 
                           guide = "colourbar", colors = (rainbow(12))) +
      ggtitle(paste("%licht op de bodem in", eag)) +
      labs(x=" ",y=" ") +
      theme(
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(0.5, "cm"))+
      annotate(geom = "text", label = "WK meetpunt ZGW:", 
               x = (max(df$x)-(max(df$x)-min(df$x))/10), y = max(df$y)+(max(df$y)-min(df$y))/30,size = 3)+
      annotate(geom = "text", label = paste(as.character(MPext)), 
               x = (max(df$x)-(max(df$x)-min(df$x))/10), y = max(df$y),size = 3)+
      annotate(geom = "text", label = "WP meetpunt GW:", 
               x = (max(df$x)-(max(df$x)-min(df$x))/10), y = max(df$y) - (max(df$y)-min(df$y))/30,size = 3)+
      annotate(geom = "text", label = paste(as.character(MPpeil)), 
               x = (max(df$x)-(max(df$x)-min(df$x))/10), y = (max(df$y) - (max(df$y)-min(df$y))/15),size = 3)
    ggsave(file = paste(j, "lichtopdebodem_gem_rb", ".png"), path = path)
    print(i)
    print(j)
    } #einde loop per eag
} #einde loop per raster

#########################################
###### plaatjes maken scenarios #########
#########################################

# omzetten voor ggplotbest
# mapb <- rasterToPoints(percentageOppervlakteLichtlaagpeil)
mapb <- rasterToPoints(percentageOppervlakteLichtbest)
dfb <- data.frame(mapb)
dfb[,3][dfb[,3]>20] = 20
dfb[,3][dfb[,3]< 0] = NA

# gghistogram best case
ggplot(data=dfb, aes(dfb[,3])) +
  geom_histogram(bins = 20, na.rm = TRUE) +
  ggtitle(paste("%licht op de bodem", i, "best case scenario")) +
  labs(x="% licht ",y="oppervlak (m2)") 
ggsave(file = paste(j, "lichtopdebodem_best_his", ".png"), path = path)

#Now make the map best case scenario
ggplot(data=dfb, aes(y=y, x=x), col = dfb[,3]) +
  geom_raster(aes(fill=dfb[,3])) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradientn(name = "0 - >20%", limits= c(0, 20), 
                       breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20),
                       values = NULL, space = "Lab",na.value = "grey50", 
                       guide = "colourbar", colors = rainbow(12)) +
  ggtitle(paste("%licht op de bodem", j, "best case waterpeil scenario")) +
  labs(x=" ",y=" ") +
  theme(
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.5, "cm") )
ggsave(file = paste(j, "lichtopdebodem bij laag waterpeil", ".png"), path = path)

# omzetten voor ggplotworst
mapw <- rasterToPoints(percentageOppervlakteLichtworst)
dfw <- data.frame(mapw)
dfw[,3][dfw[,3]>20] = 20
dfw[,3][dfw[,3]< 0] = NA

# gghistogram worst case
ggplot(data=dfw, aes(dfw[,3])) +
  geom_histogram(bins = 20, na.rm = TRUE) +
  ggtitle(paste("%licht op de bodem", i, "worst case scenario")) +
  labs(x="% licht ",y="oppervlak (m2)") 
ggsave(file = paste(i, "lichtopdebodem_worst_his", ".png"))

#Now make the map worst case scenario
ggplot(data=dfw, aes(y=y, x=x), col = dfw[,3]) +
  geom_raster(aes(fill=dfw[,3])) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradientn(name = "0 - >20%", limits= c(0, 20), 
                       breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20),
                       values = NULL, space = "Lab",na.value = "grey50", 
                       guide = "colourbar", colors = rev(rainbow(12))) +
  ggtitle(paste("%licht op de bodem", i, "worst case scenario")) +
  labs(x=" ",y=" ") +
  theme(
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.5, "cm") )
ggsave(file = paste(i, "lichtopdebodem_worst", ".png"))

