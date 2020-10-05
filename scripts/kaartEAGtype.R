setwd("T:/WS/AFD WP&BEST/Prog KRW/12 Beheerregister/begrenzingTyperingStatusEnOppervlakken")

require(sf)
require(raster)
require(data.table)
require(RColorBrewer)
require(leaflet)

# other settings
proj4.rd <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.87740,4.0725 +units=m +no_defs")
proj4.google <- CRS("+proj=longlat +datum=WGS84 +no_defs")

## data voor kaart ----------
gEAG<- st_read("../../11 Stand van zake waterkwaliteit/R/data/EAG20191205.gpkg") %>% st_transform(proj4.google)
## let op: als nieuwe EAG (gEAG) dan deze tabel aanpassen en aanvullen
eag_wl <- fread('./Oppervlakken/EAG_Opp_kenmerken_20200701.csv')

map <- sp::merge(gEAG, eag_wl, by.x = 'GAFIDENT', by.y =
                   'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
map$param <- as.factor(paste(map$StedelijkLandelijk, map$LANDBOUWGEB))
col <- rainbow(length(unique(map$param)))
pal <- colorFactor(palette = col,  domain = map$param)

leaflet() %>%
  addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                               "watertype:", map$watertype.y, "<br>",
                                                               "Stedelijk landelijk:", map$StedelijkLandelijk, "<br>",
                                                               "Type gebied:", map$LANDBOUWGEB),
              stroke = T, color= 'grey', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
              fill=T, fillColor = ~pal(param), fillOpacity = 0.6) %>%
  addLegend("bottomright", colors=col, labels=unique(map$param))%>%
  addProviderTiles("Esri.WorldGrayCanvas")#addTiles()
