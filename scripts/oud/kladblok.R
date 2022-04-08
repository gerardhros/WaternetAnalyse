
leaflet() %>% 
  addTiles(urlTemplate = "http://geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartpastel/EPSG:28992/{z}/{x}/{y}.png",
           attribution = "Kaartgegevens &copy; Kadaster",
           options = tileOptions(minZoom = 6, maxZoom = 18),
           group = "pastel"
  ) %>%
  addTiles(urlTemplate = "http://geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartwater/EPSG:28992/{z}/{x}/{y}.png",
           attribution = "Kaartgegevens &copy; Kadaster",
           options = tileOptions(minZoom = 6, maxZoom = 18),
           group = "water"
  ) %>%
  addTiles(urlTemplate = "http://geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/Actueel_ortho25/EPSG:28992/{z}/{x}/{y}.jpeg",
           attribution = "Kaartgegevens &copy; Kadaster",
           group = "luchtfoto"
  ) %>%
  
  addLayersControl(baseGroups = c("standaard", "pastel", "water", "luchtfoto","n2000"),
                   options = layersControlOptions(position = "topright"))

# # The WFS allows spatial filtering (this is not always the case) en bewerken, maar ik krijg geen mooi beeld
# WFS_url <- paste0("http://geodata.nationaalgeoregister.nl/bodemkaart50000/wfs?",
#                   "&service=WFS&version=2.0.0&request=GetFeature&",
#                   "typeName=bodemkaart50000:bodemkaart50000&outputFormat=application/json"
#                   )
# 
# # get WFS feature
# bodem <- st_read(WFS_url)
# 
# # Transform to WGS84
# bodem_wgs84 <- st_transform(bodem,4326)
# 
# # load data in leaflet
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = bodem_wgs84,
#               label = ~bodemcode,
#               popup = ~bodemcode)