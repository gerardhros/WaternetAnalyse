# inladen achtergrondkaart
basemap <- tmaptools::read_osm(gebied)

lijsttmap_mode('plot')  
# Create the map
levels(l$klasse) <- labels

#map <- 
m1 <- 
  # tm_basemap(basemap)+
  #   tm_raster+
  tm_shape(gebied) +
  tm_borders(col = "springgreen4", lwd =2) +
  tm_shape(l)+
  tm_symbols(col = "klasse", palette = col, size = 3)+
  tm_layout(main.title = subtitel,
            main.title.position = "center",
            main.title.size = 1.1,
            legend.position = c("left", "bottom"),
            legend.title.size = 1.1,
            legend.text.size = 0.75,
            legend.bg.color = "white",
            legend.bg.alpha = 0.9,
            legend.frame = TRUE)+
  tm_facets(by = "jaar", nrow =1, free.coords = FALSE, inside.original.bbox = TRUE)

#tmap_animation(m1, filename="Dutch_provinces.gif", width=800, delay=40)
#tmap_save(map, filename = paste0(subtitel, format(Sys.time(),"%Y%m%d%H%M"),".png"))