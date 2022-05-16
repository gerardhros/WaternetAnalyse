#requires packages
require(ggspatial)
require(stars)
require(tmaptools)
require(tmap)
require(sf)



loadbasemap <- function (sf, source, expand, zoom){
  if (missing(expand) == TRUE) {
    expand <- 1.2
    print("Expand missing. Set to 1.2 (default).")
  }
  if (missing(source) == TRUE) {
    source <- "hybrid"
    print("Source missing. Set to hybrid (default).")
  }
  if (missing(zoom) == TRUE) {
    sf <- sf::st_transform(sf, 28992)
    bbox <- sf::st_bbox(sf)
    bbox <- rosm::extract_bbox(bbox)
    bbox <- matrix(bbox, ncol = 2, byrow = TRUE)
    zoom <- suppressWarnings(rosm:::tile.raster.autozoom(bbox, 
                                                         epsg = 28992)) + 10
    print(paste0("Autozoom based on bbox. Zoomlevel = ", 
                 zoom))
  }
  if (source == "hybrid") {
    basemap <- tmaptools::read_osm(sf, type = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}", 
                                   zoom = zoom, ext = expand)
  }
  else if (source == "topo") {
    basemap <- tmaptools::read_osm(sf, type = "https://mt1.google.com/vt/lyrs=r&x={x}&y={y}&z={z}", 
                                   zoom = zoom, ext = expand)
  }
  else if (source == "topo.v2") {
    basemap <- tmaptools::read_osm(sf, type = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.png", 
                                   zoom = zoom, ext = expand)
  }
  else if (source == "aerial") {
    basemap <- tmaptools::read_osm(sf, type = "https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}", 
                                   zoom = zoom, ext = expand)
  }
  return(basemap)
}

