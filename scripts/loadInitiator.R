# load additional data (initiator/giab)


# 1. Initiator --------------------------------------------------------

loadInitiator <- function(){
  init0 <- read_excel('input/160705_initiator.xlsx', sheet=1) %>% 
    select(pol=POLDER, 
           Ndm = Ninam, Nkm = Ninfe, Pdm = Pinam, Pkm = Pinfe, 
           POX = Poxt, PAL1 = Palfe1, PAL2 = Palfe2, PAL3 = Palfe3, 
           bt1 = BT, bt2 =SOIL, GWT = GT, VEG, GEW) %>% 
    mutate(pol = as.character(pol)) %>% 
    group_by(pol)
  
  # mean of all numeric collumns
  initNum <- init0 %>% select_if(is.numeric) %>% 
    summarise_all(mean)
  
  # Fucntion to calculate the mode
  mode <- function(x) {
    # returns value that appears most often in a set of data
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # mode of all character collumms
  initFac <- init0 %>% select_if(is.character) %>% summarise_all(mode) 
  
  # join them together to get all data fram initiator (13 vars)
  init <- inner_join(initNum, initFac, by='pol')
  names(init) <- paste0('i_', names(init))
  return(init)
}


# 2. Bedrijfs grondgebruik ------------------------------------------------
loadGiab <- function(){
  # read data
  giab0 <- read_excel('input/160706_giab.xlsx', sheet=1) %>% 
    select(x, y, gras = gras_ha, bouw=bouw_ha, nat=nat_ha)
  
  # make spatial
  coordinates(giab0) <- ~x+y
  proj4string(giab0) <- CRS(proj4string(shp)) 
  
  # Calculate sum landbouw gebieden per polder
  giab = data.frame(giab0@data) %>% 
    group_by(CODE) %>% 
    summarise_all(funs(sum(.), n())) %>% 
    select(pol =CODE, gras=gras_sum, bouw=bouw_sum, nat=nat_sum, n=gras_n) %>% 
    mutate(pol = as.character(pol)) %>% 
    as.data.frame()
  names(giab) = paste0('g_', names(giab))
  return(giab)
}


# 3. GXG Data Jos ----------------------------------------------------------
loadGXG <- function(){
  gxgDat <- readOGR(dir_shp, layer = 'GXG_AGV')[,c('GLG', 'GHG')]
  gxg <- over(shp,gxgDat, fn = median) 
  gxg$CODE = shp$CODE
  return(gxg)
}
# er moet geen rood puntje voor de regel staan

# 4. Bodemkaart 50 data ---------------------------------------------------

loadKaart <- function(){
  # kaartDat <- readOGR(dir_shp, layer = 'bodem50_AGV')[,c('LETTER', 'CYFER')]
  # r <- raster(ncol = 1000, nrow = 1000)
  # extent(r) <- extent(kaartDat)
  # rL <- rasterize(kaartDat, r) 
  # save(rL, file = 'data/raster/bodem50.Rdata')
  
  # load('data/raster/bodem50.Rdata')
  # 
  # t1 <- rasterToPoints(rL, spatial=TRUE)
  # tmp = rL@data@attributes
  # tmp2 = cbind(tmp[[1]], data.frame(coordinates(rL)))
  # 
  # mode <- function(x) {ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}
  # 
  # tmp  = over(rL,shp)
  # mean = aggregate(tmp$var1.pred, list(tmp$CODE), median, na.rm=T) # mean voor elke polder
  # 
  # 
  # test = mask(rL, shp)
  # over(shp, rL, fn=mode)
  
}






