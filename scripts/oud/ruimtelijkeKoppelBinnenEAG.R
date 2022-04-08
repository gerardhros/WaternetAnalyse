#koppelen bodemdata en vegetatiegegevens obv dichtsbijzijnde locatie in dezelfde EAG
# vegetatie aan bodem koppelen ipv bodem aan vegetatie
locP <- dcast(selp, locatiecode+ x + y + EAG ~ analysecode, value.var = "x", fun.aggregate = mean) # selp vervangen door hybi
xy <- locP[,c(2,3)]
spP <- SpatialPointsDataFrame(coords = xy, data = locP, proj4string = proj4.rd_new)
locB <- dcast(selb, Monsterp_1+ xcoor0xv_wb + ycoor0xv_wb + EAG ~ tp5a_wb, value.var = "xcoor0xv_wb", fun.aggregate = mean) # selb vervangen door bod
xy <- locB[,c(2,3)]
spB <- SpatialPointsDataFrame(coords = xy, data = locB, proj4string = proj4.rd_new)

afsPB = matrix(nrow = 0, ncol = 4)
for (i in unique(locB$EAG)){
  sppeag <- spP[spP$EAG == i,]
  spbeag <- spB[spB$EAG == i,]
  for (j in unique(sppeag$locatiecode)){
    locBodem <- spbeag$Monsterp_1[which.min(gDistance(sppeag[sppeag$locatiecode == j,], spbeag, byid=TRUE))]
    dist <- min(gDistance(sppeag[sppeag$locatiecode == j,], spbeag, byid=TRUE))
    x<-cbind(j,as.character(locBodem),dist,i)
    afsPB<- rbind(x,afsPB)
  }
  colnames(afsPB) <- c("locatieplant", "locatiebodem", "afstand", "EAG")
}
afsPB <- as.data.frame(afsPB)
BB <- merge(selb, afsPB, by.x = 'Monsterp_1', by.y = 'locatiebodem')
selp <- merge(BB, selp, by.y = 'locatiecode', by.x = 'locatieplant', all.x = TRUE, all.y = FALSE)

d1 = fread('data/alles_reliable.csv')
