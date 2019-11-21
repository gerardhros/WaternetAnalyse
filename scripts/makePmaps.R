
makePmaps <- function(dat){
#hier tabel van sven overnemen voor kP
  
# dat$watertype[dat$GAF == '8070'] <- 'M3'
# Selecteer data en summarise -------------------------------------------
dat$bodem <- dat$i_bt1
dat$bodem[is.na(dat$i_bt1)& dat$watertype %in% c('M8','M10','M27','M25')] <- "VEEN"
dat$bodem[is.na(dat$i_bt1)& dat$watertype %in% c("M3", "M1a","M30","M14","M11","M6a","M7b","M6b","M7a")] <- "KLEI"  

# dat1<- dat[!is.na(dat$KRW),]
dg <- dat %>%
  # toegepaste filters
  filter(jaar %in% c(2010:2018)) %>%          
  # groeperingsfactor
  group_by(pol, EAG, GAF, KRW, watertype, bodem, 
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
# hier toevoegen welke wel aan max belasting moet


# koppel kp plassen obv invoertabel per EAG
Overzicht_kP_plas <- Overzicht_kP

sel <- dgwatdte$watertype %in% c('M20','M27','M25',"M14")  
PvskPplas <-  merge(dgwatdte[sel & !is.na(dgwatdte$EAG) ,], Overzicht_kP_plas[!is.na(Overzicht_kP_plas$EAG),c('EAG',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.', 
                                                        'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                    by.x = 'EAG', by.y = 'EAG', all.x = F, all.y = T)

PvskPplas1 <-  merge(dgwatdte[sel& !is.na(dgwatdte$GAF),], Overzicht_kP_plas[!is.na(Overzicht_kP_plas$afvoergebied),c('afvoergebied','EAG',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.', 
                                                         'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                     by.x = 'GAF', by.y = 'afvoergebied', all.x = F, all.y = T)

PvskPplas2 <-  merge(dgwatdte[sel,], Overzicht_kP_plas[,c('EAG',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.',
                                                         'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                     by.x = 'KRW', by.y = 'EAG', all.x = F, all.y = T)

pvskp <- smartbind(PvskPplas, PvskPplas1)

PvskP <- merge(PvskP, pvskp[,c('pol','EAG','Troebel.naar.helder..mg.P.m2.d.', 'P.load_year..mgP.m2.d.', 
                            'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')], by = c('pol','EAG'), all = TRUE)

PvskP$wp_min_sum[!is.na(PvskP$P.load_year..mgP.m2.d.)]  <- PvskP$P.load_year..mgP.m2.d.[!is.na(PvskP$P.load_year..mgP.m2.d.)] 
PvskP$PvskPLake <- PvskP$wp_min_sum/PvskP$Helder.naar.troebel..mg.P.m2.d. # >1 is te veel

PvskP <- PvskP[!is.na(PvskP$wp_min_sum),]
return(PvskP)
#write.table(PvskP, file = paste(getwd(),"/pbelasting/output/PvskPditchlakeALL",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
# schrijf data weg voor controle ------------------------------------------------
# geen p belasting in ouderkerk, gaasperplas, loenderveen en terra nova, boezem toevoegen: toevoegen tabel

}

makePmaps1 <- function(PvskP){

  col <- c('1'="blue",'2'="deepskyblue", '3'="yellow",'4'="orange",'5'="red")
  labels <- c('1'="0-0.9",'2'="0.9-1.0" ,'3'="1.0-1.2",'4'="1.2-2.0",'5'=">2.0")
  
  lake <- PvskP[!is.na(PvskP$PvskPLake),]
  lake$lake <- cut(lake$PvskPLake, breaks= c(0, 0.9, 1, 1.2, 2, 20))
  lake$lake <- as.factor(lake$lake)
  pal <- colorFactor(palette = col ,  domain = lake$lake)
  
  ditch <- PvskP[is.na(PvskP$PvskPLake),]
  ditch$ditch <- cut(ditch$PvskPDitch, breaks= c(0, 0.9, 1, 1.2, 2, 20))
  ditch$ditch <- as.factor(ditch$ditch)
  pal2 <- colorFactor(palette = col ,  domain = ditch$ditch)
  
  map2 <- sp::merge(gEAG, lake[, c('pol', 'EAG','GAF','KRW', 'bodem','watdte','w_debiet', 'wp_min_sum','wp_inc_sum', 'wp_tot_sum',
                                    'wp_meting_mgm2d', 
                                    "P.load_year..mgP.m2.d.", "kPDitch",
                                    'Troebel.naar.helder..mg.P.m2.d.', 
                                    'Helder.naar.troebel..mg.P.m2.d.', 'PvskPLake', 'lake','lake.ditch.vollenweider')],
                    by.x = 'GAFIDENT', by.y ='EAG' ,all.x = FALSE, duplicateGeoms = T)
  map <- sp::merge(gEAG, ditch[, c('pol', 'EAG','GAF','KRW', 'bodem','watdte','w_debiet', 'wp_min_sum','wp_inc_sum', 'wp_tot_sum',
                                   'wp_meting_mgm2d', 
                                  "P.load_year..mgP.m2.d.", "kPDitch",
                                  'Troebel.naar.helder..mg.P.m2.d.', 
                                  'Helder.naar.troebel..mg.P.m2.d.', 'PvskPDitch', 'ditch','lake.ditch.vollenweider')],
                   by.x = 'GAFIDENT', by.y ='EAG' ,all.x = FALSE, duplicateGeoms = T)
  map3 <- sp::merge(gGAF, lake[, c('pol', 'EAG','GAF','KRW', 'bodem','watdte','w_debiet', 'wp_min_sum','wp_inc_sum', 'wp_tot_sum',
                                   'wp_meting_mgm2d',
                                   "P.load_year..mgP.m2.d.", "kPDitch",
                                   'Troebel.naar.helder..mg.P.m2.d.',
                                   'Helder.naar.troebel..mg.P.m2.d.', 'PvskPLake', 'lake','lake.ditch.vollenweider')],
                    by.x = 'GAFIDENT', by.y ='GAF' ,all.x = FALSE, duplicateGeoms = T)
  map4 <- sp::merge(gGAF, ditch[, c('pol', 'EAG','GAF','KRW', 'bodem','watdte','w_debiet', 'wp_min_sum','wp_inc_sum', 'wp_tot_sum',
                                    'wp_meting_mgm2d',
                                   "P.load_year..mgP.m2.d.", "kPDitch",
                                   'Troebel.naar.helder..mg.P.m2.d.',
                                   'Helder.naar.troebel..mg.P.m2.d.', 'PvskPDitch', 'ditch','lake.ditch.vollenweider')],
                   by.x = 'GAFIDENT', by.y ='GAF' ,all.x = FALSE, duplicateGeoms = T)

  
  leaflet() %>%
     addPolygons(data = map4, layerId = map4$GAFIDENT, popup= paste("GAF naam", map4$GAFNAAM, "<br>",
                                                                    "GAF", map4$GAFIDENT, "<br>",
                                                                    "P belasting min:", map4$wp_min_sum , "<br>",
                                                                    "P belasting max:", map4$wp_tot_sum , "<br>",
                                                                    "kritische P belasting:", map4$kPDitch, "<br>",
                                                                    "PvskP:", map4$PvskPDitch, "<br>",
                                                                    "waterdiepte:", map4$meetwaarde.x, "<br>",
                                                                    "bodemtype:", map4$bodem),
                 stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                 fill=T, fillColor = ~pal2(map4$ditch), fillOpacity = 0.6) %>%
     addPolygons(data= map3, layerId = map3$GAFIDENT, popup= paste("GAF naam", map3$GAFNAAM, "<br>",
                                                                   "GAF", map3$GAFIDENT, "<br>",
                                                                   "P belasting min:", map3$wp_min_sum , "<br>",
                                                                   "P belasting max:", map3$wp_tot_sum , "<br>",
                                                                   "Kritische P belasting - th:", map3$Troebel.naar.helder..mg.P.m2.d., "<br>",
                                                                   "Kritische P belasting - ht:", map3$Helder.naar.troebel..mg.P.m2.d., "<br>",
                                                                   "PvskP obv hoge grens en minimale belasting:", map3$PvskPLake, "<br>",
                                                                   "waterdiepte:", map3$watdte, "<br>",
                                                                   "bodemtype:", map3$bodem),
                 stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                 fill=T, fillColor = ~pal(map3$lake), fillOpacity = 0.6) %>%
     addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                                  "EAG", map$GAFIDENT, "<br>",
                                                                  "P belasting min:", map$wp_min_sum , "<br>",
                                                                  "P belasting nax:", map$wp_tot_sum , "<br>",
                                                                  "kritische P belasting:", map$kPDitch, "<br>",
                                                                  "PvskP:", map$PvskPDitch, "<br>",
                                                                  "waterdiepte:", map$meetwaarde.x, "<br>",
                                                                  "bodemtype:", map$bodem),
                 stroke = T, color= 'green', opacity=0.8, weight = 1.5, smoothFactor = 0.8,
                 fill=T, fillColor = ~pal2(map$ditch), fillOpacity = 0.6) %>%

      addPolygons(data= map2, layerId = map2$GAFIDENT, popup= paste("EAG naam", map2$GAFNAAM, "<br>",
                                                                    "EAG", map2$GAFIDENT, "<br>",
                                                                   "P belasting min:", map2$wp_min_sum , "<br>",
                                                                   "P belasting max:", map2$wp_tot_sum , "<br>",
                                                                   "Kritische P belasting - th:", map2$Troebel.naar.helder..mg.P.m2.d., "<br>",
                                                                   "Kritische P belasting - ht:", map2$Helder.naar.troebel..mg.P.m2.d., "<br>",
                                                                   "PvskP obv hoge grens en minimale belasting:", map2$PvskPLake, "<br>",
                                                                   "waterdiepte:", map2$watdte, "<br>",
                                                                   "bodemtype:", map2$bodem),
                 stroke = T, color= 'green', opacity=0.8, weight = 1.5, smoothFactor = 0.8,
                 fill=T, fillColor = ~pal(map2$lake), fillOpacity = 0.6) %>%


    
    addLegend("bottomright", colors= col, labels=labels, title = 'P / kP')%>%
    addTiles()
}

makePmaps2 <- function(PvskP){
  
  col <- c('1'="blue",'2'="deepskyblue", '3'="green",'4'="darkgreen",'5'="yellow",'6'="orange",'7'="darkorange",'8'="red",'9'="darkred")
  labels <- c('1'="0-0.9",'2'="0.9-1.0" ,'3'="1.0-1.2",'4'="1.2-2.0",'5'="2.0",'6'='5','7'='10','8'='20','9'='>80')
  
  PvskP <- PvskP[!is.na(PvskP$wp_tot_sum),]
  PvskP$wp_tot_sumK <- cut(PvskP$wp_tot_sum, breaks= c(0, 0.9, 1, 1.5, 2, 5,10,20,max(PvskP$wp_tot_sum)))
  PvskP$wp_tot_sumK <- as.factor(PvskP$wp_tot_sumK)
  pal <- colorFactor(palette = col ,  domain = PvskP$wp_tot_sum)
  
  
  map3 <- sp::merge(gGAF, PvskP[, c('pol', 'EAG','GAF','KRW', 'bodem','watdte','w_debiet', 'wp_min_sum','wp_inc_sum', 'wp_tot_sum',
                                   'wp_meting_mgm2d', 
                                   "P.load_year..mgP.m2.d.", "kPDitch",
                                   'Troebel.naar.helder..mg.P.m2.d.', 
                                   'Helder.naar.troebel..mg.P.m2.d.', 'PvskPLake', 'lake.ditch.vollenweider')],
                    by.x = 'GAFIDENT', by.y ='GAF' ,all.x = FALSE, duplicateGeoms = T)
  map2 <- sp::merge(gEAG, PvskP[, c('pol', 'EAG','GAF','KRW', 'bodem','watdte','w_debiet', 'wp_min_sum','wp_inc_sum', 'wp_tot_sum',
                                    'wp_meting_mgm2d', 
                                    "P.load_year..mgP.m2.d.", "kPDitch",
                                    'Troebel.naar.helder..mg.P.m2.d.', 
                                    'Helder.naar.troebel..mg.P.m2.d.', 'PvskPDitch', 'lake.ditch.vollenweider')],
                    by.x = 'GAFIDENT', by.y ='EAG' ,all.x = FALSE, duplicateGeoms = T)
 
  leaflet() %>%
   addPolygons(data= map3, layerId = map3$GAFIDENT, popup= paste("EAG naam", map3$GAFNAAM, "<br>",
                                                                  "P belasting:", map3$P.load_year..mgP.m2.d. , "<br>",
                                                                  "P belasting balans:", map3$wp_tot_sum , "<br>",
                                                                  "Kritische P belasting - th:", map3$Troebel.naar.helder..mg.P.m2.d., "<br>",
                                                                  "Kritische P belasting - ht:", map3$Helder.naar.troebel..mg.P.m2.d., "<br>",
                                                                  "PvskP:", map3$PvskPLake, "<br>",
                                                                  "waterdiepte:", map3$watdte, "<br>",
                                                                  "bodemtype:", map3$bodem),
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(map3$wp_tot_sum), fillOpacity = 0.6) %>%
    addPolygons(data= map2, layerId = map2$GAFIDENT, popup= paste("EAG naam", map2$GAFNAAM, "<br>",
                                                                  "P belasting:", map2$P.load_year..mgP.m2.d. , "<br>",
                                                                  "P belasting balans:", map2$wp_tot_sum , "<br>",
                                                                  "Kritische P belasting - th:", map2$Troebel.naar.helder..mg.P.m2.d., "<br>",
                                                                  "Kritische P belasting - ht:", map2$Helder.naar.troebel..mg.P.m2.d., "<br>",
                                                                  "PvskP:", map2$PvskPLake, "<br>",
                                                                  "waterdiepte:", map2$watdte, "<br>",
                                                                  "bodemtype:", map2$bodem),
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(map2$wp_tot_sum), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors= col, labels=labels, title = 'P mg/m2/dag')%>%
    addTiles()
}
# plot fracties bronnen P belasting -------------------------------------


