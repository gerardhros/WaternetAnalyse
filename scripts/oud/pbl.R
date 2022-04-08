# totaalvrachten combibalansen soms en kunnen niet zomaar gesommeerd per WL, niet alle in en uit is ook in en uit van het gehele waterlichaam
# dus gemiddelde m3/dag en dan vermenigvuldigen met aantal dagen per kwartaal
# dus gemiddelde mg/m2/dag en dan vermigvuldigen met som wateroppervlak verschillende EAGs en aantal dagen per kwartaal
# niet alle eag inlaat is inlaat wl, gesommeerd per kwartaal - niet over jaren

# overzicht balans ----------------------------
# import dat
pbl <- dat %>%
  #merge code en krw wl
  left_join(doelen, by = c('CODE'='gebied'))%>%
  # toegepaste filters
  filter(jaar %in% c(2012:2017)) %>%    
  # groeperingsfactor #kwartaal toevoegen
  group_by(HoortBijGeoobject.identificatie, seiz) %>% 
  select(starts_with("wp_"), starts_with("ww_"), starts_with("wc_"))%>%
  # neem gemiddelde
  summarise_all(mean, na.rm=T) 

wl <- read.csv("./pbelasting/gebied_wateroppervlak_toetsgebied_watertype.csv", header = TRUE, na.strings = " ", sep=";", dec =".", 
                 stringsAsFactors = F)
opp <- wl %>%
  # groeperingsfactor #kwartaal toevoegen
  group_by(toetsgebied) %>% 
  summarise_all(mean, na.rm=T) %>%
  select(toetsgebied, OppervlakLandWater, OppervlakWater)

pbl$uit_afspoeling_drain <- pbl$wc_afstroom + pbl$wc_drain + pbl$wc_uitspoel + pbl$wc_verhard
pbl$inlaat_tot <- pbl$wc_inlaat1 + pbl$wc_inlaat2 + pbl$wc_inlaat3 + pbl$wc_inlaat4 + pbl$wc_inlaat5
pbl$uitlaat_tot <- pbl$ww_o_uitlaat + pbl$ww_o_uitlaat1 + pbl$ww_o_uitlaat2 + pbl$ww_o_uitlaat3 + pbl$ww_o_uitlaat4 + pbl$ww_o_uitlaat5
pbl$puitspoel_tot <- pbl$wp_min_afstroom + pbl$wp_min_drain + pbl$wp_min_uitspoel + pbl$wp_min_verhard
pbl$pinlaat_tot <- pbl$wp_min_inlaat + pbl$wp_min_inlaat1 + pbl$wp_min_inlaat2 + pbl$wp_min_inlaat3 + pbl$wp_min_inlaat4 + pbl$wp_min_inlaat5

# omzetten naar kg/kwartaal sum om van m3/d naar m3/kwartaal te gaan bij P in mg/m2/dag * wateroppervlak/ 1000.000?
pbl2 = pbl %>% select(waterlichaam = HoortBijGeoobject.identificatie,
                      kwartaal = seiz,
                      neerslag_m3d = wc_neerslag,
                      kwel_m3d = wc_kwel,
                      uit_afspoeling_drain_m3d = uit_afspoeling_drain,
                      riool_m3d = wc_riol,
                      inlaat_m3d = inlaat_tot,
                      
                      verdamping_m3d = ww_o_verdamping,
                      wegzijging_m3d = ww_o_wegzijg,
                      intrek_m3d = ww_o_intrek,
                      uitlaat_m3d = uitlaat_tot,
                      
                      neerslag_mgm2d = wp_min_neerslag,
                      kwel_mgm2d = wp_min_kwel,
                      uit_afspoeling_drain_mgm2d = puitspoel_tot,
                      riool_mgm2d = wp_min_riol,
                      inlaat_mgm2d = pinlaat_tot) %>% 
    left_join(opp, by = c('waterlichaam' = 'toetsgebied')) %>%
    filter(str_detect(waterlichaam, "^NL11_"))

pbl3 <- melt(pbl2)
pbl3 <- dcast(pbl3, variable ~ waterlichaam + kwartaal)
write.table(pbl3, file = paste(getwd(),"./output/pblbalans",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

# P vs kP en bodemnalevering per waterlichaam ------------------------
# import bod
Overzicht_kP <- importCSV('pbelasting/Overzicht_kP.csv', path = getwd()) 
nomogram <- importCSV('pbelasting/nomogram.csv', path = getwd())

nomogram$watdte_m <- cut(nomogram$watdte_m, breaks = c('0','0.3','0.5','0.7'))

# bodem toevoegen
selb <- dcast(bod, locatie.EAG+locatie.x+locatie.y+locatie.z+datum+jaar ~ fewsparameter+compartiment, value.var = "meetwaarde", fun.aggregate = mean)
selb$FESPPWratio <-(((selb$`Fe_ug/l_nf_PW`/1000)/55.845)-(selb$`Stot_mg/l_PW`/32.065))/(selb$`Ptot_mgP/l_nf_PW`/30.974)
selb$nlvrFW <- 0.0247*selb$`Ptot_mgP/l_ng_BS`-1.6035
#selb$nlvrOls <- 0.0058*(selb$`Ptot_mgPOlsen/l_ng_BS`/0.030974)-1.1361
#selb$nlvrDW <- 0.0077*(selb$`Ptot_gP/kg_dg_BS`*1000)-4.7259
selb$nlvrPW <- 0.8095*selb$`Ptot_mgP/l_nf_PW`-0.2905

mdPtb <- hybi %>%
  filter(jaar %in% c(2010:2017)) %>% 
  filter(fewsparameter %in% 'WATDTE_m') %>%               
  group_by(locatie.EAG) %>% 
  summarise_at(c('meetwaarde'), median, na.rm=T) 
mdPtb$watdte <- cut(mdPtb$meetwaarde, breaks = c('0','0.3','0.5','0.7'))

mdPtbG <- hybi %>%
  filter(jaar %in% c(2010:2017)) %>% 
  filter(fewsparameter %in% 'WATDTE_m') %>%               
  group_by(locatie.afaanvoergebied) %>% 
  summarise_at(c('meetwaarde'), median, na.rm=T) 
mdPtbG$watdte <- cut(mdPtbG$meetwaarde, breaks = c('0','0.3','0.5','0.7'))

# koppel waterdiepte per eag en afvoergebied aan water en stoffenbalans
dgwatdte <- merge(dg, mdPtb, by.x = 'CODE', by.y = 'locatie.EAG', all.x = TRUE)
dgwatdte <- merge(dgwatdte, mdPtbG, by.x = 'CODE', by.y = 'locatie.afaanvoergebied', all.x = TRUE)
dgwatdte$watdte <- dgwatdte$watdte.y
dgwatdte$watdte[!is.na(dgwatdte$watdte.x)] <- dgwatdte$watdte.x[!is.na(dgwatdte$watdte.x)]
dgwatdte$watdte[dgwatdte$meetwaarde.x > 0.7] <- '(0.5,0.7]'
dgwatdte <- dgwatdte[!is.na(dgwatdte$watdte),] 

ditchkP <- NULL
for (i in unique(nomogram$bodemtype)){
  nomogram_bt <- nomogram[nomogram$bodemtype == i,]
  dgwatdte_bt <- dgwatdte[dgwatdte$bt_klasse1 == toupper(i),]
  for(j in unique(dgwatdte_bt$watdte)){
    nomogram_btdp <- nomogram_bt[nomogram_bt$watdte == j,]
    dgwatdte_btdp <- dgwatdte_bt[dgwatdte_bt$watdte == j,]
    # hier gaat het mis
    ditch <- merge(dgwatdte_btdp, nomogram_btdp, by.x = 'ww_a_debiet', by.y = 'debiet..mm.dag.', all = TRUE)
    ditch <- ditch[order(ditch$ww_a_debiet),]
    ditch$kP2 <- na.approx(ditch$kP, method = "linear", rule = 2) # interpolatie kP tussen debieten in nomogram
    ditch$ww_a_debiet <- as.numeric(ditch$ww_a_debiet)
    ditchkP <- rbind(ditch[!is.na(ditch$CODE),], ditchkP)
  }
}

#koppel kritische fosforgrenzen obv waterdiepte, hydraulische belasting en bodemtype aan metamodel PCditch
PvskP <- ditchkP

#koppel kritische fosforgrenzen obv waterdiepte, hydraulische belasting en bodemtype aan metamodel PCditch
PvskP <- ditchkP
# koppel kp plassen
Overzicht_kP_plas <- Overzicht_kP
PvskPplas <-  merge(dgwatdte, Overzicht_kP_plas, by.x = 'CODE', by.y = 'EAG', all.x = FALSE, all.y = TRUE)

# bereken kP ------------------------------------------------------------
PvskP$wp_tot_sum <- PvskP$wp_min_sum+PvskP$wp_inc_sum
PvskP$PvskPDitch<- PvskP$wp_min_sum/PvskP$kP2 # >1 is te veel
PvskPplas$wp_tot_sum <- PvskPplas$wp_min_sum+PvskPplas$wp_inc_sum
PvskPplas$PvskPLake <- PvskPplas$wp_min_sum/PvskPplas$Troebel.naar.helder..mg.P.m2.d. # >1 is te veel
sel <- PvskPplas$lake.ditch.vollenweider == 'vollenweider'
PvskPplas$PvskPLake[sel] <- PvskPplas$P.load_year..mgP.m2.d.[sel]/PvskPplas$Helder.naar.troebel..mg.P.m2.d.[sel] # >1 is te veel

# schrijf data weg voor controle ------------------------------------------------
sel <- PvskP[,c('CODE', 'bt_klasse1','watdte.x','ww_a_debiet', 'wp_min_sum','wp_inc_sum', 'wp_tot_sum','wp_meting_mgm2d', 
                "kP2", 'PvskPDitch')]
#write.table(sel, file = paste(getwd(),"/output/PvskPditch",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

sel2 <- PvskPplas[,c('CODE', 'bt_klasse1','watdte','ww_a_debiet', 'wp_min_sum','wp_inc_sum', 'wp_tot_sum','wp_meting_mgm2d', 
                     "P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.', 
                     'Helder.naar.troebel..mg.P.m2.d.', 'PvskPLake', 'lake.ditch.vollenweider')]
#write.table(sel2, file = paste(getwd(),"/output/PvskPlake",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)


# merge ekr kP -----
pkp = PvskP %>% select(CODE,
                       P_belasting = wp_tot_sum,
                       kP = Troebel.naar.helder..mg.P.m2.d.,
                       PvskP) %>%
  left_join(wl, by = c('CODE' = 'gebied')) %>%
  left_join(selb, by = c('CODE' = 'locatie.EAG')) %>%
  filter(str_detect(toetsgebied, "^NL11_")) %>%
  # groeperingsfactor #kwartaal toevoegen
  group_by(toetsgebied) %>% 
  summarise_all(mean, na.rm=T) %>%
  select(toetsgebied, P_belasting, kP, PvskP, FESPPWratio, nlvrPW)

pkp2 <- melt(pkp)
pkp3 <- dcast(pkp2, variable ~ toetsgebied)
write.table(pkp3, file = paste(getwd(),"./output/pblpvskp",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
