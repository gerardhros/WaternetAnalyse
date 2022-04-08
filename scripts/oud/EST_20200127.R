estpermonst <- function(macft, grenswaarden_EST){
  soortenlijst_submers <- !is.na(unique(macft$biotaxonnaam[macft$submers == '1']))
  soortenlijst_oever <- !is.na(unique(macft$biotaxonnaam[macft$oever == '1']))
  soortenlijst_kroos <- !is.na(unique(macft$biotaxonnaam[macft$kroos == '1']))
  EAGs <- sort(unique(macft$locatie.EAG))
  # w_long <- do_W_long(df,grenswaarden_EST,monster_subm)
  # o_long <- do_O_long(monster_oever,grenswaarden_EST)
  # df_long_out_totaal <- rbind(df_long_out_totaal, w_long, o_long)
}

do_monster_submers <- function(df,soortenlijst_submers, soortenlijst_kroos){
  monster_df_out <- NULL
  for(i in unique(df$monsterident)){
    sel <- df[df$monsterident == i,]
    doorz_diep <- ifelse(length(sel$meetwaarde[sel$parametercode %in% "ZICHT"])>0 &
                           length(sel$meetwaarde[sel$parametercode %in% "WATDTE"])>0,
                         sel$meetwaarde[sel$parametercode %in% "ZICHT"]/sel$meetwaarde[sel$parametercode %in% "WATDTE"], NA)
    if(!length(doorz_diep)>0){
      doorz_diep <- NA
    }
    n_soort <- nrow(sel[sel$parametercode %in% "" & sel$biotaxonnaam %in% soortenlijst_submers,]) #LET OP: DIT IS VOOR W5 en verder
    woeker <- ifelse(length(sel$meetwaarde[sel$parametercode %in% "" & sel$biotaxonnaam %in% soortenlijst_submers])==0,
                     0,
                     masel(sel$meetwaarde[sel$parametercode %in% "" & sel$biotaxonnaam %in% soortenlijst_submers]))
    SUBMS <- sel$meetwaarde[sel$parametercode %in% "SUBMSPTN"]
    if(!length(SUBMS)>0){
     SUBMS <- min(100,sum(sel$meetwaarde[sel$biotaxonnaam %in% soortenlijst_submers])) #als groeivormmeting ontbreekt, dan bedekkingen optellen
    }
    KROOS <- sel$meetwaarde[sel$parametercode %in% "KROOS"]
    if(!length(KROOS)>0){
      KROOS <- min(100,sum(sel$meetwaarde[sel$biotaxonnaam %in% soortenlijst_kroos])) #als groeivormmeting ontbreekt, dan bedekkingen optellen
    }
    FLAB <- sel$meetwaarde[sel$parametercode %in% "FLAB"]
    if(!length(FLAB)>0){
      FLAB <- 0.001
  }
    monster_df <- data.frame(monsterident=i,locatie.EAG=unique(sel$locatie.EAG),jaar=unique(sel$jaar), watertype=unique(sel$locatie.KRW.watertype),doorz_diep=doorz_diep, n_soort=n_soort, woeker=woeker, SUBMS=SUBMS, KROOS=KROOS, FLAB=FLAB)
    monster_df_out <- rbind(monster_df_out, monster_df )
  }
  #monster_df_out <- na.omit(monster_df_out)
  return(monster_df_out)
}
do_monster_oever <- function(x,z){
  x <- unique(x)
  if(nrow(x)<1){
   monster_df_out <- data.frame(monsterident=0,locatie.EAG=EAG, jaar=jaar, watertype="onbekend",beschoeid=0, n_soort=0, riet=0)  
  }else{
    monster_df_out <- NULL
  monsters <- unique(x$monsterident[ x$compartiment %in% c("OR", "EZ")])#<- GELDT DEZE OEVEROPNAMESELECTIE VOOR ALLE WATERSCHAPPEN???
  if(length(monsters)<1){
    
    monster_df_out <- data.frame(monsterident=0,locatie.EAG=unique(x$locatie.EAG), jaar=unique(x$jaar), watertype=unique(x$locatie.KRW.watertype),beschoeid=0, n_soort=0, riet=0)
  }else{
    for(i in monsters){ #SELECTEERT ALLEEN OEVEROPNAMES!
      if(length(x$meetwaarde[x$monsterident %in% i & x$parametercode %in% "OEVBSIG"])==0){ #als OEVBSIG ontbreekt wordt beschoeing op "nee" gezet!
        beschoeid <- "ja"}else{
          if(x$meetwaarde[x$monsterident %in% i & x$parametercode %in% "OEVBSIG"]==14){#14 is onbeschoeide oever 
            beschoeid <- "nee"}else{beschoeid <- "ja"
            }
        }
      n_soort <- nrow(x[x$monsterident %in% i & x$parametercode %in% "" & x$biotaxonnaam %in% z,])
      riet <- x$meetwaarde[x$monsterident %in% i & x$biotaxonnaam %in% "Phragmites australis"]
      if(length(riet)<1){riet <- 0}
      monster_df <- data.frame(monsterident=i,locatie.EAG=unique(x$locatie.EAG),jaar=unique(x$jaar), watertype=unique(x$locatie.KRW.watertype[x$monsterident %in% i]),beschoeid=beschoeid, n_soort=n_soort, riet=riet)
      monster_df_out <- rbind(monster_df_out, monster_df )
    }
   # monster_df_out <- na.omit(monster_df_out)
  }
  }
  return(monster_df_out)
}  

#eigenlijk zijn dit de functie om netjes te maken
#aggregatie als EST per monster kan obv aantal monsters achteraf
do_W_long <- function(df,grenswaarden_EST, monst_data){
  #W1
  grens_1 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% "FLAB"])
  x_sel <- df[ df$parametercode %in% "FLAB", ]
  
  if(nrow(x_sel)<1){x_w1 <- unique(df[,c("locatie.EAG", "jaar", "locatie.KRW.watertype", "monsterident")])
  x_w1$oordeel <- 99
  x_w1$EST <- "w1"
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- "w1"
    x_sel$oordeel[x_sel$meetwaarde >= grens_1] <- 1 #1=ja, 0=nee, 99=onbekend
    x_sel$oordeel[x_sel$meetwaarde < grens_1]  <- 0
    x_w1 <- x_sel[,c("locatie.EAG","jaar", "locatie.KRW.watertype", "monsterident", "oordeel", "EST")]
  }
  #W2
  grens_2 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% "KROOS"])
  x_sel <- x[  x$parametercode %in% "KROOS", ]
  if(nrow(x_sel)<1){x_w2 <- unique(x[,c("locatie.EAG", "jaar", "locatie.KRW.watertype", "monsterident")])
  x_w2$oordeel <- 99
  x_w2$EST <- "w2"
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- "w2"
    x_sel$oordeel[x_sel$meetwaarde >= grens_2] <- 1#1=ja, 0=nee, 99=onbekend
    x_sel$oordeel[x_sel$meetwaarde < grens_2]  <- 0
    x_w2 <- x_sel[,c("locatie.EAG","jaar", "locatie.KRW.watertype","monsterident", "oordeel", "EST")]
  }
  #W3
  grens_3 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W3" & grenswaarden_EST$PAR_NAME %in% "DRIJFBPTN"])
  x_sel <- x[ x$parametercode %in% "DRIJFBPTN", ]
  if(nrow(x_sel)<1){x_w3 <- unique(x[,c("locatie.EAG", "jaar", "locatie.KRW.watertype", "monsterident")])
  x_w3$oordeel <- 99
  x_w3$EST <- "w3"
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- "w3"
    x_sel$oordeel[x_sel$meetwaarde >= grens_3] <- 1 #1=ja, 0=nee, 99=onbekend
    x_sel$oordeel[x_sel$meetwaarde < grens_3]  <- 0
    x_w3 <- x_sel[,c("locatie.EAG","jaar", "locatie.KRW.watertype","monsterident", "oordeel", "EST")]
  }
  #W4
  grens_4_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W4" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  x_sel <- monst_data[!is.na(monst_data$doorz_diep),]
  if(nrow(x_sel)<1){
    x_w4 <- x_w1
    x_w4$oordeel <- 99
    x_w4$EST <- "w4"
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- "w4"
    x_sel$oordeel[x_sel$doorz_diep <= grens_4_zicht] <- 1 
    x_sel$oordeel[x_sel$doorz_diep > grens_4_zicht] <- 0
    x_w4 <- x_sel[,c("locatie.EAG","jaar", "watertype","monsterident", "oordeel", "EST")]
    names(x_w4) <- c("locatie.EAG","jaar", "locatie.KRW.watertype","monsterident", "oordeel", "EST")
  }
  #W5 
  grens_5_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W5" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_5_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W5" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_5_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W5" & grenswaarden_EST$PAR_NAME %in% c("woeker_soort")])
  grens_5_submers <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W8" & grenswaarden_EST$PAR_NAME %in% c("SUBMS")]) # Let op, check of totaal submers niet meer is dan bovengrens van toestand W8
  grens_5_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_5_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  x_sel <- monst_data[!is.na(monst_data$doorz_diep) & !is.na(monst_data$n_soort) & !is.na(monst_data$SUBMS) & !is.na(monst_data$woeker) & !is.na(monst_data$KROOS) & !is.na(monst_data$FLAB),]
  if(nrow(x_sel)<1){
    x_w5 <- x_w1
    x_w5$oordeel <- 99
    x_w5$EST <- "w5"
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- "w5"
    x_sel$oordeel[x_sel$doorz_diep >= grens_5_zicht & x_sel$n_soort >= grens_5_n_soort & x_sel$SUBMS >= grens_5_submers & x_sel$woeker < grens_5_woeker & x_sel$KROOS < grens_5_kroos & x_sel$FLAB < grens_5_flab] <- 1
    x_sel$oordeel[!(x_sel$doorz_diep >= grens_5_zicht & x_sel$n_soort >= grens_5_n_soort & x_sel$SUBMS >= grens_5_submers & x_sel$woeker < grens_5_woeker & x_sel$KROOS < grens_5_kroos & x_sel$FLAB < grens_5_flab)] <- 0
    x_w5 <- x_sel[,c("locatie.EAG","jaar", "watertype","monsterident", "oordeel", "EST")]
    names(x_w5) <- c("locatie.EAG","jaar", "locatie.KRW.watertype","monsterident", "oordeel", "EST")
  }
  #W6
  grens_6_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W6" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_6_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W6" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_6_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W6" & grenswaarden_EST$PAR_NAME %in% c("woeker_soort")])
  grens_6_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_6_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  x_sel <- monst_data[!is.na(monst_data$doorz_diep) & !is.na(monst_data$n_soort) &  !is.na(monst_data$woeker) & !is.na(monst_data$KROOS) & !is.na(monst_data$FLAB),]
  if(nrow(x_sel)<1){
    x_w6 <- x_w1
    x_w6$oordeel <- 99
    x_w6$EST <- "w6"
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- "w6"
    x_sel$oordeel[x_sel$doorz_diep >= grens_6_zicht & x_sel$n_soort >= grens_6_n_soort & x_sel$woeker >= grens_6_woeker & x_sel$KROOS < grens_6_kroos & x_sel$FLAB < grens_6_flab]<-1
    x_sel$oordeel[!(x_sel$doorz_diep >= grens_6_zicht & x_sel$n_soort >= grens_6_n_soort & x_sel$woeker >= grens_6_woeker & x_sel$KROOS < grens_6_kroos & x_sel$FLAB < grens_6_flab)]<-0
    x_w6 <- x_sel[,c("locatie.EAG","jaar","watertype","monsterident", "oordeel", "EST")]
    names(x_w6) <- c("locatie.EAG","jaar", "locatie.KRW.watertype","monsterident", "oordeel", "EST")
  }
  #W7
  grens_7_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W7" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_7_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W7" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_7_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W7" & grenswaarden_EST$PAR_NAME %in% c("SUBMS")])
  grens_7_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_7_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  x_sel <- monst_data[!is.na(monst_data$doorz_diep) & !is.na(monst_data$n_soort) &  !is.na(monst_data$SUBMS) & !is.na(monst_data$KROOS) & !is.na(monst_data$FLAB),]
  if(nrow(x_sel)<1){
    x_w7 <- x_w1
    x_w7$oordeel <- 99
    x_w7$EST <- "w7"
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- "w7"
    x_sel$oordeel[x_sel$doorz_diep >= grens_7_zicht & x_sel$n_soort <= grens_7_n_soort & x_sel$n_soort > 0 & x_sel$SUBMS < grens_7_woeker & x_sel$KROOS < grens_7_kroos & x_sel$FLAB < grens_7_flab] <-1
    x_sel$oordeel[!(x_sel$doorz_diep >= grens_7_zicht & x_sel$n_soort <= grens_7_n_soort & x_sel$n_soort > 0 & x_sel$SUBMS < grens_7_woeker & x_sel$KROOS < grens_7_kroos & x_sel$FLAB < grens_7_flab)] <-0
    x_w7 <- x_sel[,c("locatie.EAG","jaar","watertype","monsterident", "oordeel", "EST")]
    names(x_w7) <- c("locatie.EAG","jaar", "locatie.KRW.watertype","monsterident", "oordeel", "EST")
  }
  #W8
  grens_8_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W8" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_8_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W8" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_8_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W8" & grenswaarden_EST$PAR_NAME %in% c("SUBMS")])
  grens_8_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_8_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  x_sel <- monst_data[!is.na(monst_data$doorz_diep) & !is.na(monst_data$n_soort) &  !is.na(monst_data$SUBMS) & !is.na(monst_data$KROOS) & !is.na(monst_data$FLAB),]
  if(nrow(x_sel)<1){
    x_w8 <- x_w1
    x_w8$oordeel <- 99
    x_w8$EST <- "w8"
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- "w8"
    x_sel$oordeel[x_sel$doorz_diep >= grens_8_zicht & x_sel$n_soort > grens_8_n_soort & x_sel$SUBMS < grens_8_woeker & x_sel$KROOS < grens_8_kroos & x_sel$FLAB < grens_8_flab] <- 1
    x_sel$oordeel[!(x_sel$doorz_diep >= grens_8_zicht & x_sel$n_soort > grens_8_n_soort & x_sel$SUBMS < grens_8_woeker & x_sel$KROOS < grens_8_kroos & x_sel$FLAB < grens_8_flab)] <- 0
    x_w8 <- x_sel[,c("locatie.EAG","jaar","watertype","monsterident", "oordeel", "EST")]
    names(x_w8) <- c("locatie.EAG","jaar", "locatie.KRW.watertype","monsterident", "oordeel", "EST")
  }
  #W9
  grens_9_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W9" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_9_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W9" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_9_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W9" & grenswaarden_EST$PAR_NAME %in% c("SUBMS")])
  grens_9_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_9_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  x_sel <- monst_data[!is.na(monst_data$doorz_diep) & !is.na(monst_data$n_soort) &  !is.na(monst_data$SUBMS) & !is.na(monst_data$KROOS) & !is.na(monst_data$FLAB),]
  if(nrow(x_sel)<1){
    x_w9 <- x_w1
    x_w9$oordeel <- 99
    x_w9$EST <- "w9"
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- "w9"
    x_sel$oordeel[x_sel$doorz_diep >= grens_9_zicht & x_sel$n_soort < grens_9_n_soort & x_sel$SUBMS < grens_9_woeker & x_sel$KROOS < grens_9_kroos & x_sel$FLAB < grens_9_flab]<-1
    x_sel$oordeel[!(x_sel$doorz_diep >= grens_9_zicht & x_sel$n_soort < grens_9_n_soort & x_sel$SUBMS < grens_9_woeker & x_sel$KROOS < grens_9_kroos & x_sel$FLAB < grens_9_flab)]<-0
    x_w9 <- x_sel[,c("locatie.EAG","jaar", "watertype","monsterident", "oordeel", "EST")]
    names(x_w9) <- c("locatie.EAG","jaar", "locatie.KRW.watertype","monsterident", "oordeel", "EST")
  }
  
  return(rbind(x_w1,x_w2,x_w3,x_w4,x_w5,x_w6,x_w7,x_w8,x_w9))
}
do_O_long <- function(x, grenswaarden_EST){
  gr_beschoeid_1 <- grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O1" & grenswaarden_EST$PAR_NAME %in% c("beschoeiing")]
  gr_riet_1 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O1" & grenswaarden_EST$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten_1 <- as.numeric(grenswaarden_EST$waarde[ y$type %in% "O1" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  gr_beschoeid_2 <- grenswaarden_EST$waarde[ y$type %in% "O2" & grenswaarden_EST$PAR_NAME %in% c("beschoeiing")]
  gr_riet_2 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O2" & grenswaarden_EST$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten_2 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O2" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  gr_beschoeid_3 <- grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O3" & grenswaarden_EST$PAR_NAME %in% c("beschoeiing")]
  gr_riet_3 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O3" & grenswaarden_EST$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten_3 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O3" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  gr_beschoeid_4 <- grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O4" & grenswaarden_EST$PAR_NAME %in% c("beschoeiing")]
  gr_riet_4 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O4" & grenswaarden_EST$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten_4 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O4" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  gr_beschoeid_5 <- grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O5" & grenswaarden_EST$PAR_NAME %in% c("beschoeiing")]
  gr_riet_5 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O5" & grenswaarden_EST$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten_5 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O5" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  gr_beschoeid_6 <- grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O6" & grenswaarden_EST$PAR_NAME %in% c("beschoeiing")]
  gr_riet_6 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O6" & grenswaarden_EST$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten_6 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O6" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  gr_beschoeid_7 <- grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O7" & grenswaarden_EST$PAR_NAME %in% c("beschoeiing")]
  gr_riet_7 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O7" & grenswaarden_EST$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten_7 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O7" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  gr_beschoeid_8 <- grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O8" & grenswaarden_EST$PAR_NAME %in% c("beschoeiing")]
  gr_riet_8 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O8" & grenswaarden_EST$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten_8 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "O8" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  
  x_sel <- x[!is.na(x$beschoeid) &!is.na(x$n_soort) & !is.na(x$riet),]
  if(nrow(x_sel)<1){
    x_o <- data.frame(locatie.EAG=unique(x$locatie.EAG), jaar=unique(x$jaar), locatie.KRW.watertype=unique(x$watertype), monsterident=unique(x$monsterident))
    x_o$oordeel <- 99
    x_o$EST <- ""
    x_o_out <- rbind(x_o,x_o,x_o,x_o,x_o,x_o,x_o,x_o)#alle oordelen gelijk
    x_o_out$EST <- c("o1", "o2", "o3","o4","o5","o6","o7","o8")
    x_o_out$monsterident <- as.character(x_o_out$monsterident)
  }else{
    x_sel$oordeel <- 99
    x_sel$EST <- ""
    x_o1 <- x_sel
    x_o1$EST <- "o1"
    x_o1$oordeel[x_o1$beschoeid %in% "ja" & x_o1$n_soort < gr_soorten_1 & x_o1$riet < gr_riet_1] <- 1
    x_o1$oordeel[!(x_o1$beschoeid %in% "ja" & x_o1$n_soort < gr_soorten_1 & x_o1$riet < gr_riet_1)] <- 0
    x_o1 <- x_o1[,c("locatie.EAG","jaar", "watertype", "monsterident", "oordeel", "EST")]
    
    x_o2 <- x_sel
    x_o2$EST <- "o2"
    x_o2$oordeel[x_o2$beschoeid %in% "ja" & x_o2$n_soort >= gr_soorten_2 & x_o2$riet < gr_riet_2] <- 1
    x_o2$oordeel[!(x_o2$beschoeid %in% "ja" & x_o2$n_soort >= gr_soorten_2 & x_o2$riet < gr_riet_2)] <- 0
    x_o2 <- x_o2[,c("locatie.EAG","jaar", "watertype", "monsterident", "oordeel", "EST")]
    x_o3 <- x_sel
    x_o3$EST <- "o3"
    x_o3$oordeel[x_o3$beschoeid %in% "ja" & x_o3$n_soort < gr_soorten_3 & x_o3$riet >= gr_riet_3] <- 1
    x_o3$oordeel[!(x_o3$beschoeid %in% "ja" & x_o3$n_soort < gr_soorten_3 & x_o3$riet >= gr_riet_3)] <- 0
    x_o3 <- x_o3[,c("locatie.EAG","jaar", "watertype", "monsterident", "oordeel", "EST")]
    x_o4 <- x_sel
    x_o4$EST <- "o4"
    x_o4$oordeel[x_o4$beschoeid %in% "ja" & x_o4$n_soort >= gr_soorten_4 & x_o4$riet >= gr_riet_4] <- 1
    x_o4$oordeel[!(x_o4$beschoeid %in% "ja" & x_o4$n_soort >= gr_soorten_4 & x_o4$riet >= gr_riet_4)] <- 0
    x_o4 <- x_o4[,c("locatie.EAG","jaar", "watertype", "monsterident", "oordeel", "EST")]
    x_o5 <- x_sel
    x_o5$EST <- "o5"
    x_o5$oordeel[x_o5$beschoeid %in% "nee" &  x_o5$n_soort < gr_soorten_5 & x_o5$riet < gr_riet_5] <- 1
    x_o5$oordeel[!(x_o5$beschoeid %in% "nee" &  x_o5$n_soort < gr_soorten_5 & x_o5$riet < gr_riet_5)] <- 0
    x_o5 <- x_o5[,c("locatie.EAG","jaar", "watertype", "monsterident", "oordeel", "EST")]
    x_o6 <- x_sel
    x_o6$EST <- "o6"
    x_o6$oordeel[x_o6$beschoeid %in% "nee" & x_o6$n_soort >= gr_soorten_6 & x_o6$riet < gr_riet_6] <- 1
    x_o6$oordeel[!(x_o6$beschoeid %in% "nee" & x_o6$n_soort >= gr_soorten_6 & x_o6$riet < gr_riet_6)] <- 0
    x_o6 <- x_o6[,c("locatie.EAG","jaar", "watertype", "monsterident", "oordeel", "EST")]
    x_o7 <- x_sel
    x_o7$EST <- "o7"
    x_o7$oordeel[x_o7$beschoeid %in% "nee" & x_o7$n_soort < gr_soorten_7 & x_o7$riet >= gr_riet_7] <- 1
    x_o7$oordeel[!(x_o7$beschoeid %in% "nee" & x_o7$n_soort < gr_soorten_7 & x_o7$riet >= gr_riet_7)] <- 0
    x_o7 <- x_o7[,c("locatie.EAG","jaar", "watertype", "monsterident", "oordeel", "EST")]
    x_o8 <- x_sel
    x_o8$EST <- "o8"
    x_o8$oordeel[x_o8$beschoeid %in% "nee" & x_o8$n_soort >= gr_soorten_8 & x_o8$riet >= gr_riet_8] <- 1
    x_o8$oordeel[!(x_o8$beschoeid %in% "nee" & x_o8$n_soort >= gr_soorten_8 & x_o8$riet >= gr_riet_8)] <- 0
    x_o8 <- x_o8[,c("locatie.EAG","jaar", "watertype", "monsterident", "oordeel", "EST")]
    x_o_out <- rbind(x_o1, x_o2, x_o3,x_o4,x_o5,x_o6,x_o7,x_o8)
    names(x_o_out) <- c("locatie.EAG","jaar", "locatie.KRW.watertype", "monsterident", "oordeel", "EST")
    x_o_out$monsterident <- as.character(x_o_out$monsterident)
  }
  return(x_o_out)
  
}

do_W1 <- function(df, grenswaarden_EST){
  grens_1 <- as.numeric(grenswaarden_EST$waarde[grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% "FLAB"])
  x_sel <- df[df$parametercode %in% "FLAB", ]
  n_flab <- ifelse(nrow(x_sel)>0, round(nrow(x_sel[x_sel$meetwaarde >= grens_1,])/nrow(x_sel) * 100),0)
  return(n_flab)
}
do_W2 <- function(df, grenswaarden_EST){
  grens_2 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% "KROOS"])
  x_sel <- df[  df$parametercode %in% "KROOS", ]
  n_kroos <- ifelse(nrow(x_sel)>0, round(nrow(x_sel[x_sel$meetwaarde >= grens_2,])/nrow(x_sel) * 100),0)
  return(n_kroos)
}
do_W3 <- function(df, grenswaarden_EST){
  grens_3 <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W3" & grenswaarden_EST$PAR_NAME %in% "DRIJFBPTN"])
  x_sel <- df[ df$parametercode %in% "DRIJFBPTN", ]
  n_kroos <- ifelse(nrow(x_sel)>0, round(nrow(x_sel[x_sel$meetwaarde >= grens_3,])/nrow(x_sel) * 100),0) 
  return(n_kroos)
}
do_W4 <- function(monster_subm, grenswaarden_EST){ #wat te doen als doorzicht en/of diepte ontbreekt??
  grens_4_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W4" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  n_troebel <- ifelse(!is.null(nrow(monster_subm$doorz_diep)), round(length(monster_subm$doorz_diep[!is.na(monster_subm$doorz_diep) 
                                                                                                    & monster_subm$doorz_diep <= grens_4_zicht])
                                                                     /nrow(monster_subm[!is.na(monster_subm$doorz_diep),]) * 100), 0)
  return(n_troebel)
}
do_W5 <- function(monster_subm, grenswaarden_EST){ #wat te doen als doorzicht en/of diepte ontbreekt??
  grens_5_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W5" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_5_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W5" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_5_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W5" & grenswaarden_EST$PAR_NAME %in% c("woeker_soort")])
  grens_5_submers <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W8" & grenswaarden_EST$PAR_NAME %in% c("SUBMS")]) # Let op, check of totaal submers niet meer is dan bovengrens van toestand W8
  grens_5_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_5_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  check <- monster_subm$doorz_diep >= grens_5_zicht & monster_subm$n_soort >= grens_5_n_soort & monster_subm$SUBMS >= grens_5_submers & monster_subm$woeker < grens_5_woeker & monster_subm$KROOS < grens_5_kroos & monster_subm$FLAB < grens_5_flab
  
  w5_out <- round(nrow(monster_subm[check,])/nrow(monster_subm) * 100)
  return(w5_out)
}
do_W6 <- function(monster_subm,grenswaarden_EST){
  grens_6_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W6" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_6_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W6" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_6_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W6" & grenswaarden_EST$PAR_NAME %in% c("woeker_soort")])
  grens_6_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_6_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  check <- monster_subm$doorz_diep >= grens_6_zicht & monster_subm$n_soort >= grens_6_n_soort & monster_subm$woeker >= grens_6_woeker & monster_subm$KROOS < grens_6_kroos & monster_subm$FLAB < grens_6_flab
  
  w6_out <- round(nrow(monster_subm[check[!is.na(check)],])/nrow(monster_subm) * 100)
  return(w6_out)
}
do_W7 <- function(x,grenswaarden_EST){
  grens_7_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W7" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_7_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W7" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_7_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W7" & grenswaarden_EST$PAR_NAME %in% c("SUBMS")])
  grens_7_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_7_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  check <- x$doorz_diep >= grens_7_zicht & x$n_soort <= grens_7_n_soort & x$n_soort > 0 & x$SUBMS < grens_7_woeker & x$KROOS < grens_7_kroos & x$FLAB < grens_7_flab
  
  w7_out <- round(nrow(x[check[!is.na(check)],])/nrow(x) * 100)
  return(w7_out)
}
do_W8 <- function(monster_subm,grenswaarden_EST){
  grens_8_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W8" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_8_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W8" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_8_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W8" & grenswaarden_EST$PAR_NAME %in% c("SUBMS")])
  grens_8_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_8_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  
  check <- monster_subm$doorz_diep >= grens_8_zicht & monster_subm$n_soort > grens_8_n_soort & monster_subm$SUBMS < grens_8_woeker & monster_subm$KROOS < grens_8_kroos & monster_subm$FLAB < grens_8_flab
  
  w8_out <- round(nrow(monster_subm[check[!is.na(check)],])/nrow(monster_subm) * 100)
  
  return(w8_out)
}
do_W9 <- function(monster_subm,grenswaarden_EST){
  grens_9_zicht <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W9" & grenswaarden_EST$PAR_NAME %in% c("ZICHT")])
  grens_9_n_soort <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W9" & grenswaarden_EST$PAR_NAME %in% c("n_soorten")])
  grens_9_woeker <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W9" & grenswaarden_EST$PAR_NAME %in% c("SUBMS")])
  grens_9_kroos <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W2" & grenswaarden_EST$PAR_NAME %in% c("KROOS")])
  grens_9_flab <- as.numeric(grenswaarden_EST$waarde[ grenswaarden_EST$type %in% "W1" & grenswaarden_EST$PAR_NAME %in% c("FLAB")])
  check <- monster_subm$doorz_diep >= grens_9_zicht & monster_subm$n_soort < grens_9_n_soort & monster_subm$SUBMS < grens_9_woeker & monster_subm$KROOS < grens_9_kroos & monster_subm$FLAB < grens_9_flab
  
  w9_out <- round(nrow(monster_subm[check[!is.na(check)],])/nrow(monster_subm) * 100)
  
  return(w9_out)
}

do_O1 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O1" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O1" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O1" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% gr_beschoeid & 
                                          x$n_soort < gr_soorten & 
                                          x$riet < gr_riet])/nrow(x) * 100)
  return(result)
}
do_O2 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O2" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O2" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O2" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% gr_beschoeid & 
                                          x$n_soort >= gr_soorten & 
                                          x$riet < gr_riet])/nrow(x) * 100)
  return(result)
}
do_O3 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O3" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O3" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O3" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% gr_beschoeid & 
                                          x$n_soort < gr_soorten & 
                                          x$riet >= gr_riet])/nrow(x) * 100)
  return(result)
}
do_O4 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O4" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O4" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O4" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% gr_beschoeid & 
                                          x$n_soort >= gr_soorten & 
                                          x$riet >= gr_riet])/nrow(x) * 100)
  return(result)
}
do_O5 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O5" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O5" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O5" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% "nee" & 
                                          x$n_soort < gr_soorten & 
                                          x$riet < gr_riet])/nrow(x) * 100)
  return(result)
}
do_O6 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O6" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O6" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O6" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% "nee" & 
                                          x$n_soort >= gr_soorten & 
                                          x$riet < gr_riet])/nrow(x) * 100)
  return(result)
}
do_O7 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O7" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O7" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O7" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% "nee" & 
                                          x$n_soort < gr_soorten & 
                                          x$riet >= gr_riet])/nrow(x) * 100)
  return(result)
}
do_O8 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O8" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O8" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O8" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% "nee" & 
                                          x$n_soort >= gr_soorten & 
                                          x$riet >= gr_riet])/nrow(x) * 100)
  return(result)
}

do_EST_ANALYSE <- function(macft=macft, y="Waternet",grenswaarden_EST){
soortenlijst_submers <- !is.na(unique(macft$biotaxonnaam[macft$submers == '1']))
soortenlijst_oever <- !is.na(unique(macft$biotaxonnaam[macft$oever == '1']))
soortenlijst_kroos <- !is.na(unique(macft$biotaxonnaam[macft$kroos == '1']))
EAGs <- sort(unique(macft$locatie.EAG))

df_out_totaal <- NULL
df_long_out_totaal <- NULL
df_submers_totaal <- NULL
df_submers_long_totaal <- NULL

for(EAG in EAGs){
  df1 <- macft[ macft$locatie.EAG %in% EAG,]
  jaars <- sort(unique(df1$jaar))
  print(EAG)
  for(j in jaars){
    print(j)
  df <- df1[df1$jaar %in% j,] 
  WL <- df$locatiecode
  #DEEL ESTs 
  monster_subm <- do_monster_submers(df,soortenlijst_submers, soortenlijst_kroos)
  W1_result <- do_W1(df,grenswaarden_EST)
  W2_result <- do_W2(df, grenswaarden_EST)
  W3_result <- do_W3(df, grenswaarden_EST)
  W4_result <- do_W4(monster_subm, grenswaarden_EST)
  W5_result <- do_W5(monster_subm, grenswaarden_EST)
  W6_result <- do_W6(monster_subm, grenswaarden_EST)
  W7_result <- do_W7(monster_subm, grenswaarden_EST)
  W8_result <- do_W8(monster_subm, grenswaarden_EST)
  W9_result <- do_W9(monster_subm, grenswaarden_EST)
 
  monster_oever <- do_monster_oever(df, soortenlijst_oever)
  O1_result <- do_O1(monster_oever, grenswaarden_EST)
  O2_result <- do_O2(monster_oever, grenswaarden_EST)
  O3_result <- do_O3(monster_oever, grenswaarden_EST)
  O4_result <- do_O4(monster_oever, grenswaarden_EST)
  O5_result <- do_O5(monster_oever, grenswaarden_EST)
  O6_result <- do_O6(monster_oever, grenswaarden_EST)
  O7_result <- do_O7(monster_oever, grenswaarden_EST)
  O8_result <- do_O8(monster_oever, grenswaarden_EST)
  
  df_out <- data.frame(locatie.EAG=EAG, jaar = j, EST=c(paste0("W",1:9),paste0("O",1:8)), result=c(W1_result, W2_result, W3_result, W4_result, W5_result, W6_result, W7_result, W8_result, W9_result,O1_result, O2_result, O3_result, O4_result, O5_result, O6_result, O7_result, O8_result))
  df_out_totaal <- rbind(df_out_totaal, df_out)
}
}

estpermonst <- function(df, grenswaarden_EST){
  #EINDE DEEL ESTs
  #DEEL ESTs UITGEBREID
  # w_long <- do_W_long(df,grenswaarden_EST,monster_subm)
  # o_long <- do_O_long(monster_oever,grenswaarden_EST)
  # df_long_out_totaal <- rbind(df_long_out_totaal, w_long, o_long)
}

#write.table(df1, paste0("resultaten/tabellen/EST-analyse_", y, "_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
#write.table(df_long_out_totaal, paste0("resultaten/tabellen/EST_tabel_long_", y, "_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
return(df_out_totaal)
}

do_EST_post <- function(EST_WATERNET, EKRset, eag_wl){
  df1 <- dcast(EST_WATERNET, locatie.EAG+jaar ~ EST, fun=sum, value.var = "result") 
  # 
  dtEST4 <-  melt(df1,measure=patterns(O='^O',W='^W'))
  dtEST4 <- dtEST4[,lapply(.SD,which.max),.SDcols = c('O','W')] # EST typenummer van degene die het meest voorkomt
  
  ESTnaam1 <- paste0(c('W','O'),c(dtEST4[,.(W,O)]),collapse = '_')
  ESTnaam2 <- ifelse(unique(EST_sel$watertype) == 'M20','DM',ifelse(unique(EST_sel$watertype) %in% c('M14','M27',"M25"),'OM', ifelse(unique(EST_sel$watertype) %in% c('M1a','M8',"M10"),'Sl','K')))
  ESTnaam3 <- ifelse(unique(EST_sel$StedelijkLandelijk) == 'Stedelijk','St','L') 
  
  myimages <- paste0("esticon/",ESTnaam1,'_',ESTnaam2,'_', ESTnaam3, ".jpg")
}