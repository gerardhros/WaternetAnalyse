#inlezen data
biotaxon <- read_excel('../hydrobiologie/TwnList_2019-07-19.xlsx')
hybi_grenswaarden <- fread("../data/grenswaarden_est.csv")
eag_wl <- fread('../data/EAG_Opp_kenmerken_20200218.csv')
# locaties van alle metingen (water, biologie, en slootbodem)
locaties <- fread('../data/Location.csv')
# inladen gegevens hydrobiologie
hybi <- readRDS('../data/alles_reliable.rds')
hybi$meetwaarde <- as.numeric(hybi$meetwaarde)
# update, filter and clean up databases -----------
# hybi measurements
hybi <- ppr_hybi(db = hybi, syear = 1990, wtype = eag_wl, mlocs = locaties)
hybiest <- hybi[hybi$parameterfractie == "",] # FLAB alleen totaal

do_monster_submers <- function(x,z){
  monster_df_out <- NULL
  for(i in unique(x$monsterident)){
    doorz_diep <- x$meetwaarde[x$monsterident %in% i & x$parametercode %in% "ZICHT"]/x$meetwaarde[x$monsterident %in% i & x$parametercode %in% "WATDTE"]
    if(!length(doorz_diep)>0){
      doorz_diep <- NA
    }
    n_soort <- nrow(x[x$monsterident %in% i & x$parametercode %in% "" & x$biotaxonnaam %in% z,]) #LET OP: DIT IS VOOR W5 en verder
    woeker <- max(x$meetwaarde[x$monsterident %in% i & x$parametercode %in% "" & !x$biotaxonnaam %in% ""])
    if(woeker == -Inf){
      woeker <- 0
    }
    SUBMS <- x$meetwaarde[x$monsterident %in% i & x$parametercode %in% "SUBMSPTN"]
    if(!length(SUBMS)>0){
      SUBMS <- NA
    }
    monster_df <- data.frame(monsterident=i,doorz_diep=doorz_diep, n_soort=n_soort, woeker=woeker, SUBMS=SUBMS)
    monster_df_out <- rbind(monster_df_out, monster_df )
  }
  monster_df_out <- na.omit(monster_df_out)
  return(monster_df_out)
}
do_monster_oever <- function(x,z){
  monster_df_out <- NULL
  monsters <- unique(x$monsterident[ x$compartiment %in% c("OR", "EZ")])
  if(length(monsters)<1){
    monster_df_out <- data.frame(monsterident=0,beschoeid=0, n_soort=0, riet=0)
  }else{
    for(i in monsters){ #SELECTEERT ALLEEN OEVEROPNAMES!
      if(length(x$meetwaarde[x$monsterident %in% i & x$parametercode %in% "OEVBSIG"])==0){ #als OEVBSIG ontbreekt wordt beschoeing op "nee" gezet!
        beschoeid <- "nee"}else{
          if(!x$meetwaarde[x$monsterident %in% i & x$parametercode %in% "OEVBSIG"]==14){#14 is onbeschoeide oever 
            beschoeid <- "ja"}else{beschoeid <- "nee"
            }
        }
      n_soort <- nrow(x[x$monsterident %in% i & x$parametercode %in% "" & x$biotaxonnaam %in% z,])
      riet <- x$meetwaarde[x$monsterident %in% i & x$biotaxonnaam %in% "Phragmites australis"]
      if(length(riet)<1){riet <- 0}
      monster_df <- data.frame(monsterident=i,beschoeid=beschoeid, n_soort=n_soort, riet=riet)
      monster_df_out <- rbind(monster_df_out, monster_df )
    }
    monster_df_out <- na.omit(monster_df_out)
  }
  return(monster_df_out)
}  
do_W1 <- function(x, y){
  grens_1 <- as.numeric(y$waarde[ y$type %in% "W1" & y$PAR_NAME %in% "FLAB"])
  x_sel <- x[ x$fewsparameter %in% "PTN_BEDKG_%" & x$parametercode %in% "FLAB", ]
  n_flab <- round(nrow(x_sel[x_sel$meetwaarde >= grens_1,])/nrow(x_sel) * 100)
  return(n_flab)
}
do_W2 <- function(x, y){
  grens_2 <- as.numeric(y$waarde[ y$type %in% "W2" & y$PAR_NAME %in% "KROOS"])
  x_sel <- x[ x$fewsparameter %in% "PTN_BEDKG_%" & x$parametercode %in% "KROOS", ]
  n_kroos <- round(nrow(x_sel[x_sel$meetwaarde >= grens_2,])/nrow(x_sel) * 100)
  return(n_kroos)
}
do_W3 <- function(x, y){
  grens_3 <- as.numeric(y$waarde[ y$type %in% "W3" & y$PAR_NAME %in% "DRIJFBPTN"])
  x_sel <- x[ x$fewsparameter %in% "PTN_BEDKG_%" & x$parametercode %in% "DRIJFBPTN", ]
  n_kroos <- round(nrow(x_sel[x_sel$meetwaarde >= grens_3,])/nrow(x_sel) * 100)
  return(n_kroos)
}
do_W4 <- function(x, y){
  grens_4_zicht <- as.numeric(y$waarde[ y$type %in% "W4" & y$PAR_NAME %in% c("ZICHT")])
  n_troebel <- round(length(x$doorz_diep[!is.na(x$doorz_diep) & x$doorz_diep <= grens_4_zicht])/nrow(x[!is.na(x$doorz_diep),]) * 100)
  return(n_troebel)
}
do_W5 <- function(x, y){
  grens_5_zicht <- as.numeric(y$waarde[ y$type %in% "W5" & y$PAR_NAME %in% c("ZICHT")])
  grens_5_n_soort <- as.numeric(y$waarde[ y$type %in% "W5" & y$PAR_NAME %in% c("n_soorten")])
  grens_5_woeker <- as.numeric(y$waarde[ y$type %in% "W5" & y$PAR_NAME %in% c("woeker_soort")])
  
  check <- x$doorz_diep >= grens_5_zicht & x$n_soort >= grens_5_n_soort & x$woeker < grens_5_woeker
  
  w5_out <- round(nrow(x[check,])/nrow(x) * 100)
  return(w5_out)
}
do_W6 <- function(x,y){
  grens_6_zicht <- as.numeric(y$waarde[ y$type %in% "W6" & y$PAR_NAME %in% c("ZICHT")])
  #grens_6_n_soort <- as.numeric(y$waarde[ y$type %in% "W6" & y$PAR_NAME %in% c("n_soorten")])
  grens_6_woeker <- as.numeric(y$waarde[ y$type %in% "W6" & y$PAR_NAME %in% c("woeker_soort")])
  
  check <- x$doorz_diep >= grens_6_zicht & x$woeker >= grens_6_woeker
  
  w6_out <- round(nrow(x[check,])/nrow(x) * 100)
  return(w6_out)
}
do_W7 <- function(x,y){
  grens_7_zicht <- as.numeric(y$waarde[ y$type %in% "W7" & y$PAR_NAME %in% c("ZICHT")])
  grens_7_n_soort <- as.numeric(y$waarde[ y$type %in% "W7" & y$PAR_NAME %in% c("n_soorten")])
  grens_7_woeker <- as.numeric(y$waarde[ y$type %in% "W7" & y$PAR_NAME %in% c("SUBMS")])
  
  check <- x$doorz_diep >= grens_7_zicht & x$n_soort <= grens_7_n_soort & x$SUBMS < grens_7_woeker
  
  w7_out <- round(nrow(x[check,])/nrow(x) * 100)
  return(w7_out)
}
do_W8 <- function(x,y){
  grens_8_zicht <- as.numeric(y$waarde[ y$type %in% "W8" & y$PAR_NAME %in% c("ZICHT")])
  grens_8_n_soort <- as.numeric(y$waarde[ y$type %in% "W8" & y$PAR_NAME %in% c("n_soorten")])
  grens_8_woeker <- as.numeric(y$waarde[ y$type %in% "W8" & y$PAR_NAME %in% c("SUBMS")])
  
  check <- x$doorz_diep >= grens_8_zicht & x$n_soort > grens_8_n_soort & x$SUBMS < grens_8_woeker
  
  w8_out <- round(nrow(x[check,])/nrow(x) * 100)
  
  return(w8_out)
}
do_W9 <- function(x,y){
  grens_9_zicht <- as.numeric(y$waarde[ y$type %in% "W9" & y$PAR_NAME %in% c("ZICHT")])
  grens_9_n_soort <- as.numeric(y$waarde[ y$type %in% "W9" & y$PAR_NAME %in% c("n_soorten")])
  grens_9_woeker <- as.numeric(y$waarde[ y$type %in% "W9" & y$PAR_NAME %in% c("SUBMS")])
  
  check <- x$doorz_diep >= grens_9_zicht & x$n_soort < grens_9_n_soort & x$SUBMS < grens_9_woeker
  
  w9_out <- round(nrow(x[check,])/nrow(x) * 100)
  
  return(w9_out)
}

do_O1 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O1" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O1" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O1" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% "ja" & 
                                          x$n_soort < gr_soorten & 
                                          x$riet < gr_riet])/nrow(x) * 100)
  return(result)
}
do_O2 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O2" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O2" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O2" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% "ja" & 
                                          x$n_soort >= gr_soorten & 
                                          x$riet < gr_riet])/nrow(x) * 100)
  return(result)
}
do_O3 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O3" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O3" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O3" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% "ja" & 
                                          x$n_soort < gr_soorten & 
                                          x$riet >= gr_riet])/nrow(x) * 100)
  return(result)
}
do_O4 <- function(x, y){
  gr_beschoeid <- y$waarde[ y$type %in% "O4" & y$PAR_NAME %in% c("beschoeiing")]
  gr_riet <- as.numeric(y$waarde[ y$type %in% "O4" & y$PAR_NAME %in% c("Phragmites australis")])
  gr_soorten <- as.numeric(y$waarde[ y$type %in% "O4" & y$PAR_NAME %in% c("n_soorten")])
  result <- round(length(x$monsterident[x$beschoeid %in% "ja" & 
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

soortenlijst_submers <- unique(hybi$biotaxonnaam[hybi$WNA.onderwaterplantensoorten == '1'])
soortenlijst_submers <- soortenlijst_submers[!is.na(soortenlijst_submers)]
soortenlijst_oever <- unique(hybi$biotaxonnaam[hybi$WNA.oeverplantensoorten == '1'])
soortenlijst_oever <- soortenlijst_oever[!is.na(soortenlijst_oever)]
EAGs <- sort(unique(hybi$locatie.EAG))

df_out_totaal <- NULL
df_out <- NULL

for(EAG in EAGs){
  df1 <- hybiest[hybiest$locatie.EAG %in% EAG & hybiest$jaar > 2010,]
  jaars <- sort(unique(df1$jaar))
  for(jaar in jaars){
  df <- df1[df1$jaar %in% jaar,] 
  #DEEL ESTs 
  monster_subm <- do_monster_submers(df,soortenlijst_submers)
  W1_result <- do_W1(df, hybi_grenswaarden)
  W2_result <- do_W2(df, hybi_grenswaarden)
  W3_result <- do_W3(df, hybi_grenswaarden)
  W4_result <- do_W4(monster_subm, hybi_grenswaarden)
  W5_result <- do_W5(monster_subm, hybi_grenswaarden)
  W6_result <- do_W6(monster_subm, hybi_grenswaarden)
  W7_result <- do_W7(monster_subm, hybi_grenswaarden)
  W8_result <- do_W8(monster_subm, hybi_grenswaarden)
  W9_result <- do_W9(monster_subm, hybi_grenswaarden)
  
  monster_oever <- do_monster_oever(df, soortenlijst_oever)
  O1_result <- do_O1(monster_oever, hybi_grenswaarden)
  O2_result <- do_O2(monster_oever, hybi_grenswaarden)
  O3_result <- do_O3(monster_oever, hybi_grenswaarden)
  O4_result <- do_O4(monster_oever, hybi_grenswaarden)
  O5_result <- do_O5(monster_oever, hybi_grenswaarden)
  O6_result <- do_O6(monster_oever, hybi_grenswaarden)
  O7_result <- do_O7(monster_oever, hybi_grenswaarden)
  O8_result <- do_O8(monster_oever, hybi_grenswaarden)
  
  df_out <- data.frame(EAG=EAG, jaar = jaar, EST=c(paste0("W",1:9),paste0("O",1:8)), result=c(W1_result, W2_result, W3_result, W4_result, W5_result, W6_result, W7_result, W8_result, W9_result,O1_result, O2_result, O3_result, O4_result, O5_result, O6_result, O7_result, O8_result))
  print(df_out)
  df_out_totaal <- rbind(df_out_totaal, df_out)
  #EINDE DEEL ESTs
}
}

df1 <- dcast(df_out_totaal,EAG+jaar~EST, value = "result")
df1$EAG <- as.character(df1$EAG)

df1 <- merge(df1, eag_wl[,c('GAFIDENT','GAFNAAM','KRW_SGBP3','KRWmonitoringslocatie_SGBP3','SGBP3_NAAM')], by.x = c('EAG'),
            by.y = c('GAFIDENT'), all.x = TRUE)
df3 <- df1 %>% 
  arrange(EAG,KRW_SGBP3,jaar,desc(jaar)) %>% 
  group_by(EAG,KRW_SGBP3) %>%
  top_n(1, wt = jaar) 

write.table(df3, "EST.csv", sep=";", dec=".", row.names=F)
