rm(list=ls())
#instellingen
setwd("D:/Dactylis/01. Projecten/08. 2019/PJ-2019004. Detachering Waternet/03. Data/scripts")
source("hulpfuncties_20190426.R")
datum <- Sys.Date()
output_est_list <-"../output/EST_list"
output_afbeeldingen <-  "../output/afbeeldingen/"
output_afbeeldingen_fyschem <- "../output/afbeeldingen_fyschem/"
output_leaflet <- "D:/Dactylis/01. Projecten/08. 2019/PJ-2019004. Detachering Waternet/03. Data/output/kaarten_leaflet/" #"../output/kaarten_leaflet/"
#output_leaflet <- "D:/Dactylis/01. Projecten/08. 2019/PJ-2019004. Detachering Waternet/03. Data/output/kaarten_leaflet/"
dirGIS <-"../GIS"


proj4.rd <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.87740,4.0725 +units=m +no_defs")
#proj4.google <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#einde instellingen
#inlezen data

biotaxon <- inlezen("../uitgangspunten/biotaxon_MCED.csv")
namen_NL <- as.data.frame(read_excel("../uitgangspunten/namenlijst_20190405.xlsx"))
bedekkingtabel <- inlezen("../uitgangspunten/bedekking_schalen.csv")
#inlezen("../uitgangspunten/koppeldata_vegetatie_terHeerdt_20190320.csv")
hybi_grenswaarden <- inlezen("../uitgangspunten/grenswaarden_est.csv")
hybi <- inlezen("../uitgangspunten/macft_2012-2017_20190315.csv")
hybi_II <- readRDS("../uitgangspunten/EKRlijst.rds")
#hybi <- inlezen("../uitgangspunten/hybio_ronde_hoep_20190405.csv")
fyschem <- inlezen("../uitgangspunten/fyschem_ronde_hoep_20190405.csv")
SHAPE_EAG <- importOGR("EAG20170611.shp", dirGIS, CRSobj = proj4.rd)
koppel_WL <- inlezen("../uitgangspunten/gebied_wateroppervlak_toetsgebied_watertype_LM.csv")
#einde inlezen data
#voorbewerking data
hybi <- voorbewerking_hybi(hybi,namen_NL, bedekkingtabel)
hybi_II <- voorbewerking_hybi_II(hybi_II,namen_NL, bedekkingtabel)

fyschem$datum <- as.Date(fyschem$datum, format = "%d-%m-%Y")
fyschem$jaar <- as.numeric(format(fyschem$datum, '%Y'))
fyschem$meetwaarde[fyschem$limietsymbool == '<'] <- fyschem$meetwaarde[fyschem$limietsymbool == '<']/2 # meetwaarden


#einde voorbewerking data
soortenlijst_submers <- biotaxon$naam[ biotaxon$beoordeling %in% c("5. slecht", "4. matig", "3. redelijk", "2. goed", "1. zeer goed")]
soortenlijst_oever <- biotaxon$naam[ biotaxon$oever %in% c("1")]
EAGs <- sort(unique(hybi$locatie.EAG))



df_out_totaal <- NULL
df_submers_totaal <- NULL
df_submers_long_totaal <- NULL
for(EAG in EAGs){
  df <- hybi[ hybi$locatie.EAG %in% EAG,]
  df_II <- hybi_II[ hybi_II$locatie.EAG %in% EAG,]
  WL <- unique(koppel_WL$gebiednaam[ koppel_WL$gebied %in% EAG])
  if(length(WL)==0){WL <- ""}
  if(length(WL)>1){WL <- WL[1]}
  KRW <- unique(koppel_WL$KRWWaterlichaamcode[ koppel_WL$gebied %in% EAG])
  if(length(KRW)==0){KRW <- ""}
  if(length(KRW)>1){KRW <- KRW[1]}
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
  
  df_out <- data.frame(EAG=EAG, WL=WL, KRW=KRW,EST=c(paste0("W",1:9),paste0("O",1:8)), result=c(W1_result, W2_result, W3_result, W4_result, W5_result, W6_result, W7_result, W8_result, W9_result,O1_result, O2_result, O3_result, O4_result, O5_result, O6_result, O7_result, O8_result))
  df_out_totaal <- rbind(df_out_totaal, df_out)
  #EINDE DEEL ESTs
  #DEEL BIO_ANALYSE
  df_submers <- do_df_submers(df_II, soortenlijst_submers)
  df_submers_long <- do_df_submers_long(df_II, soortenlijst_submers)
  
  
  df_submers_totaal <- rbind(df_submers_totaal, df_submers)
  df_submers_long_totaal <- rbind(df_submers_long_totaal, df_submers_long)
  #EINDE DEEL BIO_ANALYSE
  #DEEL FYSCHEM
  # df_fc <- fyschem[fyschem$locatie.EAG %in% EAG,]
  # do_maak_plaatje_fyschem(df_fc)
  
  #EINDE DEEL FYSCHEM
  #FYSCHEM
}

df_out_totaal_cast <- cast(df_out_totaal,EAG+WL+KRW~EST, value = "result")
df_out_totaal_cast_redo <- redo_df_out_totaal_cast(df_out_totaal_cast)
#write.table(df_out_totaal_cast_redo, paste0(output_est_list,datum , ".csv"), sep=";", dec=".", row.names=F)

df_submers_totaal <- add.cols(df_submers_totaal)
df_submers_long_totaal <- add.cols.shortlist(df_submers_long_totaal[!df_submers_long_totaal$biotaxonnaam %in% "WEGHALEN",],df_submers_totaal)

#do_maak_plaatjes_submers(df_submers_long_totaal,locatie = F)
do_maak_plaatjes_submers_II(df_submers_long_totaal,locatie = F)
#do_maak_plaatje_gamma(df_submers_long_totaal)
do_maak_plaatje_oordeel(df_submers_long_totaal)

#do_maak_plaatje_kroos(df)

#GGMAP
#do_static_map_submers(df_submers_totaal, SHAPE_EAG)
#LEAFLET
hybi_sel <- unique(hybi_II[!is.na(hybi_II$locatie.x)&!is.na(hybi_II$locatie.x),c("locatie.EAG", "locatiecode", "locatie.x", "locatie.y", "jaar")])
colnames(hybi_sel) <- c("EAG", "locatiecode","x", "y", "jaar")

do_static_leaflet_submers(df_submers_totaal, SHAPE_EAG, hybi_sel)
#do_dynamic_map_submers(df_submers_long_totaal, SHAPE_EAG)
