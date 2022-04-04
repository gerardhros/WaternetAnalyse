#packages
require(dplyr);require(tidyverse); require(ggplot2);require(gtools);require(RColorBrewer)
require(devtools)
require(gvlma)
require(htmlwidgets)
require(mapview)
require(sf);require(rgdal)
require(raster)
require(data.table)
require(RColorBrewer)
require(leaflet)
require(rgeos)
require(maptools)
#koppeltabellen tijdelijk
koppel_meetnet <- data.frame(meetnet=c("fychem", 
                                       "prioritaire stoffen",
                                       "specifiek verontreinigende stoffen"),
                             Waardebepalingsmethode.code=c("other:Aquo-kit;OW-toetsing;KRW fysisch-chemisch 2018",
                                                           "other:Aquo-kit;OW-toetsing;KRW prioritaire stoffen SGBP 2022-2027 - zoet",
                                                           "other:Aquo-kit;OW-toetsing;KRW spec. verontr. stoffen SGBP 2022-2027 - zoet"))
koppel_ESF_scores <- data.frame(resultaat.code=c(0,1,2,3),resultaat=c("onbekend", "op orde", "at risk", "niet op orde"),resultaat.kleur=c("grey", "red", "yellow", "green"))
koppel_EKR_scores <- data.frame(oordeel=c("onbekend", "slecht", "ontoereikend", "matig", "goed"),oordeel.kleur=c("grey", "red", "orange", "yellow", "green"))
#functies
doSplit <- function(x,y=cols){
  if(length(y)==1){
    x_split <- split(x,list(x[,y]))
  }else{
    x_split <- split(x,as.list(x[,y]))
  }
  return(x_split)
}
add_doelen <- function(x=oordeel_overig,y=doelen){
  y$GHPR[y$GHPR %in% "Overige waterflora-kwaliteit"] <- maatselectie
  y_sel <- y%>%filter(GeoObject.code %in% "", GHPR %in% maatselectie)%>%
    select(gebied, Doel_2022)%>%distinct()%>%rename(EAGIDENT=gebied, GEP_2022=Doel_2022)
  x_add <- x%>%filter(GHPR %in% maatselectie)%>%left_join(y_sel,by=c("EAGIDENT"))%>%rename()
  return(data.frame(x_add))
}
get_laatste_jaar <- function(x=doSplit(db_overig,"EAGIDENT")[[1]]){
  #neem meest recente jaar...
  x_out <- x[x$jaar %in% max(x$jaar),]
  x_out <- x_out%>%select(-jaar)
  return(x_out)
}
get_oordeel_EKR <- function(x=doSplit(db_tot_gat, "id")[[1]]){
  x$GEP_gebruikt <- x$GEP_2022
  x$GEP_gebruikt <- ifelse(x$GEP_gebruik > 0.6,0.6,x$GEP_gebruikt) #afromen op 0.6
  gap <- x$GEP_2022/4
  oordeel <- ifelse(x$EKR < gap,"slecht",
                    ifelse(x$EKR < (gap*2),"ontoereikend",
                           ifelse(x$EKR < (gap*3),"matig",
                                  "goed")))
  x$oordeel <- ifelse(is.na(oordeel),"onbekend",oordeel)
  
  return(x)
}
add_empty_GHPR <- function(x=doSplit(db_tot_gat_oordeel, "gebied")[[1]]){
  if(NA %in% x$EAGIDENT & nrow(x)<4){
    if(!"Macrofauna" %in% x$GHPR){
      x_add <- x[1,]
      x_add <- x_add%>%mutate(GHPR="Macrofauna",EKR=NA, GEP_2022=NA, GEP_gebruikt=NA,doelgat=NA,oordeel="onbekend")
      x <- rbind(x, x_add)
    }
    if(!"Fytoplankton" %in% x$GHPR){
      x_add <- x[1,]
      x_add <- x_add%>%mutate(GHPR="Fytoplankton",EKR=NA, GEP_2022=NA, GEP_gebruikt=NA,doelgat=NA,oordeel="onbekend")
      x <- rbind(x, x_add)
    }
    if(!"Ov. waterflora" %in% x$GHPR){
      x_add <- x[1,]
      x_add <- x_add%>%mutate(GHPR="Ov.waterflora",EKR=NA, GEP_2022=NA, GEP_gebruikt=NA,doelgat=NA,oordeel="onbekend")
      x <- rbind(x, x_add)
      }
    if(!"Vis" %in% x$GHPR){
      x_add <- x[1,]
      x_add <- x_add%>%mutate(GHPR="Vis",EKR=NA, GEP_2022=NA, GEP_gebruikt=NA,doelgat=NA,oordeel="onbekend")
      x <- rbind(x, x_add)
    }
    
  }
  return(x)
}
get_EAG_orde <- function(x=db_tot_gat_oordeel){
  x$EAG_orde <- NA
  x <- x%>%mutate(EAG_orde=ifelse(!is.na(EAGIDENT) & substr(EAGIDENT,0,1) %in% 1,1000,
                             ifelse(!is.na(EAGIDENT) & substr(EAGIDENT,0,1) %in% 2,2000,
                                    ifelse(!is.na(EAGIDENT) & substr(EAGIDENT,0,1) %in% 3,3000,
                                           ifelse(!is.na(EAGIDENT) & substr(EAGIDENT,0,1) %in% 4,4000,
                                                  ifelse(!is.na(EAGIDENT) & substr(EAGIDENT,0,1) %in% 5,5000,
                                                         ifelse(!is.na(EAGIDENT) & substr(EAGIDENT,0,1) %in% 6,6000,
                                                                ifelse(!is.na(EAGIDENT) & substr(EAGIDENT,0,1) %in% 7,7000,
                                                                       ifelse(!is.na(EAGIDENT) & substr(EAGIDENT,0,1) %in% 8,8000,
                                                                              ifelse(!is.na(EAGIDENT) & substr(EAGIDENT,0,1) %in% 9,9000,NA)))))))))) 
  return(x)
}
do_doelgatplot <- function(x=db_tot_gat){
  #x%>%filter(gebiedtype %in% "KRW_waterlichaam")
  x <- x%>%filter(!watertype %in% "M1b")#losse waarneming eruit halen
g <- ggplot(x, aes(GHPR, doelgat, fill=GHPR))
g <- g + theme_bw()+geom_boxplot()
g <- g + facet_grid(gebiedtype~watertype, scales="free_x")
ggsave(paste0("output/doelgatplot_", Sys.Date(), ".png"), g, device="png", height=6 ,width=10)
x_sel <- x%>%filter(gebiedtype %in% "Overig water")%>%group_by(watertype, GHPR)%>%summarise(doelgat_gem =mean(doelgat, na.rm=T))%>%
  ungroup()%>%filter(!is.na(doelgat_gem))
x_sel$watertype <- factor(x_sel$watertype, levels=x_sel$watertype[order(x_sel$doelgat_gem)])
g <- ggplot(x_sel, aes(watertype, doelgat_gem))+theme_classic()
g <- g + geom_bar(stat="identity",fill="green")
g <- g + scale_y_continuous(limits=c(0,1),expand =c(0,0))
g <- g + ylab("gemiddelde afstand tot doel (delta EKR)")
ggsave(paste0("output/doelgatplot_ov_gem_", Sys.Date(), ".png"), g, device="png", height=8 ,width=8)
}
do_plot_EKR_table <- function(x=db_tot_gat_oordeel){
  x_KRW <- x%>%filter(!is.na(id))
  x_KRW$oordeel <- factor(x_KRW$oordeel , levels=c("onbekend", "slecht", "ontoereikend", "matig", "goed"))
  
  g1 <- ggplot(x_KRW, aes(gebied,fill=oordeel))+theme_minimal()
  g1 <- g1 + geom_bar(stat="count")
  g1 <- g1 + scale_y_continuous(limits=c(0,1),expand=c(0,0))
  g1 <- g1 + scale_fill_manual(values = x_KRW$oordeel.kleur, breaks = x_KRW$oordeel)
  g1 <- g1 + facet_grid(.~GHPR)
  g1 <- g1 + coord_flip()
  g1 <- g1 + theme(axis.title = element_blank(), axis.ticks.x =  element_blank(), axis.text.x = element_blank(),
                   legend.position = "bottom", legend.title = element_blank())
  ggsave(paste0("output/EKR/EKR_overzicht_KRW_wl_", Sys.Date(), ".png"), g1,device="png", height=8, width=6)
  #OVERIG WATER
  x_ov <- x%>%filter(is.na(id))
  x_ov$oordeel <- factor(x_ov$oordeel , levels=c("onbekend", "slecht", "ontoereikend", "matig", "goed"))
  for(i in unique(x_ov$EAG_orde)){
    x_ov_orde <- x_ov%>%filter(EAG_orde %in% i)
  g2 <- ggplot(x_ov_orde, aes(gebied,fill=oordeel))+theme_minimal()
  g2 <- g2 + geom_bar(stat="count")
  g2 <- g2 + scale_y_continuous(limits=c(0,1),expand=c(0,0))
  g2 <- g2 + scale_fill_manual(values = x_KRW$oordeel.kleur, breaks = x_KRW$oordeel)
  g2 <- g2 + facet_grid(.~GHPR)
  g2 <- g2 + coord_flip()
  g2 <- g2 + theme(axis.title = element_blank(), axis.ticks.x =  element_blank(), axis.text.x = element_blank(),
                   legend.position = "bottom", legend.title = element_blank())
  g2 <- g2 + ggtitle(paste0("EAG-orde: ",i))
  ggsave(paste0("output/EKR/EKR_overzicht_overig_",i,"_",Sys.Date(), ".png"), g2,device="png", height=(5+nrow(x_ov_orde)*0.1), width=8)
  }
  
}
plot_EKR_doel_per_gebied <- function(x=db_tot_long, type_sel="KRW_waterlichaam"){ #type is 'KRW_waterlichaam' of 'overig water'
  if(type_sel=="KRW_waterlichaam"){
    x_sel_oordeel <- x%>%filter(gebiedtype %in% type_sel & type %in% "EKR")
    x_sel_doel <-  x%>%filter(gebiedtype %in% type_sel & type %in% "GEP_2022")
    
    g <- ggplot(x_sel_oordeel,aes(gebied,waarde, fill="green")) + theme_classic()
    g <- g + geom_bar(stat="identity")
    g <- g + geom_point(data=x_sel_doel,aes(gebied,waarde), pch=45, size=6)
    g <- g + scale_y_continuous(limits=c(0,1), expand=c(0,0))
    g <- g + facet_grid(GHPR~watertype,scales="free_x", space="free_x",switch = "y")
    g <- g + theme(axis.text.x = element_text(angle=-90, vjust=0.1, hjust=-0.01))
    #g <- g + facet_grid(watertype~GHPR,scales="free_y", space="free_y",switch = "y")
    #g <- g + coord_flip()
    ggsave(paste0("output/EKR_vs_doel_per_KRW_WL", Sys.Date(), ".png"), g, device="png", height=10 ,width=14)
  }
  if(type_sel=="Overig water"){
    
    x_sel_oordeel <- x%>%filter(gebiedtype %in% type_sel& type %in% "EKR")
    levels <- x_sel_oordeel$EAGIDENT[order(x_sel_oordeel$waarde)]
    x_sel_doel <-  x%>%filter(gebiedtype %in% type_sel & type %in% "GEP_2022")  
    x_sel_oordeel$EAGIDENT <- factor(x_sel_oordeel$EAGIDENT, levels=levels)
    x_sel_doel$EAGIDENT <- factor(x_sel_doel$EAGIDENT, levels=levels)
    
    g <- ggplot(x_sel_oordeel,aes(EAGIDENT,waarde, fill="green")) + theme_classic()
    g <- g + geom_bar(stat="identity")
    g <- g + geom_point(data=x_sel_doel,aes(gebied,waarde), pch=73, size=4)
    g <- g + scale_y_continuous(limits=c(0,1), expand=c(0,0))
    g <- g + facet_grid(watertype~.,scales="free_y", space="free_y",switch = "y")
    g <- g + theme(axis.text.x = element_text(angle=-90, vjust=0.1, hjust=-0.01))
    g <- g + coord_flip()
    ggsave(paste0("output/EKR_vs_doel_overig_", Sys.Date(), ".png"), g, device="png", height=25 ,width=10)
    }
}

filter_gemeten_vanaf <- function(x=doSplit(db_stofDr,"parameternaam")[[10]],jaar=2020){
  if(max(x$jaar)>=jaar){return(x)}
}
#ESF-resultaten
do_plot_esf_table <- function(x=ESFset_long){
  #KRW_waterlichamen
  koppel_ESF_names <- data.frame(ESF=c("ESF1", "ESF2","ESF3", "ESF4", "ESF5","ESF6","ESF7", "ESF8"), ESF_names=factor(c("nutriënten", "lichtklimaat", "waterbodem","habitat", "verspreiding", "verwijdering","organische bel.", "toxiciteit"),levels=c("nutriënten", "lichtklimaat", "waterbodem","habitat", "verspreiding", "verwijdering","organische bel.", "toxiciteit")))
  x <- x%>%left_join(koppel_ESF_names,"ESF")
  x_KRW <- x%>%filter(grepl("NL", OWL))
  x_KRW$resultaat <- factor(x_KRW$resultaat , levels=c("onbekend", "niet op orde", "at risk", "op orde"))
  g1 <- ggplot(x_KRW, aes(OWMNAAM_SGBP3,fill=resultaat))+theme_minimal()
  g1 <- g1 + geom_bar(stat="count")
  g1 <- g1 + scale_y_continuous(limits=c(0,1),expand=c(0,0))
  g1 <- g1 + scale_fill_manual(values = x_KRW$resultaat.kleur, breaks = x_KRW$resultaat)
  g1 <- g1 + facet_grid(.~ESF_names)
  g1 <- g1 + coord_flip()
  g1 <- g1 + theme(axis.title = element_blank(), axis.ticks.x =  element_blank(), axis.text.x = element_blank(),
                   legend.position = "bottom", legend.title = element_blank())
  ggsave(paste0("output/ESF/overzicht_KRW_wl_", Sys.Date(), ".png"), g1,device="png", height=8, width=10)
  
  #OVERIG WATER
  x_ov <- x%>%filter(!grepl("NL", OWL))
  x_ov$resultaat <- factor(x_ov$resultaat , levels=c("onbekend", "niet op orde", "at risk", "op orde"))
  g2 <- ggplot(x_ov, aes(OWL_SGBP3,fill=resultaat))+theme_minimal()
  g2 <- g2 + geom_bar(stat="count")
  g2 <- g2 + scale_y_continuous(limits=c(0,1),expand=c(0,0))
  g2 <- g2 + scale_fill_manual(values = x_KRW$resultaat.kleur, breaks = x_KRW$resultaat)
  g2 <- g2 + facet_grid(.~ESF_names)
  g2 <- g2 + coord_flip()
  g2 <- g2 + theme(axis.title = element_blank(), axis.ticks.x =  element_blank(), axis.text.x = element_blank(),
                   legend.position = "bottom", legend.title = element_blank())
  ggsave(paste0("output/ESF/overzicht_overig_", Sys.Date(), ".png"), g2,device="png", height=20, width=10)
}
#stoffen
do_aantal_stoffen_en_punten <- function(x=db_stofOpp){
  #x_agg_I <- x%>%select(locatie.EAG, locatie.bodemsoortlocatie,locatiecode,jaar,fewsparametercategorie,fewsparameter,fewsparameternaam)%>%distinct()
  x_agg_I <- x%>%select(locatie.EAG,locatiecode,jaar, fewsparametercategorie)%>%distinct()%>%group_by(jaar, fewsparametercategorie)%>%summarize(aantal=n())%>%ungroup()
  for(i in unique(x_agg_I$fewsparametercategorie)){
  x_agg_i <- x_agg_I[x_agg_I$fewsparametercategorie %in% i,] 
    
  g <- ggplot(x_agg_i,aes(jaar,aantal))+theme_classic()
  g <- g + geom_bar(stat="identity")
  g <- g + scale_y_continuous(expand=c(0,0))
  g <- g + ggtitle(paste0("parametercategorie = ",i))+ylab("aantal locaties")
  ggsave(paste0("output/stoffen_agg/aantal_locaties_per_jaar_",i,"_",Sys.Date(),".png"),g,device="png", width=8, height =6)
  }
  
  x_agg_II  <- x%>%select(locatie.EAG, jaar, fewsparameter,fewsparametercategorie)%>%distinct()%>%group_by(jaar, locatie.EAG,fewsparametercategorie)%>%summarize(aantal=n())%>%ungroup()
  for(i in unique(x_agg_II$fewsparametercategorie)){
    x_agg_i <- x_agg_II[x_agg_II$fewsparametercategorie %in% i,] 
    
    g <- ggplot(x_agg_i,aes(jaar,aantal))+theme_bw()
    g <- g + geom_bar(stat="identity")
    g <- g + scale_y_continuous(expand=c(0,0))
    g <- g + ggtitle(paste0("parametercategorie = ",i))+ylab("aantal stoffen")
    g <- g + facet_grid(locatie.EAG~.)+theme(strip.text.y = element_text(angle=-0.001))
    ggsave(paste0("output/stoffen_agg/aantal_stoffen_per_EAG_jaar_",i,"_",Sys.Date(),".png"),g,device="png", width=8, height =15)
  }
  x_agg_III  <- x%>%select(jaar, fewsparameter,fewsparametercategorie)%>%distinct()%>%group_by(jaar, fewsparametercategorie)%>%summarize(aantal=n())%>%ungroup()
  for(i in unique(x_agg_III$fewsparametercategorie)){
    x_agg_i <- x_agg_III[x_agg_III$fewsparametercategorie %in% i,] 
    
    g <- ggplot(x_agg_i,aes(jaar,aantal))+theme_bw()
    g <- g + geom_bar(stat="identity")
    g <- g + scale_y_continuous(expand=c(0,0))
    g <- g + ggtitle(paste0("parametercategorie = ",i))+ylab("aantal stoffen")
    ggsave(paste0("output/stoffen_agg/aantal_stoffen_per_jaar_",i,"_",Sys.Date(),".png"),g,device="png", width=8, height =6)
  }

  }

do_stofplot <- function(x=doSplit(db_stofOpp, "parameternaam")[[30]], type="drinkwater"){
  c <- unique(x$fewsparametercategorie)
  n <- unique(x$parameternaam)
  eenh <- unique(x$eenheid)
  min_jaar <- 2010
  max_jaar <- 2021
  g <- ggplot(x, aes(datum, meetwaarde, group=locatiecode, color=locatiecode))+theme_classic()
  g <- g + geom_line()+geom_point()
  g <- g + ggtitle(paste0(c,": ", n))
  g <- g + ylab(paste0(n, "( ",eenh,")"))
  g <- g + scale_x_date(limits = c(as.Date(paste0(min_jaar,"-01-01")),as.Date(paste0(max_jaar,"-12-31"))), 
                        breaks = as.Date(paste0(min_jaar:max_jaar,"-01-01")),
                        labels=c(min_jaar:max_jaar), expand = c(0,0))
  if(type=="drinkwater"){
    ggsave(paste0("output/stoffen_drinkwater/drinkwater_", c,"_",n,"_", Sys.Date(), ".png"), device = "png", height = 6, width = 10)
  }
  if(type=="oppervlaktewater"){
    g <- g + facet_grid(locatie.EAG~.)
    ggsave(paste0("output/stoffen_oppervlaktewarter/drinkwater_", c,"_",n,"_", Sys.Date(), ".png"), device = "png", height = 6, width = 10)
  }
}
do_plot_stoftoets <- function(x=doSplit(db_stof_toets, "meetnet")[[2]]){
  m <- unique(x$meetnet)
  x_agg <- x%>%group_by(gebied, jaar, watertype, Alfanumeriekewaarde)%>%summarize(waarde=n())%>%ungroup()
  
  if(m=="fychem"){
    x_agg$Alfanumeriekewaarde <- factor(x_agg$Alfanumeriekewaarde, levels=c("Slecht", "Ontoereikend", "Matig", "Goed", "Zeer goed"))
    x_agg <- x_agg%>%mutate(oordeel_kleur = ifelse(Alfanumeriekewaarde == "Slecht", "red",
                                            ifelse(Alfanumeriekewaarde ==  "Ontoereikend", "orange",
                                                   ifelse(Alfanumeriekewaarde == "Matig", "yellow",
                                                          ifelse(Alfanumeriekewaarde == "Goed", "green",
                                                                 ifelse(Alfanumeriekewaarde == "Zeer goed", "blue","grey"))))))
  
    }else{ 
      x_agg$Alfanumeriekewaarde <- factor(x_agg$Alfanumeriekewaarde, levels=c("Voldoet niet","Niet toetsbaar", "Voldoet"))
      x_agg <- x_agg%>%mutate(oordeel_kleur = ifelse(Alfanumeriekewaarde == "Niet toetsbaar", "grey",
                                           ifelse(Alfanumeriekewaarde ==  "Voldoet niet", "red",
                                                  ifelse(Alfanumeriekewaarde ==  "Voldoet", "green","grey"))))
   # x$oordeel_kleur <- factor(x$oordeel_kleur, levels=c("grey", "red", "green"))
    }
  x_agg <- x_agg%>%rename(oordeel=Alfanumeriekewaarde)
  g <- ggplot(x_agg, aes(gebied,waarde, fill=forcats::fct_rev(oordeel)))+theme_bw()
  g <- g + geom_bar(stat="identity", position="fill")
  g <- g + facet_grid(jaar~watertype, space="free", scales="free",switch = "both")
  g <- g + scale_y_continuous(expand=c(0,0))
  g <- g + scale_fill_manual(values=unique(x_agg$oordeel_kleur[order(x_agg$oordeel)]), breaks = levels(x_agg$oordeel))
  g <- g + theme(axis.text.x = element_text(angle=-90, hjust=-0.01, vjust=0.01),axis.text.y = element_blank(),axis.ticks.y = element_blank(), legend.title = element_blank() )
  g <- g + ylab("")+xlab("")+ggtitle(unique(x$meetnet))
  #g <- g + coord_flip()
  ggsave(paste0("output/stoffen_toets/",m,"_2018_2019_2020.png"), g, device="png", height=8, width=12)
}
do_plot_stoftoets_2 <- function(x=doSplit(db_stof_toets, c("gebied", "meetnet"))[[2]]){
  if(nrow(x)>3){
  m <- unique(x$meetnet)
  geb. <- unique(x$gebied)
  if(m=="fychem"){
    x$Alfanumeriekewaarde <- factor(x$Alfanumeriekewaarde, levels=c("Slecht", "Ontoereikend", "Matig", "Goed", "Zeer goed"))
    x$Parameter.omschrijving[x$Grootheid.omschrijving %in% c("Temperatuur", "Zuurgraad", "Doorzicht")] <- x$Grootheid.omschrijving[x$Grootheid.omschrijving %in% c("Temperatuur", "Zuurgraad", "Doorzicht")]
  }else{ 
    x$Alfanumeriekewaarde <- factor(x$Alfanumeriekewaarde, levels=c("Voldoet niet","Niet toetsbaar", "Voldoet"))
    x$Parameter.omschrijving <- paste0(x$Parameter.omschrijving, " (", x$Waardebewerkingmethode.code,")") 
  }
  x <- x%>%rename(oordeel=Alfanumeriekewaarde,stof=Parameter.omschrijving)
  g <- ggplot(x, aes(jaar,group=oordeel, fill=oordeel))+theme_bw()
  g <- g + geom_bar(stat="count", position="fill")
  g <- g + facet_grid(stof~., space="free", scales="free",switch = "both")
  g <- g + scale_y_continuous(expand=c(0,0))
  g <- g + scale_fill_brewer(palette = "Spectral")
  g <- g + theme(strip.text.y.left = element_text(angle=0), axis.text.x = element_text(angle=-90, hjust=-0.01, vjust=0.01),axis.text.y = element_blank(),
                 axis.ticks.y = element_blank())
  g <- g + ylab("")+xlab("")+ggtitle(paste0(geb., ": ", m))
  #g <- g + coord_flip()
  ggsave(paste0("output/stoffen_toets/",m, "_", geb.,"_2018_2019_2020.png"), g, device="png", height=12, width=10)
  }
}


make_subset_macft_EKR <- function(x=readRDS("../data/trendekr.rds")){
  maatlatten <- c("Overige waterflora-kwaliteit","Abundantie groeivormen macrofyten", "Soortensamenstelling macrofyten","Bedekking Grote drijfbladplanten","Bedekking Emerse planten","Bedekking Flab","Bedekking Kroos","Bedekking som submerse planten en draadalgen")
  x_sel <- x%>%filter(GHPR %in% maatlatten)%>%select(id, GHPR, KRWwatertype.code, EAGIDENT, GEP_2022, POT_2022, waterlichaam, EKRref,EKR3jr, 
                                                     `2006`, `2007`,`2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`)
  write.table(x_sel,"output/EKR-tabelII_20220119.csv", sep=";", dec=".", row.names = F)
  }


##### DEEL 3
do_opwerken_KNMI_data <- function(x=db_KNMI,vanaf=2012){
names(x)[2] <- "datum"
names(x)[12] <- "TG"
names(x)[23] <- "RH"
names(x)[41] <- "EV24"
x_sel <- x%>%dplyr::select(datum, RH, EV24,TG)%>%mutate(neerslag=RH/10, verdamping=EV24/10,temperatuur=TG/10)
x_sel <- x_sel%>%mutate(neerslagoverschot = neerslag - verdamping)
x_sel$datum <- as.Date(paste0(substr(x_sel$datum,1,4),"-",substr(x_sel$datum,5,6),"-",substr(x_sel$datum,7,8)),format="%Y-%m-%d")
x_sel$jaar <- as.numeric(format(x_sel$datum,"%Y"))
x_sel$maand <- as.numeric(format(x_sel$datum,"%m"))

x_out <- x_sel%>%filter(jaar >= vanaf)%>%dplyr::select(datum,jaar,maand,temperatuur,neerslag, verdamping, neerslagoverschot)
return(x_out)
}
do_opwerken_nut_data <- function(x=db_fychem,vanaf=2012){
  x_sel <- x%>%dplyr::select(datum,locatiecode,locatie.EAG,fewsparametercode,meetwaarde,afronding)%>%distinct()
  x_sel$datum <- as.Date(substr(x_sel$datum,1,10),format="%Y-%m-%d")
  x_sel$jaar <- as.numeric(format(x_sel$datum,"%Y"))
  x_sel$maand <- as.numeric(format(x_sel$datum,"%m"))
  
  x_out <- x_sel%>%filter(jaar >= vanaf,fewsparametercode %in% c("PO4","Ptot","Ntot"))%>%dplyr::select(datum,locatie.EAG,locatiecode,fewsparametercode,meetwaarde)%>%distinct()
  return(x_out)
}
do_remove_outliers <- function(x=db_fychem,perc=0.01){
  x_percs <- x%>%group_by(fewsparametercode)%>%summarize(perc=quantile(meetwaarde,probs=1-perc))
  x_sel <- x[x$fewsparametercode %in% x_percs$fewsparametercode[1] & x$meetwaarde <x_percs$perc[1] |
               x$fewsparametercode %in% x_percs$fewsparametercode[2] & x$meetwaarde <x_percs$perc[2]|
               x$fewsparametercode %in% x_percs$fewsparametercode[3] & x$meetwaarde <x_percs$perc[3],]
  return(x_sel)
}
do_calc_periodic_means_KNMI <- function(x=db_KNMI, p=10){
 x$neerslagoverschot_mean <- NULL
 x$temperatuur_mean <- NULL
 x$neerslag_cumu <- NULL
  for(i in x$datum){
  neerslagoverschot_mean <- x%>%filter(datum %in% (i-p):i)%>%mutate(out=mean(neerslagoverschot))%>%dplyr::select(out)%>%distinct()%>%as.numeric()
  x$neerslagoverschot_mean[x$datum %in% i] <- neerslagoverschot_mean
  temperatuur_mean <- x%>%filter(datum %in% (i-p):i)%>%mutate(out=mean(temperatuur))%>%dplyr::select(out)%>%distinct()%>%as.numeric()
  x$temperatuur_mean[x$datum %in% i] <- temperatuur_mean
  x$neerslag_cumu[x$datum %in% i] <- x%>%filter(datum %in% (i-p):i)%>%mutate(out=sum(neerslag))%>%dplyr::select(out)%>%distinct()%>%as.numeric()
  x$n_dag <- p
  }
  return(x)  
  } 
do_analyse_nuts_KNMI_II <- function(x=doSplit(db_nut_KNMI_means,"locatie.EAG")[["2550-EAG-1"]],iterator="locatie.EAG",data_min=10){
  if(nrow(x)>10){
    print(unique(x[,iterator]))
    winter <- c(12,1,2) #wintermaanden
    voorjaar <- c(3,4,5)
    zomer <- c(6,7,8)
    najaar <- c(9,10,11)
    
    ## 15 gebieden -> mail maarten
    ## naam EAGs in plaatje toevoegen
    #AANPASSEN: cumulatieve neerslag (niet overschot!) van 5mm 
    zeer_droog_grens <- 0 #zeer droog: <-2mm/dag
    nat_grens <- 9         #droog: <2mm/dag
    zeer_nat_grens <- 20   #nat: < 5mm/dag, zeer nat: >=10mm/dag
    x <- x%>%mutate(seizoen=ifelse(maand %in% winter,"winter",
                                   ifelse(maand %in% voorjaar,"voorjaar",
                                          ifelse(maand %in% zomer,"zomer",
                                                 ifelse(maand %in% najaar,"najaar","")))))
    #x <- x%>%mutate(neerslagperiode=ifelse(neerslag_cumu <= zeer_droog_grens,"zeer droog",
     #                                      ifelse(neerslag_cumu < nat_grens,"droog",
      #                                            ifelse(neerslag_cumu < zeer_nat_grens,"nat","zeer nat"))))
    x <- x%>%mutate(neerslagperiode=ifelse(neerslag_cumu < nat_grens,"droog","nat"))
    x <- x%>%mutate(jaar_alt=ifelse(maand==12,jaar+1,jaar)) #neem december mee als start jaar erna!
    x$cat <- paste0(x$seizoen)
    
    lm_df_out <- NULL
    for(i in unique(x$cat)){
      for(j in unique(x$neerslagperiode)){
        for(z in unique(x$fewsparametercode)){
        x_sel <- x%>%filter(cat %in% i,neerslagperiode %in% j, fewsparametercode %in% z)
        if(nrow(x_sel)==0){next}
        
        db_assumptions <- do_test_assumptions(x_sel, type="normal")
        db_assumptions_log <- do_test_assumptions(x_sel, type="log")
        statistics_type_selector <- ifelse(is.null(db_assumptions),"geen",ifelse(db_assumptions$sum == 5,"normal",ifelse(db_assumptions_log$sum == 5,"log","geen")))
        
        if(statistics_type_selector=="normal"){
        r_sq <- round(summary(lm(meetwaarde~jaar_alt, data=x_sel))$r.squared,3)
        p <- round(data.frame(summary(lm(meetwaarde~jaar_alt, data=x_sel))$coefficients)[2,4],3)
        sl <- round(data.frame(summary(lm(meetwaarde~jaar_alt, data=x_sel))$coefficients)[2,1],3)
        jrn <- length(unique(x_sel$jaar_alt))
        jr_ltst <- max(x_sel$jaar_alt)
        lm_df_out <-rbind(lm_df_out,data.frame(cat=i,neerslagperiode=j,fewsparametercode=z,p=p,r_squared=r_sq,slope=sl, jaren=jrn,laatste_jaar=jr_ltst,statistiek="normal"))
        }
        if(statistics_type_selector=="log"){
          x_sel <- x_sel%>%mutate(meetwaarde = ifelse(meetwaarde==0,0.000001,meetwaarde))
          r_sq <- round(summary(lm(log(meetwaarde)~jaar_alt, data=x_sel))$r.squared,3)
          p <- round(data.frame(summary(lm(log(meetwaarde)~jaar_alt, data=x_sel))$coefficients)[2,4],3)
          sl <- round(data.frame(summary(lm(log(meetwaarde)~jaar_alt, data=x_sel))$coefficients)[2,1],3)
          jrn <- length(unique(x_sel$jaar_alt))
          jr_ltst <- max(x_sel$jaar_alt)
          lm_df_out <-rbind(lm_df_out,data.frame(cat=i,neerslagperiode=j,fewsparametercode=z,p=p,r_squared=r_sq,slope=sl, jaren=jrn,laatste_jaar=jr_ltst,statistiek="log"))
        }
        if(statistics_type_selector=="geen"){
          jrn <- length(unique(x_sel$jaar_alt))
          jr_ltst <- max(x_sel$jaar_alt)
          lm_df_out <-rbind(lm_df_out,data.frame(cat=i,neerslagperiode=j,fewsparametercode=z,p="ontestbaar",r_squared="ontestbaar",slope="ontestbaar", jaren=jrn,laatste_jaar=jr_ltst,statistiek="geen"))
        }  
      }
      }
    }
    lm_df_out$p[!is.na(lm_df_out$p) &! NaN %in% lm_df_out$p & lm_df_out$p <=0.05] <- paste0(lm_df_out$p[!is.na(lm_df_out$p) &! NaN %in% lm_df_out$p & lm_df_out$p <=0.05],"*")
    lm_df_out$p_text <- paste0("p: ",lm_df_out$p)
    lm_df_out$x_pos <- min(x$jaar_alt)+1.5
    lm_df_out$y_pos <- ifelse(lm_df_out$fewsparametercode %in% "Ptot",max(x$meetwaarde[x$fewsparametercode %in% "Ptot"])*1.1,
                              ifelse(lm_df_out$fewsparametercode %in% "PO4",max(x$meetwaarde[x$fewsparametercode %in% "PO4"])*1.1,
                                     max(x$meetwaarde[x$fewsparametercode %in% "Ntot"])*1.1))
    x_add <- x%>%left_join(lm_df_out,c("cat", "neerslagperiode","fewsparametercode"))
    for(i in unique(x_add$fewsparametercode)){
    x_add_sel <- x_add%>%filter(fewsparametercode %in% i)
    x_add_sel$cat <- factor(x_add_sel$cat, levels=c("winter","voorjaar","zomer","najaar"))
    #x_add_sel$neerslagperiode <- factor(x_add_sel$neerslagperiode,levels=c("zeer nat","nat","droog","zeer droog"))
    x_add_sel$neerslagperiode <- factor(x_add_sel$neerslagperiode,levels=c("nat","droog"))
    data_text <- lm_df_out[lm_df_out$fewsparametercode %in% i,]
    data_text$cat <- factor(data_text$cat, levels=c("winter","voorjaar","zomer","najaar"))
    #data_text$neerslagperiode <- factor(data_text$neerslagperiode,levels=c("zeer nat","nat","droog","zeer droog"))
    data_text$neerslagperiode <- factor(data_text$neerslagperiode,levels=c("nat","droog"))
    
    g <- ggplot(x_add_sel,aes(jaar_alt,meetwaarde, group=cat))+theme_bw()
    g <- g + geom_point()
    if(length(unique(x[,iterator]))>1){
      g <- g + ggtitle(paste0(i," in alle EAG's samen"))
    }else{ g <- g + ggtitle(paste0(i," in ", unique(x[,iterator])))
    }
    g <- g + facet_grid(neerslagperiode~cat)
    g <- g + geom_smooth(formula=y~x,method="lm")
    g <- g + scale_x_continuous(breaks=min(x$jaar):max(x$jaar))
    g <- g + geom_text(data=data_text,aes(x=x_pos,y=y_pos,label=(paste0("p: ",p,"; r^2: ",r_squared)),color="blue",size=2),show.legend = F)
    if(length(unique(x[,iterator]))>1){
      ggsave(paste0("output/nutrienten/nutrienten_per_cat_",i,"_",Sys.Date(),".png"),g,device="png",width=35,height=10)
    }else{
      ggsave(paste0("output/nutrienten/",iterator,"/nutrienten_per_cat_",unique(x[,iterator]),"_",i,"_",Sys.Date(),".png"),g,device="png",width=15,height=10) 
    }
    }
    x_out <- x_add%>%dplyr::select(-locatiecode,-meetwaarde)%>%distinct()
    return(x_out)
  }
}
#funtie in functie!
do_test_assumptions <- function(x_f=x_sel, type="normal"){#functie wordt aangeroepen in do_analyse_nuts_KNMI_II()
  #  print(paste0(unique(x$habitattype), unique(x$parameter)))
  if(length(unique(x_f$meetwaarde))>1 & length(unique(x_f$jaar_alt))>=3){ #wanneer er 0 variatie is foutmelding teruggeven!
    x_f <- x_f%>%mutate(meetwaarde=ifelse(meetwaarde==0,0.0000001,meetwaarde)) #ken een zeer laag getal toe aan nul, zodat log-transformatie niet vastloopt. Dit is toegestaan, zie: https://www.researchgate.net/post/Log_transformation_of_values_that_include_0_zero_for_statistical_analyses2
    if(type=="normal"){model <- lm(meetwaarde ~ jaar_alt, data=x_f)}
    if(type=="log"){model <- lm(log(meetwaarde) ~ jaar_alt, data=x_f)}
    
    assumptions_test <- gvlma(model)
    Global_test  <- if (assumptions_test$GlobalTest$GlobalStat4$pvalue > 0.05) {1} else {0} # Linearity. Is the relationship between X and Y linear
    Skewness <- if (assumptions_test$GlobalTest$DirectionalStat1$pvalue > 0.05) {1} else {0} # Normality. Is the distribution symmetrical or skewed
    Kurtosis <- if (assumptions_test$GlobalTest$DirectionalStat2$pvalue > 0.05) {1} else {0} # Normality. Does the data have a long tail or sharp peak
    Link_function <- if (assumptions_test$GlobalTest$DirectionalStat3$pvalue >= 0.05) {1} else {0} # Normality. Is the data truly continuous or more categorical
    Heteroscedasticity <- if (assumptions_test$GlobalTest$DirectionalStat4$pvalue   >= 0.05) {1} else {0} # Homoscedasticity. Is the variance constant across X
    
    assumptions <- data.frame(fewsparametercode=unique(x_f$fewsparametercode), neerslagperiode =unique(x_f$neerslagperiode), type=type, n_jaren=length(unique(x_f$jaar_alt)),
                              Linearity = Global_test, Normality_Skewness = Skewness, Normality_Kurtosis = Kurtosis, Normality_continuous = Link_function, Homoscedasticity = Heteroscedasticity, 
                              sum=sum(Global_test,Skewness, Kurtosis, Link_function,Heteroscedasticity))
    return(assumptions)
  }else{#print(paste0("warning voor ", unique(x$habitattype)," ", unique(x$parameter), ": variatie in waardes is 0, kon geen toets uitvoeren."))
  }
}



do_vergelijking_ecologie_nut_KNMI <- function(x=db_EKR,y=db_nut_KNMI_means_trend_EAG){
  y_sel <- y%>%dplyr::select(locatie.EAG,fewsparametercode,neerslagperiode,p,slope,r_squared,p_text, jaren,laatste_jaar,cat,statistiek)%>%distinct()
  #uitgangspunt: rsquared moet tenminste 0.3 zijn en tenminste drie meetjaren voor we spreken van een verband
  y_sel <- y_sel%>%mutate(nut_oordeel=ifelse(r_squared < 0.3 | statistiek == "geen","niet significant",
                                             ifelse(grepl("*",p) &  slope <= -0.25,"zeer wenselijk",
                                             ifelse(grepl("*",p) & slope < 0,"wenselijk",
                                                    ifelse(grepl("*",p) & slope < 0.25,"onwenselijk",
                                                           ifelse(grepl("*",p) & slope >= 0.25,"zeer onwenselijk",
                                                                  "niet significant"))))))
  y_sel_stat_zomer_nat <- y_sel%>%filter(cat %in%"zomer", neerslagperiode %in% "nat", fewsparametercode %in% "Ptot")%>%dplyr::select(locatie.EAG,statistiek)%>%rename(stat_zomer_nat_P=statistiek)
  y_sel <- y_sel %>% left_join(y_sel_stat_zomer_nat,c("locatie.EAG"))
  y_sel_wide <- y_sel%>%dplyr::select(-p,-slope,-r_squared,-p_text,-jaren,-laatste_jaar,-statistiek)%>%distinct()%>%pivot_wider(names_from = c("cat","neerslagperiode","fewsparametercode"),values_from = "nut_oordeel" )
  x_sel <- x%>%dplyr::select(GAFIDENT,cat_verschil.ref_2019, cat_verschil.ref_2020, cat_verschil.2019_2020)
  db_combi <- x_sel%>%left_join(y_sel_wide,c("GAFIDENT"= "locatie.EAG"))
  db_combi$oordeel_EKR_zomer_nat_P <- ifelse(db_combi$cat_verschil.ref_2019 %in% c("licht positief","zeer positief") &
                                               db_combi$zomer_nat_Ptot %in% c("wenselijk","zeer wenselijk"),"beide wenselijk",
                                                ifelse(db_combi$cat_verschil.ref_2019 %in% c("licht negatief","zeer negatief") &
                                                         db_combi$zomer_nat_Ptot %in% c("onwenselijk","zeer onwenselijk"),"beide onwenselijk",
                                                       ifelse(db_combi$cat_verschil.ref_2019 %in% c("gelijk") &
                                                                db_combi$zomer_nat_Ptot %in% c("niet significant") ,"beide stabiel",
                                                       ifelse(db_combi$cat_verschil.ref_2019 %in% c("onbekend",NA) &
                                                                db_combi$zomer_nat_Ptot %in% c(NA),"onbekend","ongelijke trend"
                                                                 ))))
  
  #db_combi$oordeel_EKR_zomer_nat_P <- ifelse(db_combi$cat_verschil.ref_2019 %in% c("licht positief","zeer positief") &
  #                                             (db_combi$zomer_nat_Ptot %in% c("nwenselijk","zeer wenselijk") | 
  #                                                db_combi$`zomer_zeer nat_Ptot` %in% c("wenselijk","zeer wenselijk")),"beide wenselijk",
  #                                              ifelse(db_combi$cat_verschil.ref_2019 %in% c("licht negatief","zeer negatief") &
  #                                                       (db_combi$zomer_nat_Ptot %in% c("onwenselijk","zeer onwenselijk")|
  #                                                          db_combi$`zomer_zeer nat_Ptot` %in% c("onwenselijk","zeer onwenselijk")),"beide onwenselijk",
  #                                                     ifelse(db_combi$cat_verschil.ref_2019 %in% c("gelijk") &
  #                                                              (db_combi$zomer_nat_Ptot %in% c("niet significant") |
  #                                                                 db_combi$`zomer_zeer nat_Ptot` %in% c("niet significant")),"beide stabiel",
  #                                                     ifelse(db_combi$cat_verschil.ref_2019 %in% c("onbekend",NA) &
  #                                                              (db_combi$zomer_nat_Ptot %in% c(NA) &
  #                                                                 db_combi$`zomer_zeer nat_Ptot`%in% c(NA)),"onbekend","ongelijke trend"
  #                                                               ))))
  
  g <- ggplot(db_combi,aes(GAFIDENT,fill=oordeel_EKR_zomer_nat_P))+theme_bw()
  g <- g + geom_bar(stat="count")
  g <- g + facet_grid(oordeel_EKR_zomer_nat_P~.,scales="free")
  g <- g + coord_flip()
  return(db_combi)
}
do_EKR_trendplot <- function(x=db_EKR, y=eag_wl){
  x_combi <- x%>%left_join(y,c("GAFIDENT"="GAFIDENT"))
  x_combi_1 <- x_combi%>%filter(!LANDBOUWGEB %in% c("gtb","plas","sportpark"))
  g <- ggplot(x_combi_1,aes(EKRref,verschil.ref_2019,color=LANDBOUWGEB))+theme_bw()
  g <- g + geom_abline(slope = 0,intercept = 0,linetype="dashed", color="red")
  g <- g + geom_point()
  g <- g + xlab("EKR-score SGBP1")+ylab("Verschil EKR SGBP2 - SGBP1")
  g <- g + geom_smooth(formula=y~x,method="lm")
  g <- g + facet_wrap("LANDBOUWGEB",ncol=5)
  g <- g + theme(legend.position="")
  ggsave("output/EKR_trend/EKR_vs_delta_EKR_gebruikstype.png",g,device="png",width=10,height=6)
  
  x_combi_2 <- x_combi%>%filter(!Deelgebied %in% c("Het Gooi","NZK"))
  g <- ggplot(x_combi_2,aes(EKRref,verschil.ref_2019,color=Deelgebied))+theme_bw()
  g <- g + geom_abline(slope = 0,intercept = 0,linetype="dashed", color="red")
  g <- g + geom_point()
  g <- g + xlab("EKR-score SGBP1")+ylab("Verschil EKR SGBP2 - SGBP1")
  g <- g + geom_smooth(formula=y~x,method="lm")
  g <- g + facet_wrap("Deelgebied",ncol=5)
  g <- g + theme(legend.position="")
  ggsave("output/EKR_trend/EKR_vs_delta_EKR_deelgebied.png",g,device="png",width=12,height=6)
  
  x_combi_3 <- x_combi%>%filter(!watertype %in% c("M1b"))
  g <- ggplot(x_combi_3,aes(EKRref,verschil.ref_2019,color=watertype))+theme_bw()
  g <- g + geom_abline(slope = 0,intercept = 0,linetype="dashed", color="red")
  g <- g + geom_point()
  g <- g + xlab("EKR-score SGBP1")+ylab("Verschil EKR SGBP2 - SGBP1")
  g <- g + geom_smooth(formula=y~x,method="lm")
  g <- g + facet_wrap("watertype",ncol=5)
  g <- g + theme(legend.position="")
  ggsave("output/EKR_trend/EKR_vs_delta_EKR_watertype.png",g,device="png",width=12,height=6)
  
  x_combi_4 <- x_combi
  g <- ggplot(x_combi_4,aes(EKRref,verschil.ref_2019,color=type))+theme_bw()
  g <- g + geom_abline(slope = 0,intercept = 0,linetype="dashed", color="red")
  g <- g + geom_point()
  g <- g + xlab("EKR-score SGBP1")+ylab("Verschil EKR SGBP2 - SGBP1")
  g <- g + geom_smooth(formula=y~x,method="lm")
  g <- g + facet_wrap("type",ncol=2)
  g <- g + theme(legend.position="")
  ggsave("output/EKR_trend/EKR_vs_delta_EKR_type_water.png",g,device="png",width=8,height=6)
  
  x_combi_5 <- x_combi%>%filter(!OPMERKING %in% c("M1b"))
  g <- ggplot(x_combi_5,aes(EKRref,verschil.ref_2019,color=OPMERKING))+theme_bw()
  g <- g + geom_abline(slope = 0,intercept = 0,linetype="dashed", color="red")
  g <- g + geom_point()
  g <- g + xlab("EKR-score SGBP1")+ylab("Verschil EKR SGBP2 - SGBP1")
  g <- g + geom_smooth(formula=y~x,method="lm")
  #g <- g + facet_wrap("OPMERKING",ncol=2)
  g <- g + theme(legend.position="")
  ggsave("output/EKR_trend/EKR_vs_delta_EKR_KRW_OW.png",g,device="png",width=6,height=6)
  
  delta_EKR_gem <- x_combi%>%group_by(OPMERKING)%>%summarize(gemiddeld=mean(verschil.ref_2019,na.rm=T))%>%ungroup()
  winst_KRW <- delta_EKR_gem$gemiddeld[delta_EKR_gem$OPMERKING %in% "KRW Waterlichaam"] - delta_EKR_gem$gemiddeld[delta_EKR_gem$OPMERKING %in% "KRW Overig water"]
}
##output GIS
get_map_EKR_nut <- function(eag_wl=eag_wl,db_oordeel=oordeel_EKR_nut,gEAG=gEAG,gEAG_new=gEAG_new,gEAG_sf=gEAG_sf,maptype="vastgesteld"){
  if(maptype=="vastgesteld"){
    map <- sp::merge(gEAG, eag_wl, by.x = 'GAFIDENT', by.y =
                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)  
    map <- sp::merge(map, db_oordeel, by.x = 'GAFIDENT', by.y =
                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    #map_shp <- sp::merge(gEAG_sf, eag_wl, by.x = 'GAFIDENT', by.y =
    #                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    #map_shp <- sp::merge(map_shp, db_EKR, by.x = 'GAFIDENT', by.y =
    #                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
  }
  if(maptype=="nieuw"){
    map <- sp::merge(gEAG_new, eag_wl, by.x = 'GAFIDENT', by.y =
                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    map <- sp::merge(map, db_oordeel, by.x = 'GAFIDENT', by.y =
                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    #map_shp <- sp::merge(gEAG_sf, eag_wl, by.x = 'GAFIDENT', by.y =
    #                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    #map_shp <- sp::merge(map_shp, db_EKR, by.x = 'GAFIDENT', by.y =
    #                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
  }
  
  #map$param <- as.factor(map$oordeel_EKR_zomer_nat_P)
  #map <- map[order(map$param) & !is.na(map$param) & !map$param == "",]
  #map$param <- fct_drop(map$param)
  
  #set colors
  kleuren <- data.frame(cat=c("onbekend","beide onwenselijk", "beide stabiel", "beide wenselijk", "ongelijke trend"), 
                        kleur=c("#bcbcbc","#fc2f2f","#e5e500","#0ed90e","#f3c99d"))
  #col <- kleuren$kleur
  #lab <- kleuren$cat
  #pal <- colorFactor(palette = col,  levels = lab)
  #pal_shp <- colorFactor(palette = col,  levels = lab)
  
  #set colors stat
  #map$param_stat <- as.factor(map$stat_zomer_nat_P)
  #map <- map[order(map$param_stat) & !is.na(map$param_stat) & !map$param_stat == "",]
  #map$param_stat <- fct_drop(map$param_stat)
  kleuren_stat <- data.frame(cat=c("geen","log", "normal"), kleur_stat=c("#ffffff","#b45f06","#0000FF"))
  
  map <- map%>%left_join(kleuren,c("oordeel_EKR_zomer_nat_P"="cat"))
  map <- map%>%left_join(kleuren_stat,c("stat_zomer_nat_P"="cat"))
  opac <- ifelse(is.na(map$oordeel_EKR_zomer_nat_P),0,1)
  #maak kaart
  m <- leaflet() %>%
    #flyToBounds(lat1 = 52.089900,lat2=52.443641,lng1 = 4.706125,lng2=5.284069)%>%
    fitBounds(lat1 = 52.13,lat2=52.43,lng1 = 4.75,lng2=5.2)%>%
    addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM.x, "<br>",
                                                                 "EAGIDENT:", map$GAFIDENT, "<br>",
                                                                 "Verandering:", map$oordeel_EKR_zomer_nat_P),
                stroke = T, color=map$kleur_stat, opacity=opac, weight = 0.8, smoothFactor = 0.8,
                fill=T, fillColor = map$kleur, fillOpacity = opac, ) %>%
    addLegend("bottomright", colors=kleuren$kleur, labels=kleuren$cat, opacity=1)%>%
    addProviderTiles("Esri.WorldGrayCanvas")#addTiles()
  m
  ## save html to png
  saveWidget(m, "output/html/map_oordeel.html", selfcontained = FALSE)
  mapshot(m, file = "output/html/map_oordeel.png")
}
get_map_verschil <- function(eag_wl=eag_wl,db_EKR=db_EKR,gEAG=gEAG,gEAG_new=gEAG_new,gEAG_sf=gEAG_sf,maptype="vastgesteld"){
  
  if(maptype=="vastgesteld"){
    map <- sp::merge(gEAG, eag_wl, by.x = 'GAFIDENT', by.y =
                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)  
    map <- sp::merge(map, db_EKR, by.x = 'GAFIDENT', by.y =
                       'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
    map_shp <- sp::merge(gEAG_sf, eag_wl, by.x = 'GAFIDENT', by.y =
                           'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    map_shp <- sp::merge(map_shp, db_EKR, by.x = 'GAFIDENT', by.y =
                           'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
  }
  if(maptype=="nieuw"){
    map <- sp::merge(gEAG_new, eag_wl, by.x = 'GAFIDENT', by.y =
                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    map <- sp::merge(map, db_EKR, by.x = 'GAFIDENT', by.y =
                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    map_shp <- sp::merge(gEAG_sf, eag_wl, by.x = 'GAFIDENT', by.y =
                           'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    map_shp <- sp::merge(map_shp, db_EKR, by.x = 'GAFIDENT', by.y =
                           'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
  }
  
  map$param <- as.factor(map$cat_SGBP2_SGBP1)
  #map$param <- as.factor(map$Deelgebied)
  map <- map[order(map$param) & !is.na(map$param) & !map$param == "",]
  map$param <- fct_drop(map$param)
  
  map_shp$param <- as.factor(map_shp$cat_SGBP3_SGBP1)
  map_shp <- map_shp[order(map_shp$param) & !is.na(map_shp$param) & !map_shp$param == "",]
  map_shp$param <- fct_drop(map_shp$param)
  map_shp <- map_shp[!map_shp$cat_SGBP3_SGBP1 %in% "onbekend",]
  #set colors
  kleuren <- data.frame(cat=c("onbekend","sterke achteruitgang", "lichte achteruitgang","gelijk", "lichte vooruitgang", "sterke vooruitgang"), kleur=c("#ffffab","#ff6666","#ffd8d8","#fcfc81","#e5ffd8","#99ff66"))
  #kleuren <- data.frame(cat=unique(map$Deelgebied),kleur=rainbow(n=length(unique(map$Deelgebied))))
  col <- kleuren$kleur
  lab <- kleuren$cat
  #map_test <- map%>%left_join(kleuren,c("Deelgebied"="cat"))
  map <- map%>%left_join(kleuren,c("cat_SGBP2_SGBP1" = "cat"))
  map_shp <- map_shp%>%left_join(kleuren,c("cat_SGBP3_SGBP1" = "cat"))
  pal <- colorFactor(palette = col,  levels = lab)
  pal_shp <- colorFactor(palette = col,  levels = lab)
  
  #maak kaart
 m  <-  leaflet() %>%
   fitBounds(lat1 = 52.13,lat2=52.43,lng1 = 4.75,lng2=5.2)%>%
   addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM.x, "<br>",
                                                                 "EAGIDENT:", map$GAFIDENT, "<br>",
                                                                 "Verandering:", map$cat_SGBP2_SGBP1),
                stroke = T, color= 'grey', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(param), fillOpacity = 0.6) %>%
    addCircleMarkers(data = map_shp,layerId=map_shp$GAFIDENT, label= paste("Verandering 2020:", map_shp$cat_SGBP3_SGBP1),
                     stroke = T, color= 'grey', opacity=1,
                     fill=T, fillColor = ~pal_shp(param), fillOpacity = 1)%>%
    addLegend("bottomright", colors=col, labels=lab)%>%
    addProviderTiles("Esri.WorldGrayCanvas")#addTiles()
 ## save html to png
 saveWidget(m, "output/html/map_EKR_trend.html", selfcontained = FALSE)
 mapshot(m, file = "output/html/map_EKR_trend.png")
  
}
get_map_EAGtype <- function(eag_wl=eag_wl,db_oordeel=oordeel_EKR_nut,gEAG=gEAG,gEAG_new=gEAG_new,gEAG_sf=gEAG_sf,maptype="vastgesteld"){
  if(maptype=="vastgesteld"){
    map <- sp::merge(gEAG, eag_wl, by.x = 'GAFIDENT', by.y =
                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)  
  }
  if(maptype=="nieuw"){
    map <- sp::merge(gEAG_new, eag_wl, by.x = 'GAFIDENT', by.y =
                       'GAFIDENT', all.x = TRUE, duplicateGeoms = T)
    }
  
  #map$param <- as.factor(map$oordeel_EKR_zomer_nat_P)
  #map <- map[order(map$param) & !is.na(map$param) & !map$param == "",]
  #map$param <- fct_drop(map$param)
  
  #set colors
  kleuren <- data.frame(cat=c("onbekend","beide onwenselijk", "beide stabiel", "beide wenselijk", "ongelijke trend"), kleur=c("#f3c99d","#fc2f2f","#e5e500","#0ed90e","#bcbcbc"))
  #col <- kleuren$kleur
  #lab <- kleuren$cat
  #pal <- colorFactor(palette = col,  levels = lab)
  #pal_shp <- colorFactor(palette = col,  levels = lab)
  
  #set colors stat
  #map$param_stat <- as.factor(map$stat_zomer_nat_P)
  #map <- map[order(map$param_stat) & !is.na(map$param_stat) & !map$param_stat == "",]
  #map$param_stat <- fct_drop(map$param_stat)
  kleuren_stat <- data.frame(cat=c("geen","log", "normal"), kleur_stat=c("#ffffff","#b45f06","#0000FF"))
  
  map <- map%>%left_join(kleuren,c("oordeel_EKR_zomer_nat_P"="cat"))
  map <- map%>%left_join(kleuren_stat,c("stat_zomer_nat_P"="cat"))
  #maak kaart
  m <- leaflet() %>%
    #flyToBounds(lat1 = 52.089900,lat2=52.443641,lng1 = 4.706125,lng2=5.284069)%>%
    fitBounds(lat1 = 52.13,lat2=52.43,lng1 = 4.75,lng2=5.2)%>%
    addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM.x, "<br>",
                                                                 "EAGIDENT:", map$GAFIDENT, "<br>",
                                                                 "Verandering:", map$oordeel_EKR_zomer_nat_P),
                stroke = T, color=map$kleur_stat, opacity=0.6, weight = 0.8, smoothFactor = 0.8,
                fill=T, fillColor = map$kleur, fillOpacity = 0.9, ) %>%
    addLegend("bottomright", colors=col, labels=lab, opacity=1)%>%
    addProviderTiles("Esri.WorldGrayCanvas")#addTiles()
  m
  ## save html to png
  saveWidget(m, "output/html/map_oordeel.html", selfcontained = FALSE)
  mapshot(m, file = "output/html/map_oordeel.png")
}

###OUD####
do_analyse_nuts_KNMI <- function(x=doSplit(db_nut_KNMI_means,"locatie.EAG")[[1]],iterator="locatie.EAG",data_min=10){
  if(nrow(x)>10){
    print(unique(x[,iterator]))
    winter <- c(12,1,2) #wintermaanden
    voorjaar <- c(3,4,5)
    zomer <- c(6,7,8)
    najaar <- c(9,10,11)
    
    ## 15 gebieden -> mail maarten
    ## naam EAGs in plaatje toevoegen
    
    zeer_droog_grens <- -2 #zeer droog: <-2mm/dag
    nat_grens <- 2         #droog: <2mm/dag
    zeer_nat_grens <- 5   #nat: < 5mm/dag, zeer nat: >=10mm/dag
    x <- x%>%mutate(seizoen=ifelse(maand %in% winter,"winter",
                                   ifelse(maand %in% voorjaar,"voorjaar",
                                          ifelse(maand %in% zomer,"zomer",
                                                 ifelse(maand %in% najaar,"najaar","")))))
    x <- x%>%mutate(neerslagperiode=ifelse(neerslagoverschot_mean <= zeer_droog_grens,"zeer droog",
                                           ifelse(neerslagoverschot_mean < nat_grens,"droog",
                                                  ifelse(neerslagoverschot_mean < zeer_nat_grens,"nat","zeer nat"))))
    x$cat <- paste0(x$seizoen,"_",x$neerslagperiode)
    x$cat <- factor(x$cat,levels=c("winter_zeer nat","winter_nat","winter_droog","winter_zeer droog",
                                   "voorjaar_zeer nat","voorjaar_nat","voorjaar_droog","voorjaar_zeer droog",
                                   "zomer_zeer nat","zomer_nat","zomer_droog","zomer_zeer droog",
                                   "najaar_zeer nat","najaar_nat","najaar_droog","najaar_zeer droog"))
    x <- x%>%mutate(jaar_alt=ifelse(maand==12,jaar+1,jaar)) #neem december mee als start jaar erna!
    
    lm_df_out <- NULL
    for(i in unique(x$cat)){
      for(j in unique(x$fewsparametercode)){
        x_sel <- x%>%filter(cat %in% i,fewsparametercode %in% j)
        if(nrow(x_sel)==0){next}
        r_sq <- round(summary(lm(meetwaarde~jaar_alt, data=x_sel))$r.squared,3)
        p <- round(data.frame(summary(lm(meetwaarde~jaar_alt, data=x_sel))$coefficients)[2,4],3)
        sl <- round(data.frame(summary(lm(meetwaarde~jaar_alt, data=x_sel))$coefficients)[2,1],3)
        lm_df_out <-rbind(lm_df_out,data.frame(cat=i,fewsparametercode=j,p=p,r_squared=r_sq,slope=sl))
      }
    }
    lm_df_out$p[!is.na(lm_df_out$p) &! NaN %in% lm_df_out$P & lm_df_out$p <=0.05] <- paste0(lm_df_out$p[!is.na(lm_df_out$p) &! NaN %in% lm_df_out$P & lm_df_out$p <=0.05],"*")
    lm_df_out$p_text <- paste0("p: ",lm_df_out$p)
    lm_df_out$x_pos <- min(x$jaar_alt)+1.5
    lm_df_out <- data.frame(fewsparametercode=c("Ptot","Ntot"),y_pos=c(max(x$meetwaarde[x$fewsparametercode %in% "Ptot"]),max(x$meetwaarde[x$fewsparametercode %in% "Ntot"])))%>%
      right_join(lm_df_out,"fewsparametercode")
    x_add <- x%>%left_join(lm_df_out,c("cat", "fewsparametercode"))
    g <- ggplot(x_add,aes(jaar_alt,meetwaarde, group=cat))+theme_bw()
    g <- g + geom_point()
    if(length(unique(x[,iterator]))>1){
      g <- g + ggtitle("alle EAG's samen")
    }else{ g <- g + ggtitle(unique(x[,iterator]))
    }
    g <- g + facet_grid(fewsparametercode~cat,scales="free_y")
    g <- g + geom_smooth(formula=y~x,method="lm")
    g <- g + scale_x_continuous(breaks=min(x$jaar):max(x$jaar))
    g <- g + geom_text(data=lm_df_out,aes(x=x_pos,y=y_pos,label=(paste0("p: ",p,"; r^2: ",r_squared)),color="blue",size=2),show.legend = F)
    if(length(unique(x[,iterator]))>1){
      ggsave(paste0("output/nutrienten/nutrienten_per_cat_",Sys.Date(),".png"),g,device="png",width=35,height=10)
    }else{
      ggsave(paste0("output/nutrienten/",iterator,"/nutrienten_per_cat_",unique(x[,iterator]),"_",Sys.Date(),".png"),g,device="png",width=15,height=10) 
    }
    x_out <- x_add%>%dplyr::select(-locatiecode,-meetwaarde)%>%distinct()
    return(x_out)
  }
}
do_trendanalyse <- function(x=doSplit(EAG_scores, "EAGIDENT")[[110]]){
  x_sel <- x%>%filter(GHPR %in% "Ov. waterflora")%>%select(EAGIDENT, jaar, Numeriekewaarde, GEP, GEP_2022,CODE)
  x_sel$jaar_iter <- x_sel$jaar - min(x_sel$jaar) + 1
  lm_x_sel <- lm(jaar_iter~Numeriekewaarde,x_sel)
  lm_xlog_sel <- lm(jaar_iter~log(Numeriekewaarde),x_sel)
  
  
  x_out <- data.frame(EAGIDENT=unique(x_sel$EAGIDENT), type=c("lm", "log_lm"), intercept= c(lm_x_sel$coefficients[1],lm_xlog_sel$coefficients[1]),slope=c(lm_x_sel$coefficients[2],lm_xlog_sel$coefficients[2]))
  return(x_out)
}
