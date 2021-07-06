require(reshape)
require(tidyverse)
require(stringi)
inlezen_csv <- function(x){
  read.csv(x, header=T, sep=";", dec=".", stringsAsFactors = F, quote="")
}
doSplit <- function(x,y=cols){
  if(length(y)==1){
    x_split <- split(x,list(x[,y]))
  }else{
    x_split <- split(x,as.list(x[,y]))
  }
  return(x_split)
}
find_monsters <- function(i, j=db){
  monsters <- unique(db[db$jaar %in% db_abio$jaar[i] & db$maand %in% db_abio$maand[i] & db$Meetobject.lokaalID %in% db_abio$Meetobject.lokaalID[i],c("Meetobject.lokaalID","Monster.lokaalID")])
  names(monsters)[2] <- "monster_bio"
  return(monsters)
}
filterDiepte <- function(x=db_abio){
  x_hold <- x[!x$Grootheid_Code %in% "DIEPTE",]
  x_proces <- x[x$Grootheid_Code %in% "DIEPTE",]
  x_proces_rebuild <- NULL
  for(i in unique(x_proces$Meetobject_Code)){
    x_sel <- x_proces[x_proces$Meetobject_Code %in% i,]
    #print(x_sel$Collectie_Datum)
    x_proces_sel <- if("X100" %in% x_sel$Notitie_Code){x_sel[x_sel$Notitie_Code %in% "X100",]
      }else{
          if("X150" %in% x_sel$Notitie_Code){x_sel[x_sel$Notitie_Code %in% "X150",]
            }else{
              if("" %in% x_sel$Notitie_Code & "" %in% x_sel$Parameter_Kenmerken){ x_sel[x_sel$Notitie_Code %in% "",]
              }else{warning(paste0("waterdiepte niet aanwezig in ", i))
                  next}
            }
      }
  
    x_proces_rebuild <- rbind(x_proces_rebuild, x_proces_sel)
  }
  x_out <- rbind(x_hold, x_proces_rebuild)
    #lapply(doSplit(x_proces,"Meetobject_Code"), function(x)ifelse("X100" %in% x$Notitie_Code,x[[x$Notitie_Code %in% "X100",]], ifelse("X150" %in% x$Notitie_Code,x[x$Notitie_Code %in% "X150",], ifelse("" %in% x$Notitie_Code,x[x$Notitie_Code %in% "",],warning("niet aanwezig")))))
  return(x_out[,colnames(x_out)[!colnames(x_out)%in%"Notitie_Code"]])
}
read_WATERNET <- function(x,y=koppel_WATERNET, z=db_col, col_names=cols_naamgeving, types=watertypes){
  db <- do.call(rbind.data.frame,lapply(paste0("data/Waternet/",dir("data/Waternet/")[!dir("data/Waternet/") %in% "watdtezichtoevbsig202001241039.csv"]),read.csv,sep=";"))
  db <- db[,z$Waternet[!z$Waternet %in% ""]]
  db$Resultaatdatum <- as.Date(db$Resultaatdatum, format="%Y-%m-%d")
  db$Eenheid.code <- as.character(db$Eenheid.code)
  db$Parameter.code <- as.character(db$Parameter.code)
  db$Biotaxon.naam <- as.character(db$Biotaxon.naam)
  compartiment <- as.character(db$compartiment)
  db$Meetobject.lokaalID <- substr(db$Meetobject.lokaalID,1,6)
  db$Parameter.code[db$Parameter.code %in% "sSUBMSPTDAGN"] <- "SUBMSPTN"
  db_monsters <- unique(db[,c("Meetobject.lokaalID", "Monster.lokaalID")])
  db_monsters$monster <- substr(db_monsters$Monster.lokaalID, 1,(stri_length(db_monsters$Monster.lokaalID)-19))
  
  
  db_abio <- inlezen_csv("data/Waternet/watdtezichtoevbsig202001241039.csv")
  db_abio <- db_abio[,z$Waternet_abio[!z$Waternet_abio %in% ""]]
  db_abio$datum <- as.Date(db_abio$datum, format="%Y-%m-%d")
  db_abio <- merge(db_abio, db_monsters, by.x="monsterident", by.y="monster", all.x=T)
  db_abio$monsterident <- db_abio$Monster.lokaalID
  db_abio$meetwaarde <- as.numeric(db_abio$meetwaarde)
  db_abio <- db_abio[,colnames(db_abio[c("datum", "Meetobject.lokaalID", "Monster.lokaalID", "meetwaarde", "eenheid", "parametercode", "biotaxonnaam", "compartiment")])]
  #LET OP! HANDMATIGE STAP, goed controleren!
  colnames(db_abio) <- colnames(db)
  db <- rbind(db, db_abio)
  
  db <- merge(db, y[,c("CODE", "WATERTYPE", "XCOORD", "YCOORD","EAGIDENT")],by.x="Meetobject.lokaalID", by.y="CODE", all.x=T)
  colnames(db) <- col_names$AquoKit[match(names(db),col_names$Waternet)]
  
  db <- db[db$KRWwatertype.code %in% types,]
  db$jaar <- format(as.Date(db$Resultaatdatum, format="%Y-%m-%d"),"%Y")
  return(db)
  }
read_HDSR <- function(x,y=koppel_HDSR, z=db_col, col_names=cols_naamgeving, types=watertypes){
  db <- inlezen_csv("data/HDSR/Meetdata_alle_2006-2018_M1a_M3_M8_M10.csv")  
  db <- db[,colnames(db) %in% z$HDSR,]
  db$Resultaatdatum <- as.Date(db$Resultaatdatum, format="%Y-%m-%d")
  db$Parameter.code[db$Parameter.code %in% "sSUBMSPTDAGN"] <- "SUBMSPTN"
  db_abio <- inlezen_csv("data/HDSR/Waterdiepte-zicht-oeverbeschoeiing_alle_mafy_2006-2018_M1a_M3_M8_M10.csv")
  db_abio$Parameter.code <- db_abio$Grootheid.code #neem naamgeving parameters over uit grootheid.code (net als voor Rijnland)
  db_abio$Resultaatdatum <- as.Date(db_abio$Resultaatdatum, format="%d-%m-%Y")
  
  db_abio <- db_abio[db_abio$Meetobject.lokaalID %in% unique(db$Meetobject.lokaalID),colnames(db)]
  db_abio$Biotaxon.naam[is.na(db_abio$Biotaxon.naam)] <- ""
  #db_abio$Meetobject.lokaalID
  
  db <- rbind(db, db_abio)
  
  db <- merge(db, y[,c("Afvoergebied",	"KRWwatertype.code",	"Identificatie")],by.x="Meetobject.lokaalID", by.y="Identificatie", all.x=T)
  colnames(db) <- col_names$AquoKit[match(names(db),col_names$HDSR)]
  
  db <- db[db$KRWwatertype.code %in% types,]
  db$jaar <- format(as.Date(db$Resultaatdatum, format="%Y-%m-%d"),"%Y")
  return(db)
  }
read_RIJNLAND <- function(x,z=db_col, y=koppel_RIJNLAND, col_names=cols_naamgeving,types=watertypes, plantcheck=biotaxon_info){
  db <- inlezen_csv("data/Rijnland/OW_bio_IM.csv")
  db_abio <- inlezen_csv("data/Rijnland/OW_bio.csv") 
  db_meta <- inlezen_csv("data/Rijnland/gegevens_OW_meetpunten.csv")
  
  
  db <- merge(db, db_meta[,c("Meetpunt", "watertype")], by.x="Meetobject.lokaalID", by.y="Meetpunt", all.x=T)
  db <- db[,z$Rijnland[!z$Rijnland %in% ""]]
  db <- merge(db, y[,c("Meetpunt.Code", "Polder.Naam")],by.x="Meetobject.lokaalID", by.y="Meetpunt.Code", all.x=T)
  colnames(db) <- col_names$AquoKit[match(names(db),col_names$Rijnland)]
  
  db <- db[db$KRWwatertype.code %in% types,]
  db$jaar <- format(as.Date(db$Resultaatdatum, format="%d-%m-%Y"),"%Y")
  db$maand <- as.numeric(format(as.Date(db$Resultaatdatum, format="%d-%m-%Y"),"%m"))
  db <- db[db$Monster.lokaalID %in% unique(db$Monster.lokaalID[db$Biotaxon.naam %in% plantcheck$naam[plantcheck$taxontype=="MACFT"]]),]
  #haal info op over doorzicht en diepte in abiotische data van zelfde meetpunt en zelfde maand
  db_abio <- db_abio[db_abio$Grootheid_Code %in% c("OEVRST","DIEPTE", "ZICHT"), c("Meetobject_Code", "Collectie_Datum", "Collectie_Nummer", "Grootheid_Code", "Waarde_Berekend", "Notitie_Code")]
  db_abio$Collectie_Datum <- as.Date(db_abio$Collectie_Datum, format="%d-%m-%Y")
  db_abio <- filterDiepte(db_abio)
  
  names(db_abio) <- c("Meetobject.lokaalID", "Resultaatdatum", "Monster.lokaalID", "Grootheid.code", "Numeriekewaarde")
  db_abio$jaar <- format(as.Date(db_abio$Resultaatdatum, format="%d-%m-%Y"),"%Y")
  db_abio$maand <- as.numeric(format(as.Date(db_abio$Resultaatdatum, format="%d-%m-%Y"),"%m"))
  monster_dummys <- unique(do.call(rbind.data.frame,lapply(1:nrow(db_abio), find_monsters, db)))
  db_abio <- merge(db_abio, monster_dummys, by="Meetobject.lokaalID", all.x=T)  
  names(db_abio)[names(db_abio) %in% "Monster.lokaalID"] <- "Eenheid.code" #hernoem en maak leeg
  names(db_abio)[names(db_abio) %in% "monster_bio"] <- "Monster.lokaalID"
  names(db_abio)[names(db_abio) %in% "Grootheid.code"] <- "Parameter.code"
  db_abio$Eenheid.code <- "m"
  db_abio <- merge(db_abio, unique(db[,c("Monster.lokaalID", colnames(db)[!colnames(db) %in% c("Biotaxon.naam", "Eenheid.code", "compartiment", colnames(db_abio))])]), by="Monster.lokaalID", all.x=T)
  db_abio$compartiment <- ""
  db_abio$Biotaxon.naam <- ""
  db <- rbind(db, db_abio[,names(db)])
  
  
  db$Numeriekewaarde[db$Parameter.code %in% "OEVRST" & db$Numeriekewaarde %in% "1"] <- 14 #verander code voor natuurlijk naar 14 (code van Waternet)
  db$Parameter.code[db$Parameter.code %in% "OEVRST"] <- "OEVBSIG" #neem naamgeving over van Waternet
  db$Parameter.code[db$Parameter.code %in% "DIEPTE"] <- "WATDTE" #neem naamgeving over van Waternet
  
  db$Numeriekewaarde <- as.numeric(db$Numeriekewaarde)
  return(db)
}

koppel_biotaxon <- function(x=hybi,y=biotaxon_info[biotaxon_info$taxontype %in% "MACFT",]){
  db <- merge(x, y[,c("code", "oever", "submers", "kroos")], by.x="Biotaxon.naam", by.y="code",all.x=T)
  return(db)
}

#do_combine_results <- function(db_W=EST_WATERNET, db_H=EST_HDSR, db_R=EST_RIJNLAND){
#  db_W$Waterbeheerder <- "Waternet"
#  db_W <- merge(db_W, unique(koppel_WATERNET$EAGIDENT, ))
#}