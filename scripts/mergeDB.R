# This script merges various datasets of AGV

# clear environment
rm(list=ls())

# require packages
require(data.table);require(dplyr)

# source functions
source('scripts/ppr_funs.R')
source('scripts/calc_funs.R')
source('scripts/createOutput_gr.R')
source('scripts/funs_mergeDB.R')

# Load base files ----
if (!"ppr_db.RData" %in%  list.files("data")){
  
  # water types
  watertypen <- fread('data/KRWWatertype.csv')
  
  # locaties van alle metingen (water, biologie, en slootbodem)
  locaties <- fread('data/Location.csv')
  
  # locaties van EAG oppervlaktes
  eag_wl <- fread('data/EAG_Opp_kenmerken_20200218.csv')
  eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]
  
  # shape with EAG
  gEAG <- sf::st_read("data/EAG20191205.gpkg",quiet = T) %>% sf::st_transform(28992)
  
  # KRW doelen 
  doelen <- ppr_doelen()
  
  # nonogram
  nomogram <- fread('data/nomogram.csv')
  
  # waterbalans data (made by loadBalances)
  dat <- readRDS("pbelasting/dat.rds")  
  dat[,date := as.POSIXct(paste0(jaar,"-",maand,"-01"), format = '%Y-%m-%d')]
  
  # gegevens hydrobiologie
  hybi <- readRDS('data/alles_reliable.rds')
  hybi <- ppr_hybi(db = hybi, syear = 2000, wtype = eag_wl, mlocs = locaties)
  
  # EKR sets KRW en overig water
  EKRset1 <- readRDS('hydrobiologie/EKRsetKRW.rds') %>% as.data.table()
  EKRset2 <- readRDS('hydrobiologie/EKRsetOvWater.rds') %>% as.data.table()
  EKRset <- ppr_ekr(krwset = EKRset1, ovwatset = EKRset2,eag_wl = eag_wl, doelen = doelen)
  
  # noodgreep omdat er fouten zitten in de toetsgegevens
  EKRset$KRWwatertype.code[EKRset$Identificatie == 'VaartenRondeHoep'] <- 'M8'
  EKRset$KRWwatertype.code[EKRset$Identificatie == 'VaartenZevenhoven'] <- 'M1a'
  
  # select alleen nieuwe maatlatten
  EKRset <- EKRset[!Waardebepalingsmethode.code %in% c("Maatlatten2012 Ov. waterflora","Maatlatten2012 Vis"),]
  
  
  # slootbodem measurements
  bod  <- fread("data/bodemfews.csv")
  bod <- ppr_slootbodem(db = bod, wtype = eag_wl, mlocs = locaties)
  
  # waterquality measurements
  wq  <- readRDS("data/ImportWQ.rds") %>% as.data.table()
  wq <-  ppr_wq(db = wq, syear = 2000, wtype = eag_wl, mlocs = locaties)
  
  # datafile with P-load PC Ditch
  Overzicht_kP <- fread('data/Overzicht_kP.csv') 
  Overzicht_kP <- ppr_pcditch(db = Overzicht_kP)
  
  # toxiciteitsdata simoni
  simoni <- readRDS('data/simoni.rds')
  
} else {
  #load pre-processed dataset locaties, bod, wq, hybi, dat, EKRset
  load("data/ppr_db.RData")
}

## Combine databases based on location ---

# remove the records whose CODE is "*"
locaties <- locaties[CODE != "*", ]

# column names to be included in the database
col_para <- c('locatiecode', 'monsterident', 'datum', 'fewsparameter', 'fewsparameternaam', 'meetwaarde', 'eenheid')


## Step 1: merge water quality data ('wb') ------

# check if all XY-coordinate of wq exist in locaties
miss_loc_wq<- check_missing_xy(wq$locatiecode, locaties$CODE, "wq")

# check if there are completely identical rows. If so, remove them (after giving a warning)
wq <- remove_duplicate(wq)

db1 <- merge(wq[, ..col_para], 
            locaties[, .(CODE, NAAM, XCOORD, YCOORD, GAFIDENT, EAGIDENT, OWMIDENT)],
            by.x = "locatiecode", by.y = "CODE", all.x = TRUE)
db1[, bron := "wq"]

# check if there is no duplicate of data
dat1N <- db1[, .N, by = .(locatiecode, datum, fewsparameter)]
if(max(dat1N$N) > 1){
  print("There are duplicates of records for location x fewsparameter x datum.")
  print(table(dat1N$N))
  #show example
  #wq[locatiecode == dat1N$locatiecode[dat1N$N == max(dat1N$N)][1] & datum ==  dat1N$datum[dat1N$N == max(dat1N$N)][1] & fewsparameter == dat1N$fewsparameter[dat1N$N == max(dat1N$N)][1],]
}


## Step 2: merge hydrobiological data ('hybi') ------

# check if all XY-coordinate of hybi exist in locaties
miss_loc_hybi<- check_missing_xy(hybi$locatiecode, locaties$CODE, "hybi")

# check if there is overlap in parameters between hybi and wq
dup_para <- unique(hybi$fewsparameter)[!is.na(match(unique(hybi$fewsparameter), unique(wq$fewsparameter)))]
if(length(dup_para) > 0){
  print(paste0("WARNING: following parameters are included both in hybi and wq: ", paste(dup_para, collapse = ",")))
}

# check if there are completely identical rows. If so, remove them (after giving a warning)
hybi <- remove_duplicate(hybi)

db2 <- merge(hybi[, ..col_para], 
             locaties[, .(CODE, NAAM, XCOORD, YCOORD, GAFIDENT, EAGIDENT, OWMIDENT)],
             by.x = "locatiecode", by.y = "CODE", all.x = TRUE)
db2[, bron := "hybi"]

# # check if there is no duplicate of data
# dat2N <- db2[, .N, by = .(locatiecode, datum, fewsparameter)]
# table(dat2N$N)
# db2[locatiecode == dat2N$locatiecode[dat2N$N == max(dat2N$N)][1] & datum ==  dat2N$datum[dat2N$N == max(dat2N$N)][1] & fewsparameter == dat2N$fewsparameter[dat2N$N == max(dat2N$N)][1],]


## Step 3: merge EKR data ('EKRset') ------

# check if all XY-coordinate of EKR exist in locaties
miss_loc_ekr <- check_missing_xy(EKRset$CODE, locaties$CODE, "EKRset")


# check if there are completely identical rows. If so, remove them (after giving a warning)
EKRset <- remove_duplicate(EKRset)

# Get date from hybi (because the datum of EKRset is not correct)
EKRset[, datum_ekr := datum][, datum := NULL]
# dcast hybi
# TO DO: check if there are more than 1 date per year
hybi_dc <- dcast(hybi[, .(locatiecode, datum, jaar)], locatiecode ~ jaar, value.var = 'datum', fun.aggregate = last)
hybi_m <- melt(hybi_dc, id.vars = "locatiecode", variable.name = "jaar", value.name = "datum", na.rm = TRUE)
hybi_m$jaar <- as.integer(as.character(hybi_m$jaar))
# add correct datum (Here, records whose locations don't exist in hybi are excluded.)
EKRset2 <- merge(EKRset, hybi_m, 
                 by.x = c("CODE", "jaar"), by.y = c("locatiecode", "jaar"), all.x = FALSE)

setnames(EKRset2, old = c("CODE", "GHPR_level", "GHPR", "Numeriekewaarde", "Eenheid.code", "Identificatie"), 
         new = c("locatiecode", "fewsparameter", "fewsparameternaam", "meetwaarde", "eenheid", "monsterident"))


# merge location info
db3 <- merge(EKRset2[, ..col_para], 
             locaties[, .(CODE, NAAM, XCOORD, YCOORD, GAFIDENT, EAGIDENT, OWMIDENT)],
             by.x = "locatiecode", by.y = "CODE", all.x = TRUE)
db3[, bron := "ekr"]

# dat3N <- db3[, .N, by = .(locatiecode, datum, fewsparameter)]
#table(dat3N$N)
#db3[locatiecode == dat3N$locatiecode[dat3N$N == max(dat3N$N)][1] & datum ==  dat3N$datum[dat3N$N == max(dat3N$N)][1] & fewsparameter == dat3N$fewsparameter[dat3N$N == max(dat3N$N)][1],]



## Step 4: Combine databases
db <- rbind(db1, db2, db3)

setnames(db, old = "NAAM", new = "locatie_naam")


save(db, file = 'C:/Users/YukiFujita/SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI/NMI_Data - Documents/project/WaternetAnalyse/db_test.RData')


# temp # visual check
library(ggplot2)
ggplot(db[fewsparameter == "WATDTE_m", ]) + 
  geom_point(aes(x = XCOORD, y =YCOORD, col = meetwaarde)) 
  





