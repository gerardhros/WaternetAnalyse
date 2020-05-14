# This script wraps functions to merge various point datasets of AGV
# project Waternet 1729.N19
# Yuki Fujita (NMI)
# may 2020

# clear environment
rm(list=ls())

# require packages
require(data.table);require(dplyr)

# source functions
source('scripts/funs_mergeDB.R')
source('scripts/fun_getwd.R') 

# get data directory name of the project 
iloc_project <- getwd()


# Load base (pre-processed) files ----
if (!"ppr_db.RData" %in%  list.files(iloc_project)){
  source("scripts/prepDB.R")
} else {
  #load pre-processed dataset locaties, bod, wq, hybi, dat, EKRset
  load(paste0(iloc_project, "ppr_db.RData"))
}

## Combine databases based on location ---

# remove the records whose CODE is "*"
locaties <- locaties[CODE != "*", ]

# column names to be included in the database
col_para <- c('locatiecode', 'monsterident', 'datum', 'fewsparameter', 'fewsparameternaam', 'meetwaarde', 'eenheid')


## Step 1: merge water quality data ('wq') ------

# replace -999 values to NA
wq <- wq[meetwaarde == -999, meetwaarde := NA]

db1 <- merge_wq(locaties, wq, col_para)

# check if there is no duplicate of data
dat1N <- db1[, .N, by = .(locatiecode, datum, fewsparameter)]
if(max(dat1N$N) > 1){
  print("There are duplicates of records for location x fewsparameter x datum.")
  print(table(dat1N$N))
  #show example
  #wq[locatiecode == dat1N$locatiecode[dat1N$N == max(dat1N$N)][1] & datum ==  dat1N$datum[dat1N$N == max(dat1N$N)][1] & fewsparameter == dat1N$fewsparameter[dat1N$N == max(dat1N$N)][1],]
}

## Step 2: merge hydrobiological data ('hybi') ------

db2 <- merge_hybi(locaties, hybi, col_para)

# # check if there is no duplicate of data
# dat2N <- db2[, .N, by = .(locatiecode, datum, fewsparameter)]
# table(dat2N$N)
# db2[locatiecode == dat2N$locatiecode[dat2N$N == max(dat2N$N)][1] & datum ==  dat2N$datum[dat2N$N == max(dat2N$N)][1] & fewsparameter == dat2N$fewsparameter[dat2N$N == max(dat2N$N)][1],]


## Step 3: merge EKR data ('EKRset') ------

# change column names (so that they match with other database)
setnames(EKRset, old = c("CODE", "GHPR_level", "GHPR", "Numeriekewaarde", "Eenheid.code", "Identificatie"), 
         new = c("locatiecode", "fewsparameter", "fewsparameternaam", "meetwaarde", "eenheid", "monsterident"))

db3 <- merge_ekr(locaties, EKRset, hybi, col_para)

# # check if there is no duplicate of data
# dat3N <- db3[, .N, by = .(locatiecode, datum, fewsparameter)]
#table(dat3N$N)
#db3[locatiecode == dat3N$locatiecode[dat3N$N == max(dat3N$N)][1] & datum ==  dat3N$datum[dat3N$N == max(dat3N$N)][1] & fewsparameter == dat3N$fewsparameter[dat3N$N == max(dat3N$N)][1],]


## Step 4: Combine databases ----------
db <- rbind(db1, db2, db3)

setnames(db, old = "NAAM", new = "locatie_naam")

save(db, file = paste0(iloc_project, "db.RData"))


# temp # visual check
library(ggplot2)
ggplot(db[fewsparameter == "WATDTE_m", ]) + 
  geom_point(aes(x = XCOORD, y =YCOORD, col = meetwaarde)) 
  

#temp # dcast

# db_dc <- dcast(db[, .(locatiecode, datum, fewsparameter, meetwaarde)], 
#                locatiecode + datum ~fewsparameter, value.var = "meetwaarde", fun.aggregate = last)
# db_dc_erk <- dcast(db[bron == "ekr", .(locatiecode, datum, fewsparameter, meetwaarde)], 
#                    locatiecode + datum ~fewsparameter, value.var = "meetwaarde", fun.aggregate = last)
# 



