# This script wraps functions to get attributes of datapoints of AGV
# project Waternet 1729.N19
# Yuki Fujita (NMI)
# may 2020


# clear environment
rm(list=ls())

# require packages
require(data.table);require(dplyr)

# source functions
source('scripts/funs_get_attribute.R')
source('scripts/funs_mergeDB.R')
source('scripts/fun_getwd.R') 

# get data directory name of the project 
iloc_project <- getwd()

# load merged database
load(paste0(iloc_project, "db.RData"))

#load pre-processed dataset locaties, bod, wq, hybi, dat, EKRset
load(paste0(iloc_project, "ppr_db.RData"))

## Step 1: merge slootbodemanalyse data ('bod') ------

# Option 1-1: Get parameter values of all point locations based on area average
db_att_bod <- fun_areaave_bod(bod, locaties, col_para = names(db),
                       #loc_new = unique(locaties$CODE), # for all locations
                       loc_new = unique(db$locatiecode[db$bron == "ekr"]), # for locations for which ERK data exist
                       col_paraid = "parm.fews", 
                       ave_area = "EAGIDENT", ave_area2 = "GAFIDENT")


# Option 1-2: Get parameter values of all point locations based on regression model with parcel data



## Step 2: merge waterbalans data ('dat') ------

# Check if some EAG has multiple records for the same date  
nrecord_eag <- dat[!is.na(EAG), .N, by = .(EAG, date)]
nrecord_gaf <- dat[!is.na(GAF), .N, by = .(GAF, date)]
nrecord_wl <- dat[is.na(EAG) & is.na(GAF) & !is.na(KRW), .N, by = .(KRW, date)] 

# TO DO: Remove the duplicate records based on certain criteria
# remove scenario simulations
dat <- dat[!grepl("_S", pol),]

# remove data of aggregated EAG's (e.g. "2500-EAG-3-4-5_F001.xlsx")
# TO CHECK: It seems that some EAG has both aggregated and not-aggregated records
# see: dat[jaar == 1996 & maand == 1 & grepl("3201-EAG", pol), .(pol, date, maand, w_peil, wp_min_sum)]
# some EAG have aggregated records only
# see for example: dat[jaar == 1996 & maand == 1  & grepl("2550-EAG", pol), .(pol, date, w_peil, wp_min_sum, EAG)]
# So, remove aggregated records only when non-aggregated records are present
#dat2 <- dat[!grepl("EAG--", gsub("[[:digit:]]+", "", pol)),] # this exlude many EAG's :(

# select columns of data which contain parameter values
col_dat_para <- names(dat)[grepl("^w_|^wp_|^a_", names(dat))]
col_dat_para <- col_dat_para[!grepl("a_inlaat|a_uitlaat", col_dat_para)] # exclude nominal variables

# merge water and stoffen balans based on EAG, GAF, or WL
#db5 <- fun_areamerge_dat(dat, locaties, col_dat_para) # -> Error: cannot allocate vector of size 1.1 Gb
db_att_waterbalance <- fun_areamerge_dat(dat, locaties, col_dat_para, col_para = names(db), 
                         #loc_new = unique(locaties$CODE), # for all locations
                         loc_new = unique(db$locatiecode[db$bron == "ekr"]), # for locations for which ERK data exist
                         jaar_sel = 2005:2019)

# TO DO: change class of dat$date to "Date"

## Step 3: merge parcel info ------



## Step 4: combine database of attributes

#db_att <- rbind(db4, db5) # -> this gives error due to data type of dat$date

save(db_att_bod, file = paste0(iloc_project, "db_att_bod.RData"))
save(db_att_waterbalance, file = paste0(iloc_project, "db_att_waterbalance.RData"))