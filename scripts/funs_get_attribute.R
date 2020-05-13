## Step 4: merge slootbodemanalyse data ('bod') ------
# Because this data is measured at fewer and not-overlapping points, some kind of extrapolation is needed.
# Here are two options of extrapolation.

# check if all XY-coordinate of bod exist in locaties
check_missing_xy(bod$loc.code, locaties$CODE, "bod")

# Option 4-1: Get parameter values of all point locations based on area average
db4 <- fun_areaave_bod(bod, locaties, col_para,
                       col_paraid = "parm.fews", 
                       ave_area = "EAGIDENT", ave_area2 = "GAFIDENT")


# Option 4-2: Get parameter values of all point locations based on regression model with parcel data


## Step 5: merge waterbalans data ('dat') ------

# Check if some EAG has multiple records for the same date  
nrecord_eag <- dat[!is.na(EAG), .N, by = .(EAG, date)]
nrecord_gaf <- dat[!is.na(GAF), .N, by = .(GAF, date)]
nrecord_wl <- dat[is.na(EAG) & is.na(GAF) & !is.na(KRW), .N, by = .(KRW, date)] 

# TO DO: Remove the duplicate records based on certain criteria
# remove scenario simulations
dat <- dat[!grepl("_S", pol),]
# remove data of aggregated EAG's

# select columns of data which contain parameter values
col_dat_para <- names(dat)[grepl("^w_|^wp_|^a_", names(dat))]
col_dat_para <- col_dat_para[!grepl("a_inlaat|a_uitlaat", col_dat_para)] # exclude nominal variables

# merge water and stoffen balans based on EAG, GAF, or WL
db4 <- fun_areamerge_dat(dat, locaties, col_dat_para)





## Step 6: merge parcel info ------