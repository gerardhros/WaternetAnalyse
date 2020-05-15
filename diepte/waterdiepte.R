rm(list = ls())

# source functions
source('scripts/ppr_funs.R')


# locaties van alle metingen (water, biologie, en slootbodem)
locaties <- fread('data/Location.csv')

# locaties van EAG oppervlaktes
eag_wl <- fread('data/EAG_Opp_kenmerken_20200218.csv')
eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]

# gegevens hydrobiologie (only data after 2006)
hybi <- readRDS('data/alles_reliable.rds')
hybi <- ppr_hybi(db = hybi, syear = 2006, wtype = eag_wl, mlocs = locaties)

