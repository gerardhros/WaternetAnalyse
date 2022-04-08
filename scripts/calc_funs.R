# calc functions to process data
# Laura Moria & Gerard H. Ros, december-19

# ppr hydrobiology -----------

calc_mean_EKR <- function (db, nyears = 3, smonth = 1:12, pEAG = TRUE, pYEAR = FALSE, pSEASON = FALSE){
  
  # input description
  # db: the database with EKR scores, a data.table
  # nyears: a numeric value x to select only the scores of the most recent x years per EAG
  # smonth: a vector used to select the relevant months (options 1:12)
  # pEAG, pYEAR, pSEASON (boolean): grouping needed for EAG, YEAR or SEASON
  
  # make local copy
  db <- copy(db)
  
  # simplify data.table to relevant columns only and rename those columns
  cols <- c('Identificatie','XCOORD','YCOORD','datum','HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code','KRW_SGBP3',
            'Waardebepalingsmethode.code','GHPR_level','GHPR','level','jaar','Numeriekewaarde','GEP','GEP_2022')
  d0 <- db[,mget(cols)]
  setnames(d0,cols,c('mpid','x','y','datum','id','EAGIDENT','watertype','KRW_SGBP3','wbmethode','GHPR_level','GHPR','level','jaar',
                     'meetwaarde','GEP','GEP_2022'))
  
  # add season and filter on selected months
  d0[,month := month(datum)]
  d0[,season := fifelse(month %in% 4:10,'summer',fifelse(month %in% c(1,2,3,11,12),'winter',''))]
  d0 <- d0[month %in% smonth]
  
  # remove empty spaces in GHPR (needed for joining later)
  d0[,GHPR := gsub(' ','',GHPR)]
  
  # add dynamic grouping variable depening on function input
  groups <- c('EAGIDENT','id','watertype','KRW_SGBP3','GHPR_level','GHPR','level','wbmethode')
  if(pSEASON){groups <- c(groups,'season')}
  
  # add year number and take only nyears most recent years (selection per EAG)
  d0 <- d0[,yearid := frank(-jaar,ties.method = 'dense'),by = groups][yearid <= nyears]
  
  # calculate mean EKR value per group per jaar
  if(pEAG){d1 <- d0[,.(EKR = mean(meetwaarde,na.rm=TRUE),
                       EKRmedian = median(meetwaarde,na.rm=TRUE),
                       GEP = mean(GEP,na.rm=TRUE),
                       GEP_2022 = mean(GEP_2022,na.rm=TRUE)),by = c(groups,'jaar')]}
  
  # calculate meerjarig gemiddelde EKR for each EAG
  if(!pYEAR & pEAG){d1 <- d1[,.(EKR = mean(EKR,na.rm=TRUE),
                                EKRmedian = median(meetwaarde,na.rm=TRUE),
                                GEP = mean(GEP,na.rm=TRUE),
                                GEP_2022 = mean(GEP_2022,na.rm=TRUE)),by = groups]}
  
  # calculate mean EKR for each measurement point and year or the meerjarig gemiddelde per mp
  if(!pEAG){d1 <- d0[,.(EKR = mean(meetwaarde,na.rm=TRUE),
                        EKRmedian = median(meetwaarde,na.rm=TRUE),
                        GEP = mean(GEP,na.rm=TRUE),
                        GEP_2022 = mean(GEP_2022,na.rm=TRUE)),by = c(groups,'mpid','jaar')]}
  if(!pYEAR & !pEAG){d1 <- d1[,.(EKR = mean(EKR,na.rm=TRUE),
                                 EKRmedian = median(meetwaarde,na.rm=TRUE),
                                 GEP = mean(GEP,na.rm=TRUE),
                                 GEP_2022 = mean(GEP_2022,na.rm=TRUE)),by = c(groups,'mpid')]}
  
  # rename GHPR in more readible (and less long names)
  d1[,GPHRnew := renameGHPR(GHPR)]
  d1[,wbmethode := renameWbmethode(wbmethode)]
  
  # return the object
  return(d1)
}

# calculate oordelen (doel compared with state)
eval_EKR <- function(id,GHPR,EKR,doelen) {
  
  # make local copy
  doelen <- copy(doelen)
  
  # make data.table from inputs
  db <- data.table(id = id, EKR = EKR, GHPR = GHPR)
  
  # add row number id
  db[,rid := .I]
  
   # rename columns doelen object
  setnames(doelen,c('HoortBijGeoobject.identificatie'),c('id'))
  
  # mean GEP per object
  doelgeb <- doelen[,.(GEP = mean(Doel,na.rm=TRUE)),by =.(id,bronddoel,GHPR)]
  
  # merge db with doelen
  db <- merge(db, doelgeb, by = c('id','GHPR'), all.x = TRUE)
  
  # add classification for EKR
  db[EKR < GEP/3,oordeel := 'slecht']
  db[EKR >= GEP/3 & EKR < 2 * GEP / 3,oordeel := 'ontoereikend']
  db[EKR >= 2 * GEP / 3,oordeel := 'matig']
  db[EKR >= GEP, oordeel := 'goed']
  
  # reorder to original input
  setorder(db,rid)
  
  # return object
  out <- db[,.(GEP,oordeel)]
  
  return(out)
}

# estimate waterbalans fluxes per EAG
calc_mean_wb <- function(db){
  
  
  
}

# compartiment slecteren en soms EZ en soms OW: nog niet gedaan
calcMeanHybi <- function(hybi = hybi, nyears = 3, smonth = 1:12, pEAG = TRUE, pYEAR = TRUE, pSEASON = FALSE){
  
  # make local copy
  b = copy(hybi) 
  
  # adjust fews parameter names
  b[,fewsparameter := gsub("/","_",fewsparameter)]
  
  # dcast table
  b <- dcast(b, locatiecode+locatie.EAG+locatie.KRW.watertype+compartiment+jaar~fewsparameter+parametercode+parameterfractie, 
             value.var = "meetwaarde", fun.aggregate = mean)
  
  # calculate and classify zichtdiepte
  b[,DTEZICHT := ZICHT_m_ZICHT_/WATDTE_m_WATDTE_]
  b[DTEZICHT > 1, DTZICHT := NaN]
  b[,DTEZICHTfac := cut(DTEZICHT, breaks = c('0.1','0.2','0.3','0.4','0.6', '0.8','1.0'))]
  
  # calculate nspecies
  
  
  # filter and sort database
  b <- b[!is.na(DTEZICHTfac) & !is.na(jaar) &!is.na(locatie.EAG),]
  setorder(b,jaar)
  
  # rename relevant columns oever
  cols <- c('PTN_BEDKG_%_SUBMSPTN_','PTN_BEDKG_%_FLAB_SUBMS','PTN_BEDKG_%_FLAB_DRIJVD','PTN_BEDKG_%_EMSPTN_','PTN_BEDKG_%_KROOS_',
            'TALBVWTR_graad_TALBVWTR_','ZICHT_m_ZICHT_','WATDTE_m_WATDTE_',"WATERBTE_m_WATERBTE_","SLIBDTE_m_SLIBDTE_",'DTEZICHT','OEVBSIG_SOORT_OEVBSIG_')
  colsn <- c('bedsubmers','draadwieren','FLAB','bedemers','kroos','taludhoek','doorzicht',
             'waterdiepte','waterbreedte','slibdikte','dieptedoorzicht','oeverbeschoeiing')
  setnames(b,cols,colsn)
  
  # select those columns
  b <- b[,mget(c('locatiecode','compartiment','locatie.EAG','locatie.KRW.watertype','jaar',colsn))]
  
  # add dynamic grouping variable depening on function input
  groups <- c('compartiment','locatie.EAG','locatie.KRW.watertype')
  if(pSEASON){groups <- c(groups,'season')}
  
  # add year number and take only nyears most recent years (selection per EAG)
  b <- b[,yearid := frank(-jaar,ties.method = 'dense'), by = groups][yearid <= nyears]
 
  # calculate median value per loc, EAG, watertype and year
  cols <- colnames(b)[sapply(b, is.numeric)]
  # calculate median value per location and year
  if(!pEAG){
    b <- b[,lapply(.SD, mean),.SDcols = cols[!cols =='jaar'],by= c(groups,'locatiecode','jaar')]}
  # calculate median value per location nyear average
  if(!pYEAR & !pEAG){ b <- b[,lapply(.SD, mean),.SDcols = cols,by= c(groups,'locatiecode')]}
  # calculate meerjarig gemiddelde EKR for each EAG en jaar
  if(pEAG){b <- b[,lapply(.SD, mean),.SDcols = cols[!cols =='jaar'],by= c(groups,'jaar')]}
  
  # return database
  return(b)
  
}

#
calcNtax <- function(hybi = hybi, nyears = 3, smonth = 1:12, pEAG = TRUE, pYEAR = TRUE, pSEASON = FALSE){
  
  # make local copy
  b = copy(hybi) 
  
  # adjust fews parameter names
  b[,fewsparameter := gsub("/","_",fewsparameter)] 
  b <- b[b$fewsparameter %in% c('PTN_BEDKG_%','PTN_BEDKG_BraunBS','PTN_BEDKG_NatS',
                                      'PTN_BEDKG_TansleyS'),]
  
  # add dynamic grouping variable depening on function input
  groups <- c('compartiment','locatie.EAG','locatie.KRW.watertype')
  setkeyv(b, groups)
  
  # add year number and take only nyears most recent years (selection per EAG)
  b <- b[,yearid := frank(-jaar,ties.method = 'dense'),by = groups][yearid <= nyears]

  # add catagory to new taxa which are not categorized
  1 -> b$WNA.emerse.plantensoorten[b$biotaxonnaam == 'Bolboschoenus laticarpus']
  1 -> b$WNA.oeverplantensoorten[b$biotaxonnaam == 'Bolboschoenus laticarpus']
  
  # aantal soorten per locatie
  ntaxa <- dcast(b,locatiecode+locatie.EAG+jaar+compartiment ~ ., sum, 
                     value.var= c("WNA.onderwaterplantensoorten",'WNA.emerse.plantensoorten','WNA.oeverplantensoorten'), na.rm =T)
  # alfa, gem aantal soorten per locatie op niveau EAG
  alfdiv <- dcast(ntaxa,locatie.EAG+jaar+compartiment ~ ., mean, 
                  value.var= c("WNA.onderwaterplantensoorten",'WNA.emerse.plantensoorten','WNA.oeverplantensoorten')) 
  # gamma, tot aantal soorten per EAG
  gamdiv <- dcast(b,locatie.EAG+jaar+compartiment ~ ., sum, 
                  value.var= c("WNA.onderwaterplantensoorten",'WNA.emerse.plantensoorten','WNA.oeverplantensoorten'), na.rm =T)
  # beta
  div <-  merge(alfdiv,gamdiv,by = c('locatie.EAG','jaar','compartiment'), all.x = FALSE, all.y = TRUE, suffixes=c(".alf", ".gam"))
  div$WNA.onderwaterplantensoorten.beta <- div$WNA.onderwaterplantensoorten.gam/div$WNA.onderwaterplantensoorten.alf
  div$WNA.emerse.plantensoorten.beta <- div$WNA.emerse.plantensoorten.gam/div$WNA.emerse.plantensoorten.alf
  div$WNA.oeverplantensoorten.beta <- div$WNA.oeverplantensoorten.gam/ div$WNA.oeverplantensoorten.alf
  
  # NAN vervangen door 0
  
  if(!pEAG){return(ntaxa)}
  
  # calculate median value per location nyear average
  if(!pYEAR & !pEAG){
    # calculate median value per loc, EAG, watertype and year
    cols <- colnames(div)[sapply(div, is.numeric)]
    # calculate median value per location and year
    div <- div[,lapply(.SD, mean),.SDcols = cols[!cols =='jaar'],by= c(groups,'jaar')]
    return(div)
  }
  
  # calculate meerjarig gemiddelde EKR for each EAG en jaar, klopt niet moet eerst per locatie
  if(pEAG){return(div)}
  
  
}

# rename categories in more easier names
renameGHPR <- function(inp){
  
  # make local copy in a data.table format
  dt <- data.table(GHPR = inp)
  
  # set to lower case
  dt[,GHPRnew := tolower(GHPR)]
  
  # replace part of the strings
  dt[,GHPRnew := gsub('massafractie','mf_',GHPRnew)]
  dt[,GHPRnew := gsub('soortensamenstelling','ss_',GHPRnew)]
  dt[,GHPRnew := gsub('bedekking','be_',GHPRnew)]
  dt[,GHPRnew := gsub('abundantie','abun_',GHPRnew)]
  dt[,GHPRnew := gsub('afwijking','afw_',GHPRnew)]
  
  # replace and move kwaliteit to front
  dt[grepl('-kwaliteit$',GHPRnew), GHPRnew := paste0('kw_',GHPRnew)]
  dt[,GHPRnew := gsub('-kwaliteit','',GHPRnew)]
  
  # remove info between brackets
  dt[,GHPRnew := gsub('\\(floatingalgaebeds)','',GHPRnew)]
  dt[,GHPRnew := gsub('\\(bb)|\\(o2)|\\(bk)|\\(pm)','',GHPRnew)]
  
  # replace '-' into '_'
  dt[,GHPRnew := gsub('-','_',GHPRnew)]
  
  # return dt
  return(dt[,GHPRnew])
}

# rename Waardebepalingsmethode.code
renameWbmethode <- function(inp){
  
  # make local copy in a data.table format
  dt <- data.table(wbm = inp)
  
  # set to lower case
  dt[,wbmnew := tolower(wbm)]
  
  # replace part of the string
  dt[,wbmnew := gsub('maatlatten','ml_',wbmnew)]
  
  # replace spaces
  dt[,wbmnew := gsub(' ','',wbmnew)]
  
  # replace part of the string
  dt[,wbmnew := gsub('vis','_vis',wbmnew)]
  dt[,wbmnew := gsub('fytoplankton','_fytoplankton',wbmnew)]
  dt[,wbmnew := gsub('ov.waterflora','_ov.wflora',wbmnew)]
  dt[,wbmnew := gsub('macrofauna','_macrofauna',wbmnew)]
  
  # return
  return(dt[,wbmnew])
}

# merge balances with critical P loads ------------------
makePmaps <- function(dbwbal,dbhybi,dbnomogram,dbov_kP,dbeag_wl){
  
  # convert to data.table, and make local copies
  dbwbal <- as.data.table(dbwbal)
  dbhybi <- copy(dbhybi)
  dbnomogram <- copy(dbnomogram)
  kP_plas <- copy(dbov_kP)
  dbeag_wl <- copy(dbeag_wl)
  
  # reset names of the databases to simplify references
  setnames(dbnomogram,"debiet (mm/dag)","debiet",skip_absent=TRUE)
  setnames(dbhybi,c('locatie.EAG','locatie.afaanvoergebied','locatie.KRWmeetpuntlocatie'),c('EAG','GAF','KRW'),skip_absent=TRUE)
  
  # adapt datasets
  dbeag_wl[,GAF:= substr(GAFIDENT, 1, 4)]
  dbwbal[GAF == '8070',watertype := 'M3']
  dbwbal[,bodem := i_bt1]
  dbwbal[is.na(i_bt1) & watertype %in% c('M8','M10','M27','M25'),bodem := 'VEEN']
  dbwbal[is.na(i_bt1) & watertype %in% c("M3", "M1a","M30","M14","M11","M6a","M7b","M6b","M7a"),bodem := 'KLEI']
  
  # select relevant water balance data
  
  # select only relevant years
  dg <- dbwbal[jaar %in% 2010:2018]
  
  # get mean per EAG-GAF-bodem-polder (gr: is this combined grouping really needed?)
  colsg <- colnames(dg)[grepl('^a_in|^a_uit|^EAG|^GAF|^KRW$|watertype|^bodem$|^pol',colnames(dg))]
  colss <- colnames(dg)[grepl('^a_|^wp_|jaar|maand|^w_|^p_i',colnames(dg))]
  colss <- colss[!colss %in% colsg]
  dg <- dg[,lapply(.SD,mean,na.rm=TRUE),.SDcols=colss,by=colsg]
  
  # add total P-load
  dg[,wp_tot_sum := wp_min_sum + wp_inc_sum]
  
  # select relevant hydrobiological data 
  
  # filter hydrobiologische data.base before calculating water depth
  dbhybi <- dbhybi[jaar %in% 2010:2017 & fewsparameter =='WATDTE_m']
  
  # mean water depth per EAG
  mdPtb <- copy(dbhybi)[,.(watdte = median(meetwaarde,na.rm=TRUE)),by='EAG']
  mdPtb[,watdteF := cut(watdte, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # mean water depth per KRW waterlichaam, die bestaat uit 1 of meerdere EAGs
  mdPtbK <- copy(dbhybi)[,.(watdte = median(meetwaarde,na.rm=TRUE)),by='KRW']
  mdPtbK[,watdteF := cut(watdte, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # mean water depth per GAF
  mdPtbG <- copy(dbhybi)[,.(watdte = median(meetwaarde,na.rm=TRUE)),by='GAF']
  mdPtbG[,watdteF := cut(watdte, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # merge water balance data with water depth 
  
  # koppel waterdiepte per eag en afvoergebied aan water en stoffenbalans
  dgwatdte  <- merge(dg[is.na(GAF),], mdPtb, by = 'EAG', all.x = F)
  dgwatdteG <- merge(dg[is.na(EAG),], mdPtbG, by = 'GAF', all.x = F)
  dgwatdteK <- merge(dg[is.na(EAG) & is.na(GAF),], mdPtbK, by = 'KRW', all.x = T)
  dgwatdte <- rbindlist(list(dgwatdte,dgwatdteG,dgwatdteK),fill=TRUE)
  
  # manuel correction for high watdte (needed for coupling with nomogram)
  dgwatdte[watdte > 0.7, watdteF := '(0.5,0.7]']
  
  # remove temporary objects
  rm(dgwatdteG,dgwatdteK,mdPtb,mdPtbG,mdPtbK)
  
  # retreive kP from meta-model PCditch 
  
  # add depth category, similar to dbhybi dataset
  dbnomogram[,watdteF := cut(watdte_m, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # model to predict kP as function of debiet (given soil and water depth)
  m1 <- lm(kP~bodemtype*watdteF*debiet*I(debiet^0.5)*I(debiet^2)*I(debiet^3),data=dbnomogram)
  
  # predict kP for dataset (suppress warnings ivm rank-deficient fit)
  suppressWarnings(dgwatdte[,kP := predict(m1,newdata = data.frame(debiet = w_debiet, bodemtype = tolower(bodem), watdteF = watdteF))])
  
  # renamed by Laura
  dgwatdte[,kPDitch := kP]
  
  # calc critical P-concentration 
  dgwatdte[,PvskPDitch := wp_min_sum / kP]
  
  # koppel kp plassen obv invoertabel per EAG 
  
  # relevant columns to be merged
  cols <- colnames(kP_plas)[grepl('^pc_|^lake|^p_bel|^EAG$|^GAF$',colnames(kP_plas))]
  
  # merge per EAG and per GAF, and combine both (assuming its either EAG or GAF)
  PvskPplas1 <- merge(dgwatdte[watertype %in% c('M20','M27','M25',"M14") & !is.na(EAG),],
                      kP_plas[,mget(cols)],by='EAG',all.y = TRUE,all.x = FALSE)
  PvskPplas2 <- merge(dgwatdte[watertype %in% c('M20','M27','M25',"M14") & !is.na(GAF),],
                      kP_plas[,mget(cols)],by = 'GAF',all.y = TRUE,all.x = FALSE) 
  pvskp <- rbindlist(list(PvskPplas1,PvskPplas2),fill = TRUE)
  
  # merge plas kP with original water balance db
  dgwatdte <- merge(dgwatdte, pvskp[,c('pol','EAG','pc_troebel_helder', 'p_bel_year', 
                                       'pc_helder_troebel', 'lake_ditch_vol')], by = c('pol','EAG'), all = TRUE)
  
  # calc PvskP for lakes
  dgwatdte[!is.na(p_bel_year),wp_min_sum := p_bel_year]
  dgwatdte[,PvskPlake := wp_min_sum / pc_helder_troebel]
  
  # remove rows without estimated P-belasting
  dgwatdte <- dgwatdte[!is.na(wp_min_sum)]
  
  # remove cases without name (was originally in makematrix)
  dgwatdte <- dgwatdte[!is.na(pol),]
  dgwatdte <- dgwatdte[!is.na(EAG) |!is.na(GAF),]
  
  # update EAG code for GAFs (was originally in makematrix) 
  
  # split file in data.tables for EAG and GAF
  dgwatdte.eag <- dgwatdte[!is.na(EAG)]
  dgwatdte.gaf <- dgwatdte[!is.na(GAF)]
  
  # merge GAFIDENT from eag_wl and rename as EAG
  dgwatdte.gaf <- merge(dgwatdte.gaf,dbeag_wl[,c("GAFIDENT","GAF")], by = "GAF")
  dgwatdte.gaf[ , EAG := GAFIDENT][,GAFIDENT :=NULL]
  
  # set columns order equal (prevents warning in rbindlist)
  setcolorder(dgwatdte.gaf,colnames(dgwatdte.eag))
  
  # subset only those that are not yet in subset EAG
  dgwatdte.gaf <- dgwatdte.gaf[!EAG %in% dgwatdte.eag$EAG]
  
  # combine again
  dgwatdte <- rbindlist(list(dgwatdte.eag,dgwatdte.gaf))
  
  # return output
  return(dgwatdte)
  
}

# esf1 en 3 bodem ----
bodsam <- function(bod, cmean = FALSE){
  
  # dcast slootbodem 
  selb <- dcast(bod, loc.eag+loc.code+loc.oms+loc.x+loc.y+loc.z+datum+jaar ~ parm.fews+parm.compartiment, value.var = "meetwaarde", fun.aggregate = mean)
  
  # calculate relevant ratios
  selb[,FESPFWratio := (Fe_mg_l_ng_BS/55.845 - Stot_mg_l_ng_BS/32.065)/(Ptot_mgP_l_ng_BS/30.974)]
  selb[,FESPDWratio := (Fe_mg_kg_dg_BS/55.845-Stot_mg_kg_dg_BS/32.065)/(Ptot_gP_kg_dg_BS*1000/30.974)]
  
  # add SP-ratio
  selb[!is.na(SO4_mg_l_nf_PW),FESPPWratio := (Fe_mg_l_nf_PW/55.845 - SO4_mg_l_nf_PW/96.06)/(Ptot_mgP_l_nf_PW/30.974)]
  selb[!is.na(Stot_mg_l_PW),FESPPWratio := (Fe_mg_l_nf_PW/55.845 - Stot_mg_l_PW/32.06)/(Ptot_mgP_l_nf_PW/30.974)]
  selb[!is.na(Stot_mg_l_nf_PW),FESPPWratio := (Fe_mg_l_nf_PW/55.845 - Stot_mg_l_nf_PW/32.065)/(Ptot_mgP_l_nf_PW/30.974)]
  
  # filter only op samples where FESPFWratio, FESPDWratio and FESPPWratio are present
  selb <- selb[!(is.na(FESPFWratio)|is.na(FESPDWratio)|is.na(FESPPWratio))]
  
  # calculate nalevering
  selb[,nlvrFW := 0.0247 * Ptot_mgP_l_ng_BS - 1.6035]
  selb[,nlvrDW := 0.0077 * Ptot_gP_kg_dg_BS * 1000 - 4.7259]
  selb[,nlvrPW := 0.8095 * Ptot_mgP_l_nf_PW - 0.2905]
  
  # add categories
  selb[,classFESPFWratio := cut(FESPFWratio, breaks = c((min(FESPFWratio)-1), 1.4, 4, max(FESPFWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  selb[,classFESPDWratio := cut(FESPDWratio, breaks = c((min(FESPDWratio)-1), 1.4, 4, max(FESPDWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  selb[,classFESPPWratio := cut(FESPPWratio, breaks = c((min(FESPPWratio)-1), 1.4, 4, max(FESPPWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  
  # calculate a mean per EAG and year if requested
  if(cmean){
    
    # what are the numeric columns
    cols <- colnames(selb)[sapply(selb, is.numeric)]
    selb.num <- selb[,lapply(.SD,median),.SDcols = cols[!cols=='jaar'],by=.(loc.eag,jaar)]
    
    # function to get modal value for categorial columns
    fmod <- function(x){names(sort(table(x),decreasing = T)[1])}
    
    # categorial columns to get modal
    cols <- colnames(selb)[grepl('^class',colnames(selb))]
    selb.cat <- selb[,lapply(.SD,fmod),.SDcols = cols,by=.(loc.eag,jaar)]
    
    # combine categorial and numerical columns
    selb <- merge(selb.num,selb.cat,by=c('loc.eag','jaar'))
  }
  
  # return extended soil-ditch properties database
  return(selb)
}

