# helper functions

tabelPerWL3jaargemEAG <- function (EKRset,gEAG,doelen){
  
  # make local copy (only within this function)
  doelen <- copy(doelen)
  
  # calculate mean per groep
  colgroup <-c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code.y',
               'Waardebepalingsmethode.code','GHPR_level','GHPR','level','jaar')
  d1 <- EKRset[,.(waarde = mean(Numeriekewaarde,na.rm=TRUE)),by=colgroup]
  
  # rename columns and order data.table
  setnames(d1,colgroup,c('id','EAGIDENT','watertype','wbmethode','GHPR_level','GHPR','level','jaar'))
  setorder(d1,EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode,-jaar)
  
  # add year number (given ordered set), and take only three most recent years
  d1 <- d1[,yearid := seq_len(.N),by=.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode)][yearid < 4]
  
  # calculate mean EKR per group over the three years
  d1 <- d1[,.(EKR = mean(waarde,na.rm=T)),by =.(EAGIDENT,id,watertype,GHPR_level,GHPR,level,wbmethode)]
  
  # remove empty spaces in GHPR needed for joining later
  d1[,GHPR := gsub(' ','',GHPR)]
  
  # merge with doelen
  
  # rename columns doelen object
  setnames(doelen,c('HoortBijGeoobject.identificatie'),c('id'))
  
  # mean GEP per object
  doelgeb <- doelen[,.(GEP = mean(Doel,na.rm=TRUE)),by =.(id,bronddoel,GHPR)]
  
  # merge with doelen
  d2 <- merge(d1, doelgeb, by = c('id','GHPR'), all.x = TRUE)
  
  # add classification for EKR
  d2[EKR < GEP/3,oordeel := 'slecht']
  d2[EKR >= GEP/3 & EKR < 2 * GEP / 3,oordeel := 'ontoereikend']
  d2[EKR >= 2 * GEP / 3,oordeel := 'matig']
  d2[EKR >= GEP, oordeel := 'goed']
  
  # add type water body
  d2[,wl := sapply(strsplit(id, '_'), `[`, 2)]
  d2[is.na(wl), wl := paste0('gewogen_',id)]
  d2[wl=='OvWa',wl := sapply(strsplit(id, '_'), `[`, 3)]
  d2[!is.na(EAGIDENT), wl := EAGIDENT]
  
  # merge with EAG shape
  d3 <- merge(d2, gEAG[,c('GAFIDENT','GAFNAAM')], by.x = 'wl',by.y = 'GAFIDENT', all.x = TRUE)
  
  # setnames
  setnames(d3,c('wl','id'),c('waterlichaam','geo_id'))
  
  # remove the geo component
  d3[,geom := NULL]
  
  # return the object
  return(d3)
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
  dt[,GHPRnew := gsub('abundantie','abon_',GHPRnew)]
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
  
  # select relevant water balance data ----
    
    # select only relevant years
    dg <- dbwbal[jaar %in% 2010:2018]
    
    # get mean per EAG-GAF-bodem-polder (gr: is this combined grouping really needed?)
    colsg <- colnames(dg)[grepl('^a_in|^a_uit|^EAG|^GAF|^KRW$|watertype|^bodem$|^pol',colnames(dg))]
    colss <- colnames(dg)[grepl('^a_|^wp_|jaar|maand|^w_|^p_i',colnames(dg))]
    colss <- colss[!colss %in% colsg]
    dg <- dg[,lapply(.SD,mean,na.rm=TRUE),.SDcols=colss,by=colsg]
    
    # add total P-load
    dg[,wp_tot_sum := wp_min_sum + wp_inc_sum]
 
  # select relevant hydrobiological data ----
    
    # filter hydrobiologische data.base before calculating water depth
    dbhybi <- dbhybi[jaar %in% 2010:2017 & fewsparameter =='WATDTE_m']
    
    # mean water depth per EAG
    mdPtb <- copy(dbhybi)[,.(watdte = median(meetwaarde,na.rm=TRUE)),by='EAG']
    mdPtb[,watdteF := cut(watdte, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
    # mean water depth per GAF
    mdPtbG <- copy(dbhybi)[,.(watdte = median(meetwaarde,na.rm=TRUE)),by='GAF']
    mdPtbG[,watdteF := cut(watdte, breaks = c('0','0.3','0.5','0.7','7.0'))]
    
    # mean water depth per waterkwaliteitspunt
    mdPtbK <- copy(dbhybi)[,.(watdte = median(meetwaarde,na.rm=TRUE)),by='KRW']
    mdPtbK[,watdteF := cut(watdte, breaks = c('0','0.3','0.5','0.7','7.0'))]

  # merge water balance data with water depth ----
  
    # koppel waterdiepte per eag en afvoergebied aan water en stoffenbalans
    dgwatdte  <- merge(dg[is.na(GAF),], mdPtb, by = 'EAG', all.x = F)
    dgwatdteG <- merge(dg[is.na(EAG),], mdPtbG, by = 'GAF', all.x = F)
    dgwatdteK <- merge(dg[is.na(EAG) & is.na(GAF),], mdPtbK, by = 'KRW', all.x = T)
    dgwatdte <- rbindlist(list(dgwatdte,dgwatdteG,dgwatdteK),fill=TRUE)

    # manuel correction for high watdte (needed for coupling with nomogram)
    dgwatdte[watdte > 0.7, watdteF := '(0.5,0.7]']
 
    # remove temporary objects
    rm(dgwatdteG,dgwatdteK,mdPtb,mdPtbG,mdPtbK)
    
  # retreive kP from meta-model PCditch ----
    
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
    
  # koppel kp plassen obv invoertabel per EAG ----
  
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
  
  # update EAG code for GAFs (was originally in makematrix) ----
  
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

# esf3 bodem ----------------------------------------------------
bodsam <- function(bod){
  
  selb <- dcast(bod, locatie.EAG+locatiecode+locatie.omschrijving+locatie.x+locatie.y+locatie.z+datum+jaar ~ fewsparameter+compartiment, value.var = "meetwaarde", fun.aggregate = mean)
  selb$FESPFWratio <-((selb$`Fe_mg/l_ng_BS`/55.845)-(selb$`Stot_mg/l_ng_BS`/32.065))/(selb$`Ptot_mgP/l_ng_BS`/30.974)
  selb$FESPDWratio <-((selb$`Fe_mg/kg_dg_BS`/55.845)-(selb$`Stot_mg/kg_dg_BS`/32.065))/((selb$`Ptot_gP/kg_dg_BS`*1000)/30.974)
  #selb$`Ptot_mgP/l_nf_PW`<- selb$`Ptot_mgP/l_PW`
  
  if(is.null(selb$`Stot_mg/l_nf_PW`)){
    if(!is.null(selb$`SO4_mg/l_PW`)){
      selb$FESPPWratio <-(((selb$`Fe_ug/l_nf_PW`/1000)/55.845)-(selb$`SO4_mg/l_PW`/96.06))/(selb$`Ptot_mgP/l_nf_PW`/30.974)
    }
    if(!is.null(selb$`Stot_mg/l_PW`)){
      selb$FESPPWratio <-(((selb$`Fe_ug/l_nf_PW`/1000)/55.845)-(selb$`Stot_mg/l_PW`/32.06))/(selb$`Ptot_mgP/l_nf_PW`/30.974)
    }}
  if(!is.null(selb$`Stot_mg/l_nf_PW`)){  
    selb$FESPPWratio <-(((selb$`Fe_ug/l_nf_PW`/1000)/55.845)-(selb$`Stot_mg/l_nf_PW`/32.065))/(selb$`Ptot_mgP/l_nf_PW`/30.974)
  }
  selb$nlvrFW <- 0.0247*selb$`Ptot_mgP/l_ng_BS`-1.6035
  selb$nlvrDW <- 0.0077*(selb$`Ptot_gP/kg_dg_BS`*1000)-4.7259
  
  selb <- selb[!is.na(selb$FESPFWratio) ,]
  selb$FESPFWratio <-cut(selb$FESPFWratio, breaks= c((min(selb$FESPFWratio)-1), 1.4, 4, max(selb$FESPFWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))
  selb <- selb[!is.na(selb$FESPDWratio) ,]
  selb$FESPDWratio <-cut(selb$FESPDWratio, breaks= c((min(selb$FESPDWratio)-1), 1.4, 4, max(selb$FESPDWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))
  if(!is.null(selb$FESPPWratio)){
    selb$nlvrPW <- 0.8095*selb$`Ptot_mgP/l_nf_PW`-0.2905
    #write.table(selb, file = paste(getwd(),"baggernutQuickscan",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
    selb <- selb[!is.na(selb$FESPPWratio) ,]
    selb$FESPPWratio <-cut(selb$FESPPWratio, breaks= c((min(selb$FESPPWratio)-1), 1.4, 4, max(selb$FESPPWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))
  }
  return(selb)
}
