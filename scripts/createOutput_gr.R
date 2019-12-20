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



makePmaps <- function(dbsoil,dbhybi){
  
  # convert to data.table
  dat <- as.data.table(dbsoil)
  
  # adapt one variable manually
  dat[GAF == '8070',watertype := 'M3']
  
  # adapt soil types
  dat[,bodem := i_bt1]
  dat[is.na(i_bt1) & watertype %in% c('M8','M10','M27','M25'),bodem := 'VEEN']
  dat[is.na(i_bt1) & watertype %in% c("M3", "M1a","M30","M14","M11","M6a","M7b","M6b","M7a"),bodem := 'KLEI']
  
  # select only relevant years
  dg <- dat[jaar %in% 2010:2018]
  
  # get mean per group
  colsg <- colnames(dg)[grepl('^a_in|^a_uit|^EAG|^GAF|^KRW$|watertype|^bodem$|^pol',colnames(dg))]
  colss <- colnames(dg)[grepl('^a_|^wp_|jaar|maand|^w_^|^p_i',colnames(dg))]
  colss <- colss[!colss %in% colsg]
  dg <- dg[,lapply(.SD,mean,na.rm=TRUE),.SDcols=colss,by=colsg]
  
  # add total P-load
  dg[,wp_tot_sum := wp_min_sum + wp_inc_sum]
  
  # filter hydrobiologische data.base before calculating water depth
  dbhybi <- copy(dbhybi)
  dbhybi <- dbhybi[jaar %in% 2010:2017 & fewsparameter =='WATDTE_m']
  
  # mean water depth per EAG
  mdPtb <- copy(dbhybi)[,.(meetwaarde = median(meetwaarde,na.rm=TRUE)),by='locatie.EAG']
  mdPtb[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  setnames(mdPtb,'locatie.EAG','EAG')
  
  # mean water depth per GAF
  mdPtbG <- copy(dbhybi)[,.(meetwaarde = median(meetwaarde,na.rm=TRUE)),by='locatie.afaanvoergebied']
  mdPtbG[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  setnames(mdPtbG,'locatie.afaanvoergebied','GAF')
  
  # mean water depth per waterkwaliteitspunt
  mdPtbK <- copy(dbhybi)[,.(meetwaarde = median(meetwaarde,na.rm=TRUE)),by='locatie.KRWmeetpuntlocatie']
  mdPtbK[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  setnames(mdPtbK,'locatie.KRWmeetpuntlocatie','KRW')
  
  # merge with kP
  
  # koppel waterdiepte per eag en afvoergebied aan water en stoffenbalans
  dgwatdte  <- merge(dg[is.na(GAF),], mdPtb, by = 'EAG', all.x = F)
  dgwatdteG <- merge(dg[is.na(EAG),], mdPtbG, by = 'GAF', all.x = F)
  dgwatdteK <- merge(dg[is.na(EAG) & is.na(GAF),], mdPtbK, by = 'KRW', all.x = T)
  dgwatdte <- rbindlist(list(dgwatdte,dgwatdteG,dgwatdteK),fill=TRUE)
  setnames(dgwatdte,'meetwaarde','watdte')
  
  # one manuel correction
  dgwatdte[watdte > 0.7, watdteF := '(0.5,0.7]']
 
  
  
  nomogram$watdteF <- cut(nomogram$watdte_m, breaks = c('0','0.3','0.5','0.7'))
  
  ditchkP <- NULL
  # i <- unique(nomogram$bodemtype)[1]
  for (i in unique(nomogram$bodemtype)){
    nomogram_bt <- nomogram[nomogram$bodemtype == i,]
    dgwatdte_bt <- dgwatdte[dgwatdte$bodem == toupper(i),]
    dgwatdte_bt <- dgwatdte_bt[!is.na(dgwatdte_bt$bodem),]
    for(j in unique(nomogram_bt$watdteF)){
      nomogram_btdp <- nomogram_bt[nomogram_bt$watdteF == j,]
      dgwatdte_btdp <- dgwatdte_bt[dgwatdte_bt$watdteF == j,]
      dgwatdte_btdp <- dgwatdte_btdp[!is.na(dgwatdte_btdp$watdteF),]
      ditch <- merge(dgwatdte_btdp, nomogram_btdp[,c('debiet..mm.dag.','kP')], by.x = 'w_debiet', by.y = 'debiet..mm.dag.', all = TRUE)
      ditch <- ditch[order(ditch$w_debiet),]
      ditch$kP2 <- na.approx(ditch$kP, method = "linear", rule = 2) # interpolatie kP tussen debieten in nomogram
      ditch$w_debiet <- as.numeric(ditch$w_debiet)
      ditchkP <- rbind(ditch[!is.na(ditch$pol),], ditchkP)
    }
  }
  
  #koppel kritische fosforgrenzen obv waterdiepte, hydraulische belasting en bodemtype aan metamodel PCditch
  PvskP <- merge(dgwatdte, ditchkP[,c('kP2','pol','EAG')], by = c('pol','EAG'), all.x = TRUE, all.y =FALSE); PvskP$kPDitch <- PvskP$kP2; PvskP$kP2 = NULL
  # bereken kP ------------------------------------------------------------
  PvskP$PvskPDitch<- PvskP$wp_min_sum/PvskP$kP # >1 is te veel
  # hier toevoegen welke wel aan max belasting moet
  
  
  # koppel kp plassen obv invoertabel per EAG
  Overzicht_kP_plas <- Overzicht_kP
  
  sel <- dgwatdte$watertype %in% c('M20','M27','M25',"M14")  
  PvskPplas <-  merge(dgwatdte[sel & !is.na(dgwatdte$EAG) ,], Overzicht_kP_plas[!is.na(Overzicht_kP_plas$EAG),c('EAG',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.', 
                                                                                                                'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                      by.x = 'EAG', by.y = 'EAG', all.x = F, all.y = T)
  
  PvskPplas1 <-  merge(dgwatdte[sel& !is.na(dgwatdte$GAF),], Overzicht_kP_plas[!is.na(Overzicht_kP_plas$afvoergebied),c('afvoergebied','EAG',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.', 
                                                                                                                        'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                       by.x = 'GAF', by.y = 'afvoergebied', all.x = F, all.y = T)
  
  PvskPplas2 <-  merge(dgwatdte[sel,], Overzicht_kP_plas[,c('EAG',"P.load_year..mgP.m2.d.", 'Troebel.naar.helder..mg.P.m2.d.',
                                                            'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')],
                       by.x = 'KRW', by.y = 'EAG', all.x = F, all.y = T)
  
  pvskp <- smartbind(PvskPplas, PvskPplas1)
  
  PvskP <- merge(PvskP, pvskp[,c('pol','EAG','Troebel.naar.helder..mg.P.m2.d.', 'P.load_year..mgP.m2.d.', 
                                 'Helder.naar.troebel..mg.P.m2.d.', 'lake.ditch.vollenweider')], by = c('pol','EAG'), all = TRUE)
  
  PvskP$wp_min_sum[!is.na(PvskP$P.load_year..mgP.m2.d.)]  <- PvskP$P.load_year..mgP.m2.d.[!is.na(PvskP$P.load_year..mgP.m2.d.)] 
  PvskP$PvskPLake <- PvskP$wp_min_sum/PvskP$Helder.naar.troebel..mg.P.m2.d. # >1 is te veel
  
  PvskP <- PvskP[!is.na(PvskP$wp_min_sum),]
  return(PvskP)
  #write.table(PvskP, file = paste(getwd(),"/pbelasting/output/PvskPditchlakeALL",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  # schrijf data weg voor controle ------------------------------------------------
  # geen p belasting in ouderkerk, gaasperplas, loenderveen en terra nova, boezem toevoegen: toevoegen tabel
  
}

