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