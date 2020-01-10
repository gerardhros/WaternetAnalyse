# calc functions to process data
# Laura Moria & Gerard H. Ros, december-19

calc_mean_EKR <- function (db, nyears = 3, smonth = 1:12, pEAG = TRUE, pYEAR = FALSE, pSEASON = FALSE){
  
  # input description
  # db: the database with EKR scores, a data.table
  # nyears: a numeric value x to select only the scores of the most recent x years per EAG
  # smonth: a vector used to select the relevant months (options 1:12)
  # pEAG, pYEAR, pSEASON (boolean): grouping needed for EAG, YEAR or SEASON
  
  # make local copy
  db <- copy(db)
  
  # remove some elements from db
  db <- db[!Grootheid.code %in% c("AANTPVLME", "SOORTRDM")]
                         
  # simplify data.table to relevant columns only and rename those columns
  cols <- c('Identificatie','XCOORD','YCOORD','datum','HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code.y',
            'Waardebepalingsmethode.code','GHPR_level','GHPR','level','jaar','Numeriekewaarde')
  d0 <- db[,mget(cols)]
  setnames(d0,cols,c('mpid','x','y','datum','id','EAGIDENT','watertype','wbmethode','GHPR_level','GHPR','level','jaar','meetwaarde'))
  
  # add season and filter on selected months
  d0[,month := month(datum)]
  d0[,season := fifelse(month %in% 4:10,'summer','winter')]
  d0 <- d0[month %in% smonth]
  
  # remove empty spaces in GHPR (needed for joining later)
  d0[,GHPR := gsub(' ','',GHPR)]
  
  # add dynamic grouping variable depening on function input
  groups <- c('EAGIDENT','id','watertype','GHPR_level','GHPR','level','wbmethode')
  if(pSEASON){groups <- c(groups,'season')}
  
  # add year number and take only nyears most recent years (selection per EAG)
  d0 <- d0[,yearid := frank(-jaar,ties.method = 'dense'),by = groups][yearid <= nyears]
  
  # calculate mean EKR value per group per jaar
  if(pEAG){d1 <- d0[,.(EKR = mean(meetwaarde,na.rm=TRUE)),by = c(groups,'jaar')]}
  
  # calculate meerjarig gemiddelde EKR for each EAG
  if(!pYEAR & pEAG){d1 <- d1[,.(EKR = mean(EKR,na.rm=TRUE)),by = groups]}
  
  # calculate mean EKR for each measurement point and year or the meerjarig gemiddelde per mp
  if(!pEAG){d1 <- d0[,.(EKR = mean(meetwaarde,na.rm=TRUE)),by = c(groups,'mpid','jaar')]}
  if(!pYEAR & !pEAG){d1 <- d1[,.(EKR = mean(EKR,na.rm=TRUE)),by = c(groups,'mpid')]}
  
  # return the object
  return(d1)
}


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
calc_mean_wb <- function(db,){
  
  
  
}