# adoption from original script
# done by Gerard & Laura, december 2019

# 1. Directories and names -----------------------------------------------

  # file directory koppeltabel. NB: zorg dat sheetname='AGV', kolommen= 'CODE', 'balansnaam', 'agrarisch'
  dir_kop <- 'pbelasting/input/190324_koppeltabel.xlsx' 

  # folder directory waterbalansen NB: zorg dat alle foute balansen niet in deze dir staan
  dir_bal <- "../../balansen/" 
  dir_bal <- "F:/AGV data/"
  
# 2. input-----------------------

  # read the koppeltabel
  kopTab <- readxl::read_xlsx(dir_kop) %>% as.data.table()

  # eag weg bij eerdere versies, 2500-eag-5 weg, 1balansen aan meerdere eags koppelen
  files <- ppr_wbalfiles(dir_bal)
  
  # data van G.Ros obv balansen M. Ouboter 201808
  init <- readRDS("pbelasting/input/init.rds") %>% as.data.table()
  meanSoil <- readRDS("pbelasting/input/meanSoil.rds")

# 3. functions to read parameters for waterbalances--------------------
# - loadAlgemeen -> uitgangspunten (opp verhard etc.)
# - loadBalans2  -> maand data

# functie om algemene gegevens van de excelsheet te laden ----------------------
loadAlgemeen = function(x,wdir){
  
  # file name including location
  fname <- paste0(wdir,x)
  
  # print progress
  print(paste0('algemene data from ',basename(fname),' worden ingelezen'))
        
  # read the tab uitangspunten
  alg = suppressMessages(readxl::read_xlsx(fname, sheet = 'uitgangspunten', col_names = F, skip = 2)[1:34,1:9])
  
  # make data.table to store results
  out <- data.table(pol = basename(fname))
  
  # lees oppervlaktes
  cols <- c('a_tot','a_drain','a_verhard','a_gemengd','a_water')
  out[, c(cols) := as.list(as.numeric(unlist(alg[5:9,4]))/10000)]
  
  # lees bodemhoogte en slootdiepte
  cols <- c('a_bodemhoogte','a_slootdiepte')
  out[,c(cols) := as.list(as.numeric(unlist(alg[10,c(4,7)])))]
  
  # lees inlaten en uitlaten
  cols <- c(paste0('a_inlaat',1:5),paste0('a_uitlaat',1:4))
  out[,c(cols) := as.list(as.character(unlist(alg[22:30,1])))]
  
  # return output
  return(out)
}
  
loadBalance2 = function(x,wdir){
  
  # file name including location
  fname <- paste0(wdir,x)
  
  # print commands to show progress
  print(paste0('water and P fluxes from ',basename(fname),' worden ingelezen'))
  
  # read excel water balance, different for xls and xlsx
  if(grepl(pattern = '.xls$', fname)){
    balans  = readxl::read_xls(fname, sheet = 'Q+P_mnd', col_names = F, na = '#N/A', skip = 13 )
  } else {
    balans = suppressMessages(readxl::read_xlsx(fname, sheet = 'Q+P_mnd',col_names = F, na = '#N/A')[-c(1:13),] )
  }
  
  # convert to data.table and give names (p1 to p..n)
  balans <- as.data.table(balans)  
  setnames(balans,paste0('p',1:ncol(balans)))
  
  # add date and time
  balans[,date := as.Date(as.numeric(p1),origin = "1900-01-01")]
  balans[,maand := month(date)]
  balans[,jaar := year(date)]
  balans[,seiz := fifelse(maand %in% 4:9,'zomer','winter')]
   
  # peil [m NAP], volume [m3], debiet[mm/dag], berging [m3/dag] en sluitfout [m3/dag]
  cols <- paste0('p',c(2:4,28:30))
  balans[,(cols) := lapply(.SD,as.numeric),.SDcols = cols]
  setnames(balans,cols,paste0('w_',c('peil','volume','debiet','berging','sluitfout','maalstaat')))
  
  # IN waterposten [m3/dag]
  cols <- paste0('p',6:17)
  colsn <- c('neerslag','kwel','verhard','riol','drain','uitspoel','afstroom',paste0('inlaat',1:5))
  balans[,(cols) := lapply(.SD,as.numeric),.SDcols = cols]
  setnames(balans,cols,paste0('w_i_',colsn))
  
  # UIT waterposten [m3/dag]
  cols <- paste0('p',19:26)
  balans[,(cols) := lapply(.SD,function(x) as.numeric(x) * -1),.SDcols = cols]
  setnames(balans,cols,paste0('w_o_',c('verdamping','wegzijging','intrek',paste0('uitlaat',1:5))))
    
  # IN P-posten op basis van minimum [mg/m2/dag]
  cols <- paste0('p',35:46)
  colsn <- c('neerslag','kwel','verhard','riol','drain','uitspoel','afstroom',paste0('inlaat',1:5))
  balans[,(cols) := lapply(.SD,as.numeric),.SDcols = cols]
  setnames(balans,cols,paste0('wp_min_',colsn))
           
  # IN P-posten op basis van increment [mg/m2/dag]
  cols <- paste0('p',48:59)
  balans[,(cols) := lapply(.SD,as.numeric),.SDcols = cols]
  setnames(balans,cols,paste0('wp_inc_',colsn))
  
  # UIT P-posten (bij gemaal)
  balans[,wp_meting_gm3 := as.numeric(p5)]
  balans[wp_meting_gm3 == 0, wp_meting_gm3 := NA]
  balans[,wp_meting_mgm2d := -1 * as.numeric(p63)]

  # remove columns not used
  cols <- colnames(balans)[grepl('^p',colnames(balans))]
  balans[,(cols) := NULL]
  
  # add poldernaam
  balans[,pol := basename(fname)]
  
  # filter data given availability of precipitation (per month)
  out <- balans[w_i_neerslag >0]

  # rest column order with polder name and time first
  setcolorder(out,c('pol','date','jaar','maand','seiz'))
  
  # return output
  return(out)
}

# Wrapper function -------------------------------------------------------------
loadBalances_lm <- function(dir_bal,kopTab,sfile = FALSE){
  
  # file names
  files <- ppr_wbalfiles(dir_bal)
  
  # read excel data from sheet 'uitgangspunten' and combine all output in one data.table
  alg <- lapply(files,function(x) loadAlgemeen(x,wdir = dir_bal))
  alg <- rbindlist(alg)
  
  # read excel data from sheet 'jaargemiddelden' 
  bal <- lapply(files,function(x) loadBalance2(x,wdir = dir_bal))
  bal <- rbindlist(bal)
  
  # Koppel EAG, GAF en KRW waterlichamen
  dat <- merge(bal,alg,by='pol',all.x = TRUE)
  dat <- merge(dat,kopTab,by.x = 'pol',by.y = 'balans',all.x = TRUE)
    
  # koppel data initiator
  dat <- merge(dat,init,by.x = 'GAF', by.y = 'i_pol')
  
  # do some extra calculations (defined by Laura)
  
    # remove rows without maalstaat
    dat <- dat[!is.na(w_maalstaat)]
    
    # add red DAW value
    dat[,p_i_redDAW := 0.1 * wp_min_uitspoel]
  
    # add wp_min_sum and wp_inc_sum
    dat[,wp_min_sum := rowSums(.SD,na.rm=T),.SDcols = grep("wp_min_",names(dat))]
    dat[,wp_inc_sum := rowSums(.SD,na.rm=T),.SDcols = grep("wp_inc_",names(dat))]
    
  # save file
  if(sfile){saveRDS(dat, file = paste0('pbelasting/dat','.rds'))}
  
  # return data.table
  return(dat)
}



