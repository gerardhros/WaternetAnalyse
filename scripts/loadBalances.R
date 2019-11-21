# 1. Directories and names -----------------------------------------------
# file directory koppeltabel. NB: zorg dat sheetname='AGV', kolommen= 'CODE', 'balansnaam', 'agrarisch'
dir_kop <- 'pbelasting/input/190324_koppeltabel.xlsx' 
# folder directory waterbalansen NB: zorg dat alle foute balansen niet in deze dir staan
dir_bal <- "../../balansen/" 

# 2. input-----------------------
kopTab <- read_excel(dir_kop)
#eag weg bij eerdere versies, 2500-eag-5 weg, 1balansen aan meerdere eags koppelen
files  <- list.files(dir_bal)
init <- readRDS("./pbelasting/input/init.rds") # data van G.Ros obv balansen M. Ouboter 201808
meanSoil <- readRDS("./pbelasting/input/meanSoil.rds")

#write.table(files0, file = paste(getwd(),"/output/namenBalansen",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

# 3. functions to read parameters for waterbalances--------------------
# - loadAlgemeen -> uitgangspunten (opp verhard etc.)
# - loadBalans2  -> maand data

# functie om algemene gegevens van de excelsheet te laden ----------------------
loadAlgemeen = function(x){
  alg           = read_excel(x, sheet = 'uitgangspunten', col_names = F, skip = 2)[1:34,1:9]
  a_tot         = as.numeric(alg[5,4])/10000
  a_drain       = as.numeric(alg[6,4])/10000
  a_verhard     = as.numeric(alg[7,4])/10000
  a_gemengd     = as.numeric(alg[8,4])/10000
  a_water       = as.numeric(alg[9,4])/10000
  a_bodemhoogte = as.numeric(alg[10,4])
  a_slootdiepte = as.numeric(alg[10,7])
  a_inlaat1 = as.character(alg[22,1])
  a_inlaat2 = as.character(alg[23,1])
  a_inlaat3 = as.character(alg[24,1])
  a_inlaat4 = as.character(alg[25,1])
  a_inlaat5 = as.character(alg[26,1])
  a_uitlaat1 = as.character(alg[27,1])
  a_uitlaat2 = as.character(alg[28,1])
  a_uitlaat3 = as.character(alg[29,1])
  a_uitlaat4 = as.character(alg[30,1])
  pol           = as.character(files[match(x, fileLocs)])
  names(pol)    = 'pol'
  
  # toevoegingen mogelijk voor geohydrology: zomer en winter kwelwaardes
  out = data.frame(pol, a_tot, a_drain, a_verhard, a_gemengd, a_water, 
                   a_bodemhoogte, a_slootdiepte, a_inlaat1,a_inlaat2,a_inlaat3,a_inlaat4,a_inlaat5,a_uitlaat1,a_uitlaat2,a_uitlaat3,a_uitlaat4)
  out$pol = as.character(out$pol)
  out$a_inlaat1 = as.character(out$a_inlaat1)
  out$a_inlaat2 = as.character(out$a_inlaat2)
  out$a_inlaat3 = as.character(out$a_inlaat3)
  out$a_inlaat4 = as.character(out$a_inlaat4)
  out$a_inlaat5 = as.character(out$a_inlaat5)
  out$a_uitlaat1 = as.character(out$a_uitlaat1)
  out$a_uitlaat2 = as.character(out$a_uitlaat2)
  out$a_uitlaat3 = as.character(out$a_uitlaat3)
  out$a_uitlaat4 = as.character(out$a_uitlaat4)
  return(out)
}
loadBalance2 = function(file, file2){
  print(file)
  print(file2)
  
  pol           = as.character(files[match(file, fileLocs)])
  names(pol)    = 'pol'
  
  num <- function(x){as.numeric(x)}
  
  if(grepl(pattern = '.xls$', file)){
    balans  = read_excel(file, sheet = 'Q+P_mnd', col_names = F, na = '#N/A', skip = 13 )
  } else{
    # als geen xlsx dan hele sheet laden, anders worden sommige kolommen geskipped
    rows    = -(1:13)
    balans  = read_excel(file, sheet = 'Q+P_mnd',col_names = F, na = '#N/A')[rows,] # niet skip=13 gebruiken, anders fout door verschil xls en xlsx in huidige versie van read_excel (170220)
  }
  
  balans  = as.data.frame(balans)                               # maak dataframe

  # maanden
  maand = num(format(as.Date(num(balans[,1]), origin = "1900-01-01"), "%m"))
  jaar  = num(format(as.Date(num(balans[,1]), origin = "1900-01-01"), "%Y"))
  seiz  = ifelse(maand<4 | maand>9, 'w', 'z') # seizoen: winter of zomer
  
  # peil, volume en debiet
  w_peil        = num(balans[,2]) # peil[m NAP]
  w_volume      = num(balans[,3]) # volume [m3]
  w_debiet      = num(balans[,4]) # debiet [mm/dag]
  w_berging     = num(balans[,28]) # berging [m3/dag]
  w_sluitfout   = num(balans[,29]) # sluitfout [m3/dag]
  w_maalstaat   = num(balans[,30])
  
  # IN waterposten
  w_i_neerslag  = num(balans[,6]) # neerslag [m3/dag]
  w_i_kwel      = num(balans[,7]) # kwel [m3/dag] (simulatie)
  w_i_verhard   = num(balans[,8]) # instroom via verhard [m3/dag]
  w_i_riol      = num(balans[,9]) # instroom via riolering [m3/dag]
  w_i_drain     = num(balans[,10])# instroom via drainage [m3/dag]
  w_i_uitspoel  = num(balans[,11])# instroom via uitspoeling [m3/dag]
  w_i_afstroom  = num(balans[,12])# instroom via afspoeling [m3/dag]
  w_i_inlaat1   = num(balans[,13])
  w_i_inlaat2   = num(balans[,14])
  w_i_inlaat3   = num(balans[,15])
  w_i_inlaat4   = num(balans[,16])
  w_i_inlaat5   = num(balans[,17])
  
  # UIT waterposten
  w_o_verdamping= -num(balans[,19]) # verdamping [m3/dag]
  w_o_wegzijg   = -num(balans[,20]) # [m3/dag]
  w_o_intrek    = -num(balans[,21]) # [m3/dag]
  w_o_uitlaat1  = -num(balans[,22])
  w_o_uitlaat2  = -num(balans[,23])
  w_o_uitlaat3  = -num(balans[,24])
  w_o_uitlaat4  = -num(balans[,25])
  w_o_uitlaat5  = -num(balans[,26])
  
  # IN P Posten (op basis van minimum!!!)
  wp_min_neerslag  = num(balans[,35]) # [mg/m2/dag]
  wp_min_kwel      = num(balans[,36]) # [mg/m2/dag]
  wp_min_verhard   = num(balans[,37]) # [mg/m2/dag]
  wp_min_riol      = num(balans[,38]) # [mg/m2/dag]
  wp_min_drain     = num(balans[,39]) # [mg/m2/dag]
  wp_min_uitspoel  = num(balans[,40]) # [mg/m2/dag]
  wp_min_afstroom  = num(balans[,41]) # [mg/m2/dag]
  wp_min_inlaat1   = num(balans[,42])
  wp_min_inlaat2   = num(balans[,43])
  wp_min_inlaat3   = num(balans[,44])
  wp_min_inlaat4   = num(balans[,45])
  wp_min_inlaat5   = num(balans[,46])
  
  # IN P Posten (op basis van increment!!!)
  wp_inc_neerslag  = num(balans[,48]) # [mg/m2/dag]
  wp_inc_kwel      = num(balans[,49]) # [mg/m2/dag]
  wp_inc_verhard   = num(balans[,50]) # [mg/m2/dag]
  wp_inc_riol      = num(balans[,51]) # [mg/m2/dag]
  wp_inc_drain     = num(balans[,52])# [mg/m2/dag]
  wp_inc_uitspoel  = num(balans[,53])# [mg/m2/dag]
  wp_inc_afstroom  = num(balans[,54])# [mg/m2/dag]
  wp_inc_inlaat1   = num(balans[,55])
  wp_inc_inlaat2   = num(balans[,56])
  wp_inc_inlaat3   = num(balans[,57])
  wp_inc_inlaat4   = num(balans[,58])
  wp_inc_inlaat5   = num(balans[,59])
  
  # UIT P posten (bij gemaal)
  wp_meting_gm3     = ifelse(num(balans[,5]) == 0, NA, num(balans[,5]))
  wp_meting_mgm2d   = -num(balans[,63])    # [mg/m2/dag]
  
  
  # Maak data.frame van alle posten 
  DF = data.frame(wp_meting_gm3, wp_meting_mgm2d, 
                  jaar, maand, seiz,w_peil, w_volume, w_debiet, w_berging, w_sluitfout,  
                  w_maalstaat, w_i_neerslag,w_i_kwel, w_i_verhard,w_i_riol,      
                  w_i_drain,w_i_uitspoel,w_i_afstroom,
                  w_i_inlaat1,w_i_inlaat2,w_i_inlaat3,w_i_inlaat4,w_i_inlaat5
                  ,w_o_verdamping,
                  w_o_wegzijg,w_o_intrek,
                  w_o_uitlaat1,w_o_uitlaat2,w_o_uitlaat3,w_o_uitlaat4,w_o_uitlaat5,
                  wp_min_neerslag,wp_min_kwel,     
                  wp_min_verhard, wp_min_riol,wp_min_drain,wp_min_uitspoel,wp_min_afstroom,  
                  wp_min_inlaat1,wp_min_inlaat2,wp_min_inlaat3,wp_min_inlaat4,wp_min_inlaat5,
                  wp_inc_neerslag,wp_inc_kwel,     
                  wp_inc_verhard, wp_inc_riol,wp_inc_drain,wp_inc_uitspoel,wp_inc_afstroom,  
                  wp_inc_inlaat1,wp_inc_inlaat2,wp_inc_inlaat3,wp_inc_inlaat4,wp_inc_inlaat5, stringsAsFactors = F) 
  DF <- cbind(DF , pol) 
  DF <- DF[!DF$w_i_neerslag <= 0,] # om te voorkomen dat legen reeksen worden meegenomen. Geen neerslag op maandbasis komt niet voor.
  # return output
  return(DF)
}

# Wrapper function -------------------------------------------------------------
loadBalances_lm = function(...){
  files  <- list.files(dir_bal)
  fileLocs = paste0(dir_bal, files)
  
  # 1. Algemene data ---------------
  # read excel data from sheet 'uitgangspunten' 
  alg = suppressWarnings(do.call(rbind, lapply(fileLocs, loadAlgemeen)))
  # save in Rdata file (per date)
  # saveRDS(alg, file = paste0('pbelasting/data/balans/alg_',gsub('^20','',gsub('-', '', Sys.Date())), '.rds'))
  # 2. Maand en seizoen data -----------------
  # read excel data from sheet 'jaargemiddelden' 
  balM <- suppressWarnings(do.call(rbind, lapply(1:length(fileLocs), function(x) loadBalance2(fileLocs[x], files[x]))))
  balM$pol <- as.character(balM$pol)
  
  # Koppel EAG, GAF en KRW waterlichamen
  dat <- 
    inner_join(alg, balM, by='pol') %>%
    left_join(kopTab, by = c('pol' = 'balans')) %>%     # om de andere data te koppelen
    mutate(GAF = as.numeric(GAF))%>%
    mutate(GAF = as.character(GAF)) %>%
    left_join(init, by = c('GAF' = 'i_pol')) #%>%       # initiator data toevoegen
    #left_join(meanSoil, by= c('EAG' = 'CODE'))  %>%       # initiator data toevoegen
    #left_join(meanSoil, by= c('GAF' = 'CODE'))                # bodem data toevoegen
  
  dat <- dat[!is.na(dat$w_maalstaat),]
  dat$p_i_redDAW <- 0
  #sel <- dat$agrarisch == '1'& !is.na(dat$wp_min_uitspoel) & !dat$wp_min_uitspoel == 0
  dat$p_i_redDAW <-   (0.1 * dat$wp_min_uitspoel)
  dat$wp_min_sum <- dat$wp_min_neerslag + dat$wp_min_kwel +  dat$wp_min_verhard+ dat$wp_min_riol+ dat$wp_min_drain + dat$wp_min_uitspoel +
    dat$wp_min_afstroom + dat$wp_min_inlaat1 + dat$wp_min_inlaat2 + dat$wp_min_inlaat3 + dat$wp_min_inlaat4 + dat$wp_min_inlaat5
  dat$wp_inc_sum <- dat$wp_inc_neerslag+dat$wp_inc_kwel+ dat$wp_inc_verhard+ dat$wp_inc_riol+dat$wp_inc_drain+dat$wp_inc_uitspoel+dat$wp_inc_afstroom+  
    dat$wp_inc_inlaat1+dat$wp_inc_inlaat2+dat$wp_inc_inlaat3+dat$wp_inc_inlaat4+dat$wp_inc_inlaat5
  
  
  write.table(dat[!is.na(dat$KRW),], file = paste(getwd(),"/pbelasting/output/gemMaandBalansenWL",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  write.table(dat, file = paste(getwd(),"/pbelasting/output/gemMaandBalansen",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  
  saveRDS(dat, file = paste0('pbelasting/dat','.rds'))
  return(dat)
}



