# locaties van alle metingen (water, biologie, en slootbodem)
locaties <- fread('../data/Location.csv')
# KRW doelen 
doelen <- fread('../hydrobiologie/Doelen.csv')
doelen$Doel_2022 <- as.numeric(doelen$Doel_2022)

## let op: als nieuwe EAG (gEAG) dan deze tabel aanpassen en aanvullen
eag_wl <- fread('../data/EAG_Opp_kenmerken_20201005.csv')

hybi <- readRDS('../data/alles_reliable.rds')
# hybi measurements
hybi <- ppr_hybi(db = hybi, syear = 1990, wtype = eag_wl, mlocs = locaties)
hybi$meetwaarde <- as.numeric(hybi$meetwaarde)
saveRDS(hybi, file = './data/hybi.rds')

# EKR sets KRW en overig water
EKRset1 <- readRDS('../hydrobiologie/EKRsetKRW.rds') %>% as.data.table()
EKRset2 <- readRDS('../hydrobiologie/EKRsetOvWater.rds') %>% as.data.table()
# EKR measurements
EKRset <- ppr_ekr(krwset = EKRset1, ovwatset = EKRset2, doelen=doelen, eag_wl = eag_wl)
saveRDS(EKRset, file = './data/ekrset.rds')

# mooiste sloot
# EKRset  <- EKRset[,maxscore := Numeriekewaarde==max(Numeriekewaarde,na.rm=T), by = c('HoortBijGeoobject.identificatie','EAGIDENT','Waardebepalingsmethode.code','level','GHPR')]
# ptnset <- EKRset[EKRset$Waardebepalingsmethode.code == 'Maatlatten2018 Ov. waterflora',]
# write.table(ptnset, file = paste(getwd(),"/output/ptnsetmaxpereagmaatlat",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
# hybisel <- hybi[paste0(hybi$locatiecode,hybi$jaar) %in% paste0(ptnset$CODE[ptnset$maxscore == TRUE],ptnset$jaar[ptnset$maxscore == TRUE]),] #mooiste sloot
# write.table(hybisel, file = paste(getwd(),"/output/mooistesloothybi",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

source('./scripts/createOutput.R')

# EKR
tabset <- tabelEKRPerWLEnEAGPerJaar(EKRset, detail = "deel") %>% as.data.table() # deelmaatlatten, wide format, oordelen, percentielen, 3jarig 
tabset[,oordeelsort := EKR3jr / GEP_2022]
tabset <- tabset[GHPR == 'Waterdiepte', GHPR := as.factor(gsub('Waterdiepte',"Vestigingsdiepte waterplanten", GHPR))]

# zoek laagste oordeel per hoofdmaatlat 
tabset <- tabset[!is.na(id) & level == 1, minscore := oordeelsort==min(oordeelsort,na.rm=T), by = c('id','EAGIDENT')]
# zoek laagste score per (deel)maatlat
tabset <- tabset[!level == 1 ,minscore := EKR3jr==min(EKR3jr,na.rm=T), by = c('id','EAGIDENT','facet_wrap_code','level')]
# subset 3 correctie laagte score deelmaatlat soorten abundantie waarbij oever en drijfblad niet meedoet
tabset<- tabset[wbmethode =="Maatlatten2018 Ov. waterflora" &
                  level == 3 & !(GHPR %in% c('Bedekking Grote drijfbladplanten','Bedekking Kruidlaag')),minscore := EKR3jr==min(EKR3jr,na.rm=T), by = c('id','EAGIDENT','facet_wrap_code','level')]
saveRDS(tabset, file = './data/tabset.rds')

# bereken trend per toetsgebied
trendekr <- trend(EKRset, detail = "deel") 
#koppel trend aan ekr per jaar en gemiddeld
trendekr <- merge(trendekr[,c('id','facet_wrap_code', 'GHPR_level','Waardebepalingsmethode.code','KRWwatertype.code','estimate','r.squared','p.value')], tabset, by.x = c('id','facet_wrap_code', 'GHPR_level','Waardebepalingsmethode.code','KRWwatertype.code'), by.y = c('id','facet_wrap_code', 'GHPR_level','wbmethode','watertype'), all.y = T)
trendekr$estimate <- trendekr$estimate*12 # ekr per 2 planperioden
trendekr$estimate[trendekr$estimate < 0.05 & trendekr$estimate > -0.05] <- 0 # verwaarloosbaar klein
trendekr$estimate <- round(trendekr$estimate, digits = 2)
trendekr$facet_wrap_code <- gsub("Ov. waterflora", "Waterflora", trendekr$facet_wrap_code)
ekrtrend <- trendekr[!(grepl('^NL11*', trendekr$id) & !trendekr$KRW_SGBP3 == "") & trendekr$level == 1,] # alleen gewogen scores

# bereken trend per eag (ook voor krw wl)
trendekreag <- trendEAG(EKRset, detail = "deel") 
#koppel trend aan ekr per jaar en gemiddeld
trendekreag <- merge(trendekreag[,c('id','EAGIDENT','facet_wrap_code', 'GHPR_level','Waardebepalingsmethode.code','KRWwatertype.code','estimate','r.squared','p.value')], tabset, by.x = c('id',"EAGIDENT",'facet_wrap_code', 'GHPR_level','Waardebepalingsmethode.code','KRWwatertype.code'), by.y = c('id',"EAGIDENT",'facet_wrap_code', 'GHPR_level','wbmethode','watertype'), all.y = T)
trendekreag$estimate <- trendekreag$estimate*12 # ekr per 2 planperioden
trendekreag$estimate[trendekreag$estimate < 0.05 & trendekreag$estimate > -0.05] <- 0 # verwaarloosbaar klein
trendekreag$estimate <- round(trendekreag$estimate, digits = 2)
trendekreag$facet_wrap_code <- gsub("Ov. waterflora", "Waterflora", trendekreag$facet_wrap_code)
ekrtrendeag <- trendekreag[!is.na(trendekreag$EAGIDENT) & trendekreag$level == 1,] # alleen scores per eag op hoofdmaatlatten

saveRDS(trendekr, file = './data/trendekreag.rds')
saveRDS(trendekreag, file = './data/trendekreag.rds')
saveRDS(ekrtrend, file = './data/ekrtrend.rds')
saveRDS(ekrtrendeag, file = './data/ekrtrendeag.rds')
# write.table(trendekreag, file = paste(getwd(),"/output/OordeelTrendPerGebiedScorePerJaarWide",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

# waterquality measurements
wq  <- readRDS("../data/ImportWQ.rds") %>% as.data.table()
# water quality
wq <-  ppr_wq(db = wq, syear = 2000, wtype = eag_wl, mlocs = locaties)
saveRDS(wq, file = './data/wq.rds')

# laden: Rdata file die meegestuurd is
dat <- readRDS("../pbelasting/dat.rds") # data kan worden gecreerd obv script: loadbalances
dat[,date := as.POSIXct(paste0(jaar,"-",maand,"-01"), format = '%Y-%m-%d')]

# nonogram
nomogram <- data.table::fread('../data/nomogram.csv')
# datafile with P-load PC Ditch
Overzicht_kP <- data.table::fread('../data/Overzicht_kP.csv')
Overzicht_kP <- ppr_pcditch(db = Overzicht_kP)
pvskp <- ppr_pmaps(dat, Overzicht_kp, hybi, nomogram) 
saveRDS(pvskp, file ='./data/pvskp.rds')
