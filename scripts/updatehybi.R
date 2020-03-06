# update hybi
#data 2019 toevoegen
locaties <- fread('../data/Location.csv')
twn <- fread('../hydrobiologie/biotaxon.csv')
hybi2019 <- read.csv("../development/hybi2019.csv", header = TRUE, na.strings = " ", sep=";", dec =",", stringsAsFactors = F)
hybi2019 <- hybi2019%>%as.data.table()
hybi2019[, datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
hybi2019[, jaar := year(datum)]
hybi2019[limietsymbool == '<', meetwaarde := meetwaarde * 0.5]
hybi2019 <- merge(hybi2019,locaties[,c('CODE','EAGIDENT',"OWMIDENT")],by.x ='locatiecode', by.y = 'CODE')
hybi2019 <- merge(hybi2019,eag_wl[,c('watertype','GAFIDENT')],by.x ='EAGIDENT', by.y = 'GAFIDENT')

# remove some columns based on judgement Gerard
cols <- c('TWN.naam','TWN.taxontype','TWN.taxonlevel','WNA.nederlandse.soortnaam','TWN.taxongroup',"beschermingsstatus.florafaunawet",'WNA.oeverplantensoorten','WNA.emerse.plantensoorten','WNA.soorten.met.drijvend.blad','WNA.kroossoorten','WNA.onderwaterplantensoorten','WNA.stromingsgilde.vis','WNA.migratiegilde.vis','WNA.habitatvoorkeur.vis','WNA.plantminnendheid.vis','WNA.reynolds.groep.cyanobacterieen','WNA.zuurstoftolerante.vis','WNA.fytoplankton.groep','WNA.fytoplankton.subgroep','WNA.toxische.blauwalg')
# ensure that cols are present in colnames db
cols[cols %in% colnames(hybi2019)]
# remove columns en merge opnieuw aan TWN
hybi2019[,c(cols):=NULL]
hybi2019 <- merge(hybi2019,twn[,c('code','naam','TWN.taxontype','TWN.taxonlevel','WNA.nederlandse.soortnaam','TWN.taxongroup',"beschermingsstatus.florafaunawet",'WNA.oeverplantensoorten','WNA.emerse.plantensoorten','WNA.soorten.met.drijvend.blad','WNA.kroossoorten','WNA.onderwaterplantensoorten','WNA.stromingsgilde.vis','WNA.migratiegilde.vis','WNA.habitatvoorkeur.vis','WNA.plantminnendheid.vis','WNA.reynolds.groep.cyanobacterieen','WNA.zuurstoftolerante.vis','WNA.fytoplankton.groep','WNA.fytoplankton.subgroep','WNA.toxische.blauwalg')],by.x ='biotaxonnaam', by.y = 'code', all.x=T)

# add codes (is this really needed?)
hybi2019[,locatie.EAG := EAGIDENT]
hybi2019[,locatie.KRW.watertype := watertype]
hybi2019[,locatie.KRWmeetpuntlocatie := OWMIDENT]
hybi2019[,TWN.naam := naam]
hybi2019[,c("EAGIDENT","watertype","OWMIDENT","naam"):=NULL]

#data tm 2018
hybi <-  read.csv("../development/alles_reliable.csv", header = TRUE, na.strings = " ", sep=";", dec =".", stringsAsFactors = F)%>%as.data.table()
hybi[, datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
hybi[, jaar := year(datum)]
hybi[limietsymbool == '<', meetwaarde := meetwaarde * 0.5]
hybi[fewsparameter == 'WATDTE_m' & jaar == 2006 & planvanaanpak == 'PVA MAFY LIJN 2006 WP', meetwaarde := meetwaarde * 0.1]
hybi <- merge(hybi,locaties[,c('CODE','EAGIDENT',"OWMIDENT")],by.x ='locatiecode', by.y = 'CODE', all.x = T) # niet alle vis locaties zitten in location
hybi <- merge(hybi,eag_wl[,c('watertype','GAFIDENT')],by.x ='EAGIDENT', by.y = 'GAFIDENT', all.x=T)

# remove some columns based on judgement Gerard
cols <- c('TWN.naam','TWN.taxontype','TWN.taxonlevel','WNA.nederlandse.soortnaam','TWN.taxongroup',"beschermingsstatus.florafaunawet",'WNA.oeverplantensoorten','WNA.emerse.plantensoorten','WNA.soorten.met.drijvend.blad','WNA.kroossoorten','WNA.onderwaterplantensoorten','WNA.stromingsgilde.vis','WNA.migratiegilde.vis','WNA.habitatvoorkeur.vis','WNA.plantminnendheid.vis','WNA.reynolds.groep.cyanobacterieen','WNA.zuurstoftolerante.vis','WNA.fytoplankton.groep','WNA.fytoplankton.subgroep','WNA.toxische.blauwalg')
# ensure that cols are present in colnames db
cols[cols %in% colnames(hybi)]

# remove columns en merge opnieuw aan TWN
hybi[,c(cols):=NULL]
hybi <- merge(hybi,twn[,c('code','naam','TWN.taxontype','TWN.taxonlevel','WNA.nederlandse.soortnaam','TWN.taxongroup',"beschermingsstatus.florafaunawet",'WNA.oeverplantensoorten','WNA.emerse.plantensoorten','WNA.soorten.met.drijvend.blad','WNA.kroossoorten','WNA.onderwaterplantensoorten','WNA.stromingsgilde.vis','WNA.migratiegilde.vis','WNA.habitatvoorkeur.vis','WNA.plantminnendheid.vis','WNA.reynolds.groep.cyanobacterieen','WNA.zuurstoftolerante.vis','WNA.fytoplankton.groep','WNA.fytoplankton.subgroep','WNA.toxische.blauwalg')],by.x ='biotaxonnaam', by.y = 'code', all.x =T)

# add codes (is this really needed?)
hybi[,locatie.EAG := EAGIDENT]
hybi[,locatie.KRW.watertype := watertype]
hybi[,locatie.KRWmeetpuntlocatie := OWMIDENT]
hybi[,TWN.naam := naam]
hybi[,c("EAGIDENT","watertype","OWMIDENT","naam"):=NULL]

hybi<- smartbind(hybi2019,hybi)%>%as.data.table()
hybi[, datum := as.Date(datum, format = "%Y-%m-%d")]
hybi <- saveRDS(hybi, file = '../data/alles_reliable.rds')

