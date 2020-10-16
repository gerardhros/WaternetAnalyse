## Waterplantensoorten per gebied
# In onderstaande tabel kan zowel de gemeten soortensamenstelling (grootheid is BEDKG) als de kenmerkende soorten die de score op de deelmaatlat 'Soortensamenstelling macrofyten' bepalen (grootheid is AANWZHD) worden bekeken. De meetwaarde (numeriekewaarde) die wordt weergegeven in de grafiek geeft het waardeoordeel dat aan het voorkomen van iedere soort is gekoppeld. Dit waardeoordeel is gebaseerd op de indicatieve waarde van een soort in een bepaald watertype en de mate van voorkomen opeen 3-delige schaal. Hoe hoger de waarde, hoe hoger de score op de maatlat. De deelmaatlat soortensamenstelling waterplanten wordt berekend op basis van de som de scores van alle soorten en het aantal aangetroffen soorten uit de lijst met kenmerkende soorten (soortenrijkdom). 

selectedData3 <- EKRlijst[EKRlijst$Analysecompartiment.code %in% 'OW',
                          c('CODE','HoortBijGeoobject.identificatie','EAGIDENT','Biotaxon.naam',
                            'Biotaxon.naam.nl', 'KRWwatertype.code.y','Grootheid.code','jaar',
                            'Numeriekewaarde', 'Waardebepalingsmethode.code')]

selectedData2 <- selectedData3[selectedData3$Grootheid.code %in% c('AANWZHD','BEDKG','SOORTRDM') & !is.na(selectedData3$Biotaxon.naam),]

'1_somScorePlanten' -> selectedData2$Biotaxon.naam[selectedData2$Biotaxon.naam == 'Macrofyten' & selectedData2$Grootheid.code == 'AANWZHD']
'2_gemiddeldeSoortenrijkdomPlanten' -> selectedData2$Biotaxon.naam[selectedData2$Biotaxon.naam == 'Macrofyten' & selectedData2$Grootheid.code == 'SOORTRDM']

rpivotTable(
  selectedData2,
  rows = c("Grootheid.code","Biotaxon.naam", "Waardebepalingsmethode.code"),
  cols = c("jaar"),
  aggregatorName = "Average",
  inclusions = list(HoortBijGeoobject.identificatie = list("NL11_Botshol")),
  exclusions= list( Grootheid.code = list( "BEDKG")),
  vals = "Numeriekewaarde",
  rendererName = "Heatmap"
)
