# welke databases worden gekoppeld

# database 1. EKRset: bevat EKR scores per meetpunt

  # meetpunt identificatie (column: Identificatie), where code before first underscore matches CODE in locaties
  # meetpunt coordinaten X en Y  (columns: XCOORD, YCOORD)
  # EKR score (column: numerieke waarde)
  # maatlat (column: Waardebepalingsmethode.code, welke maatlat is gebruikt)
  # datum (column: datum)
  # GEP (columns GEP en GEP_2022)
  # gebiedsinfo: EAGIDENT, GAFNAAM, KRW Watertype.code, KRW_SGBP3 

# database 2. dat: bevat gegevens van alle waterbalansen EAG en GAF op maandniveau

  # datum (column: date)
  # waterfluxen in via kwel, neerslag, riolering, drain, uitspooeling (column: starts with w_i_)
  # waterfluxen out via verdamping, intrek (column: starts with w_o)
  # fosfaatfluxen minimaal (columns start with wp_min) en increment (columns start with wp_inc)
  # arealen per 'bakje' (column: starts with a_)
  # debieten en peil (column : w_debiet en w_peil)
  # gemeten P belasting (column: starts with wp_meting)
  # gebiedsinfo: GAF, EAG, watertype, KRW
  # gemiddelde N en P bemesting uit kunstmest en dierlijke mest en P pools (column start with  i_N en i_P)
  # meest voorkomende bodemtype, Gt en gewas (columns start with i_bt, i_GWT, i_VEG en i_GEW)

# database 3. hybi: bevat hydrobiological data op individuele meetpunten

  # meetsample identificatie (column: monsterident)
  # meetsample identificatie2 (column: locatiecode)
  # meetpunt coordinaten (columns xcoormonster, ycoormonster)
  # datum (column: datum)
  # compartiment: ow voor oppervlaktewater
  # parameter analyzed (column: fewsparameter) 
  # meetwaarde (column: meetwaarde)
  # eenheid meetwaarde (column: eenheid)
  # gebiedsinfo: locatie.EAG, locatie.KRW.watertype, KRW

# database 4. wq : bevat waterkwaliteitsparameters op individuele meetpunten

  # meetsample identificatie (column: monsterident)
  # meetsample identificatie2 (column: locatiecode)
  # meetpunt coordinaten (columns locatie.x, locatie.y)
  # datum (column: datum)
  # parameter analyzed (column: fewsparameter) 
  # meetwaarde (column: meetwaarde)
  # eenheid meetwaarde (column: eenheid)
  # gebiedsinfo: GAF, EAGIDENT, locatie.KRW.watertype, KRW, watertype, OWMIDENT

# database 5. Locaties : bevat locatiegegevens van databases wq, hybi en bod

  # meetsample identificatie (column CODE : 11178 unieke meetpunten, corresponds to wq_monsterident)
  # meetpunt coordinaten (columns XCOORD, YCOORD)
  # gebiedsinfo: EAGIDENT, OWMIDENT, GAFIDENT, watertype

# database 6. eag_wl : bevat gegevens van waterlichamen en EAGs

  # gebiedsinfo for 386 polygonen: GAFIDENT, KRW_SGBP3
  # oppervlakte (column oppWater, oppland, omtrekwater)
  # watertype (column: Plas watertype)

# database 7. doelen : doelen per maatlat en gebied

  # doel score (column: Doel)
  # maatlat (column: GHPR)
  # gebiedsinfo: EAGcode (column: gebied) en KRW waterlichaam (GeoObject.Code en HoortbijGeoobject.identificatie)

# database 8. gEAG : Ruimtelijke shapefile met informatie van EAGS

  # gebiedsnaam (column: GAFIDENT)

# database 9. bod : Informatie slootbodem analyses

  # meetsample identificatie (ccolumn: monsterident)
  # meetsample identificatie2 (column: loc.code)
  # datum (column: datum)
  # meetpunt coordinaten (columns xcoormonster, ycoormonster OR loc.x and loc.y?)
  # parameter analyzed (column: fewsparameter) 
  # meetwaarde (column: meetwaarde)
  # eenheid meetwaarde (column: eenheid)
  # gebiedsinfo: EAGIDENT, watertype

# database 10. Overzicht_kp : datafile with P-load PC Ditch

  # gebiedsinfo: (columns EAG, GAF)
  # properties: PLV, strijklengte, debiet, fr_moeras, diepte, p_bel_year, PC Ditch omslaggrenzen


