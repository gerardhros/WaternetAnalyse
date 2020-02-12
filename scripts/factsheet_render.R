# factsheet rendering
# authors: Laura Moria, Sven Verweij en Gerard Ros

# Load packages and functions -------------------------------------
source('scripts/loadPackages.R')
source('scripts/factsheetfuncties.R')



# collect all input data needed to make factsheets for all EAGs
source('scripts/factsheet_ppr.R')


test = factsheetExtract(i=37,brondata = brondata)
