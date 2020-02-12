# factsheet rendering
# authors: Laura Moria, Sven Verweij en Gerard Ros

# clear environment
rm(list=ls())  

# Load packages and functions -------------------------------------
source('scripts/loadPackages.R')

# collect all input data needed to make factsheets for all EAGs
source('scripts/factsheet_ppr.R')

# add functions
source('scripts/factsheetfuncties.R')

i = 37
out = factsheetExtract(i=37,brondata = brondata)

outputF <- "html"
rmarkdown::render(input = "factsheets/factsheets_new.Rmd", 
                  output_format = "flexdashboard::flex_dashboard", #pdf_document
                  output_file = paste("FS_", out$wlname, ".html", sep=''),
                  output_dir = "output/")

out = out[c("waterlichamenwl","wlname", "my_title","eagwl",'deelgebieden',
            'd3','d3_deel','d3$deelptn','ESTnaam','mapEAG','mapDEELGEBIED',
            'mapEKR','ESFtab','maatregelen1','maatregelen2')]
saveRDS(out,'factsheets/routput/out.rds')


for (i in 1:nrow(ESFoordelen))
  
  
  


## Render preview
webshot::webshot(
  url = paste("output/FS_", wlname, ".html", sep=''),
  file = paste("output/FS_", wlname, ".png", sep=''),
  vwidth = 3408,
  vheight = 2400,   # Use A series aspect ratio
  delay = 1,        # Wait to allow all element to load
  zoom = 2.5,       # Adjust elements relative size
  debug = FALSE)

# hoi <- png::readPNG(paste("output/FS_", wlname, ".png", sep=''))
# pdf("hoi.pdf", paper="a4", width = 8.27, height = 11.64)
# grid.raster(hoi, width = unit(8, "inches"), height = unit(11, "inches"))
# dev.off()

outputF <- "word"
rmarkdown::render(input = "factsheets.Rmd", #of rmd of script
                  output_format = "bookdown::word_document2", #pdf_document
                  output_file = paste("FS_", wlname, ".docx", sep=''),
                  output_dir = "output/")

}