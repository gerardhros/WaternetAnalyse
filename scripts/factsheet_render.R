# factsheet rendering
# authors: Laura Moria, Sven Verweij en Gerard Ros

# clear environment
rm(list=ls())  

# Load packages and functions -------------------------------------
require(rmarkdown);require(flexdashboard)
require(knitr);require(kableExtra)

# collect all input data needed to make factsheets for all EAGs
source('scripts/factsheet_ppr.R')

# add functions
source('scripts/ppr_funs.R')

# run example
out = factsheetExtract(i=12,brondata = brondata, splot = TRUE)

# deze render werkt nog als splot=FALSE => referentie aanpassen
outputF <- "html"
rmarkdown::render(input = "factsheets/factsheets_html.Rmd", 
                  output_format = "flexdashboard::flex_dashboard", #pdf_document
                  output_file = paste("FS_", out$wlname, ".html", sep=''),
                  output_dir = "output/")

# save relavant output and run file for latex pdf
saveRDS(out,'factsheets/routput/out.rds')
setwd("factsheets")

knitr::knit2pdf("factsheets_latex.Rnw",compiler = 'pdflatex')
file.rename(from = 'factsheets_latex.pdf',to = paste0("output/FS_", out$wlname, ".pdf"))




knitr::knit("factsheets_test.Rnw", output = paste0("output/FS_", out$wlname, ".tex"))
tools::texi2pdf(paste0("output/FS_", out$wlname, ".tex"))
ofiles <- list.files(pattern='.pdf')[grep('^FS_',list.files(pattern='.pdf'))]
file.copy(from = ofiles, to = 'output',overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
ofiles <- list.files()[grep('^FS_',list.files())]
file.remove(ofiles)


outputF <- "word"
rmarkdown::render(input = "factsheets.Rmd", #of rmd of script
                  output_format = "bookdown::word_document2", #pdf_document
                  output_file = paste("FS_", wlname, ".docx", sep=''),
                  output_dir = "output/")

}