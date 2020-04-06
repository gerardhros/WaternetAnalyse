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

# run for all files
for(eagnr in 17 : 17){
  
  # collect the data for that specific water body / EAG / GAF
  out = factsheetExtract(i=eagnr,brondata = brondata, splot = TRUE)
  
  # render the html flexdashboard
  outputF <- "html"
  rmarkdown::render(input = "factsheets/factsheets_html.Rmd", 
                    output_format = "flexdashboard::flex_dashboard", #pdf_document
                    output_file = paste("FS_", out$wlname, ".html", sep=''),
                    output_dir = "factsheets/output/")
  
  # save relavant output and run file for latex pdf
  saveRDS(out,'factsheets/routput/out.rds')
  
  # change working directory (needed for knit2pdf)
  setwd("factsheets")
  
  # make the pdf file
  knitr::knit2pdf("factsheets_latex.Rnw",compiler = 'pdflatex')
  
  # copy the pdf to the correct directory (factsheets/output)
  file.rename(from = 'factsheets_latex.pdf',to = paste0("output/FS_", out$wlname, ".pdf"))
  setwd('../')
  
}



outputF <- "word"
rmarkdown::render(input = "factsheets.Rmd", #of rmd of script
                  output_format = "bookdown::word_document2", #pdf_document
                  output_file = paste("FS_", wlname, ".docx", sep=''),
                  output_dir = "output/")

