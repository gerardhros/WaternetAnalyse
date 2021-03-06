# factsheet rendering
# authors: Laura Moria, Sven Verweij en Gerard Ros

# clear environment
rm(list=ls())

# Load packages and functions -------------------------------------
require(rmarkdown);require(flexdashboard)
require(knitr);require(kableExtra)
require(dplyr) ;require(stringr)
# load required packages for these factsheetppr
require(sf);require(data.table)
require(magrittr);require(ggplot2)
require(plotly)
require(grid)
require(gridExtra)

# add functions
source('scripts/ppr_funs.R')

# collect all input data needed to make factsheets for all EAGs
source('scripts/factsheet_ppr.R')

# add functions again omdat factsheets.ppr ze weer wist
source('scripts/ppr_funs.R')

# run for all files

for(eagnr in c(178:nrow(brondata$ESFoordelen))){#5, 13, 32 : nrow(brondata$ESFoordelen)){ 

  # voor debug attach(brondata) detach(brondata)
  # pdf gaat mis bij veel fs
  # eagnr <- 47 gansenhoef
  # eagnr <- 199 
  
  # collect the data for that specific water body / EAG / GAF
  out = factsheetExtract(i=eagnr, brondata = brondata, splot = TRUE)
  if(identical(out$wlname, character(0)) |nrow(out$d3)== 0|!is.character(out$ESTnaam)){next}
  
  # #render the html flexdashboard
  outputF <- "html"
  # rmarkdown::render(input = "factsheets/factsheets_html.Rmd",
  #               output_format = "flexdashboard::flex_dashboard", #pdf_document
  #               output_file = paste("FS_", out$my_title2, ".html", sep=''),
  #               output_dir = "factsheets/output/")
  
  # output voor domeinen, K3: naam met EAG_ en KRWIDENT_
  titel <- paste(out$eagwl$GAFIDENT, collapse = "_")
  if(!out$eagwl$KRW_SGBP3 == ""){
  titel2 <- paste(out$waterlichamenwl$OWL_SGBP3, collapse = '_')
  titel <- paste(titel2, titel, sep = '_', collapse = '_')}
  titel <- gsub(", ","_",titel)
  titel <- gsub(",","_",titel)
  rmarkdown::render(input = "factsheets/factsheets_html.Rmd",
                    output_format = "flexdashboard::flex_dashboard", #pdf_document
                    output_file = paste("FS_", titel, ".html", sep=''),
                    output_dir = "factsheets/output/")

  # save relavant output and run file for latex pdf
  # saveRDS(out,'factsheets/routput/out.rds')

  # # change working directory (needed for knit2pdf)
  # setwd("factsheets")
  # # 
  # # # make the pdf file
  # knitr::knit2pdf("factsheets_latex_v2.Rnw",compiler = 'pdflatex')
  # # 
  # # # copy the pdf to the correct directory (factsheets/output)
  # file.rename(from = 'factsheets_latex_v2.pdf',to = paste0("output/FS_", tolower(out$my_title2), ".pdf"))
  # # 
  # # # reset working directory
  #  setwd('../')
}


# helper functie (not used yet)
rm_factsheets <- function(x, brondata){

  # make local extraction for a polder/ water body
  suppressWarnings(
  out = factsheetExtract(i=x,brondata = brondata, splot = TRUE)
  )
  # render the html flexdashboard
  outputF <- "html"
  rmarkdown::render(input = "factsheets/factsheets_html.Rmd",
                    output_format = "flexdashboard::flex_dashboard", #pdf_document
                    output_file = paste("FS_", out$my_title2, ".html", sep=''),
                    output_dir = "factsheets/output/", quiet = TRUE)

  print(paste0('factsheet for polder ',out$my_title2,' is finished'))

}


outputF <- "word"
rmarkdown::render(input = "factsheets.Rmd", #of rmd of script
                  output_format = "bookdown::word_document2", #pdf_document
                  output_file = paste("FS_", wlname, ".docx", sep=''),
                  output_dir = "output/")

