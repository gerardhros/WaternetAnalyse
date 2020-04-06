# Load packages 

# nodig voor xlsx package
options(java.parameters = "-Xmx36000m") 

# function to check whether package is installed
packages <- function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,dependencies = T)
    require(x,character.only=TRUE)
  } }

# Load packages
packages(sf)
packages(rgdal) # voor inlezen shape
packages(gtools) # voor smartbind
packages(zoo) # lineaire interpolatie makepmaps
packages(dplyr)
packages(data.table)
packages(magrittr)
packages(ggplot2)
packages(grid)
packages(gridExtra)
packages(stringr)
packages(rmarkdown);packages(flexdashboard)
packages(knitr);packages(kableExtra)
packages(tidyr)
