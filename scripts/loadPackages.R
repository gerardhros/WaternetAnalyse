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

# packages(tint)
# packages(ggpubr)
# packages(ggplotify)
# packages(grid)
# packages(gridExtra)
# packages(gganimate)
# packages(pdftools)
# packages(tidyverse)
# packages(tidyr)
packages(manipulateWidget)
packages(rpivotTable)
# packages(mapview)
# packages(readxl)
# packages(stringr)
packages(plyr) #voor polarbarchart
packages(dplyr)
packages(sp) # zit ook in raster # voor CRS projectie functie
# packages(ggthemes)
# packages(mapproj)
# packages(knitr)# pdf maken van tabellen
# packages(devtools)
# packages(ggseas)
# packages(scales)
packages(broom)

packages(leaflet)
packages(plotly)
# packages(rgdal)
# packages(shinythemes)
# packages(ggmap)
packages(gtools) #voor smartbind
packages(kableExtra)
# packages(knitr)
# packages(gridExtra)
# packages(bibtex)
# packages(knitcitations)
# packages(webshot)
# #packages(PhantomJS)
# packages(nlgeocoder)
# packages(leaflet.extras)
packages(sf)
packages(zoo)
# packages(processx)

packages(rmarkdown)
# packages(bookdown)
packages(RColorBrewer)
# packages(corrplot)
# packages(raster)
packages(ggplot2)
packages(flexdashboard)
packages(flextable)
packages(officer) #opmaak in flextable
packages(DT)
packages(data.table)