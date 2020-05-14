# Get working directory of data (iloc, iloc_onedrive, iloc_obic)

getwd <- function(fdnm =  "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI"){
  # parent directory of one drive
  iloc_onedrive <- paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")), "/", fdnm, "/")
  
  # Data directory of the project
  iloc_project <- paste0(iloc_onedrive, "NMI_Data - Documents/project/WaternetAnalyse/")
  
  
  return(iloc_project)
}

