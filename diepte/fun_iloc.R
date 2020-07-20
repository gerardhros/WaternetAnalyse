#' define folder names (iloc_onedrive, iloc_project, iloc_afk)
#' 
fun_iloc <- function(fdnm = "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI"){
  
  # parent directory of one drive
  iloc_onedrive <- paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")), "/", fdnm, "/")
  
  # project folder of AGV
  iloc_project <- paste0(iloc_onedrive, "NMI_Data - Documents/project/WaternetAnalyse/")
  
  # raw data folder of alkalvig project (Job's)
  iloc_afk <- paste0(iloc_onedrive, "NMISite - 1781.N.19 Oorzaken en oplossingen afkalving sloten veenweide/ml studie/data/raw/")
  
  return(c(iloc_onedrive, iloc_project, iloc_afk))
}

