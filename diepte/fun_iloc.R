#' define folder names (iloc_onedrive, iloc_project, iloc_afk)
#' 
fun_iloc <- function(fdnm = "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI/Sven Verweij - NMI-DATA"){
  
  # parent directory of one drive
  iloc_onedrive <- paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")), "/", fdnm, "/")
  
  # project folder of AGV
  iloc_project <- paste0(iloc_onedrive, "/project/WaternetAnalyse/")
  
  # raw data folder of alkalvig project (Job's)
  iloc_afk <- "C:/Users/gerard.ros.nmi/Agrocares/NMISite - Documenten/Projecten/O 1700 - O 1800/1781.N.19 Oorzaken en oplossingen afkalving sloten veenweide/ml studie/data/raw/"
  
  return(c(iloc_onedrive, iloc_project, iloc_afk))
}

