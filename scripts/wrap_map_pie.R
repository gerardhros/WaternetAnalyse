# This script wraps the scripts to make map of polygons with piechart


# clear environment
rm(list=ls())


library(ggplot2); library(sf); library(data.table); library(dplyr); 
library(scatterpie); library(ggnewscale); library(tmap); library(cowplot); library(ggspatial)

source("../scripts/fun_map_pie.R")

# load dataset of KRW 
krwset <- readRDS("../data/krwset.rds") # is ekrscores1
# ovwatset <- readRDS("data/ovwatset.rds")

# load map of waterlichaam
WL_EAG <- st_read("../data/WL_EAG.gpkg") %>% st_transform(28992)

# load polygons of EAG (to use for background)
eag <- st_read("../data/EAG20191205.gpkg") %>% st_transform(28992)

# Threshold values of EKR for 4 levels, separately defined for 4 EKR type.
#'Dimension should be 4 rows (= EKR type) x 5 columns (= 5 breaks for 4 levels)
ekr_breaks = rbind(c(0, 0.22, 0.5, 0.7, 1), #"Fytoplankton
                    c(0, 0.2, 0.4, 0.65, 1), #Overige waterflora
                    c(0, 0.2, 0.4, 0.6, 1), #Macrofauna
                    c(0, 0.21, 0.4, 0.68, 1)) #Vis

#ekr_breaks = matrix(rep(c(0, 0.25, 0.5, 0.75, 1), each = 4), ncol = 5)


# Make a map with pie chart (based on the numerical variable "EKR") and save as JPEG
gp_c <- draw_map(WL_EAG, krwset, bkpl = eag,
                 col_ekr = "EKR", ekr_fac = FALSE, 
                 col_area_sf = "SGBP3_NAAM", col_area_dt = "waterlichaam",
                 ekr_breaks = ekr_breaks,
                 filename = "map_EKR_waterlichaam.jpg")

# Make a map with pie chart  (based on the factor variable "oordeel_2022") and save as JPEG
gp_c <- draw_map(WL_EAG, krwset, bkpl = eag,
                 col_ekr = "oordeel_2022",ekr_fac = TRUE, 
                 col_area_sf = "SGBP3_NAAM", col_area_dt = "waterlichaam",
                 filename = "map_oordeel_2022_waterlichaam.jpg")
