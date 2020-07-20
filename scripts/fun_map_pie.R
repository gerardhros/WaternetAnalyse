#' Draw a map with pie chart, legend, and a map of whole Netherlands
#' 
#' This function makes a layout of three figures: a map with pie chart, legend, and a map of NL.
#' The final map is saved as JPEG.
#' 
#'@param pl (sf object) polygon object
#'@param dt (data table) table containing EKR data to color pie chart
#'@param bkpl (sf object) polygon object to insert as background. Should have the same coordinate system as pl.
#'When NULL, background polygons are not drawn.
#'@param col_ekr (CHAR) a column name of pl which gives EKR values (either numeric or factor). 
#'options are  "EKR" (fraction between 0 - 1) or "oordeel_2022" (slecht/matig/..)
#'@param ekr_fac (boolean) whether the EKR variable ('col_ekr') is factor (TRUE) or numerical (FALSE)
#'@param ekr_order (CHAR) 4 levels of EKR, ordered from bad to good. 
#'When ekr_facb = TRUE, this should match with the levels of 'col_ekr'.
#''@param ekr_col (CHAR) 4 colors of EKR, ordered from bad to good. 
#'@param ekr_breaks (NUM) Threshold values of EKR for 4 levels, separately defined for 4 EKR type.
#'Dimension should be 4 rows (= EKR type) x 5 columns (= 5 breaks for 4 levels).
#'This argument is needed only when EKR variable is numerical (i.e. ekr_fac = FALSE).
#'@param col_area_sf (CHAR) column name of pl for area names.
#'@param col_area_dt (CHAR) column name of dt for area names.
#' The levels of area name should match with those of pl.
#'@param col_ekrtype (CHAR) column name of dt in which type of EKR is stored
#'@param label_type (CHAR) Names of EKR types. THe order of types is: levels(as.factor(dt$col_ekrtype))
#'@param filename (CHAR) filename of output figure. When NULL, the figure is not saved.
#'
#'@import ggplot2
#'@import data.table
#'@import sf
#'@import dplyr
#'@import ggplot2
#'@import scatterpie
#'@import cowplot
#'@import ggspatial
#'@import ggnewscale
#'@import tmap
#'
#'
#'@return gp_c (ggplot object) map with pie chart + legend + map of whole Netherlands

draw_map <- function(pl, dt, bkpl = NULL,
                     col_ekr = "EKR", 
                     ekr_fac = FALSE,  
                     ekr_order = c("slecht","ontoereikend","matig","goed"), 
                     ekr_col = c("red", "orange", "yellow", "green"),
                     ekr_breaks = matrix(rep(c(0, 0.25, 0.5, 0.75, 1), each = 4), ncol = 5),
                     col_area_sf = "SGBP3_NAAM", col_area_dt = "waterlichaam", 
                     col_ekrtype = "GHPR_level",
                     label_type = c("Fytoplankton", "Overige waterflora", "Macrofauna", "Vis"),
                     filename = NULL){
  
  # Check if all necessary columns are included
  if(!col_area_sf %in% colnames(pl)){
    print("WARNING: The polygon data does not include the correct column name of area ID: col_aea_sf")
  }
  if(!col_area_dt %in% colnames(dt)){
    print("WARNING: The datatable does not include the correct column name of area ID: col_aea_dt")
  }
  if(!col_ekrtype %in% colnames(dt)){
    print("WARNING: The datatable does not include the correct column name of EKR type: col_ekrtype")
    
  }
  
  # Make a map with pie chart
  gp <- make_map_pie(pl, dt, bkpl = bkpl, col_ekr = col_ekr, ekr_fac = ekr_fac,  ekr_order = ekr_order, 
                     ekr_col = ekr_col, ekr_breaks = ekr_breaks, col_area_sf = col_area_sf, 
                     col_ekrtype = col_ekrtype, label_type = label_type)
  
  # Make NL map with boundary box 
  nl <- make_nl_box(pl)
  
  # Make legend 
  leg <- make_legend()
  
  # combine multiple plots
  gp_c <- cowplot::ggdraw() +
    draw_plot(gp, x = 0, y = 0, width = .7, height = 1) +
    draw_plot(leg, x = 0.7, y = 0.6, width = .27, height = .35) +
    draw_plot(nl, x = 0.7, y = 0.1, width = .3, height = .4, hjust = 0.2) 
  #show(gp_c) # when this does not work, try "dev.off()"
  
  # save map
  if(!is.null(filename )){
    ggsave(gp_c, filename = filename, 
           width = 12, height = 8, unit = "in")
  }
  
  return(gp_c)
}



#'Make a map of polygons and piechart on each polygon centers
#'
#'This function makes a map of given polygons, colored with minimum value of EKR of each area. 
#'When multiple polygons have same area names, they are dissolved. 
#'In the center of each area polygons, a pie chart of 4 elements is drawn. 
#'The pie chart is colored based on the EKR value of each EKR type.
#'
#'@param pl (sf object) polygon object
#'@param dt (data table) table containing EKR data to color pie chart
#'@param bkpl (sf object) polygon object to insert as background. Should have the same coordinate system as pl.
#'#'When NULL, background polygons are not drawn.
#'@param col_ekr (CHAR) a column name of pl which gives EKR values (either numeric or factor). 
#'options are  "EKR" (fraction between 0 - 1) or "oordeel_2022" (slecht/matig/..)
#'@param ekr_fac (boolean) whether the EKR variable ('col_ekr') is factor (TRUE) or numerical (FALSE)
#'@param ekr_order (CHAR) 4 levels of EKR, ordered from bad to good. 
#'When ekr_facb = TRUE, this should match with the levels of 'col_ekr'.
#''@param ekr_col (CHAR) 4 colors of EKR, ordered from bad to good. 
#'@param ekr_breaks (NUM) Threshold values of EKR for 4 levels, separately defined for 4 EKR type.
#'Dimension should be 4 rows (= EKR type) x 5 columns (= 5 breaks for 4 levels).
#'This argument is needed only when EKR variable is numerical (i.e. ekr_fac = FALSE).
#'@param col_area_sf (CHAR) column name of pl for area names.
#'@param col_area_dt (CHAR) column name of dt for area names.
#' The levels of area name should match with those of pl.
#'@param col_ekrtype (CHAR) column name of dt in which type of EKR is stored
#'@param label_type (CHAR) Names of EKR types. THe order of types is: levels(as.factor(dt$col_ekrtype))
#'
#'@import ggplot2
#'@import data.table
#'@import sf
#'@import dplyr
#'@import ggplot2
#'@import scatterpie
#'@import cowplot
#'@import ggspatial
#'@import ggnewscale
#'
#'@return gp (ggplot object) map of polygons with pie chart
#'

make_map_pie <- function(pl, dt, bkpl = NULL,
                         col_ekr = "EKR", ekr_fac = FALSE,  
                         ekr_order = c("slecht","ontoereikend","matig","goed"), 
                         ekr_col = c("red", "orange", "yellow", "green"),
                         ekr_breaks = matrix(rep(c(0, 0.25, 0.5, 0.75, 1), each = 4), ncol = 5),
                         col_area_sf = "OWMIDENT", col_area_dt = "KRW_SGBP3", 
                         col_ekrtype = "GHPR_level",
                         label_type = c("Fytoplankton", "Overige waterflora", "Macrofauna", "Vis")){
  
  # make copy
  dt <- copy(dt) # dt <- copy(ekr_scores1) 
  pl <- copy(pl) # pl <- copy(gKRW)
  
  # change column names
  setDT(pl)
  setnames(pl, old = col_area_sf, new = "areaID_pl")
  setnames(dt, old = c(col_ekr, col_area_dt, col_ekrtype), new = c("ekrval", "areaID_dt", "type_krw"))
  pl <- st_as_sf(pl)
  
  # Give warning if there are multipl records of EKR per area
  if(max(table(dt$areaID_dt, dt$type_krw)) > 1){
    print("Warning: there are duplicate records of EKR for some areas in the data table!")
    print("The last record is used for further analysis.")
  }
  
  ## Dissolve polygons with same area IDs ---------
  pl <- pl %>% group_by(areaID_pl) %>% summarize()
  
  
  ## Join Data table to map ----
  if(ekr_fac == TRUE){
    # when EKR is factor, convert that to integer 1 - 4
   dt$ekrval <- as.numeric(factor(as.factor(dt$ekrval), levels = ekr_order))
  }
  
  #dcast table
  dt_dc <- dcast(dt[, .(areaID_dt, type_krw, ekrval)],
                     areaID_dt ~  type_krw,
                     fun.aggregate = last,
                     value.var = 'ekrval',fill=TRUE)
  #print(paste0(names(dt_dc)[-1], " are changed to cat", 1:4))
  setnames(dt_dc, old = names(dt_dc),
           new = c( "areaID_dt", "cat1", "cat2", "cat3", "cat4"))
  if(ekr_fac ==FALSE){
    # when EKR variable is numerical, convert it to integer (1:4)
    dt_dc[,  cat1 := as.integer(cut(cat1, ekr_breaks[1,], labels = 1:4))]
    dt_dc[,  cat2 := as.integer(cut(cat2, ekr_breaks[2,], labels = 1:4))]
    dt_dc[,  cat3 := as.integer(cut(cat3, ekr_breaks[3,], labels = 1:4))]
    dt_dc[,  cat4 := as.integer(cut(cat4, ekr_breaks[4,], labels = 1:4))]
  }
  # join
  setDT(pl)
  pl <- merge(pl,dt_dc, by.x = "areaID_pl", by.y = "areaID_dt", all.x = TRUE)
  
  
  # Compute minimum EKR value 
  pl[, min_ekr := pmin(cat1, cat2, cat3, cat4, na.rm = T)]

  # Convert EKR values of each type into color
  pl[,  cat1_col := as.character(cut(cat1, 0:4+0.5, labels = ekr_col))]
  pl[,  cat2_col := as.character(cut(cat2, 0:4+0.5, labels = ekr_col))]
  pl[,  cat3_col := as.character(cut(cat3, 0:4+0.5, labels = ekr_col))]
  pl[,  cat4_col := as.character(cut(cat4, 0:4+0.5, labels = ekr_col))]
  # replace NA with white
  cols = c("cat1_col", "cat2_col", "cat3_col", "cat4_col")
  pl[,  (cols) := lapply(.SD, function(x) ifelse(is.na(x), "white", x)), .SDcols = cols]
  
  pl <- st_as_sf(pl)
  
  
  ## Make a data table for pie chart ----
  # get centroid of polygons
  pl <- mutate(pl, ID = 1:nrow(pl))
  wl_cent <- as.data.table(st_coordinates(st_centroid(pl)))
  wl_cent[, ID := 1:nrow(wl_cent)]
  # merge
  dt_pie <- merge(as.data.table(pl)[, .(ID, cat1_col,cat2_col,cat3_col,cat4_col)], wl_cent, by = 'ID')
  #add dummy columns for piechart 
  dt_pie[, (c("d1", "d2", "d3", "d4")) := 1] 
  dt_pie[, r := 500] 
  
  ## Draw main map----

  # Araw background polygon
  if(!is.null(bkpl)){
    gpbk <- ggplot() + geom_sf(data = eag, fill = alpha("white", alpha = 0), col = "gray") 
  } else {
    gpbk <- ggplot() 
  }
  
  # Add polygons of each area with color of minimum EKR value
  gpbs <-  gpbk + 
    geom_sf(data = pl, aes(fill = as.factor(min_ekr)), 
            color = "black", show.legend = FALSE) +
    scale_fill_manual(values = c("1" =  ekr_col[1],
                                 "2" = ekr_col[2],
                                 "3" = ekr_col[3],
                                 "4" = ekr_col[4])) +
    theme_void() +
    # add north
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
    # add scale bar
    ggspatial::annotation_north_arrow(location = "bl",which_north = "true", 
                           pad_x = unit(0, "in"), pad_y = unit(0.3, "in"),
                           height = unit(0.5, 'in'), width = unit(0.5, 'in'), 
                           style = north_arrow_fancy_orienteering)
  
  gp <- gpbs
  ## Add pie chart (same proportions but different colors)
  #for (i in 1:2){
  for (i in 1:nrow(dt_pie)){
    gp <- gpbs + 
      ggnewscale::new_scale_fill() +
      scatterpie::geom_scatterpie(aes(x=X, y=Y, r = r), data = dt_pie[i, ],
                                  show.legend = FALSE,
                                  cols=c("d1", "d2", "d3", "d4")) +
      scale_fill_manual(values = c("d1" = dt_pie$cat1_col[i],
                                   "d2" = dt_pie$cat2_col[i],
                                   "d3" = dt_pie$cat3_col[i],
                                   "d4" = dt_pie$cat4_col[i]))
  }

  return(gp)
}



#'Make legend of 4 colors and pie 
#'@param label_type (CHAR) Names of EKR types.
#'@param ekr_order (CHAR) 4 levels of EKR, ordered from bad to good. 
#'@param ekr_col (CHAR) 4 colors of EKR, ordered from bad to goo
#'
#'@import ggplot
#'
#'@return leg (ggplot object) 
make_legend <- function(label_type = c("Fytoplankton", "Overige waterflora", "Macrofauna", "Vis"),
                        ekr_order = c("slecht","ontoereikend","matig","goed"), 
                        ekr_col = c("red", "orange", "yellow", "green")){
  #label_type <- c("Fytoplankton", "Overige waterflora", "Macrofauna", "Vis")
  
  # margin
  par(mar = c(0,0,0,0), oma =c(0,0,0,0))
  
  r <- 0.5 #radius
  c <- c(3, 1) #center
  leg <- ggplot() + 
    ggforce::geom_circle(aes(x0 = c[1], y0 = c[2], r = r), size =0.8) +
    geom_line(aes(x = c(c[1]-r, c[1]+r), y = c(c[2], c[2])), size = 0.8) +
    geom_line(aes(x = c(c[1], c[1]), y = c(c[2]+r, c[2]-r)), size = 0.8) +
    geom_text(aes(x = c(3.8, 3.8, 2.2, 2.2), 
                  y = c(1.5, 0.5, 0.5, 1.5), label = label_type),
              hjust = c("left", "left", "right", "right"), show.legend = F, 
              position = position_dodge(width=0.9), size = 4.5) + 
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 3, ymax = 3.5), fill = ekr_col[1]) +
    geom_text(aes(x = 1.2, y = 3.25, label = ekr_order[1]), size = 5.5, hjust = "left") + 
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 4, ymax = 4.5), fill = ekr_col[2]) +
    geom_text(aes(x = 1.2, y = 4.25, label =  ekr_order[2]), size = 5.5, hjust = "left") + 
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 5, ymax = 5.5), fill = ekr_col[3]) +
    geom_text(aes(x = 1.2, y = 5.25, label =  ekr_order[3]), size = 5.5, hjust = "left") + 
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 6, ymax = 6.5), fill = ekr_col[4]) +
    geom_text(aes(x = 1.2, y = 6.25, label =  ekr_order[4]), size = 5.5, hjust = "left") + 
    xlim(c(0, 8)) + ylim(c(0, 7)) +
    #theme(text = element_text(size = 1)) +
    theme_void() 
  
  return(leg)
}

#' Make NL map with a bounrdry box

#'@param pl (sf object)
#'
#'@import tmap
#'@import sf
#'
#'@return nl (ggplot object) map of whole Netherlands and boundary of subset
make_nl_box <- function(pl){

  #load Nederlands Map
  data(NLD_prov)
  
  # bounding box 
  box <- st_make_grid(pl, n = 1)
  
  # draw
  nl <- ggplot(NLD_prov) + 
    geom_sf(col = "#2ecc71", fill = alpha("#2ecc71", 0.2)) + 
    geom_sf(data = box, col = "red", fill = alpha("red",0), size =1) +
    theme_void() 
  
  return(nl)
}


# col_ekr = "EKR"
# ekr_fac = FALSE
# #col_ekr = "oordeel_2022"
# #ekr_fac = TRUE
# pl = WL_EAG
# dt = copy(krwset)
# col_area_sf = "SGBP3_NAAM"
# col_area_dt = "waterlichaam"
# col_ekrtype = "GHPR_level"
# ekr_order = c("slecht","ontoereikend","matig","goed")
# ekr_col = c("red", "orange", "yellow", "green")
# ekr_breaks = matrix(rep(c(0, 0.25, 0.5, 0.75, 1), each = 4), ncol = 5)
# label_type = c("Fytoplankton", "Overige waterflora", "Macrofauna", "Vis")
# bkpl = eag
