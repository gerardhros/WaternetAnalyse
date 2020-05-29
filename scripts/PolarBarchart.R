#' builds many histograms and arranges them around a circle to save space.
#'
#' The data frame is expected to have at least four columns: family, item, score and value.
#' The three first columns are categorical variables, the fourth column contains non-negative values.
#' See the vignette for an example.
#' Each bar represents the proportion of scores for one item. Items can be grouped by families.
#' The resulting graph can be busy and might be better off saved as a pdf, with ggsave("myGraph.pdf").
#'
#' @author Christophe Ladroue
#' @param df a data frame containing the data
#' @param family list for defining families
#' @param binSize width of the bin. Should probably be left as 1, as other parameters are relative to it.
#' @param spaceItem space between bins
#' @param spaceFamily space between families
#' @param innerRadius radius of inner circle
#' @param outerRadius radius of outer circle. Should probably be left as 1, as other parameters are relative to it.
#' @param guides a vector with percentages to use for the white guide lines
#' @param alphaStart offset from 12 o'clock in radians
#' @param circleProportion proportion of the circle to cover
#' @param direction either of "inwards" or "outwards". Whether the increasing count goes from or to the centre.
#' @param familyLabels logical. Whether to show family names
#' @param normalised logical.
#' @return a ggplot object

# df <- EKRset[EKRset$GHPR_level == '2V21 Soortensamenstelling macrofyten',]

polarHistogramDT <-function (df, family = df$family, binSize = 1,
                           spaceItem = 0.2, spaceFamily = 1.2, innerRadius = 0.3, outerRadius = 1,
                           guides = c(10, 20, 40, 80), alphaStart = -0.3, circleProportion = 0.8,
                           direction = "inwards", familyLabels = TRUE, normalised = TRUE) {
  
  df <- EKRset[EKRset$GHPR_level == '2V21 Soortensamenstelling macrofyten',]
  df$family <- df$KRWwatertype.code
  df$item <- df$EAGIDENT
  df$score <- df$klasse
  #titel <- paste0(unique(c(df$Waardebepalingsmethode.code, df$GHPR, df$KRWwatertype.code.y)), sep= '', collapse = '_')
  df$value <- as.numeric(df$Numeriekewaarde) # dataframe voor ggplot
  
  #df <- df[df$level == '1',]
  
  df <- df[!is.na(df$EAGIDENT) & !is.na(df$CODE) & 
             !(is.na(df$Waardebepalingsmethode.code)) & 
             !(is.na(df$GHPR)) & !(is.na(df$KRWwatertype.code))& 
             !(is.na(df$HoortBijGeoobject.identificatie)),]
    
  df <- arrange(df, family, item, score)
  
  df2 <- as.data.table(df)
  setorder(df2,family,item,score)
    
  # normalise value per family-item group or over all groups
  if(normalised){
    df2[,value := cumsum(value/sum(value)), by = .(family,item)]  
  } else {
    df2[,maxFamily := sum(value),by = .(family,item)][,maxFamily := max(maxFamily)]
    df2[,value := cumsum(value)/maxFamily, by = .(family,item)]
  }
  
  # add previous
  df2[,previous := shift(value,n = 1, fill = 0)]
  
  # add indexItem
  df2[,indexItem := .GRP,by=.(family,item)]
  
  # add indexFamily
  df2[,indexFamily := .GRP,by=.(family)]
  
  # sort database
  setorder(df2,family,item,score)

  # helper function to calculate size of circle
  affine <- switch(direction,
                   inwards = function(y) (outerRadius - innerRadius) * y + innerRadius,
                   outwards = function(y) (outerRadius - innerRadius) * (1 - y) + innerRadius,
                   stop(paste("Unknown direction")))
  
  # helper readableAngle
  readableAngle <- function(x) {
    angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
    angle + ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 180, 0)
  }
  
  # helper readableJustification
  readableJustification <- function(x) {
    angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
    ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 1, 0)
  }
  
  # add xmin, xmax, ymin, ymax
  df2[,xmin := (indexItem - 1) * binSize + (indexItem - 1) * spaceItem + (indexFamily - 1) * (spaceFamily - spaceItem)]
  df2[,xmax := xmin + binSize]
  df2[,ymin := affine(1-previous)]
  df2[,ymax := affine(1 - value)]
  
  # prepare guidesDT and guidelabels
  if(normalised){
    guidesDF <- data.table(xmin = rep(df2$xmin, length(guides)),y = rep(1 - guides/100, 1, each = nrow(df2)))
    guideLabels <- data.table(x = 0, y = affine(1 - guides/100),label = paste(guides, "% ", sep = ""))
    
    } else {
    guidesDF <- data.table(xmin = rep(df2$xmin, length(guides)),y = rep(1 - guides/maxFamily, 1, each = nrow(df2)))
    guideLabels <- data.table(x = 0, y = affine(1 - guides/maxFamily),label = paste(guides, " ", sep = ""))
    
    }
  
  guidesDF[,xend := xmin + binSize]
  guidesDF[,y := affine(y)]
  
  # total length
  totalLength <- tail(df2$xmin + binSize + spaceFamily, 1)/circleProportion - 0
  
  # prepare item labels
  dfItemLabels <- df2[,.(xmin = head(xmin,1)),by=.(family,item)]
  dfItemLabels[, x := xmin + binSize/2]
  dfItemLabels[,angle := readableAngle(xmin + binSize/2)]
  dfItemLabels[,hjust := readableJustification(xmin + binSize/2)]
  
  # prepare plot
  p <- ggplot(df2) + 
       geom_rect(aes(xmin = xmin, xmax = xmax,ymin = ymin, ymax = ymax, fill = score)) +
       # add itemlabels
       geom_text(aes(x = x, label = item, angle = angle,hjust = hjust), 
                     y = 1.02, size = 4, vjust = 0.5, data = dfItemLabels) +
       # add guidesDF
       geom_segment(aes(x = xmin, xend = xend, y = y, yend = y),
                        colour = "white", data = guidesDF) +
       # add guideLabels
       geom_text(aes(x = x, y = y, label = label), data = guideLabels,
                     angle = -alphaStart * 180/pi, hjust = 1, size = 2)
  
  # add family labels
  if (familyLabels) {
    
    familyLabelsDF <- df2[,.(xmin = mean(xmin + binSize)),by=.(family)]
    familyLabelsDF[, x:= xmin]
    familyLabelsDF[,angle := xmin * (-360/totalLength) - alphaStart * 180/pi]
    
    p <- p + geom_text(aes(x = x, label = family, angle = angle),
                       data = familyLabelsDF, y = 1)
  }
  
  # update and finalize figure
  p <- p + theme(panel.background = element_blank(), axis.title.x = element_blank(),
                 axis.title.y = element_blank(), panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), axis.text.x = element_blank(),
                 axis.text.y = element_blank(), axis.ticks = element_blank()) +
      # add limits for y axis and y axis
      xlim(0, tail(df2$xmin + binSize + spaceFamily, 1)/circleProportion) +
      ylim(0, outerRadius + 0.2) +
      # make circle
      coord_polar(start = alphaStart) +
      # add colors
      scale_fill_manual(values=c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red", '1' = "grey"), 
                        labels =c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2", '1'= "niet bemonsterd"))
  p
}

polarHistogram <-function (df, family = df$family, binSize = 1,
                           spaceItem = 0.2, spaceFamily = 1.2, innerRadius = 0.3, outerRadius = 1,
                           guides = c(10, 20, 40, 80), alphaStart = -0.3, circleProportion = 0.8,
                           direction = "inwards", familyLabels = TRUE, normalised = TRUE) {
  
  df$family <- df$KRWwatertype.code
  df$item <- df$EAGIDENT
  df$score <- df$klasse
  df$value <- df$Numeriekewaarde
  #titel <- paste0(unique(c(df$Waardebepalingsmethode.code, df$GHPR, df$KRWwatertype.code.y)), sep= '', collapse = '_')
  df$value <- as.numeric(df$Numeriekewaarde) # dataframe voor ggplot
  
  df <- df[df$level == '1',]
  
  df <- df[!is.na(df$EAGIDENT) & !is.na(df$CODE) & 
             !(is.na(df$Waardebepalingsmethode.code)) & 
             !(is.na(df$GHPR)) & !(is.na(df$KRWwatertype.code))& 
             !(is.na(df$HoortBijGeoobject.identificatie)),]
  
  df <- arrange(df, family, item, score)
  
  if(normalised){
    df <- ddply(df, .(family, item), transform, value = cumsum(value/(sum(value))))
  } else {
    maxFamily <- max(plyr::ddply(df,.(family,item), summarise, total = sum(value))$total)
    df <- ddply(df, .(family, item), transform, value = cumsum(value))
    df$value <- df$value/maxFamily
  }
  
  df <- ddply(df, .(family, item), transform, previous = c(0, head(value, length(value) - 1)))
  
  df2 <- ddply(df, .(family, item), summarise, indexItem = 1)
  df2$indexItem <- cumsum(df2$indexItem)
  df3 <- ddply(df, .(family), summarise, indexFamily = 1)
  df3$indexFamily <- cumsum(df3$indexFamily)
  df <- merge(df, df2, by = c("family", "item"))
  df <- merge(df, df3, by = "family")
  df <- arrange(df, family, item, score)
  
  affine <- switch(direction,
                   inwards = function(y) (outerRadius - innerRadius) * y + innerRadius,
                   outwards = function(y) (outerRadius - innerRadius) * (1 - y) + innerRadius,
                   stop(paste("Unknown direction")))
  
  df <- within(df, {
    xmin <- (indexItem - 1) * binSize + (indexItem - 1) *
      spaceItem + (indexFamily - 1) * (spaceFamily - spaceItem)
    xmax <- xmin + binSize
    ymin <- affine(1 - previous)
    ymax <- affine(1 - value)
  })
  
  if(normalised){
    guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                           y = rep(1 - guides/100, 1, each = nrow(df)))
    
  } else {
    guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                           y = rep(1 - guides/maxFamily, 1, each = nrow(df)))
  }
  
  guidesDF <- within(guidesDF, {
    xend <- xmin + binSize
    y <- affine(y)
  })
  
  totalLength <- tail(df$xmin + binSize + spaceFamily, 1)/circleProportion - 0
  
  p <- ggplot(df) + geom_rect(aes(xmin = xmin, xmax = xmax,
                                  ymin = ymin, ymax = ymax, fill = score))
  readableAngle <- function(x) {
    angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
    angle + ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 180, 0)
  }
  
  readableJustification <- function(x) {
    angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
    ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 1, 0)
  }
  
  dfItemLabels <- ddply(df, .(family, item), summarize, xmin = xmin[1])
  dfItemLabels <- within(dfItemLabels, {
    x <- xmin + binSize/2
    angle <- readableAngle(xmin + binSize/2)
    hjust <- readableJustification(xmin + binSize/2)
  })
  
  p <- p + geom_text(aes(x = x, label = item, angle = angle,
                         hjust = hjust), y = 1.02, size = 4, vjust = 0.5, data = dfItemLabels)
  
  p <- p + geom_segment(aes(x = xmin, xend = xend, y = y, yend = y),
                        colour = "white", data = guidesDF)
  
  if(normalised){
    guideLabels <- data.frame(x = 0, y = affine(1 - guides/100),
                              label = paste(guides, "% ", sep = ""))
  } else{
    guideLabels <- data.frame(x = 0, y = affine(1 - guides/maxFamily),
                              label = paste(guides, " ", sep = ""))
  } 
  p <- p + geom_text(aes(x = x, y = y, label = label), data = guideLabels,
                     angle = -alphaStart * 180/pi, hjust = 1, size = 2)
  if (familyLabels) {
    familyLabelsDF <- aggregate(xmin ~ family, data = df,
                                FUN = function(s) mean(s + binSize))
    familyLabelsDF <- within(familyLabelsDF, {
      x <- xmin
      angle <- xmin * (-360/totalLength) - alphaStart * 180/pi
    })
    p <- p + geom_text(aes(x = x, label = family, angle = angle),
                       data = familyLabelsDF, y = 1)
  }
  
  p <- p + theme(panel.background = element_blank(), axis.title.x = element_blank(),
                 axis.title.y = element_blank(), panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), axis.text.x = element_blank(),
                 axis.text.y = element_blank(), axis.ticks = element_blank())
  
  p <- p + xlim(0, tail(df$xmin + binSize + spaceFamily, 1)/circleProportion)
  p <- p + ylim(0, outerRadius + 0.2)
  p <- p + coord_polar(start = alphaStart)
  p <- p + scale_fill_manual(values=c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red", '1' = "grey"), 
                             labels =c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2", '1'= "niet bemonsterd"))
  p
}

