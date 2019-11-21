# Script to load soil data (mean for each shp of interpolations)

loadSoil <- function(shp, res){
  
  if(!file.exists(paste0('pbelasting/data/soil/meanSoil', res,'.Rdata'))) {
    
    # load kriged predictions
    pred = get(load(paste0('pbelasting/data/soil/predSoil', res,'.Rdata')))
    
    # Calculate median value per polder ------------------------------------------
    calcMedian <- function(pvar, ...){      # pvar = pred$som
      tmp = cbind(pvar@data, over(pvar, shp))
      mean = aggregate(tmp$var1.pred, list(tmp$CODE), median, na.rm=T) # mean voor elke polder
    }
    
    meanSoil0 = lapply(pred, calcMedian)  
    meanSoil1= do.call(what = cbind, args = meanSoil0)
    meanSoil = meanSoil1[,-seq(3, dim(meanSoil1)[2], 2)]
    names(meanSoil) = c('CODE', paste0('b_', names(pred)))
    
    # Calculate sd value per polder ------------------------------------------
    calcSD <- function(pvar, ...){   # pvar = pred$som
      tmp = cbind(pvar@data,over(pvar,shp))
      sd = aggregate(tmp$var1.pred, list(tmp$CODE), sd, na.rm=T) # mean voor elke polder
    }
    
    sdSoil0 = lapply(pred, calcSD)  
    sdSoil1= do.call(what = cbind, args = sdSoil0)
    sdSoil = sdSoil1[,-seq(3, dim(sdSoil1)[2], 2)]
    names(sdSoil) = c('CODE', paste0('b_sd_', names(pred)))
    
    # mean std of each interpolation prediction 
    calcMSD <- function(pvar, ...){   # pvar = pred$som
      tmp = cbind(pvar@data,over(pvar,shp))
      msd = aggregate(tmp$std, list(tmp$CODE), mean, na.rm=T) # mean voor elke polder
    }
    

    
    meanSoil$CODE = as.character(meanSoil$CODE)
    meanSoil = data.frame(meanSoil, sdSoil[,-1])
    
    # save results
    save(meanSoil, file= paste0('data/soil/meanSoil',res,'.Rdata'))
    
  
  } else{
    meanSoil <- get(load(paste0('data/soil/meanSoil', res,'.Rdata')))
  }
  
  return(meanSoil)
}


