inverseDensity <- function(inv, Data, r = GLOBE5){
  
  rClass <- class(r)[1]
  isRL <- rClass == "RasterLayer"
  if (isInventory(inv) & isArray(Data) & isRL){
  # check that inventory stations and Data stations match
  if (!identical(getStations(inv), getStations(Data))) stop("stations must match")
  # dimensions of Array
  dimensions   <- dim(Data) 
  # array of weights for output that looks like temperatures
  weights  <- array(NA,dimensions) 
   
  lonBin   <- ncol(r)
  latBin   <- nrow(r)
  halfLat       <- latBin/2
  eq            <- halfLat * (lonBin +1) + (halfLat+1)
  cellSize      <- res(r)[1]
  cellId   <- cellFromXY(r,cbind(inv$Lon,inv$Lat))
   
  #weights for latitude changes
  Rarea <- area(r)*r
  maxCell <- max(extract(Rarea,1:ncell(Rarea)), na.rm =T)
  Rarea <- Rarea/maxCell
   
  allWeights  <- extract(Rarea, 1:ncell(Rarea))
   
  for(months in 1:dimensions[2]){
      for(years in 1:dimensions[3]){
           # get T/F if temperature is there
           reports         <- !is.na(Data[ , months, years])
           # get the cell numbers of those cells with reports
           cells           <-  cellId[reports]          
          # a count of reports per cell per month
           counts         <- tabulate(c(cells, lonBin * latBin))         
          # Ok is the logical of counts > 0
           ok         <-  counts > 0 
           # make a copy of allweights to modify 
           density     <-  allWeights 
           # note only "ok" elements are valid
           density[ok]     <-  allWeights[ok] / counts[ok]
           #print(wc[ok]) those True cells get a density area/count
           weights[reports, months, years] <-  density[cells] 
            
      }
      
  }
  return( weights )
  }
  if ( !isInventory(inv) ) stop(" must be a valid inventory")
  if ( !isArray(Data)) stop("must be a valid data array")
  if ( !isRL ) stop("must be a valid raster")
   
}
  
  
  
  
  
  
  
  
  
  
 