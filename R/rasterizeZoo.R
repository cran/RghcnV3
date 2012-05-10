GLOBE5   <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90, 
                   ncol = 72, nrow = 36,
                   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 



rasterizeZoo <- function(inventory, Zoo, land = GLOBE5){
  require("zoo")
  require("raster")
  # check the various calling parameters
  # check for a good cellsize
  # check inventory as inventory data.frame
  #check Zoo as a zoo
  # stop if they dont match
  # proceeding otherwise
  if (!is.zoo(Zoo)){
    cat("Zoo is a ", class(Zoo), "\n" ) 
    stop("Zoo must be a zoo object")
  }
  if (!isInventory(inventory)) stop("Inventory is not valid")
   
   
  matching <- identical(getStations(inventory), getStations(Zoo))
  if (matching == FALSE){
    print(" mismatching stations between inventory and anomalies")
    stop("use the function intersectInvAnomaly to rectify")
    
  }  
  if (!class(land)[1] == "RasterLayer") stop("land must be a raster object")
  resolution <- res(land)
  if (resolution[1] != resolution[2])stop(" must use a land mask with square cells")
  
   
  
  ###########  ready to process 
  #  get the lon lats of stations
  #  suck the time out of the zoo object
  #  transform the zoo object into a matrix with right orientation
  #  rasterize with mean
  #test <- rasterize(xy,r,field=vals,fun=mean)
  xy      <- getLonLat(inventory)
  TIME    <- time(Zoo)
  # core data wacks the row names
  data    <- coredata(Zoo)
  dimnames(data)[[1]] <- TIME
  tdata   <- t(data)  
  gridded <- rasterize(xy, land, field = tdata, fun = mean, na.rm = TRUE)
  gridded <- setZ(gridded, as.yearmon(time(Zoo)), name = 'time')
  return(gridded)
 
}
