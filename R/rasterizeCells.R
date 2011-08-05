rasterizeCells <- function( ZooCells, r = GLOBE5){
  require("zoo")
  require("raster")
  # check the various calling parameters
  # check for a good cellsize
  # check inventory as inventory data.frame
  #check Zoo as a zoo
  # stop if they dont match
  # proceeding otherwise
  if (!is.zoo(ZooCells)){
    cat("Zoo is a ", class(ZooCells), "\n" ) 
    stop("Zoo must be a zoo object")
  }
  cells <- 1:ncell(r)
  zoo.cells <- as.integer(colnames(ZooCells))
  
  matching <- identical(intersect(zoo.cells,cells),zoo.cells)
  
  if (matching == FALSE){
    print(" mismatching cells between Zoocell and raster")
    stop("check your Raster")
    
  }  
  if (!class(r)[1] == "RasterLayer") stop("r must be a raster object")
  resolution <- res(r)
  if (resolution[1] != resolution[2])stop(" must use a raster with square cells")
  
  zooAtt <- names(attributes(ZooCells))
   
  if (!("Res" %in% zooAtt)) stop("ZooCells has no resolution")
  if (attributes(ZooCells)$Res != resolution[1]){
    cat(" ZooCells was made with a raster resolution of ", attributes(ZooCells)$Res,"\n")
    cat(" r has a resolution of ", res(r), "\n")
    stop(" bad resolution in one of those")
  } 
  
  ###########  ready to process 
  #  get the lon lats of Cells
  #  suck the time out of the zoo object
  #  transform the zoo object into a matrix with right orientation
  #  rasterize with mean
  #test <- rasterize(xy,r,field=vals,fun=mean)
  xy      <- xyFromCell(r,zoo.cells)
  TIME    <- time(ZooCells)
  # core data wacks the row names
  data    <- coredata(ZooCells)
  dimnames(data)[[1]] <- TIME
  tdata   <- t(data)  
  gridded <- rasterize(xy, r, field = tdata, fun = mean, na.rm = TRUE)
  gridded <- setZ(gridded, as.yearmon(time(ZooCells)), name = 'time')
  return(gridded)
 
}