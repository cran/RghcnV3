GLOBE5   <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, ncol =72,nrow=36,
                  crs="+proj=longlat +datum=WGS84") 




rasterizeAnomaly<-function(inventory,anomaly,land=GLOBE5){
  require("zoo")
  require("raster")
  # check the various calling parameters
  # check for a good cellsize
  # check inventory as inventory data.frame
  #check anomaly as a zoo
  # stop if they dont match
  # proceeding otherwise
  if(!is.zoo(anomaly)){
    print(cat("anomaly is a ",class(anomaly),"\n" ))
    stop("anomaly must be a zoo object")
  }
  if(!is.data.frame(inventory)){
    print(cat("inventory is a ",class(inventory),"\n" ))
    stop("inventory must be a data.frame")
  }
  if(names(inventory)[1]!="Id"){
    print("inventory column 1 must be Id")
    stop(cat("column 1 name is ",names(inventory)[1],"\n"))
  }
  if(names(inventory)[2]!="Lat"){
    print("inventory column 2 must be Lat")
    stop(cat("column 2 name is ",names(inventory)[2],"\n"))
  }
  if(names(inventory)[3]!="Lon"){
    print("inventory column 3 must be Lon")
    stop(cat("column 3 name is ",names(inventory)[3],"\n"))
  }
   
  matching <- identical(getStations(inventory),getStations(anomaly))
  if(matching==FALSE){
    print(" mismatching stations between inventory and anomalies")
    stop("use the function intersectInvAnomaly to rectify")
    
  }  
  if(!class(land)[1] == "RasterLayer") stop("land must be a raster object")
  resolution <- res(land)
  if(resolution[1]!=resolution[2])stop(" must use a land mask with square cells")
  
   
  
  ###########  ready to process 
  #  get the lon lats of stations
  #  suck the time out of the zoo object
  #  transform the zoo object into a matrix with right orientation
  #  rasterize with mean
  #test <- rasterize(xy,r,field=vals,fun=mean)
  xy <- getLonLat(inventory)
  TIME <- time(anomaly)
  # core data wacks the row names
  data <- coredata(anomaly)
  dimnames(data)[[1]]<-TIME
  tdata<-t(data)  
  gridded <- rasterize(xy,land,field=tdata,fun = mean,na.rm=TRUE)
  return(gridded)
 
}