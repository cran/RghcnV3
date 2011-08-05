#######################################################  
#  A demo to perform station counts for various CAM periods 
#  Author: Steven Mosher
#  July 4th 2011
#  License: GPL >-2
#
#######################
if (!file.exists(GHCN.V3.DATA)) {
  dir.create(GHCN.V3.DATA)
  meanAdj     <- downloadV3(url = V3.MEAN.ADJ.URL)  
  meanAdata   <- readV3Data(filename=meanAdj$DataFilename)
   
} else {
 ### check to see what files exists using a utility function
demoFiles <- getDemoFiles()
if (is.null(demoFiles$Data)){
    # if the data file is missing download it
    meanAdj     <- downloadV3(url = V3.MEAN.ADJ.URL)    
    v3Mean   <- readV3Data(filename = meanAdj$DataFilename,output="Zoo")
} else {
    # the files are there just use them    
    v3Mean   <- readV3Data(filename = demoFiles$Data,output="Zoo")
}
}

###################################################
#  make a  utility function to get a station count#
###################################################

stationCount <- function( zoodata){
  
   validValues <- rowSums(!is.na(zoodata))
   return(zoo(validValues, order.by =time(zoodata)))
}
  
##################################################
#  now we create anomalies which will drop stations that dont meet the criteria
#  I show a few tests for different criteria
#######################################################




  
 Anomaly <- createAnomaly(v3Mean)
 newCriteria <- list(Start = 1961, End = 1990, Years = 30, Threshold = 12)
 Anomaly2 <- createAnomaly(v3Mean, criteria = newCriteria)
 newCriteria <- list(Start = 1961, End = 1990, Years = 20, Threshold = 10)
 Anomaly3 <- createAnomaly(v3Mean, criteria = newCriteria)
 newCriteria <- list(Start = 1961, End = 1990, Years = 25, Threshold = 12)
 Anomaly4 <- createAnomaly(v3Mean, criteria = newCriteria)
   
tcount <- stationCount(v3Mean)
acount <- stationCount(Anomaly)
a2count<- stationCount(Anomaly2)
a3count<- stationCount(Anomaly3)
a4count<- stationCount(Anomaly4)
   
plot(tcount, main = "station counts", ylab = "stations")
lines(acount, col= "red")
lines(a2count, col = "blue")
lines(a3count, col = "purple")
lines(a4count, col = "yellow")
legend("topleft", legend = c('all stations', "15/12", "30/12", "20/10", "25/12"),
       col = c("black", "red", "blue", "purple", "yellow") ,lwd = 1)


 
