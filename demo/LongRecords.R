#######################################################  
#  A demo to extract stations with long records 
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
    v3Mean   <- readV3Data(filename=meanAdj$DataFilename,output="Zoo")
} else {
    # the files are there just use them    
    v3Mean   <- readV3Data(filename=demoFiles$Data,output = "Zoo")
}
}

 
########################################################################
# Create Anomalies for the default period 1961-1990, 15 years 12 months
########################################################################
 Anomaly <- createAnomaly(v3Mean)
############ window the data to period  after 1900
##  NOTE that times are in fractional months  
Anomaly1900 <- window( Anomaly, start = 1900, end = 2010 + (11/12))
##  Count the months where you have temperature
stationMonths <- colSums(!is.na(Anomaly1900))
##  create and apply a True false mask to the columns
#  stations are in columns
stationMask <- rep(FALSE, length(stationMonths))
dex <- which(stationMonths >= 1200)
stationMask[dex]<-TRUE
## now apply the mask by column and you have all the long records
longRecords <- Anomaly1900[ ,stationMask]
# next take the mean by row ( rows are times)
# recast  as a zoo object rowMeans 
unWeightedMean <-  zoo(rowMeans(longRecords, na.rm = TRUE), order.by = time(longRecords))
# This is an UNWEIGHTED Mean, not area weighted
annual <- annualize(unWeightedMean)
plot(unWeightedMean,
     xlab = "Date",
     ylab = "Anomaly(C)",
     main = "Unweighted Mean Anomaly",
     col = "grey")
lines(annual, col = "blue")



 

 