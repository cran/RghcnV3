
########################################################################
# Author Steven Mosher
#
# license GPL 2
#
#
########################################################################
#  Get the temperature and inventory data
########################################################################
meanAdj     <- downloadV3(url=V3.MEAN.ADJ.URL) 
meanAdata   <- readV3Data(filename=meanAdj$DataFilename)
########################################################################
# Create Anomalies for the default period 1961-1990, 15 years 12 months
########################################################################
meanAnomaly <- createAnomaly(meanAdata)
############ window the data to period  after 1900
##  NOTE that times are in fractional months  
Anomaly1900 <- window(meanAnomaly,start=1900,end= 2010.99)
##  Count the months where you have temperature
stationMonths <- colSums(!is.na(Anomaly1900))
##  create and apply a True false mask to the columns
#  stations are in columns
stationMask <- rep(FALSE,length(stationMonths))
dex <- which(stationMonths >= 1200)
stationMask[dex]<-TRUE
## now apply the mask by column and you have all the long records
longRecords <- Anomaly1900[,stationMask]
# next take the mean by row ( rows are times)
# recast  as a zoo object rowMeans 
unWeightedMean <-  zoo(rowMeans(longRecords,na.rm=TRUE),order.by=time(longRecords))
# This is an UNWEIGHTED Mean, not area weighted
plot(unWeightedMean,
     xlab="Date",
     ylab="Anomaly(C)",
     main="Unweighted Mean Anomaly",
     col="grey")



 

 