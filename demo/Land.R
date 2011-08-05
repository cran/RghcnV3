
########################################################################
# Calculate Land average
#
# license GPL 2
# Author Steven Mosher
#
########################################################################
#  Get the temperature and inventory data
########################################################################
require("maps")
if (!file.exists(GHCN.V3.DATA)) {
  dir.create(GHCN.V3.DATA)
  meanAdj     <- downloadV3(url = V3.MEAN.ADJ.URL)  
  meanAdata   <- readV3Data(filename=meanAdj$DataFilename)
  Inventory    <- readInventory(filename=meanAdj$InventoryFile)
   
} else {
 ### check to see what files exists using a utility function
demoFiles <- getDemoFiles()
if (is.null(demoFiles$Data)){
    # if the data file is missing download it
    meanAdj     <- downloadV3(url = V3.MEAN.ADJ.URL)    
    v3Mean     <- readV3Data(filename=meanAdj$DataFilename,output="Zoo")
    Inventory    <- readInventory(filename=meanAdj$InventoryFile)
} else {
    # the files are there just use them    
    v3Mean   <- readV3Data(filename = demoFiles$Data,output="Zoo")
    Inventory    <- readInventory(filename = demoFiles$Inv)
}
}



 
v3Mean   <- window(v3Mean,start = 1900,end = 2010 + (11/12))
########################################################################
# Create Anomalies for the default period 1961-1990, 15 years 12 months
########################################################################
Anomaly     <- createAnomaly(v3Mean)
DATA        <- intersectInvData(Inventory,Anomaly)
#  create a raster to accept the points
Land        <- rasterizeZoo(DATA$Inventory,DATA$Zoo,GLOBE5)
## Land contains anomalies that have been gridded.


# first assuming any grid with land is 100% land
L          <- Land*area(Land,na.rm = TRUE, weight = TRUE)
Monthly <- cellStats(L ,sum)
Monthly <- zoo(Monthly,order.by=as.numeric(layerNames(Land)))
Annual  <- aggregate(Monthly,by=floor(index(Monthly)+1),FUN = mean)
plot(Monthly,main="Land Temperature Anomaly",
     ylab=" Anomaly (C)",xlab = "Date",col="grey50")
lines(Annual,col = "blue")
abline(h = 0,col = "black")
 

 

