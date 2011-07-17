#######################################################  
#  A demo to Grid the land based data
#  into a raster image
#  Author: Steven Mosher
#  July 4th 2011
#  License: GPL >-2
#
#######################
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
    meanAdata   <- readV3Data(filename=meanAdj$DataFilename)
    Inventory    <- readInventory(filename=meanAdj$InventoryFile)
} else {
    # the files are there just use them    
    meanAdata   <- readV3Data(filename = demoFiles$Data)
    Inventory    <- readInventory(filename = demoFiles$Inv)
}
}
startYear <- 1900
endYear   <- 2010

meanAdata   <- windowV3(meanAdata, start = startYear, end = endYear)
########################################################################
# Create Anomalies for the default period 1961-1990, 15 years 12 months
########################################################################
Anomaly     <- createAnomaly(meanAdata)
DATA        <- intersectInvZoo(Inventory, Anomaly)
#  create a raster to accept the points
Land        <- rasterizeZoo(DATA$Inventory, DATA$Zoo, GLOBE5)
## Land contains anomalies that have been gridded.
# area weighting would be applied next
# here we will just select a month and display
Amonth <- raster(Land,layer = 1000)
 

plot(Amonth, main = layerNames(Land)[1000])
map("world", add = TRUE)

#############

#########  turn the monthly numbers into an annual series

yearIndex   <- floor(as.numeric(layerNames(Land)))
yearIndex   <- yearIndex - (startYear - 1)
annualLand   <- stackApply(Land,
                     indices= yearIndex  ,
                     fun=mean,na.rm=TRUE)

layerNames(annualLand)  <- as.character(startYear:endYear)

layer1998              <- which(layerNames(annualLand) == "1998")

displayLayer  <- raster(annualLand,layer=layer1998)

plot(displayLayer, main="1998")
map("world", add = TRUE)

