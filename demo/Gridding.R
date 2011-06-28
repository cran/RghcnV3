
########################################################################
# Grid Anomaly data
#
# license GPL 2
# Author Steven Mosher
#
########################################################################
#  Get the temperature and inventory data
########################################################################
meanAdj     <- downloadV3(url=V3.MEAN.ADJ.URL) 
meanAdata   <- readV3Data(filename=meanAdj$DataFilename)
Inventory   <- readInventory(filename=meanAdj$InventoryFile)
meanAdata   <- windowV3(meanAdata,start=1900,end=2010)
########################################################################
# Create Anomalies for the default period 1961-1990, 15 years 12 months
########################################################################
Anomaly     <- createAnomaly(meanAdata)
DATA        <- intersectInvAnomalies(Inventory,Anomaly)
#  create a raster to accept the points
Land        <- rasterizeAnomaly(DATA$Inventory,DATA$Anomalies,GLOBE5)
## Land contains anomalies that have been gridded.
# area weighting would be applied next
# here we will just select a month and display
Amonth <- raster(Land,layer=1000)
print(layerNames(Land)[1000])

plot(Amonth)
