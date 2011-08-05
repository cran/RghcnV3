##########################################################
#  Airports versus Non Airports
#  Steven Mosher
#
#
#
#
############################################################
demoDir <- file.path(getwd(), GHCN.V3.DATA, fsep =.Platform$file.sep)

#### Check if the demo directory has been created
#    otherwise create it and download the data we need
if (!file.exists(GHCN.V3.DATA)){
   cat(GHCN.V3.DATA, "Doesnt Exist.. creating","\n")    
   dir.create(GHCN.V3.DATA)    
   tfile  <-  downloadV3(directory = GHCN.V3.DATA)
   V3DATA <-  tfile$DataFilename
   V3INV  <-  tfile$InventoryFile
     
   
} else {
  #### Directory exists
  DL <- getDemoFiles()
    
   
  if (is.null(DL$Data)){
     tfile <- downloadV3(directory = demoDir)
     V3DATA <-  tfile$DataFilename
     V3INV  <-  tfile$InventoryFile
     } else {
     V3DATA <- DL$Data
     V3INV  <- DL$Inv             
     }    
  
}
#######  now we should have all the files we need.
#######  Since we stop this analysis at 2010 we dont
#######  worry about downloading fresh files for data
########################################################  
 
 
###########################
startYear <- 1900
endYear   <- 2010

###########################################
 
#####################################################
v3Mean    <- readV3Data(filename = V3DATA,output = "Zoo")
v3Inv     <- readInventory(filename = V3INV)
v3Mean    <- window(v3Mean, start = startYear, end = endYear + 11/12)
Aprt      <- v3Inv[which(v3Inv$Airport == TRUE), ]
notAprt   <- v3Inv[which(v3Inv$Airport == FALSE), ]
anomaly   <- createAnomaly(v3Mean)
APRT      <- intersectInvData(Aprt, anomaly)
NAPRT     <- intersectInvData(notAprt, anomaly)
################################################
##########  Now rasterize the two data sets
aprtRaster    <- rasterizeZoo(APRT$Inventory, APRT$Zoo, GLOBE5)
aprtRaster    <- aprtRaster * area(aprtRaster, na.rm = TRUE, weight = TRUE)
NaprtRaster   <- rasterizeZoo(NAPRT$Inventory, NAPRT$Zoo, GLOBE5)
NaprtRaster   <- NaprtRaster * area(NaprtRaster, na.rm = TRUE, weight = TRUE)
  
### now collect the monthly anomalies and cast as a zoo series
aprtMonthly <- zoo(cellStats(aprtRaster, sum), 
                   order.by = as.numeric(layerNames(aprtRaster)))
NaprtMonthly <- zoo(cellStats(NaprtRaster, sum), 
                   order.by = as.numeric(layerNames(NaprtRaster)))
#### create annula series and remove nas. 
aprtAnnual   <- annualize(aprtMonthly, na.rm = TRUE)
NaprtAnnual  <- annualize(NaprtMonthly, na.rm = TRUE)
  
plot(aprtAnnual, main = "Airport/Non Airport",
                 xlab = "1961 -1990 Base Period",
                 ylab = "Anomaly (C)", col = "red", lwd = 3)
lines(NaprtAnnual, col = "blue", lwd = 3)
abline(h = 0)
legend(x = "topleft", legend = c("aiport", "non-Aprt"),
       col = c("red", "blue"), lwd = 1, inset = .05)


plot(cumsum(NaprtMonthly), main = "Airport/Non Airport",
                 xlab = "1961 -1990 Base Period",
                 ylab = "Anomaly (C)", col = "blue",lwd = 3)
lines(cumsum(aprtMonthly), col = "red", lwd = 3)
abline(h = 0)
legend(x = "bottomleft", legend = c("aiport", "non-Aprt"),
       col = c("red", "blue"), lwd = 1, inset = .025)
   