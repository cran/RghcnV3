##########################################################
#  test of Tamino's reference station method'
#  Steven Mosher
#
#
#
#
############################################################
require("maps")
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
v3Mean    <- readV3Data(filename = V3DATA, output ="Zoo")
v3Inv     <- readInventory(filename = V3INV)
v3Mean    <- window(v3Mean, start = startYear, end = endYear +(11/12))
v3Mean    <- removeNaStations(v3Mean)
texas     <- extent(-105,-95,25,35)
texasInv  <- cropInv(v3Inv,texas)
 

DATA      <- intersectInvData(texasInv,v3Mean)
regionAve <- referenceStation(DATA$Zoo)
plot(regionAve$Zoo[,1], main = "Texas Heat", xlab = "date", col ="red", lwd =2)
plot(regionAve$Zoo$stations, main = "Stations", xlab = "date", col ="red", lwd =1)
plot(regionAve$Zoo$se, main = "Standard Error", xlab = "date", col ="red", lwd =1)
plot(regionAve$Zoo$std.dev, main = "Std devs", xlab = "date", col ="red", lwd =1)
plot(extent(-120,-80,20,40))
points(getLonLat(texasInv))
map("state",add=TRUE)


  