##########################################################
#  Global Temperature Calculation
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
   print("downloading SST.. this will take a while")
   dir.create(GHCN.V3.DATA)
   ofile  <-  downloadSST(url = HADSST2.URL, directory = GHCN.V3.DATA)
   tfile  <-  downloadV3(directory = GHCN.V3.DATA)
   V3DATA <-  tfile$DataFilename
   V3INV  <-  tfile$InventoryFile
   mfile  <-  downloadMask(url = OCEAN.MASK.URL, directory = GHCN.V3.DATA)   
   
} else {
  #### Directory exists
  DL <- getDemoFiles()
  if (is.null(DL$OCEANMASKS)){
     mfile <- downloadMask(url = OCEAN.MASK.URL, directory = demoDir)
  } else mfile <- DL$OCEANMASKS
  if (is.null(DL$SST)){ 
     ofile <- downloadSST(directory = demoDir)
  } else ofile <- DL$SST
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


 


## get the land mask data
landMask     <- readMask(filename = mfile)
landMask     <- aggregate(landMask, fact = 20)
 
# Workaround for issue in raster fixed in version 1.9 Jul 18th
oceanMask    <- calc(landMask,function(x) 1 - x)
###########################
 
startYear <- 1900
endYear   <- 2010
totalYears     <- endYear + 1 - startYear 
totalMonths    <- totalYears * 12
###########################################
#############################################
###  get the SST   Data and trim  start to end
SST       <- readSST(ofile)
dl        <- which(layerNames(SST) == as.character(startYear))
SST       <- dropLayer(SST, 1:(dl - 1))
SST       <- dropLayer(SST, (totalMonths +1 ):nlayers(SST))
#####################################################
v3Mean    <- readV3Data(filename = V3DATA,output = "Zoo")
v3Inv     <- readInventory(filename = V3INV)
v3Mean    <- window(v3Mean, start = startYear, end = endYear + (11/12))
anomaly   <- createAnomaly(v3Mean)
DATA      <- intersectInvData(v3Inv,anomaly)
################################################
  
# Land and Sea data is all in place
#  SST is in a brick
#  Land data is in DATA$Anomalies
#  Inventory is in DATA$Inventory
### rasterize the Land
  Land        <- rasterizeZoo(DATA$Inventory, DATA$Zoo, GLOBE5)
# calculate the coastal cells by adding together the cells from
# land that are coastal with the cells from SST that are coastal
  L          <- Land*area(Land,na.rm=TRUE,weight=TRUE)
  Monthly    <- cellStats(L ,sum)
  Monthly    <- zoo(Monthly,order.by=as.yearmon(as.numeric(layerNames(Land))))
  Annual     <- annualize(Monthly)
  
  plot(Monthly,main="Land Temperature Anomaly",
     ylab=" Anomaly (C)",xlab = "Date",col="grey50")
     lines(Annual,col = "blue")
     abline(h = 0,col = "black")

 # now we should check against CRUTemp

CruTemp   <- readHadResults(url =CRUTEMP3.RESULTS.URL )
CruTemp   <- window(CruTemp, start = startYear, end = (endYear + 11/12))
cruAnnual <- annualize(CruTemp)
 
plot(Annual, main = "RghcnV3 versus CRU", xlab = "Date", ylab = "C",lwd=2)
lines(cruAnnual, col = "blue", lwd = 2)
lines(Annual - cruAnnual, col = "red")
abline( h = 0)

#################  Now comes the SST


  S    <- SST*area(SST,na.rm=TRUE,weight=TRUE)
  SSTMonthly <- cellStats(S ,sum)
  SSTMonthly <- zoo(SSTMonthly,order.by=as.yearmon(as.numeric(layerNames(SST))))
  SSTAnnual  <- annualize(SSTMonthly)
  plot(SSTMonthly,main="SST Temperature Anomaly",
     ylab=" Anomaly (C)",xlab = "Date",col="grey50")
     lines(SSTAnnual,col = "blue")
     abline(h = 0,col = "black")     
# 
hadSst    <- readHadResults(url =HADSST2.RESULTS.URL )
hadSst  <- window(hadSst, start = startYear, end = (endYear + 11/12))
hadAnnual <- annualize(hadSst)

plot(SSTAnnual, main = "RghcnV3 versus HADSST", xlab = "Date", ylab = "C",lwd=2)
lines(hadAnnual, col = "blue", lwd = 2)
lines(SSTAnnual - hadAnnual, col = "red")
abline( h = 0)

###################################################
#  now comes the tricky bit
#  First we creat a raster where the cells that have BOTH
#  a land value and a sst value are Added.
#  if either land or SST are NA, we get NA
#  if Both are NA, we get NA.
#  So coastal had those cells where BOTH are present.
#####################################################
                          
                          
Coastal  <-  (Land * landMask) + (SST * oceanMask)
 
##  Now for the next tricky part
#   we start with coastal and 'cover' it with the land values
#   where the Coastal has NA, and the Land is not NA, those
#   values get Added. That result is cells with land values OR
#   coastal values ( land and SST). Then we layer on SST.
#   where NAs appear and SST has values, they get added.
  
  Global   <- cover(Coastal, Land ,SST )           
  Global   <- Global * area(Global, na.rm = TRUE, weight=TRUE)
  GMonthly <- cellStats(Global ,sum)
  GMonthly <- zoo(GMonthly,order.by= as.yearmon(as.numeric(layerNames(Land))))
  GAnnual  <- annualize(GMonthly)
plot(GMonthly,main="Global Temperature Anomaly",
     ylab=" Anomaly (C)",xlab = "Date",col="grey50")
     lines(GAnnual,col = "blue")
     abline(h = 0,col = "black")
     
                             
 hadCrut    <- readHadResults(url =HADCRUT3.RESULTS.URL )
 hadCrut   <- window(hadCrut, start = startYear, end = (endYear + 11/12))
 hadCrutAnnual <- annualize(hadCrut)

plot(GAnnual, main = "RghcnV3 versus HADCRUT3", xlab = "Date", ylab = "C",lwd=2)
lines(hadCrutAnnual, col = "blue", lwd = 2)
lines(GAnnual - hadCrutAnnual, col = "red")
abline( h = 0)