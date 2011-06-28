#################### demo
#  creating anomalies 
#  rectifying the inventory to reflect
#  which stations pass the "anomaly test"
#  criteria is set to default Start=1961,End=1990, Years=15, Threshold=15
#
#######################
meanAdj     <- downloadV3(url=V3.MEAN.ADJ.URL)
MeanAInv    <- readInventory(filename=meanAdj$InventoryFile)
meanAdata   <- readV3Data(filename=meanAdj$DataFilename)
meanAnomaly <- createAnomaly(meanAdata)

DATA  <- intersectInvAnomalies(inv=MeanAInv,meanAnomaly)

plot(x=as.numeric(colnames(DATA$Anomalies)),y=DATA$Anomalies[3,],type="l")

#  DATA$Anomalies contains anomalies in a matrix

str(DATA$Anomalies)
print(rownames(DATA$Anomalies))
print(colnames(DATA$Anomalies))

plot(x=as.numeric(colnames(DATA$Anomalies)),y=colSums(!is.na(DATA$Anomalies)),type="l")
 
