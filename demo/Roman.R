####################################
#  test of Roman Ms functions
#
#
#
####################################

 TEXAS.DAT  <- system.file("external/Texas.dat", package = "RghcnV3")
 data       <- readV3Data(filename=TEXAS.DAT,output = "Mts")
 i          <- system.file("external/Texas.inv", package = "RghcnV3")
 inv        <-readInventory(i)
 data       <- window(data,start =1900, end = 2010 +(11/12))
  
 
# If you window the data check that the inventory matches
 DATA <- intersectInvData(inv,data)
 globe3 <- GLOBE5
 res(globe3) <- 3

 CellsOfZoo <- averageByCell(r = globe3, inventory = DATA$Inventory, Mts= DATA$Mts) 

 # cells of zoo will have a column for every cell

 TexasBrick <- rasterizeCells(ZooCells=CellsOfZoo, r= globe3)

 # now you have a brick of months
 # image in R 2.13.1  has bugs.. argg
 # month 1000
 plot(raster(TexasBrick, layer = 1000),   main="Texas")
 map("world", add= TRUE)

 unweightedAverage <- cellStats(TexasBrick, mean)
 f  <- system.file("external/Texas.dat", package = "RghcnV3")
 data <- readV3Data(filename=f)
 i <- system.file("external/Texas.inv", package = "RghcnV3")
 inv <-readInventory(i)
 data <- windowV3(data,start =1900, end = 2010)
 texasMts <- v3ToMts(data)
 
# If you window the data check that the inventory matches
 DATA <- intersectInvMts(inv,texasMts)
 globe3 <- GLOBE5
 res(globe3) <- 3

 CellsOfZoo <- averageByCell(r = globe3, inventory = DATA$Inventory, Mts= DATA$Mts) 

 # cells of zoo will have a column for every cell

 TexasBrick <- rasterizeCells(ZooCells=CellsOfZoo, r= globe3)

 # now you have a brick of months
 # image in R 2.13.1  has bugs.. argg
 # month 1000
 plot(raster(TexasBrick, layer = 1000), xlim =c(-150,-80), ylim =c(0,50), main="Texas")
 map("state", add= TRUE)

 unweightedAverage <- cellStats(TexasBrick, mean)

 monthly <- zoo(unweightedAverage, order.by=names(unweightedAverage)) 
 plot(window(monthly, start =1920,end =1960),ylim=c(-5,30))
 lines(anomalize(monthly),col="red")
 
  