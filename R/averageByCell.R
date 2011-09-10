averageByCell <- function( inventory, Mts,tol,weights = NULL,r = GLOBE5){
  # written by Steven Mosher
  # check class  
  if (!class(r)[1] == "RasterLayer") stop("r must be a raster object")
  resolution <- res(r)
  if (resolution[1] != resolution[2])stop(" must use a raster with square cells")
  if (!isInventory(inventory)) stop(" inventory must be an inventory")
  if (!isMts(Mts)) stop(" Mts-- must be an mts object")
  if (!identical(getStations(inventory),getStations(Mts))) stop("Stations Mismatch")
   
  myCells <-  cellFromXY(r, getLonLat(inventory))
  # used to get the column numbers for each cell
  DF <- data.frame(Id = getStations(inventory), Cell = myCells)
  uCells <- unique(myCells)
  #  dex contains the column numbers for each CELL
  cellcount <- length(uCells)
  
  for( j in 1:cellcount){
       dex <- which(DF$Cell == uCells[j])
        
       CellAve <- averageStations(Mtsdata = Mts[ ,dex], tol = tol,weights=weights,all=TRUE)
        
       if (j == 1){           
           out <- CellAve$Average          
        }else {           
           out <- cbind(out,CellAve$Average)           
           colnames(out)<-uCells[1:j]
         } 
   }
  Zout <- as.zoo(out)
  att  <- attributes(Zout)
  attributes(Zout) <- c(att, Res = resolution[1] )
   
  return(Zout)
  
}