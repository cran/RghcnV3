intersectInvV3 <- function(inventory, V3data){
  if (!isV3(V3data)) stop("V3data must be a v3 dataframe")
  if (!isInventory(inventory)) stop("must be an inventory")
   
  v3.stations      <- unique(V3data$Id)        
  stations         <- getStations(inventory)
  common           <- intersect(stations, v3.stations)
  print(common)
  if (length(common) == 0) stop("No Stations in intersection")
	InvOut           <- inventory[which(stations %in% common), ]
	V3Out            <- V3data[which(V3data$Id %in% common), ]
  
  if (!identical(getStations(InvOut),unique(V3Out$Id))) stop ("Stations dont match")
	intersectInvV3 <- list(Inventory = InvOut, V3 = V3Out)
}