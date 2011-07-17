intersectInvMts <- function(inventory, Mts){
  if (!isMts(Mts))  stop(" must be multiple time series")
  if (!isInventory(inventory)) stop("Must be an inventory")
  Mts              <- removeNaStations(Mts)
  mts.stations     <- getStations(Mts)
  stations         <- getStations(inventory)
  common           <- intersect(stations, mts.stations)
  if (length(common) == 0) stop("No Stations in intersection")
	InvOut           <- inventory[which(stations %in% common), ]
	MtsOut           <- Mts[ ,which(mts.stations %in% common)]	   
	intersectInvMts <- list(Inventory = InvOut, Mts = MtsOut)
}