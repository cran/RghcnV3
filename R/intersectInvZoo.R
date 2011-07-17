intersectInvZoo <- function(inventory, zooObj){
  if (!is.zoo(zooObj))stop("zooObj must be a zoo object")
  if (!isInventory(inventory)) stop("must be an inventory")
  zooObj           <- removeNaStations(zooObj)
  zoo.stations     <- getStations(zooObj)
  stations         <- getStations(inventory)
  common           <- intersect(stations, zoo.stations)
  
  if (length(common) == 0)stop("No Stations in intersection")
	InvOut           <- inventory[which(stations %in% common), ]
	ZooOut           <- zooObj[ ,which(zoo.stations %in% common)]	   
	intersectInvZoo  <- list(Inventory = InvOut, Zoo = ZooOut)
}