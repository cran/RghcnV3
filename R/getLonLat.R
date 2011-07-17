getLonLat <- function(inventory){ 
    if (!isInventory(inventory)) stop(" Must be an inventory")
    coords <- matrix(NA, ncol = 2, nrow = nrow(inventory))
    coords[ ,2] <- inventory$Lat
	  coords[ ,1] <- inventory$Lon 
    return(coords)    
}