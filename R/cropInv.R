cropInv <- function(inventory, bBox) {
   require("raster")
   e <- class(bBox)[1]
   if( e != "Extent") stop( "bBox must be a raster extent object")  
   if (!isInventory(inventory)) stop(" Not a valid inventory")
   dex <- which(inventory$Lon >= xmin(bBox) &  inventory$Lon <= xmax(bBox)  & 
                 inventory$Lat >= ymin(bBox) &  inventory$Lat <= ymax(bBox))             
 
   return(inventory[dex, ])
  
}