intersectInvData <- function(inventory, Data){
   
  if (!isInventory(inventory)) stop("Must supply an inventory")
  if (isMts(Data) | is.zoo(Data) | isArray(Data)){
      Data         <- removeNaStations(Data)
      invStations  <- getStations(inventory)
      dataStations <- getStations(Data)
      common       <- intersect(invStations,dataStations)
      if (length(common) == 0) stop("No Stations in intersection")
      inventory    <- inventory[which(invStations %in% common), ]
      
      if (isMts(Data)){
          Data  <- Data[ ,which(dataStations %in% common)]
          return(list(Inventory = inventory, Mts = Data))
      }
      if (is.zoo(Data)){
          Data  <- Data[ ,which(dataStations %in% common)]
          return(list(Inventory = inventory, Zoo = Data))
        
      }
      if (isArray(Data)){
           Data  <- Data[which(dataStations %in% common), , ]
          return(list(Inventory = inventory, Array = Data))
        
      }
  }
  stop("Must supply an mts, zoo, or Array type object")
     
}