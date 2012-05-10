intersectInvData <- function(inventory, Data){
   
  if (!isInventory(inventory)) stop("Must supply an inventory")
  if (isMts(Data) | is.zoo(Data) | isArray(Data)){
      Data         <- removeNaStations(Data)
      invStations  <- getStations(inventory)
      dataStations <- getStations(Data)
      if(sum(duplicated(dataStations)) != 0)stop("stations in data are duplicated")
      if(sum(duplicated(invStations)) != 0)stop("stations in inventory are duplicated")
      
      common       <- intersect(invStations,dataStations)
       
      if (length(common) == 0) stop("No Stations in intersection")
      inventory    <- inventory[which(invStations %in% common), ]
      inventory    <- inventory[order(inventory$Id),]
      if (isMts(Data)){
          Data  <- Data[ ,which(dataStations %in% common)]
          Data  <- Data[ ,order(colnames(Data))]
          if(!identical(getStations(Data) ,getStations(inventory)))stop("mismatched Stations")
          if(!validData)stop("Invalid Data")
          return(list(Inventory = inventory, Mts = Data))
      }
      if (is.zoo(Data)){
          Data  <- Data[ ,which(dataStations %in% common)]
          Data  <- Data[ , order(colnames(Data))]
          if(!identical(getStations(Data) ,getStations(inventory)))stop("mismatched Stations")
          if(!validData)stop("Invalid Data")
          return(list(Inventory = inventory, Zoo = Data))
        
      }
      if (isArray(Data)){
        
           Data  <- Data[which(dataStations %in% common), , ]
           Data  <- Data[order(unlist(dimnames(Data)[1])),,]
           if(!identical(getStations(Data) ,getStations(inventory)))stop("mismatched Stations")
           if(!validData)stop("Invalid Data")
           return(list(Inventory = inventory, Array = Data))
        
      }
  }
  stop("Must supply an mts, zoo, or Array type object")
     
}