removeNaStations<-function(Data){
  
  if (isArray(Data)){   
    allMissing <- apply(is.na(Data), MARGIN = 1, FUN = all) 
    if(sum(allMissing) == 0) return(Data) else
    return(Data[!allMissing, , ])      
  }
  
  if (!is.zoo(Data) & !isMts(Data) & !is.ts(Data) & !isBigMatrix(Data) ) stop(" Must be zoo, Mts,array, ts")
    
     
  if ( isBigMatrix(Data) ){
    ncount <- colna(Data, seq(1,ncol(Data)))
    dex    <- which(ncount < nrow(Data))
    Data   <- Data[,dex]
    return(Data)
    
  }  
  
   
  if (!is.null(ncol(Data))){
        trecords <- nrow(Data)        
        naSums   <- colSums(is.na(Data))
        dex      <- which(naSums == trecords)
        keepStations <- setdiff(1:ncol(Data),dex)
     return(Data[,keepStations])
  } else {
        if (all(is.na(Data))) stop("empty Station")
        return(Data)    
     }
  
}