removeNaStations<-function(Data){
  if (!is.zoo(Data) & !isMts(Data)) stop("Must be a zooobject or Mts")
   
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