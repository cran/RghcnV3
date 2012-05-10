findStartEnd <- function(Data){
  
  if( isArray(Data) | is.zoo(Data) | isMts(Data)){
     if(isArray(Data) ) monthCount = dim(Data)[2]*dim(Data)[3]
     if(is.zoo(Data) | isMts(Data)) monthCount = nrow(Data)
     if(!validMonthCount(Data))stop("Data has incomplete Years. Use trimHeadTail")
     if(is.zoo(Data) | isMts(Data)) years <- unique(time(Data) %/% 1)
     if( isArray(Data)) years <- as.numeric(unlist(dimnames(Data)[3]))
     mc <- allNA(Data)
     ys <- matrix(as.numeric(mc), nrow =12, byrow  = FALSE)
     fullYears <- apply(ys, MARGIN=2, FUN =function(x){all(x==0)})
     mindex <- min(which(fullYears == TRUE))
     maxdex <- max(which(fullYears == TRUE))
     return(list(Start = years[mindex] ,End = years[maxdex]))
    
  }
  stop("must be zoo, Mts or Array object")
  
  
  
  
}