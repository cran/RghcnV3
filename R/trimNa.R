trimNA <- function(Data){
  
  if(!isMts(Data) &  !is.zoo(Data) & !isArray(Data))stop( "must be zoo, Mts or Array")
  
  if(is.zoo(Data)){
    warning("code for Zoo not implemented yet. returning Mts")
    Data <-asMts(Data)
    
  }
  
  if(isMts(Data)){
    mc <- apply(is.na(Data), MARGIN =1, FUN = all)
    dex <- min(which(mc == FALSE))
    if(length(dex) == 0)stop(" data is all NA")     
      nt<- time(Data)[dex:nrow(Data)]
      Data <- ts(Data[dex:nrow(Data),], start = min(nt),frequency = 12)           
       
  }
  
  
  if(isArray(Data)){
    mc <- apply(is.na(Data), MARGIN=3, FUN = all)
    dex <- min(which(mc == FALSE))
    if(length(dex) == 0)stop(" data is all NA")
    Data <- Data[,,dex:dim(Data)[3]]
  }
  return(Data)
}