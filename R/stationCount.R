stationCount <- function(Data){
  
  if ( is.zoo(Data) | isMts(Data) | isArray(Data)){
    
    if ( isArray(Data)) Data <- asZoo(Data)
    
    SC <- apply(!is.na(Data), MARGIN = 1, FUN = sum)
    return(ts(SC, start = min(time(Data)),frequency = frequency(Data)))
  
  }else{
    stop("must be an array, mts or zoo object")
    
  }
  
  
}