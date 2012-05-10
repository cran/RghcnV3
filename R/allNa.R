allNA <- function(Data){
  
  if( isArray(Data) | is.zoo(Data) | isMts(Data)){
    if( is.zoo(Data) | isMts(Data)){
      mc <- apply(is.na(Data), MARGIN=1, FUN = all)
      monthTs <- ts(mc ,start = min(time(Data)), frequency = 12)
      return(monthTs)
    }
    if(isArray(Data)){
      mc <- apply(is.na(Data), MARGIN = c(2,3), FUN =all)
      mc <- c(mc)
      monthTs <- ts(mc ,start = min(as.numeric(unlist(dimnames(Data)[3]))), frequency = 12)
      return(monthTs)
    }
    
  }
  stop("must be zoo, Mts or Array object")
  
}