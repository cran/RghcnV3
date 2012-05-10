trimHeadTail <- function(Data){
  if(isArray(Data))return(Data)
  if( is.zoo(Data) | isMts(Data)){
    months <- cycle(Data)
    firstJan <- min(which(months ==1))
    lastDec  <- max(which(months == 12))
    Data <- window(Data,start =time(Data)[firstJan],end = time(Data)[lastDec],frequency =12)
    return(Data)
  }
  stop("must be Mts, Zoo or Array Object")
  
}