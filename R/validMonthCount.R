validMonthCount <- function(Data){
  if(isArray(Data))return(TRUE)
  if( is.zoo(Data) | isMts(Data)){
     
     monthCount = nrow(Data)
    if(monthCount %% 12 == 0){
      firstMonth <- cycle(Data)[1]
      lastMonth  <- cycle(Data)[nrow(Data)]
      if(firstMonth != 1)return(FALSE)
      if(lastMonth != 12)return(FALSE)
      return(TRUE)
    }
    return(FALSE)
  }
  stop("must be a zoo, mts or Array Object")
  
}