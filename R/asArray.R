 
asArray <- function(Data){
  if (isMts(Data) | is.zoo(Data)){
    dimIn <-dim(Data)
    months = 12
    years <- dimIn[1]/months
    stations <- dimIn[2]
    begin <- min(time(Data))
    end   <- floor(max(time(Data)))
    A <- array(t(Data),dim=c(stations,months,years))
    dimnames(A) <- list(getStations(Data),month.abb,begin:end)
    return(A)    
  }
  stop("Must be a zoo or mts object")
  
}