anomalize <- function(Data, period = list(Start = 1961, End = 1990)){
  
  if (period$Start >= period$End) stop("start must be less than end")
  
  if( is.ts(Data) | is.zoo(Data) | isArray(Data)){
    swapOut = is.ts(Data)
    if (swapOut) Data <- asZoo(Data)
    if (isArray(Data)){
          
          basePeriod <- windowArray(Data, start = period$Start, end =period$End)
          u <-   t(apply(basePeriod, MARGIN = 1, FUN = rowMeans, na.rm = TRUE))            
          out <- sweep(Data,c(1,2),u,FUN = "-")
          dimnames(out) <- dimnames(Data)
          
      }
    if (is.zoo(Data)){
      basePeriod <- window(Data, start = period$Start, end = period$End + (11/12))
      monthly.mean <- aggregate(basePeriod , cycle(basePeriod), FUN = mean, na.rm = TRUE)
      if (is.null(ncol(Data))){
             out <- Data - coredata(monthly.mean)
          } else {
             out <- Data - coredata(monthly.mean)[coredata(cycle(Data)), ]
      }
    if (swapOut) Data <- asMts(Data)
      
    }
     out <- removeNaStations(out)
     return(out)  
      
    }
   stop("data must be an Array, mts, or zoo") 
  
}
  
  
 