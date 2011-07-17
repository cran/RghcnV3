
anomalize  <- function(TemperatureZoo, period = list(Start = 1961, End = 1990) ){ 
       if (!is.zoo(TemperatureZoo)){
       if (isV3(TemperatureZoo)) print("use v3ToZoo to create a zoo object")
        stop ("TemperatureZoo must be a zoo object" ) 
       } 
       a <- window(TemperatureZoo, start = period$Start, end = period$End + (11/12))
        
       monthly.mean <- aggregate(a , cycle(a), FUN = mean, na.rm = TRUE)
        
       if (is.null(ncol(TemperatureZoo))){
         out <- TemperatureZoo - coredata(monthly.mean)
       } else {
          
         out <- TemperatureZoo - coredata(monthly.mean)[coredata(cycle(TemperatureZoo)), ]
       }
       Zout <- zoo(coredata(out),order.by = time(TemperatureZoo))
       Zout <- removeNaStations(Zout)
  return(Zout)
}