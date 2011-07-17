v3ToMts   <- function(V3data){ 
       zooSeries <- function(x){       
          dat <- x[-(1:2)]
          tim <- as.yearmon(outer(x$Year, seq(0,length=ncol(dat))/12, "+"))
          zoo(c(as.matrix(dat)), tim)
       }   
   require("zoo")
   g <- do.call(cbind, by(V3data, V3data$Id, zooSeries))
   z <- as.ts(g) 
   colnames(z) <- sub("X", "", colnames(z))
   z <- removeNaStations(z)
  return(z)
}


