

createTemperature  <- function(data){
  
    zooSeries <- function(x){
       
       dat <- x[-(1:2)]
       tim <- as.yearmon(outer(x$Year,seq(0,length=ncol(dat))/12,"+"))
       zoo(c(as.matrix(dat)),tim)
      }
    
      require("zoo")
     	g <- do.call(cbind, by(data, data$Id, zooSeries))
    	z <- as.zooreg(as.ts(g))
	    colnames(z) <- sub("X","",colnames(z))
  return(z)
}




createAnomaly<-function(V3data,criteria=list(Start=1961,End=1990,Years=15,Threshold=12) ){
  
  in.Base <- function(Tdata,criteria){
       
        data  <- Tdata[which(Tdata$Year >= criteria$Start & Tdata$Year <= criteria$End),]
        # column 3 is where temps start
        mcount <- rowSums(!is.na(data[ ,3:14]),na.rm=T)
	      DF <- data.frame(Ghcnid=data[ ,1],Year=data[ ,2],Count=mcount)
        DF$Count <- DF$Count >= criteria$Threshold
        DF2 <- rowsum(as.numeric(DF$Count),DF$Ghcnid,na.rm=T)
        DF3 <- data.frame(Ghcnid=rownames(DF2),Years=DF2[ ,1])
        ids <- as.vector(unique(DF3$Ghcnid[DF3$Years >= criteria$Years]))
	      mask <- which(Tdata$Id %in% as.numeric(ids))
       return(mask)	
   } 
 
   normalize  <- function(dataZoo,period ){   
       a <- window(dataZoo,start = period$Start,end = period$End+(11/12))  
	      monthly.mean <- aggregate(a , cycle(a),FUN= mean,na.rm=T)
	     out <- dataZoo - coredata(monthly.mean)[coredata(cycle(dataZoo)), ]
       return(zoo(coredata(out),order.by=time(dataZoo)))
}

    
  require("zoo")
  V3data <- V3data[in.Base(V3data,criteria=criteria),]
  V3data <- createTemperature(V3data)
  V3data <- normalize(V3data,period=criteria)
  return(V3data)
  
}
 
  
intersectInvAnomalies <- function(inv,anomalies){
    anomaly.stations <- getStations(anomalies)
    stations <- inv$Id
    common <- intersect(inv$Id,anomaly.stations)
	  if(length(common)==0)stop("No Stations in intersection")
		InvOut <- inv[which(stations %in% common),]
	  anomalyOut <- anomalies[,which(anomaly.stations %in% common)]
	  anomalyOut <- t(anomalyOut)
	  intersectInvAnomalies <- list(Inventory=InvOut,Anomalies=anomalyOut)
}	
 


getStations <- function(data){
  if(is.zoo(data)){
    return(as.numeric(colnames(data)))   
  }
	if(is.data.frame(data)){
    if(!("Id" %in% names(data)))stop(" Id must be a column in the data.frame")
    return(as.numeric(data$Id))    
	 }
  if(is.matrix(data)){
    return(as.numeric(rownames(data)))
  }

}
  
getLonLat <- function(inventory){ 
    coords <- matrix(NA,ncol=2,nrow=nrow(inventory))
	  coords[ ,2] <- inventory$Lat
	  coords[ ,1] <- inventory$Lon 
    return(coords)    
}	
	
