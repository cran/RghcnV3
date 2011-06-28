.checkCriteria <- function(criteria){
  
    if(criteria$Start >= criteria$End)stop( " Start must be less than End")
    if(criteria$Start %% 1 !=0){
       warning(cat(criteria$Start, " is being trimmed to ",floor(criteria$Start),"\n"))
       criteria$Start <- floor(criteria$Start)
    } 
    if(criteria$End %% 1 !=0){
       warning(cat(criteria$End, " is being trimmed to ",floor(criteria$End),"\n"))
       criteria$End <- floor(criteria$End)
    }
    if(criteria$Year >(criteria$End-(criteria$Start-1))){
      stop("Year must be less than End-Start")
    }
    if(criteria$Threshold>12)stop("Threshold is too large.use 12 months or less")
 return(criteria)  
}

anomalize  <- function(TemperatureZoo,period=list(Start=1961,End=1990) ){ 
       if(!is.zoo(TemperatureZoo)){
        if(ncol(TemperatureZoo)==14)print("use createTemperature to create a zoo object")
        stop ("TemperatureZoo must be a zoo object" ) 
       } 
       a <- window(TemperatureZoo,start = period$Start,end = period$End+(11/12))  
        monthly.mean <- aggregate(a , cycle(a),FUN= mean,na.rm=T)
       out <- TemperatureZoo - coredata(monthly.mean)[coredata(cycle(TemperatureZoo)), ]
       return(zoo(coredata(out),order.by=time(TemperatureZoo)))
}


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
 
   

    
  require("zoo")
  criteria <- .checkCriteria(criteria)
  V3data <- V3data[in.Base(V3data,criteria=criteria),]
  V3data <- createTemperature(V3data)
  V3data <- anomalize(V3data,period=criteria)
  return(V3data)
  
}

whichStationsCAM <- function(dataZoo, criteria =list(Start=1961,End=1990,Year=15,Threshold=12)){
   
  criteria<-.checkCriteria(criteria)
   
  a <- window(dataZoo,start = criteria$Start,end = criteria$End+(11/12))
  I <- floor(index(a))
  DF <- as.data.frame(a[,1:ncol(a)])
  NAdf <- !is.na(DF)
  Mcount <- aggregate(NAdf,by=list(I),FUN=sum)
   
  Tcount <- apply(Mcount,MARGIN=2,function(x) ifelse(x>=criteria$Threshold,T,F))
  
  YearsIn   <- colSums(Tcount)
  YearsIn   <- YearsIn[-1]
  Mask      <- rep(FALSE,length(YearsIn))
  dex       <- which(YearsIn >= criteria$Year)
  Mask[dex] <- TRUE
  return(dataZoo[,Mask])
  
}
 
  
intersectInvAnomalies <- function(inv,anomalies){
    anomaly.stations <- getStations(anomalies)
    stations <- inv$Id
    common <- intersect(inv$Id,anomaly.stations)
	  if(length(common)==0)stop("No Stations in intersection")
		InvOut <- inv[which(stations %in% common),]
	  anomalyOut <- anomalies[,which(anomaly.stations %in% common)]	   
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

windowV3 <-function(v3Data, start, end){
  if(!is.data.frame(v3Data))stop("v3Data must be a data frame")
  if(!("Year" %in% names(v3Data)))stop("Column named Year is missing")
  if(start > end)stop("start must be less than end")
  if(start %% 1 !=0){
       warning(cat(start, " is being trimmed to ",floor(start),"\n"))
       start <- floor(start)
    } 
  if(end %% 1 !=0){
       warning(cat(end, " is being trimmed to ",floor(end),"\n"))
       end <- floor(end)
    }
  
  Data <- v3Data[which(v3Data$Year >= start & v3Data$Year <= end),]
  return(Data)
}
