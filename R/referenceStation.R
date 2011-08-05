referenceStation = function(Data, offres = .001){
  
  # orginal author Tamino.
   # http://tamino.wordpress.com
   # rewrite and adaptation Steven Mosher July 11th 2011
   # second rewrite July 17th to adapt for mts objects
   # changes output options
   #  single series get reflected back as single series
   # a mts cannot be a single series
   if ( !is.ts(Data)){
     if(!isMts(Data)){
       if(!is.zoo(Data)){
        stop("Must be a time series, mts or zoo object")
       }
     }    
   }
     
   Data <- removeNaStations(Data)
     
   if(is.null(ncol(Data))){
     # output single series
     if (is.zoo(Data)) zout = list(Zoo = Data, offset = 0)
     if (is.ts(Data))  zout = list(Ts = Data,  offset = 0)
     return(zout)
     
   }
  # code below is changed from tamino to get rid of dataframe
  # input and switch to a time based object
  # variable names are the same.
     
nparts <- ncol(Data)
zdat <- as.matrix(Data)
ntimes <- nrow(zdat)
mu <-  rep(0, nparts)
gam <- rep(NA, ntimes)
dmu <- 9999
 
while (dmu >  offres){
  for (jj in 1:ntimes){
     xx <- zdat[jj, ]
     xx <- xx - mu
     gam[jj] <- mean(xx, na.rm = TRUE)      
   }
  oldmu <- mu
  for (jj in 1:nparts){
      xx <- zdat[ ,jj]
      xx <- xx - gam
     mu[jj] <- mean(xx, na.rm = TRUE)    
  }
##################################
# shift offsets so 1st offset is 0
##################################
  mu <- mu - mu[1]  
  dmu <- sum(abs(mu - oldmu))
   
}
############
# final mean
############
num <- rep(NA, ntimes)
se  <- rep(NA, ntimes)
std.dev <- rep(NA, ntimes)
for (jj in 1:ntimes){
   xx <- zdat[jj, ]
   xx <- xx - mu
   xx <- xx[is.finite(xx)]
   num[jj] <- length(xx)
   gam[jj] <- mean(xx)
   std.dev[jj] <- sd(xx)
}
se <- std.dev/sqrt(num)
zout <- data.frame(data = gam, stations=num, se, std.dev)
   
if (is.zoo(Data)){
  zooOut <- zoo(zout,order.by = time(Data))  
  zout <- list(Zoo = zooOut,  offsets = mu)  
}
if (isMts(Data)){
  MtsOut <- ts(zout, start = time(Data)[1], frequency = frequency(Data))
  zout   <- list(Mts = MtsOut, offsets = mu)
}

    
return(zout)
}