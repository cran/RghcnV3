if(!file.exists(GHCN.V3.DATA)){
   cat(GHCN.V3.DATA, "Doesnt Exist.. creating","\n")
   print("downloading SST.. this will take a while")
   dir.create(GHCN.V3.DATA)
   f<-  downloadSST(url = HADSST2.URL, directory = GHCN.V3.DATA)
    
   
}

targetDir <- file.path(getwd(), GHCN.V3.DATA, fsep=.Platform$file.sep)
files     <- list.files(targetDir, full.names = TRUE, pattern =  "(1850on\\.nc)$"   )
if(length(files)  == 0 )stop("HadSST2_1850on.nc not found")


SST       <- readSST(files)
startYear <- 1900
dl        <- which(layerNames(SST) == as.character(startYear))
SST       <- dropLayer(SST,1:(dl - 1))
endYear   <- 2010
Years     <- endYear + 1 -startYear 
Months    <- Years * 12
SST       <- dropLayer(SST, (Months + 1):nlayers(SST))

SST     <- SST * area(SST, na.rm = TRUE, weight = TRUE) 

sstMonthly <- cellStats(SST ,sum)
sstMonthly <- zoo(sstMonthly, order.by = as.numeric(layerNames(SST)))
sstAnnual  <- aggregate(sstMonthly, by = floor(index(sstMonthly) + 1), FUN = mean)
plot(sstMonthly,main = "SST Temperature Anomaly",
     ylab = " Anomaly (C)",xlab = "Date", col = "grey50")
lines(sstAnnual, col = "blue")
abline(h = 0, col = "black")