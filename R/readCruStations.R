

readCruStations <- function(filename = "stations-data.txt", output = c("Array","Zoo","Mts")){
       
      if (length(output) > 1) {
         warning("Select One of either V3 or ARRAY or Mts. Using first element by default")
        returnType  <- output[1]
      } else {
        returnType  <- output
      }
      X <- readLines("station-data.txt")
      dex <- which(nchar(X[]) == 72)
      headers  <- X[dex]

      Inv <- data.frame(Id  =  as.numeric(substr(headers,1,6)),
                        Lat = as.numeric(substr(headers,7,10)),
                        Lon = as.numeric(substr(headers,11,15)),
                        Altitude = as.numeric(substr(headers,16,20)),
                        Name = substr(headers,22,42),
                        Country = substr(headers,43,56),
                        StartYear = as.numeric(substr(headers,57,60)),
                        EndYear  =  as.numeric(substr(headers,61,64)),
                        Source   =  as.numeric(substr(headers,67,68)),
                        GoodFirst = as.numeric(substr(headers,69,72)))

    Inv$Lat[Inv$Lat == -999]    <- NA
    Inv$Lon[Inv$Lon == -1999]   <- NA
    Inv$Lat <- Inv$Lat / 10
    Inv$Lon <- Inv$Lon / 10
            
    Inv$Altitude[Inv$Altitude == -999] <- NA

    begin <- min(Inv$StartYear)
    end   <- max(Inv$EndYear)
    Years <- begin:end
    stations <- nrow(Inv)
    DATA <- array(NA,dim=c(stations,12,length(Years)))
    dimnames(DATA)<-list(Inv$Id,month.abb,Years)

    stationCount <- 0
    for(line in 1:length(X)){
         if (nchar(X[line]) == 72){
            # we have a header
            stationCount <- stationCount + 1
          } else {
             # we have a data record
            thisLine <- as.numeric(unlist(strsplit(X[line]," +")))
            thisLine[thisLine == -999] <-NA
            thisYear <- thisLine[1] - begin + 1
            DATA[stationCount,,thisYear] <- thisLine[2:13]/10
     
          }
    }
  ## Prepare for output
      if (returnType == "Mts" | returnType == "Zoo"){
           DATA <- apply(DATA,MARGIN = 1, FUN = c)
           DATA <- ts(DATA, start = begin, frequency = 12)
           if (returnType == "Zoo") DATA <- asZoo(DATA)
      }
      if (returnType == "Mts"){
        return(list(Inventory = Inv,Mts = DATA))
        
      }
      if (returnType == "Zoo"){
        return(list(Inventory = Inv,Zoo = DATA))
      }
      if (returnType == "Array"){
        return(list(Inventory = Inv,Array = DATA))
      }
 
}

 
 