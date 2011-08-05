passesCam <- function(Data, criteria = list(Start = 1961, End = 1990, Years = 15, Threshold = 12)){
       
        criteria <- checkCriteria(criteria)
         
        if ( is.zoo(Data) | isMts(Data)){
             criteria <- checkCriteria(criteria)
             basePeriod    <- window(Data, start = criteria$Start, end = criteria$End + (11/12))
             I    <- floor(index(basePeriod))
             Mcount  <- aggregate(!is.na(basePeriod),by=list(I), FUN=sum)
             Tcount  <- apply(Mcount[,-1], MARGIN = 2, function(x) ifelse(x >= criteria$Threshold, TRUE, FALSE))    
             YearsIn   <- colSums(Tcount)   
             Mask      <- rep(FALSE, length(YearsIn))       
             dex       <- which(YearsIn >= criteria$Years)   
             Mask[dex] <- TRUE
             Data <- Data[,Mask]
              
          return( Data )         
        }
        if (isArray(Data)){
            years <- as.numeric(unlist(dimnames(Data)[3]))
            begin <- which(years == criteria$Start)
            end   <- which(years == criteria$End)
            basePeriod <- !is.na(Data[ , ,begin:end])
            Mcount  <- apply(basePeriod, MARGIN = 1,FUN = colSums)          
            Tcount  <- apply(Mcount ,MARGIN = 2, function(x) ifelse( x >= criteria$Threshold,TRUE,FALSE))
            YearsIn <- colSums(Tcount )
            Mask       <- rep(FALSE, length(YearsIn ))
            dex        <- which(YearsIn  >= criteria$Years)
            Mask[dex ] <- TRUE
            Data <- Data[Mask,,]
              
           return(Data)
          
        }
         
         
        stop(" Must be a zoo object, Mts or Array")
        
   }