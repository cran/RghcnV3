passesCamV3 <- function(Tdata, criteria = list(Start = 1961, End = 1990, Years = 15, Threshold = 12)){
       
        criteria <- checkCriteria(criteria)
        if (!isV3(Tdata)) stop("must be a v3 dataset")
        data  <- Tdata[which(Tdata$Year >= criteria$Start & Tdata$Year <= criteria$End), ]
        # column 3 is where temps start
        mcount   <- rowSums(!is.na(data[ ,3:14]), na.rm = TRUE)
        DF       <- data.frame(Ghcnid = data[ ,1], Year = data[ ,2], Count = mcount)
        DF$Count <- DF$Count >= criteria$Threshold
        DF2      <- rowsum(as.numeric(DF$Count), DF$Ghcnid, na.rm = TRUE)
        DF3      <- data.frame(Ghcnid = rownames(DF2), Years = DF2[ ,1])
        ids      <- as.vector(unique(DF3$Ghcnid[DF3$Years >= criteria$Years]))
        mask     <- which(Tdata$Id %in% as.numeric(ids))
       Tdata     <- Tdata[mask, ]
       return(Tdata)	
   }