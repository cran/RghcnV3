tempStats <- function(data){
  if(isArray(data) | isMts(data) | is.zoo(data)){
    if( !is.zoo(data)) data <- asZoo(data)
    
    AveTemp <-  apply(coredata(data),MARGIN = 2,FUN = mean, na.rm =TRUE)
    MinTemp <-  apply(coredata(data),MARGIN = 2,FUN = min, na.rm =TRUE) 
    MaxTemp <-  apply(coredata(data),MARGIN = 2,FUN = max, na.rm =TRUE)
    SD      <-  apply(coredata(data),MARGIN = 2,FUN = sd, na.rm =TRUE)
    Mcount  <-  apply(!is.na(coredata(data)),MARGIN = 2,FUN = sum )
     
    ydex    <- rep(1:(nrow(data)/12), each = 12)
    
    Y       <-  aggregate(!is.na(data), by = list(ydex), FUN = sum)
    W       <- apply(Y, MARGIN = 2, FUN = function(x) {ifelse(x == 12,T,F)})
    Years   <- apply(W,MARGIN = 2, FUN = sum)
    Years   <- Years[-1]
     
    options(warn = -1)
    FY      <- apply(W, MARGIN= 2, FUN = function(x){ min(which(x == TRUE),na.rm = TRUE)})
     LY      <- apply(W, MARGIN= 2, FUN = function(x){ max(which(x == TRUE),na.rm = TRUE)})
    options(warn = 0)
    FY[is.infinite(FY)]    <- 0
    LY[is.infinite(LY)]    <- 0
    FY     <- FY + as.integer(min(time(data)))
    FY     <- FY - 1
    LY     <- LY + as.integer(min(time(data)))
    
    FY[FY < as.integer(min(time(data)))] <- NA
    
    maskEarly <-  camMask(data,criteria = list(Start = min(time(data)), 
                                               End = (min(time(data))+9) , Years = 10, Threshold = 10))
    maskLate <-  camMask(data,criteria = list(Start = (max(time(data))-9), 
                                               End =  max(time(data))  , Years = 10, Threshold = 10))
     
    DF      <- data.frame(Id = names(AveTemp), mean = AveTemp,max = MaxTemp, 
                          min=MinTemp, sd = SD, Tcount = Mcount, 
                          FullYears = Years,FirstYear = FY[ -1],
                          LastYear = LY[ -1],
                           
                          First10Cam =maskEarly,
                          Last10Cam  =maskLate)
    return(DF)
  }
  stop("must be an array mts or zoo object")
  
}