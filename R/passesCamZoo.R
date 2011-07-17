passesCamZoo <- function(dataZoo, criteria = list(Start = 1961, End = 1990,
                                                     Years = 15, Threshold = 12)){
   
  criteria <- checkCriteria(criteria)
  if (!is.zoo(dataZoo)) stop("must be a Zoo object")
  a    <- window(dataZoo, start = criteria$Start, end = criteria$End + (11/12))
  I    <- floor(index(a))
  DF   <- as.data.frame(a[ ,1:ncol(a)])
  NAdf <- !is.na(DF)
  Mcount <- aggregate(NAdf, by = list(I), FUN = sum)
   
  Tcount <- apply(Mcount, MARGIN = 2, function(x) ifelse(x>=criteria$Threshold,T,F))
  
  YearsIn   <- colSums(Tcount)
  YearsIn   <- YearsIn[-1]
  Mask      <- rep(FALSE, length(YearsIn))
  dex       <- which(YearsIn >= criteria$Years)
  Mask[dex] <- TRUE
  return(dataZoo[ ,Mask])
  
}