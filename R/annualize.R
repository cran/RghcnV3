annualize <- function(zooMonthly, user.fun = NULL, na.rm =TRUE, ...){
     
  if (!is.zoo(zooMonthly)) stop(" zooMonthly must be a zoo object")
  if (frequency(zooMonthly) != 12) stop("Frequency must be 12")
  if (is.null(user.fun)  && (!is.logical(na.rm)))stop("na.rm must be a logical")  
  if (is.function(user.fun)  && (is.logical(na.rm)))stop("na.rm must be NULL, if fun is specified")
  
  if (is.logical(na.rm)){
    return( aggregate(zooMonthly, by = floor(index(zooMonthly)),
                      FUN = mean, na.rm = na.rm, regular = TRUE ))
  }
  if (is.function(user.fun)){
    return( aggregate(zooMonthly, by = floor(index(zooMonthly)),
                      FUN = user.fun, ..., regular = TRUE ))
  }
    
}
 