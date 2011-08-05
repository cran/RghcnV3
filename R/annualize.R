annualize <- function(Data, user.fun = NULL, na.rm = TRUE, ...){
  
  if (isArray(Data) | is.zoo(Data) | isMts(Data) | is.ts(Data)){
  if (isArray(Data)) Data <- asZoo(Data)
  if (frequency(Data) != 12) stop("Frequency must be 12")
  if (is.null(user.fun)  && (!is.logical(na.rm))) stop("na.rm must be a logical") 
  if (is.function(user.fun)  && (is.logical(na.rm))) stop("na.rm must be NULL, if fun is specified") 
     if (is.logical(na.rm)){
         return( aggregate(Data, by = floor(index(Data)),
                      FUN = mean, na.rm = na.rm, regular = TRUE ))
  
      }
     if (is.function(user.fun)){
         return( aggregate(Data, by = floor(index(Data)),
                      FUN = user.fun, ..., regular = TRUE ))
      }
  
  }
  stop("input Must be an Array, zoo or mts class object")
}  
  
  
  
    
  
  
  
  
    
 