 checkCriteria <- function(criteria){
  
    if (criteria$Start >= criteria$End) stop( " Start must be less than End")
    if (criteria$Start %% 1 != 0){
       warning(cat(criteria$Start, " is being trimmed to ", floor(criteria$Start), "\n"))
       criteria$Start <- floor(criteria$Start)
    } 
    if (criteria$End %% 1 != 0){
       warning(cat(criteria$End, " is being trimmed to ", floor(criteria$End), "\n"))
       criteria$End <- floor(criteria$End)
    }
    if (criteria$Year > (criteria$End - (criteria$Start - 1))){
      stop("Year must be less than End-Start")
    }
    if (criteria$Threshold > 12) stop("Threshold is too large.use 12 months or less")
 return(criteria)  
}
