getStations <- function(data){
  if (is.zoo(data)){
   
    return(tryCatch(as.numeric(colnames(data)), 
                    warning = function(warn){as.character(colnames(data))}))
  }
  if (isInventory(data)){     
     
    return(tryCatch(as.numeric(data$Id), 
                    warning = function(warn){as.character(data$Id)}))
	 } 
  if (isMts(data)){
     
    return(tryCatch(as.numeric(colnames(data)), 
                    warning = function(warn){as.character(colnames(data))}))
  }
  if (isArray(data)){
    stations <- unlist(dimnames(data)[1])
    return(tryCatch(as.numeric(stations), 
                    warning = function(warn){as.character(stations)}))
     
  }
  warning(" data type not supported. Returning NULL")
  return(NULL) 
}
  

