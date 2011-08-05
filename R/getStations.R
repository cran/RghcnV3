getStations <- function(data){
  if (is.zoo(data)){
    return(as.numeric(colnames(data)))   
  }
  if (isInventory(data)){     
    return(as.numeric(data$Id))    
	 } 
  if (isMts(data)){
    return(as.numeric(colnames(data)))
  }
  if (isArray(data)){
    stations <- unlist(dimnames(data)[1])
    return(unique(as.numeric(stations)))
  }
  warning(" data type not supported. Returning NULL")
  return(NULL) 
}
  

