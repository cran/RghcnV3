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
  if (isV3(data)){
    return(unique(as.numeric(data$Id)))
  }
  warning(" data type not supported. Returning NULL")
  return(NULL) 
}
  

