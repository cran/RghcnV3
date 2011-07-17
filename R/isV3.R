isV3 <- function(data){
  if (!is.data.frame(data)){
    
    return(FALSE)
  }
  if (ncol(data) == 14){
    cnames <- colnames(data)
    if (!identical(FILE.PARAMETERS$DataColumns,colnames(data))){
     
      return(FALSE)
    }
  return(TRUE)
    
  } else {
    
    return(FALSE)
  }
    
}


 