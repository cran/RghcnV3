isBigMatrix <- function(x){
  
  if ( class(x)[1] != "big.matrix") return(FALSE) 
  return(TRUE)
  
}