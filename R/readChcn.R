 

readChcn  <- function(filename, output = c("Array","Mts","Zoo")){
  
    if (length(output) > 1) {
         warning("Select One of either V3 or ARRAY or Mts. Using first element by default")
        returnType  <- output[1]
    } else {
        returnType  <- output
    }
     
    Data  <- read.table(filename)
     
    
    minYear <- min(Data[,2])
    Data    <- .toArray(as.matrix(Data))
    if (returnType == "Mts" | returnType == "Zoo"){
              Data <- apply(Data,MARGIN = 1, FUN = c)
              Data <- ts(Data, start = minYear, frequency = 12)
              if (returnType == "Zoo") Data <- asZoo(Data)
    }
    
     
  return(Data)    
}