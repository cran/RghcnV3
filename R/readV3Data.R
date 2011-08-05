 

readV3Data  <- function(filename, output = c("Array","Mts","Zoo"), Parameters = FILE.PARAMETERS){
  
    if (length(output) > 1) {
         warning("Select One of either V3 or ARRAY or Mts. Using first element by default")
        returnType  <- output[1]
    } else {
        returnType  <- output
    }
    
    txt <- readLines(filename) 
    n <-   length(txt) 
    out <- matrix(NA,n,14)  
    out[,1] <- as.numeric(substr(txt,1,11)) 
    out[,2] <- as.numeric(substr(txt,12,15)) 
    ii <- seq(from =20, to = 108, by = 8 ) 
    for(i in 1:12)out[ ,i + 2] <- as.numeric(substr(txt, ii[i], ii[i] +4 ))/100 
    out[out == -99.99] <- NA
     
    minYear <- min(out[ ,2])
    out    <- .toArray(out)
    if (returnType == "Mts" | returnType == "Zoo"){
              out <- apply(out,MARGIN = 1, FUN = c)
              out <- ts(out, start = minYear, frequency = 12)
              if (returnType == "Zoo") out <- asZoo(out)
    }
    
     
  return(out)    
}    
    
     