asMts <- function(x){
  if ( is.zoo(x)){
    return(ts(x, start = min(time(x)),frequency = 12))     
  }  
  if ( isArray(x)){
        years <-  unlist(dimnames(x)[3])
        out   <-   apply(x,MARGIN = 1, FUN = c)
        
        begin <-   min(as.numeric(years))
        out   <-   ts(out, start = begin, frequency = 12)
    return(out)   
  }
  stop(cat(x, "must be an array or mts", "\n"))
  
}