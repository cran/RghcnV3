asZoo <- function(x){
  if ( isMts(x) | is.ts(x)){
    return(zoo(x, order.by = as.yearmon(time(x))))     
  }  
  if ( isArray(x)){
       
        years <-  unlist(dimnames(x)[3])
        out   <-   apply(x,MARGIN = 1, FUN = c)        
        begin <-   min(as.numeric(years))
        out   <-   ts(out, start = begin, frequency = 12)
    return(zoo(out, order.by = as.yearmon(time(out))))   
  }
  stop( "must be an array or mts" )
  
}