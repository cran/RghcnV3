windowArray <-function(Data, start, end){
      if (!isArray(Data)) stop(" Data must be an array")
   
      if (start > end)stop("start must be less than end")
      if(start %% 1 != 0){
           warning(cat(start, " is being trimmed to ", floor(start), "\n"))
           start <- floor(start)
      } 
      if(end %% 1 != 0){
           warning(cat(end, " is being trimmed to ", floor(end), "\n"))
          end <- floor(end)
      }
    years <- as.numeric(unlist(dimnames(Data)[3]))
     
    begin <- which(years == start)
    end   <- which(years == end)
  
    Data <-  Data[ , ,begin:end]
  return(Data)
}