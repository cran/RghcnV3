windowV3 <-function(v3Data, start, end){
  if (!isV3(v3Data)) stop("v3Data must be a data frame")
   
  if (start > end)stop("start must be less than end")
  if(start %% 1 != 0){
       warning(cat(start, " is being trimmed to ", floor(start), "\n"))
       start <- floor(start)
    } 
  if(end %% 1 != 0){
       warning(cat(end, " is being trimmed to ", floor(end), "\n"))
       end <- floor(end)
    }
  
  Data <- v3Data[which(v3Data$Year >= start & v3Data$Year <= end), ]
  return(Data)
}
