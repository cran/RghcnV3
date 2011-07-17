keepIf  <- function(yearData, cnt){ 
  temp <- sum(!is.na(yearData))
  if (temp >= cnt) return(mean(yearData,na.rm = TRUE)) else return(NA)    
}