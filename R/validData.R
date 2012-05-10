validData <- function(Data){
  
  if(isArray(Data) | isMts(Data) |is.zoo(Data)){
    if(sum(duplicated(getStations(Data))) !=0){
      print("duplicated stations")
      return(FALSE)
    }  
    if(!validMonthCount(Data)){
       print("Invalid Month count. USE trimHeadtail")
       return(FALSE)
    
    }
    if(sum(allNA(Data)) != 0){
      print("Some Months have no data for any station")
      return(FALSE)
     }
     return(TRUE)
  }else{
    print("Data must me a zoo object or Mts or 3D array")
    return(FALSE)
    
    
  }
  
  
}