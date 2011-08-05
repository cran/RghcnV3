 

createAnomaly<-function(Data, criteria=list(Start = 1961, End = 1990,
                                              Years = 15, Threshold = 12) ){
  require("zoo")
  if (is.zoo(Data) | isMts(Data) | isArray(Data)){
    Data <- passesCam(Data,criteria)
    return(anomalize(Data))   
  }
  stop( "Must supply an Array,Zoo or mts object")  
  
}