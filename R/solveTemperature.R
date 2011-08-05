solveTemperature=function(Data,Weights){ 
  # check if data is an array
  
  # NAs become 0
  Weights[is.na(Weights)]  <- 0
  Data[is.na(Data)]        <- 0
  Data <- Data * Weights 
  d    <- dim(Data) 
  d    <- c(d[1] * d[2], d[3]) 
  dim(Data) = dim(Weights) = d 
  A    <- rowSums(Weights)+1.0e-9 
  R1   <- rowSums(Data) 
  C    <- colSums(Weights) 
  R2   <- colSums(Data) 
  aw   <- array(rep(1/A,d[2])*Weights,d) 

  b    <-  drop(R2-R1 %*% aw) 
  S    <-  diag(C) - t(Weights) %*% aw 

  S[1,1] <- S[1,1] + 1  
  y <- solve(S,b) 
  return(y-mean(y))
  
}