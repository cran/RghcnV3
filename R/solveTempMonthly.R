solveTempMonthly <-  function (Data, Weights){ 
    
    Weights[is.na(Weights)] <- 0
    #  The oD matrices record where there is no data
    oD <- is.na(Data)
    oDsm = rowSums(!oD, dims =2 )==0
    oDym = colSums(!oD, dims =1 )==0
    Data[oD] <- 0
    d <- dim(Data)
    Residual <- array(NA, d)
    dim(Weights) <-  d
    DW <- Data * Weights
    y <- matrix(NA, d[2], d[3])
    L <- matrix(NA, d[1], d[2])
    for(i in 1:d[2]){
      A <- rowSums(Weights[,i,]) + 1e-09
      R1 <- rowSums(DW[,i,])
      C <- colSums(Weights[,i,])
      R2 <- colSums(DW[,i,])
      aw <- array(rep(1/A, d[3]) * Weights[,i,], d[c(1,3)])
      b <- drop(R2 - R1 %*% aw)
      S <- diag(C) - t(Weights[,i,]) %*% aw
      S[1, 1] <- S[1, 1] + 1
      x <- solve(S, b)
      x <- x - mean(x)
      L[,i] <- (1/A)*R1 - aw %*% x  # Local mean (offset)
      Residual[,i,] <- Data[,i,] - rep(L[,i],d[3]) - rep(x,each=d[1]) 
      y[i,] <- x
    }
    y[oDym] <- NA
    L[oDsm] <- NA
    Residual[oD] <- NA
    return(list(Solution = y, Resid = Residual, LocalOffset =L))
}