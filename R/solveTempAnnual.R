solveTempAnnual = function (Data, Weights) 
{
    Weights[is.na(Weights)] <- 0
    #  The oD matrices record where there is no data
    oD=!is.na(Data)
    oDsm=rowSums(oD,dims=2)==0
    oDy=colSums(oD,dims=2)==0
    # Replace NA by 0 to simplify arithmetic
    Data[is.na(Data)] <- 0
    d <- dim(Data)
    d <- c(d[1] * d[2], d[3])
    dim(Data) = dim(Weights) = d
    DW <- Data * Weights
    A <- rowSums(Weights) + 1e-09
    R1 <- rowSums(DW)
    C <- colSums(Weights)
    R2 <- colSums(DW)
    aw <- array(rep(1/A, d[2]) * Weights, d)
    b <- drop(R2 - R1 %*% aw)
    S <- diag(C) - t(Weights) %*% aw
    S[1, 1] <- S[1, 1] + 1  # Deal with ambiguous steady level
    y <- solve(S, b)
    y <- y - mean(y)
    L <- (1/A)*R1 - aw %*% y # Local mean (offset)
    Residual <- Data - rep(L,d[2]) - rep(y,each=d[1]) 
    #  Restore NA's 
    y[oDy]=NA
    L[oDsm]=NA
    dim(Residual)=dim(oD)
    Residual[!oD]=NA
    return(list(Solution=y,Resid=Residual,LocalOffset=L))
    
}