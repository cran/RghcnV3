.calcxOffset = function(tdat,tol,wts){
  # Author Roman M
  ## new version
 	nr = length(wts)
 	delt.mat = !is.na(tdat)
 	delt.vec = rowSums(delt.mat)
 	row.miss= (delt.vec ==0)
 	delt2 = delt.mat/(delt.vec+row.miss)
 	co.mat = diag(colSums(delt.mat)) - (t(delt.mat)%*% delt2)
 	co.vec = colSums(delt.mat*tdat,na.rm=T) - colSums(rowSums(delt.mat*tdat,na.rm=T)*delt2)
 	co.mat[nr,] = wts
 	co.vec[nr]=0
   
 	.psxInv(co.mat,tol=tol)%*%co.vec
}