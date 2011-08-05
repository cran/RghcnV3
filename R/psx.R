### subfunction to do pseudoinverse
.psxInv = function(mat, tol = NULL){
   #Author Roman M
   # R style Steven Mosher
   if (NCOL(mat)==1) return( mat /sum(mat^2))
	msvd <- svd(mat)
 	dind <- msvd$d
	if (is.null(tol))
	{
		tol <- max(NROW(mat),NCOL(mat))*max(dind)*.Machine$double.eps
	}
 	dind[dind<tol] <- 0
 	dind[dind>0] <- 1/dind[dind>0]
 	inv <- msvd$v %*% diag(dind, length(dind)) %*% t(msvd$u)
	inv
}