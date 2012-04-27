averageStations <- function(Mtsdata, tol,weights = NULL, all = TRUE){
  # orginal authors RomanM and Jeff Id
  # rewrite steven mosher, style correction and better var names
  # modified to add tolerance
  #####################################################################      
  #  FUNCTION MAIN
  #####################################################################
  # exit if you have a single station in Mtsdata
  if (is.null(ncol(Mtsdata))) {
    return(list(Average = Mtsdata,
        Prediction = Mtsdata, Residual = 0, Offsets = 0))
  }
   
  mthInYear   <- 12
	monthCount  <-  nrow(Mtsdata)
	stationCount <- ncol(Mtsdata)
 	 
   
 	if (is.null(weights)) weights  <-  rep(1, stationCount)
 	weights  <- weights/sum(weights) 
 	off.mat  <- matrix(NA, mthInYear, stationCount)
  # get the time series parameters
 	dat.tsp   <- tsp(Mtsdata)
  startYear <- dat.tsp[1]
  ####################################
 	for (month in 1:mthInYear) {
     
     off.mat[month,] <- .calcxOffset(window(Mtsdata, start = c(startYear, month), deltat = 1),tol = tol, wts = weights)
 	}
  #######################################
 	colnames(off.mat) <-  colnames(Mtsdata)
 	rownames(off.mat) <-  month.abb
 	matoff <- matrix(NA, monthCount, stationCount)
  ##
 	for (i in 1:stationCount) {
     matoff[ ,i] <- rep(off.mat[ ,i], length = monthCount)
 	}
  #
 	temp     <- rowMeans(Mtsdata - matoff, na.rm = TRUE)
 	pred     <- NULL
 	residual <- NULL
 	if (all == TRUE)
	{
		pred     <- c(temp) + matoff
 		residual <- Mtsdata - pred
	}
	list(Average = ts(temp,start = c(startYear ,1), frequency = 12),
       Prediction = pred, Residual = residual, Offsets = off.mat)
}