\name{averageStations}
\alias{averageStations}
 
\title{A function to optimally average stations
 
}
\description{The offset method uses all available information in the collection
  of temperature series to estimate the average temperature of all the stations.
  Unlike the Common anomaly method which relies on stations have periods where 
  they overlap in temporal coverage, this method uses all series however long
  or short to determine an optimal estimate for the collection that minimizes
  the stations offsets from each other. The function takes a collection of time
  series  and then performs a regression on  to determine the optimal estimate 
  for that collection of stations
 
}
\usage{averageStations(Mtsdata, tol, weights = NULL, all = TRUE)

 
}
 
\arguments{
  \item{Mtsdata}{A "mts" class object with multiple time series in columns
 
}
  \item{tol}{tolerance for calculating the psuedoinverse. The function
   at its core relies on a psuedoinverse which relies on a singular
   value decomposition.  That function must round very small values
   to zero to operate effectively. This variable may require some
   "tweaking" in cases where their are very sparse data or very large
   data. .Machine$double.eps is a plce to start, but in some cases
   values towards .0001 or larger are required. Errors will be
   noticeable upon simple inspection. 
 
}
  \item{weights}{Optional weights
 
}
  \item{all}{output option for outputing all the data
 
}
}
\details{The function performs RomanM's offset method for anomaly regression.
   internally it calls the two functions \code{.calcxOffset} and the
   function \code{.psx()}. Those files can be viewed in the package source
   and are not exported as functions.
 
}
\value{a list of returned
 \item{Average}{The estimated average of all time series in zoo format}
 \item{Prediction}{The predicted value of every time series, missing data is predicted}
 \item{Residual}{The residual}
 \item{Offsets}{The offsets for every station. The function minimizes offsets}
  
 
 
}
\references{\url{http://statpad.wordpress.com/2010/03/08/combining-stations-plan-c/ }
 
}
\author{RomanM, JeffId and clean up by Steven Mosher
 
}
 

 

\seealso{\code{\link{referenceStation}}
 
}
\examples{\dontrun{
  TEXAS.DAT <- system.file("external/Texas.dat", package = "RghcnV3")
  TEXAS.INV <- system.file("external/Texas.inv", package = "RghcnV3")
  texas    <- readV3Data(TEXAS.DAT,output="Mts")
  inv      <- readInventory(TEXAS.INV)
  texas    <- window(texas,start =1900, end = 2010 +11/12)
   
  TexasAve <- averageStations(Mtsdata = texas)
}
 
}
 
\keyword{ spatial }
\keyword{ TimeSeries } 
\keyword{ Regression }