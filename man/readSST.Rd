\name{readSST}
\alias{readSST}
 
\title{A function to read SST data
 
}
\description{Hadley Sea surface temperature data SST is read from disk
  into a \code{raster} \code{\link[raster]{brick}}. The data extends
  from 1850 to the present. The data is contain in 5 degree grid cells.
  each cell has an anomaly frome the base period 1961-1990. If you
  choose different period for your land analysis you should rebaseline
  the SST data. 
 
}
\usage{
readSST(filename)
}
 
\arguments{
  \item{filename}{The file path to the "ncdf" file. Upon download
    this file is named \code{"HadSST2_1850on.nc"}
 
}
}
\details{From the website description:
 
 
}
\value{The function returns a \code{raster} object. The 
  \code{\link[raster]{layerNames}} have been replaced with
  time data where numeric dates are used. There is a layer
  per month
 
}
\references{url{http://www.metoffice.gov.uk/hadobs/hadsst2/rayner_etal_2005.pdf}
  The above paper describes the dataset
 
}
\author{Steven Mosher
 
}
 

\seealso{\code{\link[raster]{brick}}
 
}
\examples{
 \dontrun{SST <- readSST(filename = "HadSST2_1850on.nc")}
}
 
\keyword{ file }
 
