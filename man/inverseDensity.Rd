\name{inverseDensity}
\alias{inverseDensity}
 
\title{calculates the inverse density of station reports
 
}
\description{Stations are allocated to grid cells on the planet.
  then for every month in the entire time series a count
  is generated for the number of valid reports in that cell for
  that month. This value then scales the area of the grid cell.
  The outout is a series of weights for every gridcell on the globe
  for every month in the series. The area is effectievl divided by
  the number of reports so that series are weighted inversely to
  their density on an area basis. many stations in a small area
  would receieve little weight. One station in a large area would
  get a large weight. 
 
}
\usage{inverseDensity(inv, Data, r = GLOBE5)
}
 
\arguments{
  \item{inv}{the station inventory which is used for Lat/lon
 
}
  \item{Data}{the temperature data in Array format. If you need to pass in
  a  Zoo type object covert using \code{asZoo}
 
}
  \item{r}{a raster layer. The resolution of the raster determines the
  gridding of the data. It also determines the Area used for weighting.
  The raster cells should have values between 0 and 1. These values
  should represent the FRACTION of each cell that should be used in
  the area calculation. This allows you to use a land mask to mask
  off portions of a cell that are Ocean or water or outside an
  area of interest. A value of  .5 indicates that half of a cells
  area should not be used. A cel thatis 50 percent water thus would
  have its area adjusted accordingly. This also lets you use irregular
  areas or polygons inside the square raster
 
}
}
\details{The inventory and data are checked to ensure that stations match.
  Then an array is created to collect the "counts" of temperature
  reports for every gridcell for every month. The area of the grid
  cell is then divided by the count data to produce an inverse density
 
}
\value{Return a 3D array of weights for use in other functions
 
}
 
\author{Nick Stokes, rewritten for integration
  by Steven Mosher. Land masking added by Steven Mosher
 
}
 
 
\examples{
 \dontrun{
  weights <-inverseDensity(Inventory,Data)
 }
}
 
\keyword{ spatial }
 
