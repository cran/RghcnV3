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
  \item{r}{a raster layer. This is used to generate cell values
 
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
  by Steven Mosher
 
}
 
 
\examples{
 \dontrun{
  weights <-inverseDensity(Inventory,Data)
 }
}
 
\keyword{ spatial }
 
