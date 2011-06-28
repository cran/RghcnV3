\name{getLonLat}
\alias{getLonLat}

\title{ A convenience function to create a 2 column matrix of Longitude and Latitude
  
}
\description{ The inventory data.frame has station Latitude and Longitude as
  well as other metadata. When using the raster package several functions
  require that points be supplied in a 2 column matrix or 2 column dataframe
  of Longitude and Latitude. This function returns a data structure that
  can be used as a parameter to raster calls.
 
}
\usage{
 getLonLat(inventory)
}
 
\arguments{
  \item{inventory}{ The function is passed an inventory data.frame
    That data.frame should have  columns named Lat and Lon. Inventories
    created with this package do have those names
 
}
}
\details{ This is a convience function for creating a simple two column
  matrix of coordinates of stations with Longitude in column 1 and
  Latitude in Column 2. That order (x,y) is the order expected by
  functions in the raster package.
 
}
\value{ The function returns a matrix of two columns. Column 1 is Longitude
  (-180 to 180) and column 2 is Latitude (-90 to 90). The number of rows
  is equal to the stations. 
 
}

\author{ Steven Mosher}
 
\note{ For future versions the Id variable will be used as a
  rowname, provided that is compatible with raster calls.
   
 
}

 
\examples{
\dontrun{ stationCoordinates <- getLonLat(inventory)}
 
}
 
\keyword{ spatial }
