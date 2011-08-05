\name{averageByCell}
\alias{averageByCell}
 
\title{Average time series in a cell using regression
 
}
\description{The offset method uses all available information in the collection
  of temperature series to estimate the average temperature of all the stations.
  Unlike the Common anomaly method which relies on stations have periods where 
  they overlap in temporal coverage, this method uses all series however long
  or short to determine an optimal estimate for the collection that minimizes
  the stations offsets from each other. The function takes a collection of time
  series and divides them into various cells in a raster and then performs a
  regression on each cell to determine the optimal estimate for that cell
 
}
\usage{ averageByCell( inventory, Mts,r = GLOBE5)
}
 
\arguments{
  \item{r}{A raster object that will be used as a template to build a brick
  The default is the built in 5 degree earth. This can be changed to other 
  resolutions. The raster is used to generate the proper cells for allocating
  stations to cells on the globe
 
}
  \item{inventory}{The inventory of stations
 
}
  \item{Mts}{A "mts" class object that has station data in columns.  
 
}
}
\details{The function operates by taking in a raster, an inventory, and a "mts"
  object. The function then allocates stations to raster cells and then calls
  the function \code{\link{averageStations}} for every cell in the grid. The result
  is an object that contains a time series for every cell.
 
}
\value{The function outputs a  2D zoo series. The series has a column for every
  grid cell. Column names identify the cell by the numbering system used by raster.
  This object as an attribute added to it to identify the resolution of the raster
  that was used to generate the cell numbers. The name of the attribute is "Res".
 
}
\references{\url{http://statpad.wordpress.com/2010/03/08/combining-stations-plan-c/ }
 
}
\author{Steven Mosher
 
}
\note{This function is called prior to putting the cell and all its layers
  into a brick
 
}

 

\seealso{\code{\link{averageStations}}, \code{\link{rasterizeCells}}
 
}
\examples{\dontrun{
  TEXAS.DAT <- system.file("external/Texas.dat", package = "RghcnV3")
  TEXAS.INV <- system.file("external/Texas.inv", package = "RghcnV3")
  texas <- readV3Data(TEXAS.DAT,output = "Mts")
  inv   <- readInventory(TEXAS.INV)
  texas <- window(texas,start =1900, end = 2010 +11/12)   
  DATA  <- intersectInvData(inv,texas)
  globe3 <- GLOBE5
  res(globe3) <- 3
  ZooCell <- averageByCell(r = globe3, inventory = DATA$Inventory, Mts = DATA$Mts)
  
}
 
}
 
\keyword{ spatial }
\keyword{ TimeSeries }
\keyword{ Regression }
