\name{rasterizeCells}
\alias{rasterizeCells}
 
\title{A function to place cell series into a brick
 
}
\description{The function \code{averageByCell} creates a time series that
  represents the average temperature for a grid cells in a raster.
  This function places those time series in a raster brick structure
 
}
\usage{rasterizeCells(ZooCells, r = GLOBE5)
}
 
\arguments{
  \item{ZooCells}{A 2D zoo object of time series for the cells in a raster.
 
}
  \item{r}{a base raster layer which is used as a format for the output raster brick
    It's resolution must match the input dataset which was created by \code{averageByCell}
}
}
\details{This function takes a zoo object of time series for various cells and
  copies it to a brick. The layer names are set and the Z variable is set. If
  the resolution of ZooCells ( it has an attribute) doesnt match the raster, an
  error is thrown
 
}
\value{Returns a raster brick
 
}
 
\author{Steven Mosher
 
}
 

 

\seealso{\code{\link{averageByCell}}
 
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
  CellAve  <- averageByCell(r = globe3, inventory =DATA$Inventory, Mts = DATA$Mts)
  Texas    <- rasterizeCells(ZooCells = CellAve, r = globe3)

}
 
}
 
\keyword{ spatial }
\keyword{ TimeSeries } 
\keyword{Regression}
