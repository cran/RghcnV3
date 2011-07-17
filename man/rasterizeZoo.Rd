\name{rasterizeZoo}
\alias{rasterizeZoo}

\title{A function to rasterize 2D Zoo
 
}
\description{The first step in bringing the series of time
  data into the raster system is the process of rasterization.
  Every station has a Lat/Lon. In \code{rasterizeZoo}
  stations that lie in the same grid cell are averaged together.
  This function, then outputs a gridded average of time series.
  It relies on the raster function \code{\link[raster]{rasterize}}
  At this stage the time series are not area weighted. They
  are simply averaged with each other per grid cell. 
 
}
\usage{
rasterizeZoo(inventory, Zoo, land = GLOBE5)
}
 
\arguments{
  \item{inventory}{An inventory data.frame produced by the
  function \code{intersectInvZoo}. If the stations
  in the inventory do not match those in the zoo series, 
  an error is thrown.
 
}
  \item{Zoo}{a zoo object of data produced by the
  function \code{intersectInvZoo}, or by
  the function \code{\link{v3ToZoo}}
  If the stations in the zoo object do not match those in the inventory 
  an error is thrown. Inputs are not limited to anomalies.
  Temperature zoo objects can also be fed to the function.
  Stations must match in the two data structures.
  
 
}
  \item{land}{land is defaulted to an empty raster with
  5 degree cells. The empty raster will be filled with
  time series according to the Lat/Lon in the inventory.
 
}
}
\details{In the Common Anomaly Method stations that lie in the
  same grid cell are averaged. This average is not area weighted
  at this stage of processing. For every month in the zoo series
  a new layer of the raster object is created. This results
  in a \code{brick} with a number of layers that is equal to
  months in the data. The time index of the zoo object is
  used as the \code{layerNames} of the \code{brick}.
  It will also be set in the \code{z slot} which can
  be accessed via \code{getZ} in the raster package.
  For more details please see the documentation on the
  function \code{rasterize}. Effectively, the "points"
  from the Inventory and the zoo series of data that is associated
  with those points is fed to the \code{rasterize} function.
  
 
}
\value{The function returns a \code{brick}. That brick will
  have \code{layerNames} that represent time. Every grid
  cell will have the average anomaly of all the stations in
  that grid cell. This data can be plotted, summarized,
  or further calculations can be performed. Typically, the
  next step is to area weight the anomalies using a
  land percentage map and the raster \code{area} function.
 
}
 
\author{Steven Mosher
 
}
 
 
\examples{
\dontrun{
 Data <- intersectInvZoo(inventory,anomalies)
 Land <- rasterizeZoo(Data$Inventory,Data$Zoo)
 Amonth  <-raster(Land,layer=1000)
 plot(Amonth)
}
 
}
 
\keyword{ spatial }
 
