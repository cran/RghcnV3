\name{rasterizeAnomaly}
\alias{rasterizeAnomaly}

\title{A function to rasterize anomalies
 
}
\description{The first step in bringing the series of anomaly
  data into the raster system is the process of rasterization.
  Every station has a Lat/Lon. In \code{rasterizeAnomaly}
  stations that lie in the same grid cell are averaged together.
  This function, then outputs a gridded average of anomalies.
  It relies on the raster function \code{\link[raster]{rasterize}}
  At this stage the anomalies are not area weighted. They
  are simply averaged with each other per grid cell
 
}
\usage{
rasterizeAnomaly(inventory, anomaly, land = GLOBE5)
}
 
\arguments{
  \item{inventory}{An inventory data.frame produced by the
  function \code{intersectInvAnomalies}. If the stations
  in the inventory do not match those in the zoo series 
  an error is thrown
 
}
  \item{anomaly}{a zoo object of anomalies produced by the
  function \code{intersectInvAnomalies}. If the stations
  in the zoo object do not match those in the zoo series 
  an error is thrown
 
}
  \item{land}{land is defaulted to an empty raster with
  5 degree cells. The empty raster will be filled with
  anomaly series according to the Lat/Lon in the inventory.
 
}
}
\details{In the Common Anomaly Method stations that lie in the
  same grid cell are averaged. This average is not area weighted
  at this stage of processing. For every month in the zoo series
  a new layer of the raster object is created. This results
  in a \code{brick} with a number of layers that is equal to
  months in the data. The time index of the zoo object is
  used as the \code{layerNames} of the \code{brick}. 
 
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
 Data <- intersectInvAnomalies(inventory,anomalies)
 Land <- rasterizeAnomaly(Data$Inventory,Data$Anomalies)
 Amonth  <-raster(Land,layer=1000)
 plot(Amonth)
}
 
}
 
\keyword{ spatial }
 
