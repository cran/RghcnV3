\name{getStations}
\alias{getStations}
 
\title{ A function to return the stations from a data object}
 
\description{ This convenience function returns a vector of station Ids from
  the dataset that is passed to it. It can be passed a inventory
  v3 Data or a zoo object (temperatures or anomalies) or a
  \code{mts} object. A \code{mts} is a multiple time series. A properly
  formated \code{mts} can be created by the \code{v3ToMts} function. In
  the case of passing this function a V3 data.frame only the unique Ids
  are returned
  
  
   
 
}
\usage{
getStations(data)
}
 
\arguments{
  \item{data}{ the function takes in an inventory data.frame or a 
   matrix  of "zoo" series as created by \code{createAnomaly} or
  anomalies as created by \code{intersectInvZoo}.
  Based on the object class of \code{data} a vector of station Ids is
  returned. 
 
}
}
\details{If the \code{data} fed to the function is a data.frame with a
  column named "Id" then that column is returned as a vector.
  If the data is a "zoo" object, then the column names are returned
  as a vector of Ids. 
}

\value{ the function returns a vector of station Ids.  
 
}
 
\author{ Steven Mosher}
 

 
\examples{
\dontrun{ StationsId <- getStations(anomalies)
          StationId <- getStations(Inventory)
          identical(getStations(Inventory),Inventory$Id)}
 
}
 
\keyword{ spatial }
 