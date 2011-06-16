\name{getStations}
\alias{getStations}
 
\title{ A function to return the stations from a data object}
 
\description{ This convience function returns a vector of station Ids from
  a data.frame of anomalies, temperatures, or inventories.
  Depending on the data object the station Id can be contained in a named
  column (Inventory$Id), or as the column name in a "zoo" type data.frame
  if the Anomalies are in a transposed matrix as occurs in the return
  values of  \code{intersectInvAnomalies} , then the function senses
  that and returns the Id from the \code{rownames}
 
}
\usage{
getStations(data)
}
 
\arguments{
  \item{data}{ the function takes in an inventory data.frame or a 
  data.frame of "zoo" series as created by \code{createAnomaly} or
  a matrix of anomalies as created by \code{intersectInvAnomalies}.
  Based on the object class of \code{data} a vector of station Ids is
  returned. 
 
}
}
\details{If the \code{data} fed to the function is a data.frame with a
  column named "Id" then that column is returned as a vector.
  If the data is a "zoo" object, then the column names are returned
  as a vector of Ids. if \code{data} is a matrix of Anomalies
  then the Ids are also returned, in that case from \code{rownames}
}

\value{ the function returns a vector of station Ids  
 
}
 
\author{ Steven Mosher}
 

 
\examples{
\dontrun{ StationsId <- getStations(anomalies)
          StationId <- getStations(Inventory)
          identical(getStations(Inventory),Inventory$Id)}
 
}
 
\keyword{ spatial }
 