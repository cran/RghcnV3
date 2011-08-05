\name{getStations}
\alias{getStations}
 
\title{ A function to return the stations from a data object}
 
\description{ This convenience function returns a vector of station Ids from
  the dataset that is passed to it. It can be passed a inventory
  Array, a zoo object or a  \code{mts} object.   
  
  
   
 
}
\usage{
getStations(data)
}
 
\arguments{
  \item{data}{ the function takes in an inventory, a zoo object, a mts
  object or an Array.
 
}
}
\details{If the \code{data} fed to the function is a data.frame with a
  column named "Id" then that column is returned as a vector.
  If the data is a "zoo" object, then the column names are returned
  as a vector of Ids. The same logic is applied to mts objects
  and the Arrays. When these objects are created their column names
  are set as station Ids. This function returns those ids as numerics
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
 