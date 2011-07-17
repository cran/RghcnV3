\name{removeNaStations}
\alias{removeNaStations}
 
\title{A function to remove stations that have no temperatures
 
}
\description{During the course of processing zoo objects and Mts objects
  it is possible to create stations that have only NAs in their data
  fields. This can cause problems for code that take a mean of the data.
  a mean of NAs is not NA, but rather is Nan. This function then
  is used internally by other functions to ensure that such stations
  are removed so that they do not cause errors downstream.
  The window function from zoo does not make such a check so care
  must be taken. For saftey always call this function after windowing
  zoo data. Especially if you intend to perform other functions
  on the data. NOTE: na.trim() does not remove a single zoo series
  from a zoo object. 
 
}
\usage{
removeNaStations(Data)
}
 
\arguments{
  \item{Data}{A zoo series object or mts object
 
}
}
\details{The collection of zoo series or ts() series are examined
  and if a station ( column) is all NA, then that column is removed
  
 
}
\value{returns a zoo series or mts with the empty stations removed
 
}
 
\author{Steven Mosher
 
}
 
 
\keyword{ TimeSeries }
\keyword{ Internals } 
