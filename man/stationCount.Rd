\name{stationCount}
\alias{stationCount}
 
\title{ A function to count the stations versus time
 
}
\description{ The function counts the number of stations reporting
  temperatures for every month in the series
 
}
\usage{
stationCount(Data)
}
 
\arguments{
  \item{Data}{An Array, Zoo series or Mts  object
 
}
}
\details{ simply counts the number of valid temperatures per month
  which is the number of stations reporting for that month
 
}
\value{ returns a zoo series of stations
 
}
 
\author{Steven Mosher
 
}
 

 
\examples{ \dontrun{
  plot(stationCount(Data))
}
 
}
 
\keyword{TimeSeries }
 
