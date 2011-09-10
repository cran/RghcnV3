\name{tempStats}
\alias{tempStats}
 
\title{calculates statistics for all stations in an object
%%  ~~function to do ... ~~
}
\description{ returns a dataframe of stats for every station.
  mean, max, min, sd, first year, full years, total months
 
}
\usage{
tempStats(data)
}
 
\arguments{
  \item{data}{a Array, Zoo object or Mts object
 
}
}
\details{ Statistics are calculated for every station in an object
 
}
\value{ returns a data frame
 
}
 
\author{Steven Mosher
 
}
 

 
\examples{\dontrun{

   stat <- tempStats(data)
}
     
}
 
\keyword{ TimeSeries }
 