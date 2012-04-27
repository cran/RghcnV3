\name{trimNA}
\alias{trimNA}
 
\title{TrimNA
 
}
\description{ This function trims the leading  NA values from an
 Array object, Mts object or Zoo object
 
}
\usage{
trimNA(Data)
}
 
\arguments{
  \item{Data}{ A  3D Array of stations temps or a Mts object. Zoo is
    acceptable but is coerced to Mts on return
 
}
}
\details{The functions solveTemp and solveMonthly temp rely on having
  inputs where leading NA have been removed. For example, if a collection
  of stations starts in 1880 with the first six months missing or NA,
  the solveMonthly algorithm will fail. Leading NA must be removed. 
  In general the start year should be a year where at least one station
  has a january present
 
}
\value{returns a trimmed dataobject.  You can use windowArray or window
 to trim to an exact year
 
}
 
 
\author{Steven Mosher
 
}
 

 

\seealso{ solveTempMonthly, solveTempAnnual
 
}
 
\keyword{ Timeseries }
 
