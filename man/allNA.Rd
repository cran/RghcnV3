\name{allNA}
\alias{allNA}
 
\title{ allNA Detects Months which have no data for all stations
 
}
\description{ Least squares methods require that every month have at least
  one station reporting, otherwise a singularity results. the solveTemp routines
  and other related methods will fail if you have month where no station was
  reporting. Typiclally this happens at the begining of time series or at the
  end. If you take subsets of whole collect, say a rural sample, you must
  ensure that there is no month where all the vales for every station are NA.
  This function returns a time series of Boolean indicating whether all
  the stations are NA (TRUE) or not. If at least one station has a report, then
  FALSE is returned for that month.
 
}
\usage{
allNA(Data)
}
 
\arguments{
  \item{Data}{ Accepts an Array, a Mts obkect or a Zoo object
 
}
}
\details{This function wraps a call to apply for the various data
  formats and returns a simple time series indicating whether a
  month has any station reporting (FALSE) or all stations missing (TRUE)
 
}
\value{Retruns a times series of boolean. FALSE if at least one station
 reports for that month. TRUE if all stations have missing data for that
 month
 
}
 
\author{Steven Mosher
 
}
 

   
   
\keyword{ Timeseries}
 
