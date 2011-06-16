\name{createAnomaly}
\alias{createAnomaly}
\title{A function to transform temperature data into anomaly data
 
}
\description{This function transforms temperature data into anomaly
  data. After the stations have been screened to include only those
  stations with the required number of years in the base period, they
  are transformed into anomalies. To facilitate this transformation
  the temperatures are first cast  as a "zoo"  series which fills in
  any gaps the time series has from missing years. zoo also transforms
  the time from years and months into fractional representations.
 
}
\usage{
  createAnomaly(V3data, criteria = list(Start = 1961, End = 1990, 
  Years = 15, Threshold = 12))
}
 
\arguments{
  \item{V3data}{ V3data must be a 14 column data.frame as output by
  \code{readV3Data}. If the data is not in that format, the function
  will fail. The 14 columns are Id, Year, followed by 12 months
  of temperature data.
 
}
  \item{criteria}{ criteria is a list with 5 elements named  Start,
  End, Years, and Threshold. Start defines The first year of the base
  period. End is the last date. Years, defines how many years of
  data are required in the period. Threshold defines the number of
  months required in a full year. The default is 15 full years in
  the period 1961-1990, where full year is defined as a year with
  all 12 months reporting
 
}
}
\details{the function takes the temperature data and transforms it into
   a regular, gap free, zoo time series and calculates the average temperature 
   for each station for each month in the base period. This base period 
   monthly mean is then subtracted from the entire time series. The net effect
  is the entire database is transformed into a collection of time series
  where the monthly mean temperature from 1961 to 1990 has been subtracted
  from every measure. Thus the anomaly for every month is the departure
  from the norm for that station,where the norm is defined as the average 
  during the base period. To be included in the analysis a station's record
  must pass the screening test. The screeening test examines how many full 
  years of data a station has in the base period (1961-1990). Full years
  can be specified by the Threshold variable, typically 9-12 months. That
  is, one could screen stations based on a criteria of havin 20 "full" 
  years in the base period, where 10 months of data constituted a full year.
 
}
\value{The resultant zoo series data.frame is a collection anomalies for every
  station during its entire period. The data.frame is organized
  by stations id in the columns and time in the rows. So that column
  one contains the time series for station 1, and so forth. Missing
  years and months are already infilled with NA so that the result is suitable
  for plotting. A zoo series is similar to a time series and can
  be operated on commands like \code{window}. 
 
}
 
\author{Steven Mosher
 
}
\note{A special note of thanks to Gabor Grothendieck maintainer of zoo for help
  with the underlying algorithm.
 
}

 
\examples{
\dontrun{
  V3Anomaly <- createAnomaly(V3mean)
}
 
}
 
\keyword{ spatial }
 