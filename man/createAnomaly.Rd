\name{createAnomaly}
\alias{createAnomaly}
\title{A function to transform temperature data into anomaly data
 
}
\description{This function transforms temperature data into anomaly
  data. The function screens stations to include only those
  stations with the required number of years in the base period. The 
  temperatures for these stations are transformed into anomalies. 
     
 
}
\usage{createAnomaly(Data, criteria = list(Start = 1961, End = 1990, 
                     Years = 15, Threshold = 12))
   
}
 
\arguments{
  \item{Data}{ data must be an "Array","Zoo", or "Mts" object as output by
  \code{readV3Data}. If the data is not in that format, the function
  will fail.  
 
}
  \item{criteria}{ criteria is a list with 4 elements named  Start,
  End, Years, and Threshold. Start defines the first year of the base
  period. End is the last year. Years, defines how many years of
  data are required in the period. Threshold defines the number of
  months required to "count" as a full year. The default is 15 full years in
  the period 1961-1990, where full year is defined as a year with
  all 12 months reporting. 
 
}
}
\details{The function takes the   temperature data and  calculates the 
   average  temperature for each station for each month in the base period. 
   This base  period monthly mean is then subtracted from the entire time 
   series. The net effect is the entire database is transformed into a 
   collection of time series where the monthly mean temperature from 
   1961 to 1990 has been subtracted
  from every measure. Thus, the anomaly for every month is the departure
  from the norm for that station,where the norm is defined as the average 
  during the base period. To be included in the analysis a station's record
  must pass the screening test. The screeening test examines how many full 
  years of data a station has in the base period (1961-1990). Full years
  can be specified by the Threshold variable, typically 9-12 months. That
  is, one could screen stations based on a criteria of havin 20 "full" 
  years in the base period, where 10 months of data constituted a full year.
  see the demo "StationCounts" to understand the effect changing this
  parameter has.
 
}
\value{The resultant series is a collection anomalies for every
  station during its entire period. outpute is organized
  by stations id in the columns and time in the rows. So that column
  one contains the time series for station 1, and so forth. Missing
  years and months are already infilled with NA so that the result is suitable
  for plotting. Array type objects have their data dimensions preserved
  and stations are reurned in the first dimension.
 
}
 
\author{Steven Mosher
 
}
 

 
\examples{
\dontrun{
  V3Anomaly <- createAnomaly(V3mean)
}
 
}
 
\keyword{ TimeSeries }
 
 