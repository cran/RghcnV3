\name{whichStationsCAM}
\alias{whichStationsCAM}
 
\title{A function for picking stations that meet CAM criteria
 
}
\description{The Common Anomaly Method (CAM) relies on stations
  having a minimum number of full years of data in the common
  anomaly base period. That base period is typically taken as
  1961-1990. That period has the maximum amount of stations reporting.
  With more data in the period the estimate of the mean for that period
  is improved. The criteria is specifed by two numbers: Year and
  Threshold. If the period is 30 years long ( 1961-1990) then you
  want to insure that a station has a good number of full years
  reporting temperatures in that period. The variable Year defines
  that criteria. In addition, one has to define what consitutes a
  full year. Typically 10 to 12 months is used. This means a
  year with 10 months could qualify as a full year is Threshold 
  is set to 10. The function returns a object with the stations and their
  data
 
}
\usage{
whichStationsCAM(dataZoo, criteria = list(Start = 1961, End = 1990, Year = 15, Threshold = 12))
}
 
\arguments{
  \item{dataZoo}{The function must be supplied with a zoo object of temperatures
    or anomalies. See function \code{createTemperature} and \code{anomalize}. 
    
 
}
  \item{criteria}{criteria must be a list with 4 named elements. Start and End, 
  Year and Threshold. Use whole years for all elements. 
 
}
}
\details{ The function operates by first checking the criteria that are supplied
  for obvious errors such as Start being greater than end. Next the function 
  transforms the data into 0 and 1 depending on whether an NA is present or not.
  The number of valid temperatures or anomalies is computed for each year. A year
  with 12 months of data will have a count of 12. Then the number of years with
  months of data greater than or equal to the Threhold is calculated. Stations
  that pass the criteria are returned.
 
}
\value{a 2D data zoo object is returned. Station names are in the column names
  Station data is in columns where every month is a row. 

}
 
\author{Steven Mosher
 
}



\examples{
 \dontrun{ 
   # read the data
   mean <-readV3Data()
   # create temperature series
   temps <- createTemperature(mean) 
   # create anomalies
   anomaly <- anomalize(temps) 
   # select stations which fit criteria
   anomaly <-whichStationsCAM(anomaly) 
 
 }

}
 
\keyword{ TimeSeries }
 
