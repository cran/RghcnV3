\name{passesCamZoo}
\alias{passesCamZoo}
 
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
\usage{passesCamZoo(dataZoo, criteria = list(Start = 1961, End = 1990,
                                        Years = 15, Threshold = 12))
}
 
\arguments{
  \item{dataZoo}{A 2d zoo object
 
}
  \item{criteria}{CAM criteria. A list with elements. Start,End, Years and
    Threshold. This is used to screen stations out that do not meet the criteria
 
}
}
\details{The function operates by first checking the criteria that are supplied
  for obvious errors such as Start being greater than end. Next the function 
  transforms the data into 0 and 1 depending on whether an NA is present or not.
  The number of valid temperatures or anomalies is computed for each year. A year
  with 12 months of data will have a count of 12. Then the number of years with
  months of data greater than or equal to the Threhold is calculated. Stations
  that pass the criteria are returned.
 
}
\value{Returns a zoo object with stations removed that do not meet the criteria
 
}
 
\author{Steven Mosher
 
}
 

 

\seealso{\code{\link{passesCamV3}}
 
}
\examples{

\dontrun{
   v3data <- readV3Data(filename="yourfile")
   v3data <- windowV3(v3data,start = 1900, end = 2010)
   v3Zoo  <- v3ToZoo(v3data)
   v3data <- passesCamZoo(v3data)
   anomaly<- anomalize(v3data)
  }
 
}
 
\keyword{ TimeSeries }
 
