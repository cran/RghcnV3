\name{anomalize}
\alias{anomalize}

\title{A function to create anomalies from temperatures
 
}
\description{Anomalies are created by selecting a base period, ususally
  a 30 year period. The monthly values for a station series are averaged
  and the result is a "normal" or mean value for that station, that month.
  This mean is then subtracted from every other value in the time series,
  this normalizing the entire series to the average of the base period. The
  mean "January" temperature for the base period (1961-1990) would thus
  be subtracted from every January measure. This is done on a per station
  basis.
 
}
\usage{
anomalize(TemperatureZoo, period = list(Start = 1961, End = 1990))
}
 
\arguments{
  \item{TemperatureZoo}{The function \code{anomalize} must be supplied
  with a "zoo" series of actual temperatures. A "zoo" series contains
  the entire time series of data for a station including NAs for the 
  missing data. With this package a "zoo" series of temperatures is
  created using the function \code{createTemperature()}. As a function
  \code{anomalize()} does NOT take data directly from 
  \code{readV3Data()}. That data must first be reformated to a "zoo" type
  data structure. 
 
}
  \item{period}{period is a list of two elements \code{Start} and
  \code{End}. These are defaulted to 1961 and 1990 respectively.
  To specify a start and an end whole Years are required. If
  \code{period$End} is set to 1990, then the function will ensure
  that all months in 1990 are used. It would be an error to specify
  1990.917. warnings will be thrown and the input values will be
  trimmed to the integer part. This behavior is different from the
  behavior of the \code{window} function.
 
}
}
\details{There are two ways to create anomalies. The more direct path
  relies on the \code{createAnomaly()} function. That function does
  take input directly in the format delivered from \code{readV3Data()}.
  There, a 14 column \code{data.frame} is fed to the function. That
  function takes care of all the processing to turn the data into 
  anomalies and to return only those stations which satify the CAM 
  criteria, for example, 15 Years of data in the CAM base period. That
  function effectively wraps several other function, namely:
  \code{createTemperature();anomalize();whichStationsCAM}.
  The second method of creating anomalies is to first 
  \code{readV3Data()} then transform that output into temperature series
  by using \code{createTemperature()}. This give you a data series where
  you could plot actual temperatures if you wanted to. Then that
  Temperature series would be fed to  \code{anomalize()}. Anomalies
  could then be plotted for each station. 
  The final step would be to take the result of
  \code{anomalize()} and the apply the CAM 
  (Common Anomaly Method) criteria to select the subset of stations
  which satify the criteria for inclusion in data analysis. This
  is performed by the function \code{whichStationsCAM()}. 
 
}
\value{The function returns a zoo series of station anomalies. The
  data structure stations in columns and anomaly data in rows. The
  GHCN station Ids are used as column names. Every row of data is
  a month of anomaly data. 

}
\seealso{ \code{\link{whichStationsCAM}, \link{readV3Data},\link{createTemperature}
   \link{createAnomaly}} }

\author{Steven Mosher
 
}
 
\examples{
\dontrun{df <- readV3Data(filename="myfile.dat")
  TempSeries <- createTemperature(df)
  Anomalies  <- anomalize(TempSeries)
  Anomalies  <- whichStationsCAM(Anomalies)
}
 
}
 
\keyword{ TimeSeries }
 
