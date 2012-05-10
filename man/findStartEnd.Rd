\name{findStartEnd}
\alias{findStartEnd}
 
\title{Detects the first January and the last december in a time series
 
}
\description{Clipping or windowing time series data often requires
  detecting which year is the first full year in a dataset and which
  year is the last full year. Some datasources "pad" out their data
  to full years, while others do not. This function quickly
  returns the first full year ( with no missing data) and the
  last year with no missing data
 
}
\usage{
findStartEnd(Data)
}
 
\arguments{
  \item{Data}{ An Array object, Mts object or Zoo object with
   multiple time series
 
}
}
\details{ The function uses \code{allNA} to determine the first
 year in the dataset where every month has at least one station
 report. In addition, the last year in which every month has
 at least one station report is returned.
 
}
\value{ Returns a list of Start and End
   \item{Start}{the first full year where every month has at least one station}
   \item{End}{the last full year where every month has at least one station}
 
}
 
\author{Steven Mosher
 
}
\note{ If by change some month in the middle of a collection
 has a missing month, or a month with no stations, this function
 will not  detect that.
 
}

 
     
\keyword{ Timeseries }
 
