\name{v3ToZoo}
\alias{v3ToZoo}
 
\title{ A function to create A series of Temperatures
 
}
\description{The V3 data format is a packed format with
  14 columns of data. Column 1 is the station Id and column 2
  is the Year. Columns 3:14 are monthly data for that year.
  \code{v3ToZoo} transforms this format into a
  format where each station is a single vector of temperature
  data. This is more akin to a time series type of format and
  unlocks all the power of R's time series analysis tools. 
  \code{v3ToZoo} replaces \code{createTemperature} and performs
  exactly the same function. The function has been renamed
  to differentiate it from a similar function that outputs
  a \code{mts}, or multiple time series format which is very
  similar to zoo and useful for other purposes.
   
 
}
\usage{
v3ToZoo(V3data)
}
 
\arguments{
  \item{V3data}{V3data must be a 14 column data.frame as output by
  \code{readV3Data}. If the data is not in that format, the function
  will fail. The 14 columns are Id, Year, followed by 12 months
  of temperature data. This 14 column format, it should be noted, is the
  GHCN Version 2 format.
 
}
}
\details{As read out of the datafile by \code{readV3Data} the station
  data is compacted to remove years that have no data. In order
  to cast this as a time series like object these missing years must
  be infilled with NA. \code{v3ToZoo} provides this 
  functionality in a loop free (fast) manner. A station, for example,
  that has data for years 1920,1921,1924,1925.., will have the missing
  years ( 1922 and 1923) infilled with NA so that it can be plotted
  or aligned with other years for averaging. \code{v3ToZoo}
  is vastly different from \code{createAnomaly}. It does not screen
  out any stations. It merely transforms the packed data format into
  an expanded format. In short, the data on disk is packed in a
  economical way with missing years removed. That data is read in
  by \code{readV3Data} and then must be transformed into a gap-less
  time series. \code{v3ToZoo} does this.
 
}
\value{The output object is a regular "zoo" time series data.frame.
  In that object every column is a station. Rows are Time or months.
  station Id is transformed into the column name. A zoo type series
  thus can be plotted straightaway \code{plot(v3Anomalies[,4])} would
  plot the 4th station as a time series or zoo series. All the standard
  "zoo" functions such as \code{window()} will work on this object. 
  This format CANNOT be fed to a routine like \code{createAnomaly}.
  That function takes a packed V3 temperature dataset. 
 
}
 
\author{Steven Mosher
 
}
\note{ Creating anomalies from this temperature format can be achieved by
using zoo functions. 
 
}

 
\examples{
 \dontrun{
  V3temps <- v3ToZoo(V3mean)
 
 }
}
 
\keyword{ TimeSeries }
 