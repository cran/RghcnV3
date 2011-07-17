\name{v3ToMts}
\alias{v3ToMts}
 
\title{ A function to create A mts series of Temperatures
 
}
\description{The V3 data format is a packed format with
  14 columns of data. Column 1 is the station Id and column 2
  is the Year. Columns 3:14 are monthly data for that year.
  \code{v3ToMts} transforms this format into a
  format where each station is a single vector of temperature
  data. This is Rs base time series type of format and
  unlocks all the power of R's time series analysis tools. 
  \code{v3ToMts} compliments \code{v3ToZoo} and performs
  exactly the same function but outputs a \code{mts} format
  as opposed to the \code{zoo} format. 
   
 
}
\usage{
v3ToMts(V3data)
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
  be infilled with NA. \code{v3ToMts} provides this 
  functionality in a loop free (fast) manner. A station, for example,
  that has data for years 1920,1921,1924,1925.., will have the missing
  years ( 1922 and 1923) infilled with NA so that it can be plotted
  or aligned with other years for averaging. \code{v3ToMts}
  is vastly different from \code{createAnomaly}. It does not screen
  out any stations. It merely transforms the packed data format into
  an expanded format. In short, the data on disk is packed in a
  economical way with missing years removed. That data is read in
  by \code{readV3Data} and then must be transformed into a gap-less
  time series. \code{v3ToMts} does this. 
 
}
\value{The output object is a regular \code{mts} multiple time series  
  class object. In that object every column is a station. Rows are time or months.
  Station Id is transformed into the column name. A mts type series
  thus can be plotted straightaway \code{plot(temp[,4])} would
  plot the 4th station as a time series. All the standard
   functions such as \code{window()} will work on this object. 
  This format CANNOT be fed to a routine like \code{createAnomaly}.
  That function takes a packed V3 temperature dataset. 
 
}
 
\author{Steven Mosher
 
}
\note{ If your goal is using raster t create gridded data sets
  using the \code{rasterizeZoo} function, then you should use
  \code{v3ToZoo}. This function is provided for methods that
  are built up around the existing \code{mts} class
 
}

 
\examples{
 \dontrun{
  V3temps <- v3ToMts(V3mean)
 
 }
}
 
\keyword{ TimeSeries }
 