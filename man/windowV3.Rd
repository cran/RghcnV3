\name{windowV3}
\alias{windowV3}
 
\title{A function to subset V3 Data by year
 
}
\description{After reading in V3 data using \code{readV3Data}
  the dataset can be reduced in size by selecting a subset
  or \code{window.V3} of data. This operates exactly like
  the \code{window} function in \code{zoo} or time series
  window functions.
 
}
\usage{
windowV3(v3Data, start, end)
}
 
\arguments{
  \item{v3Data}{A 14 column data.frame of V3 Temperature data
 
}
  \item{start}{The start year you want to include. Only integer
  years are allowed
 
}
  \item{end}{The end year you want to include. Only integer
  years are allowed
 
}
}
\details{The function only takes a dataframe. It must have a 
  column named \code{$Year}
 
}
\value{Returns a V3 data.frame will Years outside the window
  removed
 
}
 
\author{Steven Mosher
 
}
 

 
\examples{
\dontrun{
  meanAdj     <- downloadV3(url=V3.MEAN.ADJ.URL) 
  meanAdata   <- readV3Data(filename=meanAdj$DataFilename)
  Inventory   <- readInventory(filename=meanAdj$InventoryFile)
  mean1900    <- windowV3(meanAdata,start=1900,end=2010)
}
 
}
 
\keyword{ TimeSeries }
 
