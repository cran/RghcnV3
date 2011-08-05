\name{windowArray}
\alias{windowArray}
 
\title{A function to subset Array Data by year
 
}
\description{After reading in V3 data using \code{readV3Data}
  the dataset can be reduced in size by selecting a subset
  or \code{windowArray} of data. This operates exactly like
  the \code{window} function in \code{zoo} or time series
  window functions.
 
}
\usage{windowArray(Data, start, end)
}
 
\arguments{
  \item{Data}{A 3D data array
 
}
  \item{start}{The start year you want to include. Only integer
  years are allowed
 
}
  \item{end}{The end year you want to include. Only integer
  years are allowed
 
}
}
\details{The function only takes an Array. It must have a 
  diemnsion with year data as dimnames
 
}
\value{Returns an Array with Years outside the window
  removed
 
}
 
\author{Steven Mosher
 
}
 

 
\examples{
\dontrun{
  meanAdj     <- downloadV3(url=V3.MEAN.ADJ.URL) 
  meanAdata   <- readV3Data(filename=meanAdj$DataFilename,output ="Array")
  Inventory   <- readInventory(filename=meanAdj$InventoryFile)
  mean1900    <- windowArray(meanAdata,start=1900,end=2010)
}
 
}
 
\keyword{ TimeSeries }
 
