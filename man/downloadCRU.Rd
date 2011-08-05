\name{downloadCRU}
\alias{downloadCRU}
 
\title{A function to download CRU station data
 
}
\description{Downloads Station data from Climate Research Unit
 
}
\usage{downloadCRU(url = CRU.STATIONS.URL, directory = getwd())
}
 
\arguments{
  \item{url}{ The URL for the CRU version of CRU stations
 
}
  \item{directory}{The download directory
 
}
}
\details{There are two version of the CRU station data.
  One version on MET website and the other on CRU. This function
  get the data from the CRU website and unpacks it
 
}
\value{returns a handle to the unzipped CRU station data
 
}
\references{\url{"http://www.cru.uea.ac.uk/cru/data/temperature/station-data/}
 
}
\author{Steven Mosher
 
}
\note{Not that this data differs in format from the data on the MET site
 
}

 

 
\examples{
 \dontrun{
 
   crufile <- downloadCRU()
 }
 
}
 
\keyword{ files }
\keyword{ Downloads } 
