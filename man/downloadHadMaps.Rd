\name{downloadHadMaps}
\alias{downloadHadMaps}
 
\title{A function to download temperature grids
 
}
\description{The results of CRU temperature analysis is stored in
  two ncdf files. One file contains crutem3 data which is land only.
  The other dataset is hadcrut3 which conatins a combined land/ocean data
  set. The grids are 5 deg by 5 degree grids running from -180 to +180
  longitude and -90 to +90 latitude. Each cell contains the monthly anomaly
  for the grid, relative to a 1961-1990 baseline
 
}
\usage{
downloadHadMaps(url = c(CRUTEMP3.MAPS.URL, HADCRUT3.MAPS.URL),
                directory = getwd())
}
 
\arguments{
  \item{url}{two urls are given. Only one url should be used in loading the data.
  Loading both datasets will require calling the function twice with the different
  urls. A warning is thrown if you list more than one url,and the first url in 
  the list is used by default.
 
}
  \item{directory}{the directory where you want the data downloaded to.
 
}
}
\details{The data is in a .nc file. That file is downloaded to the directory
  you select. There are times when the CRU server is not reliable. Downloading
  by hand is always an option. The data is up to date. It starts at 1850.
  The varname (see the ncdf package) for the data is "temp"
 
}
\value{The function returns a handle to the local file. This can be used to read
  in the data.
 
}
\references{All data is found at this url. \url{http://www.metoffice.gov.uk/hadobs/}
 
}
\author{Steven Mosher
 
}
 

 

\seealso{\code{\link{readHadMaps}}
 
}
\examples{
\dontrun{
 cru <- downloadHadMaps(url = CRUTEMP3.MAPS.URL, directory = getwd())
 hadcrut <- downloadHadMaps(url =  HADCRUT3.MAPS.URL , directory = getwd())
}
 
}
 
\keyword{ Download }
 
