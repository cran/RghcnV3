\name{readHadMaps}
\alias{readHadMaps}
 
\title{A function to read in Had/Cru gridded data
 
}
\description{Hadley and CRU publish gridded datasets of
  land temperatures and combined land/ocean data. This data
  is contained in 5 degree by 5 degree gridded data structures.
  The function takes a url and reads the ncdf file in and returns
  a 'raster' \code{brick} to the caller. Data starts at 1850.
  data for every month since 1850 is supplied.
 
}
\usage{
readHadMaps(filename, start = 1850)
}
 
\arguments{
  \item{filename}{The filename is the local filename of the dataset.
  Typically, one would call \code{downloadHadMaps} and feed the
  value of that function to the read function.
 
}
  \item{start}{Start should always be set to 1850 unless Hadley or
  CRU change the date. The dates provided in the files themselves are
  not self documenting and they appear to be a number of days from
  Jan 1850. 
 
}
}
\details{The hadley maps, either crutem3 or hadcrut3 can both be read
  in with \code{readHadMaps}. This data can be used to validate your
  work or compare your answers with hadcrut or crutem.
 
}
\value{the function returns a raster \code{brick} with the time data
  properly formated in the \code{layerNames} variable. In future releases
  the time variable will be migrated to the new \code{zSlot} in the
  raster \code{brick} structure. 
 
}
\references{data can be found at
  \url{http://www.metoffice.gov.uk/hadobs/hadcrut3/data/download.html}
 
}
\author{Steven Mosher
 
}
 

\seealso{\code{\link{CRUTEMP3.MAPS.URL}}
  \code{\link{HADCRUT3.MAPS.URL}}
  \code{\link{downloadHadMaps}}
 
}
\examples{
 \dontrun{
  cruFile <- downloadHadMaps(url = CRUTEMP3.MAPS.URL)
  cruBrick <- readHadMaps(cruFile)
 }
 
}
 
\keyword{ file }
 
