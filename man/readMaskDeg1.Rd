\name{readMaskDeg1}
\alias{readMaskDeg1}
 
\title{Reads a 1 deg land ocean mask from package sources
 
}
\description{A local copy of a 1 degree land mask is included with the package.
  This is derived from the 1/4 degree land ocean mask that the function
  \code{downloadMask} gets from ftp. The file has been saved in "raster"
  format. Downloading and building this yourself is always an option or
  you can just use the 1 degree version that comes with the package
 
}
\usage{
readMaskDeg1()
}
 
\details{See documentation on the land masks. This function reads a copy
  of the land ocean mask that has been included in the package directories.
  It is constructed from the 1/4 degree land ocean mask using the raster
  command \code{aggregate}.
 
}
\value{The function returns a raster layer.
 
}
 
\author{Steven Mosher
 
}
\note{At times the ftp server which hosts the 1/4 degree land mask is
  not available. This file is included in the package should the server
  be unavailable.
 
}

 
\examples{
 LandMask <- readMaskDeg1()
}
 
\keyword{ spatial }
 
