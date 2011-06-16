\name{readMask}
\alias{readMask}
 
\title{A function to read the mask data into a Raster
}
\description{ After a land mask is download using \code{downloadMask()}
  the next task is to read it into a Raster object. Raster objects
  are used to represent geographical dataset. This function "wraps"
  some simple raster commands for ease of use
 
}
\usage{
readMask(filename)
}
 
\arguments{
  \item{filename}{ the filename of a land mask provided with
  this package. The mask is downloaded using 
  \code{downloadmask()}
 
}
}
\details{ The masks provided through download are global masks
  the format of the data is asc, ascii data. The function reads
  in that data using the following raster calls
  \code{land <- read.table(filename, sep = " ")}\cr
  \code{world <- raster(as.matrix(land), xmn = -180, xmx = 180, 
        ymn = -90, ymx = 90, crs = "+proj=longlat +datum=WGS84")}\cr
  \code{return(world/100)}
  
  This means the raster will have a value that runs between 0 and
  One. That represents the fraction of land in the cell and is
  used for land area calculations
 
}
\value{ A raster is returned. If the 1/4 degree maps are used each
  cell will be 1/4 of a degree. That raster can be "aggregated" into
  a 1 degree map ( or any factor) by applying the raster function
  \code{aggregate}. Typically a 5 degree cell map will be used
 
}
\references{ see documentation on the raster package}
 
\author{Steven Mosher
 
}
 
 
 
\examples{
 \dontrun{
  mask <- getMask(filename="land_water_mask_qd.asc")
  mask5x5 <- aggregate(mask,fact=20)
 
 }
}
 
\keyword{ spatial}
\keyword{file} 
