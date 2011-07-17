\name{cropInv}
\alias{cropInv}
 
\title{A function to select stations that fall in a Bounding Box
 
}
\description{Every station in the inventory has an associated 
  Latitude (y value) and Longitude (x value). Selecting subsets
  of an inventory can be handled by standard indexing methods
  on the data.frame. Since selecting stations by region is a
  oft performed function a simple interface is provided to do that.
  That interface depends upon specifying an \code{extent} from
  the \code{raster} package. An extent is merely a bounding
  box in Longitude and Latitude. The extent is used to crop
  the inventory of stations. It can also be used to 
  \code{\link[raster]{crop}} a \code{Raster*} defined object.
 
}
\usage{
cropInv(inventory, bBox)
}
 
\arguments{
  \item{inventory}{An inventory data.frame of stations. The data.frame
  must conform to certain specifications. Column 1 must be a column
  named \code{Id}, column 2 is a column named \code{Lat} and column
  3 is a column named \code{Lon}. Latitudes and Longitudes should be
  in a WGS84 coordinate system. -180 to 180 for Longitude and -90 to
  90 for latitude.  
 
}
  \item{bBox}{bBox must be an extent object as defined by the
  \code{raster} package. \code{\link[raster]{extent}} is created
  by passing the 4 corners of the bounding box to the function.
  The order must be lonMin, lonMax, latMin, LatMax. This is
  also refered to as xmin,xmax,ymin,ymax. 
 
}
}
\details{ This function works by operating on the underlying dataframe
  to return only those stations that fall within the bounding box.
 
}
\value{The function returns a data.frame of stations in the standard
  inventory format
 
}
 
\author{Steven Mosher
 
}
 

 

\seealso{\code{\link[raster]{extent}}
 
}
\examples{
 \dontrun{
  cuba <- extent(-88, -65, 16, 30)
  CubaInv <- cropInv(Inventory,cuba)
  Cuba <- crop(Land, cuba)
 
 }
}
 
\keyword{ spatial }
 
