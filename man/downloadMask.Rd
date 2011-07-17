\name{downloadMask}
\alias{downloadMask}
 
\title{A function to download a land mask
  
}
\description{The function download a land mask data set from the
  provided URL and returns the local filename. The package provides
  two land mask URLs. \code{WATER.MASK.URL} and 
  \code{OCEAN.MASK.URL}. These URLs should be loaded as the
  package is loaded. Otherwise the \code{data} function will load them
  The masks are land percent masks where the value of the data runs
  from 0 to 100. See details below
   
}
\usage{
downloadMask(url = OCEAN.MASK.URL, directory = getwd())
}
 
\arguments{
  \item{url}{The "ocean" land mask is the default. This url points
  to a mask that has land and all ocean bodies. The other url
  provided is an water/land mask. That mask is more appropriate
  for checking whether stations are located in an inland body of water.
  That is, for error checking locations. For processing a global
  temperature, the ocean mask should be used. It is the default.
 
}
  \item{directory}{by default the file is downloaded to the working directory
  If you provide a directory name that does not exist it will be created.
  
 
}
}
\details{The package provides URLs to two land masks. One that looks at 
  all bodies of water and one that only considers oceans. The value in
  the cells is the percent of land in the cell. 100 represents 100
  percent land in the cell. 0 represents 0 percent land. The cell size
  is 1/4 of a degree. The entire globe is provided
 
}
\value{ The function returns the full path to the local file.
  If you provide a different directory that will be reflected in the returned
  value. 
 
}
\references{Please acknowledge the EOS DEM Science Working Group and the EOS ASTER 
  Instrument Project at JPL for provision of the 30 arcsecond EOS AM1 DEM and the 
  ISLSCP2 data collection when these data are used.
  ISLSCP2 Land/Water and Land/Sea Masks and other Ancillary Data       
                 at 0.25, 0.5 and 1 degree Spatial Resolutions
 
}
\author{Steven Mosher
 
}
\note{"All of the files in the ISLSCP Initiative II data 
  collection are in the ASCII, or text format. The file format 
  consists of numerical fields of varying length, 
  which are delimited by a single space and arranged in 
  columns and rows. .... All values in the land/water/sea fraction files 
  are written as integers from 0 to 100.

  The files at different spatial resolutions each contain the 
  following numbers of columns and rows:

   One degree (1d): 360 columns by 180 rows 
   1/2 degree (hd): 720 columns by 360 rows 
   1/4 degree (qd): 1440 columns by 720 rows 

  All files are gridded to a common equal-angle lat/long grid,
  where the coordinates of the upper left corner of the files 
  are located at 180 degrees W, 90 degrees N and the lower right
  corner coordinates are located at 180 degrees E,
  90 degrees S. Data in the files are ordered from North 
  to South and from West to East beginning at 180 degrees West
  and 90 degrees North.""
  ( From the files README)
 
}

 

 
\examples{
 \dontrun{ water <- downloadmask(url=OCEAN.MASK.URL)}
}
 
 
\keyword{Download}
 
