\name{OCEAN.MASK.URL}
\alias{OCEAN.MASK.URL}
\docType{data}
\title{ The Url of the land ocean mask
 
}
\description{This url points to the land ocean mask.
  The dataset consists of percentage of land area in a grid
  square. Ocean values are represented by 0, while land values
  can range up to 100 percent. Lakes and rivers and inland water
  is not taken into account. This is,therefore, a land-ocean mask.
  The resolution of the map is .25 degrees. Global coverage
  is -180 to 180 longitude by -90 to 90 latitude. A Land Water
  mask is also available. This files, as described below, is 
  a "qd" file or quarter degree, land fraction file.
 
}
\usage{OCEAN.MASK.URL}
\format{
  The format is:
  chr "http://www.drivehq.com/file/df.aspx/publish/
  stevenmosher/land_percent2_qd.asc Publish/land_percent2_qd.asc"
}
\details{"All of the files in the ISLSCP Initiative II data 
  collection are in the ASCII, or text format. The file format 
  consists of numerical fields of varying length, 
  which are delimited by a single space and arranged in 
  columns and rows. The values for the binary land/water or 
  land/sea masks are written as the integers
  0 and 1. All values in the land/water/sea fraction files 
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
\source{ ISLSCP2 Land/Water and Land/Sea Masks and other Ancillary Data       
                 at 0.25, 0.5 and 1 degree Spatial Resolutions  
 
}
\references{Please acknowledge the EOS DEM Science Working Group and the EOS ASTER 
  Instrument Project at JPL for provision of the 30 arcsecond EOS AM1 DEM and the 
  ISLSCP2 data collection when these data are used. 
 
}
\examples{
data(OCEAN.MASK.URL)
 
}
\keyword{datasets}
