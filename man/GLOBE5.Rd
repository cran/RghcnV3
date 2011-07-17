\name{GLOBE5}
\alias{GLOBE5}
\docType{data}
\title{A blank raster object of the globe
 
}
\description{This is a predefined global raster with 5 degree
  by 5 degree cells. Its used as a default parameter for the
  function \code{rasterizeAnomaly}. It can be simply scaled
  to any resolution by using the raster \code{res} command.
  For example a 1 degree grid is created by setting 
  \code{res} to 1. Like so, \code{res(GLOBE5)<-1}
 
}
\usage{ GLOBE5 }
\format{
  The format is:
Formal class 'RasterLayer' [package "raster"] with 15 slots
  ..@ file      :Formal class '.RasterFile' [package "raster"] with 9 slots
  .. .. ..@ name        : chr ""\cr
  .. .. ..@ datanotation: chr "FLT4S"\cr
  .. .. ..@ byteorder   : chr "little"\cr
  .. .. ..@ nodatavalue : num -Inf\cr
  .. .. ..@ nbands      : int 1\cr
  .. .. ..@ bandorder   : chr "BIL"\cr
  .. .. ..@ offset      : int 0\cr
  .. .. ..@ toptobottom : logi TRUE\cr
  .. .. ..@ driver      : chr ""\cr
  ..@ data      :Formal class '.SingleLayerData' [package "raster"] with 11 slots
  .. .. ..@ values    : logi(0)\cr 
  .. .. ..@ offset    : num 0\cr
  .. .. ..@ gain      : num 1\cr
  .. .. ..@ inmemory  : logi FALSE\cr
  .. .. ..@ fromdisk  : logi FALSE\cr
  .. .. ..@ isfactor  : logi FALSE\cr
  .. .. ..@ attributes: list()\cr
  .. .. ..@ haveminmax: logi FALSE\cr
  .. .. ..@ min       : num Inf\cr
  .. .. ..@ max       : num -Inf\cr
  .. .. ..@ band      : int 1\cr
  ..@ legend    :Formal class '.RasterLegend' [package "raster"] with 5 slots
  .. .. ..@ type      : chr(0)\cr 
  .. .. ..@ values    : NULL\cr
  .. .. ..@ color     : NULL\cr
  .. .. ..@ names     : NULL\cr
  .. .. ..@ colortable: NULL\cr
  ..@ history   : chr(0) \cr
  ..@ title     : chr(0) \cr
  ..@ extent    :Formal class 'Extent' [package "raster"] with 4 slots
  .. .. ..@ xmin: num -180\cr
  .. .. ..@ xmax: num 180\cr
  .. .. ..@ ymin: num -90\cr
  .. .. ..@ ymax: num 90\cr
  ..@ rotated   : logi FALSE\cr
  ..@ rotation  :Formal class '.Rotation' [package "raster"] with 2 slots
  .. .. ..@ geotrans: num(0)\cr 
  .. .. ..@ transfun:function () \cr 
  ..@ ncols     : int 72\cr
  ..@ nrows     : int 36\cr
  ..@ crs       :Formal class 'CRS' [package "sp"] with 1 slots
  .. .. ..@ projargs: chr "+proj=longlat +datum=WGS84"\cr
  ..@ layernames: chr ""\cr
  ..@ zname     : chr ""\cr
  ..@ zvalue    : chr ""\cr
  ..@ unit      : chr ""\cr
}
\details{This raster object is loaded when the package loads.
  it can be used as a 'blank' form for creating new rasters.
 
}
 
 
\examples{
  
  Globe1deg <- disaggregate(GLOBE5, fact = 5)
 
}
\keyword{datasets}
