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
  .. .. ..@ name        : chr ""
  .. .. ..@ datanotation: chr "FLT4S"
  .. .. ..@ byteorder   : chr "little"
  .. .. ..@ nodatavalue : num -Inf
  .. .. ..@ nbands      : int 1
  .. .. ..@ bandorder   : chr "BIL"
  .. .. ..@ offset      : int 0
  .. .. ..@ toptobottom : logi TRUE
  .. .. ..@ driver      : chr ""
  ..@ data      :Formal class '.SingleLayerData' [package "raster"] with 11 slots
  .. .. ..@ values    : logi(0) 
  .. .. ..@ offset    : num 0
  .. .. ..@ gain      : num 1
  .. .. ..@ inmemory  : logi FALSE
  .. .. ..@ fromdisk  : logi FALSE
  .. .. ..@ isfactor  : logi FALSE
  .. .. ..@ attributes: list()
  .. .. ..@ haveminmax: logi FALSE
  .. .. ..@ min       : num Inf
  .. .. ..@ max       : num -Inf
  .. .. ..@ band      : int 1
  ..@ legend    :Formal class '.RasterLegend' [package "raster"] with 5 slots
  .. .. ..@ type      : chr(0) 
  .. .. ..@ values    : NULL
  .. .. ..@ color     : NULL
  .. .. ..@ names     : NULL
  .. .. ..@ colortable: NULL
  ..@ history   : chr(0) 
  ..@ title     : chr(0) 
  ..@ extent    :Formal class 'Extent' [package "raster"] with 4 slots
  .. .. ..@ xmin: num -180
  .. .. ..@ xmax: num 180
  .. .. ..@ ymin: num -90
  .. .. ..@ ymax: num 90
  ..@ rotated   : logi FALSE
  ..@ rotation  :Formal class '.Rotation' [package "raster"] with 2 slots
  .. .. ..@ geotrans: num(0) 
  .. .. ..@ transfun:function ()  
  ..@ ncols     : int 72
  ..@ nrows     : int 36
  ..@ crs       :Formal class 'CRS' [package "sp"] with 1 slots
  .. .. ..@ projargs: chr "+proj=longlat +datum=WGS84"
  ..@ layernames: chr ""
  ..@ zname     : chr ""
  ..@ zvalue    : chr ""
  ..@ unit      : chr ""
}
\details{This raster object is loaded when the package loads.
 
}
 
 
\examples{
  
 
}
\keyword{datasets}
