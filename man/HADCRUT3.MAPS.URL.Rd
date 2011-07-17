\name{HADCRUT3.MAPS.URL}
\alias{HADCRUT3.MAPS.URL}
\docType{data}
\title{Url to temperature data from CRU
 
}
\description{Hadcrut3 is the latest gridded temperature product
  from CRU (climate research unit). The dataset linked to in
  this url starts in 1850 and goes up to the present day.
  It contains temperature anomalies for the entire land/ocean surface.
  The anomalies have been gridded in a 5 deg by 5 degree bin
  structure. The file is a ncdf type file where the varname
  is "temp". This data can be read directly into a raster
  \code{brick} object. The time variable, however, has
  to be set by hand into a format acceptable by raster.
 
}
\usage{ HADCRUT3.MAPS.URL }
\format{
  The format is:
 chr "http://hadobs.metoffice.com/hadcrut3/data/HadCRUT3.nc"
}
\details{The hadcrut3 dataset can be used a gross validation check
  of work performed with RghcnV3. The source datasets for creating
  the final grids are slightly different so an exact match is not
  expected. There are also other datasets one can use such as the
  variance adjusted dataset.
 
}
\source{The dataset is created by CRU. The publication is
  \url{http://www.metoffice.gov.uk/hadobs/crutem3/HadCRUT3_accepted.pdf}
  Brohan et al, 2006
 
}
 
\examples{
print(HADCRUT3.MAPS.URL)
 
}
\keyword{datasets}
