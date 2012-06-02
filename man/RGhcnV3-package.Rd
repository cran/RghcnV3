\name{RghcnV3-package}
\alias{RghcnV3-package}
\alias{RghcnV3}
\docType{package}
\title{A package for downloading, importing and analyzing GHCN version 3
 
}
\description{The RghcnV3 package provides some basic tools for downloading
  formating and analyzing Global Historical Climate Network temperature
  data. It is targeted at the version 3 release. The core functions allow
  for the easy creation of temperature anomalies for land surface stations.
  In addition, land masks are provided. Together with the raster package
  these functions can be used to create a global temperature index using
  the Common Anomaly Method or CAM. The functions provided will create a
  data structure that can be directly used by raster functions to create
  spatially weighted maps of temperatures. Other methods for 'averaging' 
  stations have also been added where regression approaches are taken.
 
}
\details{
\tabular{ll}{
Package: \tab RghcnV3\cr
Type: \tab Package\cr
Version: \tab 2.9\cr
Date: \tab 2012-06-03\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
LazyData: \tab FALSE\cr
}



 
}


 
\author{Steven Mosher with contributions from Tamino, RomanM and Nick Stokes
 

Maintainer: Steven Mosher <moshersteven@gmail.com>
 
}

 
\references{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README
 
}
 
\keyword{ package }
\keyword{spatial}
 
\examples{
\dontrun{

 # download the files
 files     <- downloadV3(url = V3.MEAN.ADJ.URL)
 # pass the file names to the read functions
 v3Mean    <- readV3Data(filename = files$DataFilename,output = "Zoo")
 inventory <- readInventory(filename = files$InventoryFile)
 Anomaly   <- createAnomaly(v3Mean)
 Data      <- intersectInvData(inventory,Anomaly)
 
 
 
}
}