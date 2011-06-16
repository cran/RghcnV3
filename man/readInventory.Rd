\name{readInventory}
\alias{readInventory}
 
\title{A function to read Version 3 Inventory
 
}
\description{GHCN temperature datasets contain a station Id which is
  matched in the inventory file. Between version 2 and version 3 the
  format of the inventory file has changed. This function reads in
  an inventory file and outputs a data.frame of stations and their
  metadata. During this process the function also "cleans"
  the names in the file. This removes various punctuation marks and
  country names. Country names are already encoded in the station Id
  as documented in the readme.
 
}
\usage{
readInventory(filename, Constants = FILE.PARAMETERS)
}
 
\arguments{
  \item{filename}{A filename on the local machine of the inventory data
  this file typically ends in \code{.inv}
 
}
  \item{Constants}{Defaults to the preloaded constants for reading a file
  column widths and column names are set there. The column names are
  changed from the raw data to be more expressive.
}
}
\details{ columns are renamed \cr
  column one here has the names from the README.
  column 2 has the names they have been changed into.\cr

        GhcnName          Newname      Type \cr
       
         ID                 Id          Integer\cr
         LATITUDE          Lat          Real\cr
         LONGITUDE         Lon          Real\cr
         STNELEV           Elevation    Real\cr
         NAME              Name         Character\cr
         GRELEV            GridEl       Integer\cr
         POPCLS            Rural        Character\cr
         POPSIZ            Population   Integer\cr
         TOPO              Topography   Character\cr
         STVEG             Vegetation   Character\cr
         STLOC             Coastal      Character\cr
         OCNDIS        DistanceToCoast  Integer\cr
         AIRSTN            Airport      Boolean\cr
         TOWNDIS        DistanceToTown  Integer\cr
         GRVEG             NDVI         Character\cr
         POPCSS            Light_Code   Character\cr
 
}
\value{The function returns a data.frame with columns defined by the constants
  set by  FILE.PARAMETERS.  Some variables, such as airports are transformed
  to boolean. GHCN has a varied method of representing missing data based on the
  field, this is corrected and NAs are used instead.
 
}
\references{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README
  contains description of all the columns for the inventory.
 
}
\author{Steven Mosher
 
}
 
\examples{
 \dontrun{Inv <- readInventory(filename="ghcnm.tavg.v3.0.0.20110612.qca.inv")}
}
 
 
\keyword{ file } 
