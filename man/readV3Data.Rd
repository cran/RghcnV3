\name{readV3Data}
\alias{readV3Data}
 
\title{ A Function to read Temperature data in GHCN V3 format
 
}
\description{The function performs a fixed width read of the local
  copy of the GHCN version 3 temperature data file. That file contains
  more than temperature data, unlike past versions. The read function
  allows the programmer to read in V3 data and output a V2 format which
  is much easier for processing. The QA Flags present in V3 format
  are not passed out to the calling program. They can be accessed
  by the function \code{readQC()}
  Missing values are recoded from -9999 to NA.
}
\usage{readV3Data(filename, output = c("Array", "Mts", "Zoo"), Parameters = FILE.PARAMETERS)
 
}
 
\arguments{
  \item{filename}{The filename is the local filename of the GHCN V3
  data. This file ends with the extension \code{.dat}
 
}
\item{output}{output selects the one type of data you want returned.
 If you specify more than one type, it will default to the first type.
 \code{output ="Zoo"} will return a zoo object and likewise for the other
 settings. Default is Array
 
}
  \item{Parameters}{Parameters defaults to the variables set
   in the list \code{FILE.PARAMETERS} This list describes
   the column widths and column names for the \code{read.fwf}
   call.
 
}
}
\details{In Version 3 of GHCN several Quality control flags have
  been added. There are three types of flags, DMflags, QCflags,
  and DSflags. In \code{readV3Data()} the quality control flags
  are not output. They can be read in separately with a call to
  \code{readQC()}. In addition, V3 has a column of data that
  represents the type of measure being reported, mean, max or min.
  This is removed as well. The measurand type is evident in the
  name of the file, so it's redudant
 
}
\value{The function will return  on of three types of data structures.
  in versions of RghcnV3 prior to 2.0 a 14 column format was returned
  That has been replaced with the Array format
  the format of the source data file is documented 
  at \url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README}. Essentially,
  every month column is followed by 3 columns of QA and source flags.
  After processing by \code{readV3Data()} the QC Flags are removed.
  UNITS ARE EXPRESSED IN FULL DEGREES C
 
}
\references{
  \url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README}
}
\author{ Steven Mosher; modified for speed by Nick Stokes
 
}
 
 
 
\examples{
 \dontrun{
 TEXAS.DAT <- system.file("external/Texas.dat", package = "RghcnV3")
 texas <- readV3Data(TEXAS.DAT, output = "Zoo")}
}
 
\keyword{ file }
 
