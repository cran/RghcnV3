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
\usage{
readV3Data(filename, Parameters = FILE.PARAMETERS)
}
 
\arguments{
  \item{filename}{The filename is the local filename of the GHCN V3
  data. This file ends with the extension \code{.dat}
 
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
\value{The function will a \code{data.frame} consisting of an
  Id, followed by a Year, followed by 12 columns of monthly data. 
  This is standard Version 2 data format. The format is dense and
  if a station has missing years they are not represented as Missing.
  The whole year is removed. It is vitally important to remember this.
  For a station that has 50 years of data, it will occupy 50 lines in
  the \code{data.frame}. Those 50 years  very often have gaps. In 
  order to turn this packed format into a full time series the missing
  years must be infilled with NA values. That process happens in
  two different functions: \code{createAnomaly} and \code{createTemperature}
  the format of the file is documented 
  at \url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README}. Essentially,
  every month column is followed by 3 columns of QA and source flags.
  After processing by \code{readV3Data()} the QC Flags are removed.
  UNITS ARE EXPRESSED IN FULL DEGREES C
 
}
\references{
  \url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README}
}
\author{ Steven Mosher
 
}
 
 
 
\examples{
 \dontrun{V3Mean <- readData(filename="ghcnm.tavg.v3.0.0.20110616.qca")}
}
 
\keyword{ file }
 
