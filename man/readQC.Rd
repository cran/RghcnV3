\name{readQC}
\alias{readQC}
 
\title{ A function to read in only the Quality Control Flags
 
}
\description{ the GHCN V3 dataset consists of temperature
  data interspersed with QC data. Every monthly temperature
  has 3 quality flags. To ease processing and improve the
  data structures the temperatures are read in independently
  of the QC flags. This routine reads in the QC data
 
}
\usage{
readQC(filename, Parameters = FILE.PARAMETERS)
}
 
\arguments{
  \item{filename}{a local filename to the GHCN V3 \code{.dat}
  file. This is the same file that temperatures are read out of.
 
}
  \item{Parameters}{defaulted to the file read parameters.
 
}
}
\details{The dataframes returned are all in 14 column format.
  The first column is the station Id, second column is the Year.
  Columns 3:14 are months of the year. This is the same format
  for temperatures. each Flag type is copied into its own dataframe
  DM : data measurement flag, nine possible values:
  QC : quality control flag
  DS : data source
 
}
\value{ The function returns a list of dataframes.
  \item{QC}{ a dataframe of QC flags}
  \item{DM}{ a dataframe of DM Flags}
  \item{DS}{ a dataframe of DS or source flags}
 
}
\references{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README
 
}
\author{Steven Mosher
 
}
 
\examples{
  
}
 
\keyword{ file }
\keyword{ spatial } 
