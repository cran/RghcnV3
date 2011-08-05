\name{readChcn}
\alias{readChcn}

\title{Reads a file created by package CHCN
 
}
\description{The CHCN package output 14 column datasets that are
  similar to the GHCN version 2 format. Column 1 is a station Id,
  column 2 is a year, and columns 3:14 are data for the 12
  months of the year.
 
}
\usage{readChcn(filename, output = c("Array", "Mts", "Zoo"))
}
 
\arguments{
  \item{filename}{Filename to read in
 
}
  \item{output}{Select an output format one of "Array", "Zoo" or "Mts"
 
}
}
\details{The 14 column data is read in using \code{read.table} then the
  data is collated into an Array type structure or Zoo or Mts. Stations
  with no temperature data are redacted as are stations with less than
  one year of data.
 
}
\value{returns an Array type object, Zoo or Mts
 
}
 
\author{Steven Mosher
 
}
 
 
\examples{
 \dontrun{
   data <- readChcn(filename = "TaveCHCN.dat", output = "Array")
 }
}
 
\keyword{ files }
 
