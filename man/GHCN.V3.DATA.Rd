\name{GHCN.V3.DATA}
\alias{GHCN.V3.DATA}
\docType{data}
\title{Directory name for data directory
 
}
\description{A demo program in included that allows the user to
  download all of the data files used by this package. That demo
  \code{demo(DemoSetup)} will create a directory and download the
  various datasets used in constructing a global temperature index.
 
}
\usage{ GHCN.V3.DATA }
\format{
  The format is:
 chr "GhcnV3Data"
}
\details{The data directory can be used to store various files
  that are downloaded from the internet for this package. The
  directory will be written below the current working directory
  when the \code{demo(DemoSetup)} command is issued at the
  console
 
}
\examples{
 
print(GHCN.V3.DATA)
}
\keyword{datasets}
