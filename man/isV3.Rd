\name{isV3}
\alias{isV3}
 
\title{Checks if a data.frame is a v3 temperatre dataset
 
}
\description{Code in RghcnV3 requires that the V3 format output
  by readV3Data be maintained including all the column names.
  Functions that operate on this data require the format to remain
  the same. Since the programmer has access to the dataframe
  and can change it, functions will check for destructive changes.
  Simply we check for 14 columns of data all named the correct way.
  see \code{FILE.PARAMETERS} for the column names
 
}
\usage{
isV3(data)
}
 
\arguments{
  \item{data}{any object to be checked
 
}
}
\details{The data passed to this function is checked for the following
  is it a data.frame, does it have 14 columns, are those columns
  named as defined in \code{FILE.PARAMETERS$DataColumns}
 
}
\value{return true or false and throws warnings
 
}
 
\author{Steven Mosher
 
}
 
 
\keyword{ Internals }
 