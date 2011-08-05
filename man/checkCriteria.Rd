\name{checkCriteria}
\alias{checkCriteria}
 
\title{an internal function used to check criteria
 
}
\description{This function is used to check if criteria
  for the common anomaly method (CAM) make sense. For example,
  Start must be less than End. 
 
}
\usage{checkCriteria(criteria)
}
 
\arguments{
  \item{criteria}{Takes a list with 4 elements: Start,End,Years,Threshold
   It's used by other functions that take \code{criteria} as an input.
 
}
}
\details{Throws an error if the criteria make no sense.
 
}
\value{returns true or throws an error.}
 
 
\author{Steven Mosher
 
}
 
 
\keyword{ Internals }
 
