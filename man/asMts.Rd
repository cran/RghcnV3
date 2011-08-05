\name{asMts}
\alias{asMts}
 
\title{Convert a zoo series or Array to an Mts
 
}
\description{The function conversts a zoo series or Array (3D)
  into a 3D mts object with stations in columns.
 
}
\usage{asMts(x)
}
 
\arguments{
  \item{x}{A 2D zoo series with stations in columns or an Array with
  stations in the first dimension, months in the second and years in
  the third.
 
}
}
\details{The function takes a zoo series of stations or a 3D array of
  stations and returns an Mts like object which is required by some functions
  
 
}
\value{Outputs an mts object which the correct dimnames
 
}
 
\author{Steven Mosher
 
}
 

 

\seealso{\code{\link{asArray}}; \code{\link{asZoo}}
 
}
\examples{
 \dontrun{
   data <- asMts(data)
 }
}
 
\keyword{ TimeSeries }
 
