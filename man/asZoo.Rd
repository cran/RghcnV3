\name{asZoo}
\alias{asZoo}
 
\title{Converts an Array or Mts to Zoo
 
}
\description{Given inputs of Array or "mts" type objects
  a zoo series is returned. Station data is in columns.
 
}
\usage{asZoo(x)
}
 
\arguments{
  \item{x}{An "mts" type object with station data in columns or a 3D
  Array with stations in the first dimension, maonths in the second
  and years in the third
 
}
}
\details{The function will return a properly defined zoo
  object when given an mts object or an Array object.
  
 
}
\value{returns a properly defined Zoo object
 
}
 
\author{Steven Mosher
 
}
 

 

\seealso{\code{\link{asArray}}
 
}
\examples{
 \dontrun{
  data <- asZoo(data)
 }
}
 
\keyword{ TimeSeries }
 
