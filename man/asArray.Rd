\name{asArray}
\alias{asArray}
 
\title{Convert an mts or zoo to Array
 
}
\description{This function takes an mts or zoo series and converts
  it into a 3D array of stations, months, and years. The dimensions
  are named appropriately so that the Array works with other functions.
  
 
}
\usage{asArray(Data)
}
 
\arguments{
  \item{Data}{ A zoo object or a mts object with stations in columns.
 
}
}
\details{The function takes a 2D zoo object or mts object and creates a 3D
  object with stations in the first dimension, months in the second and
  years in the third. dimnames are set to be consistent with other
  objects and functions
 
}
\value{A 3D array. dimension one accesses stations and station Ids are used
  as dimnames. Dimension 2 is months and month abbreviations are used as dimnames.
  Dimension 3 is years and the dimnames is years
 
}
 
\author{Steven Mosher
 
}
\note{Hat tip to Nick Stoke for his work with this data structure in his code
 
}

 

\seealso{\code{\link{asZoo}}; \code{\link{asMts}}
 
}
\examples{
 \dontrun{
   data <- asArray(data)
 
 }
}
 
\keyword{ TimeSeries }
 
