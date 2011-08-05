\name{isArray}
\alias{isArray}
 
\title{tests if an object is an Array type
 
}
\description{the Array object in RghcnV3 has a particular
  layout. Stations are in dimension 1, months in diemnsion 2 and
  years in dimension 3. This function checks that the array
  has 3 dimensions and that the second dimension is named properly.
  checks for valid station Ids and years will be added in future
  versions
 
}
\usage{isArray(x)
}
 
\arguments{
  \item{x}{an R object
 
}
}
\details{The function checks for the correct number of dimensions
  and that the second dimension is named properly. 
 }
\value{Boolean
 
}
 
\author{Steven Mosher
 
}
 
 
\examples{
 \dontrun{
   check <- isArray(data)
 }
}
 
\keyword{ Internals }
 
