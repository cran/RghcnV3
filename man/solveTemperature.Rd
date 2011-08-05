\name{solveTemperature}
\alias{solveTemperature}
 
\title{A function to solve a least squares
 
}
\description{In Nick Stoke's Method a collection of temperature
  series is solved used a weighted least squares approach. There is no
  common anomaly period and no combining of stations into long records.
  Every bit of station data is used to estimate the best fit to all the data
  
 
}
\usage{solveTemperature(Data, Weights)
}
 
\arguments{
  \item{Data}{A 3D Array of temperatures
 
}
  \item{Weights}{Weights as caluclated by \code{inverseDensity for example}
 
}
}
\details{ Get nick to write something
 
}
\value{returns a normalize series of temperature for the input series
 
}
\references{ link for nicks blog posts
 
}
\author{Nick Stokes, rewritten by Steven Mosher
 
}
 

 
\examples{
 \dontrun{
   # write and test a demo
 }
}
 
\keyword{ TimeSeries }
\keyword{ spatial } 
