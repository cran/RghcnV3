\name{solveTempMonthly}
\alias{solveTempMonthly}
 
\title{Solve temperature, Monthly version
 
}
\description{This version of the temperature solver is the same
 as \code{solveTemperature} except that it returns residuals
 and local offsets
 
}
\usage{
solveTempMonthly(Data, Weights)
}
 
\arguments{
  \item{Data}{ A 3D array of temperatures
 
}
  \item{Weights}{Weights created by \code{inverseDensity}
 
}
}
\details{This function is identical to \code{solveTemperature} except
  it returns residuals and offsets
 
}
\value{
  \item{Solution}{Returns the solution}
  \item{Resid}{Returns residuals}
  \item{LocalOffset}{Local mean offsets}
}
 
\author{ Nick Stokes
 
}
 

 

\seealso{ \code{solveTemperature}
 
}
\examples{
 \dontrun{
  #
 }
}
 
\keyword{ TimeSeries}
 