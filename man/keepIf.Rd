\name{keepIf}
\alias{keepIf}
 
\title{a sample function for handling NA when calculating means
 
}
\description{This function is used in conjection with the function
  \code{\link{annualize}}. The annualize function takes a monthly
  zoo series of data ( temperatures or anomalies) and returns
  an annual average. The \code{mean} function, however, only has
  two ways of calculating a mean: With NA removed or with no NA
  removed. In order to provide great granualar control over this
  the user can define a function to pass to \code{annualize}. 
  \code{keepIf} is an example of such a function.
 
}
\usage{
keepIf(yearData, cnt)
}
 
\arguments{
  \item{yearData}{yearData is a 12 month section of a zoo monthly
  series. When \code{annualize} is called it passes year sized
  chunks to \code{mean} or to the user defined function. The user
  defined function must work on a years worth of data. It should
  return a single number
 
}
  \item{cnt}{In this function the variable \code{cnt} represents
  the minimum number of months required to calculate a mean.
  So, if cnt is set to 10, then years with 10 or more months
  will have a annual mean calculated, otherwise NA will be returned
 
}
}
\details{The code for the function takes in a years worth of data.
  The data is then coverted to true/false flags by using the
  \code{is.na} function. The NAs are summed and if the year has
  enough monthly temperatures then a mean is returned to the caller.
  \code{keepIf} will work to set a threshold for the number of NAs
  one allows before rejecting a year. One could also write functions
  that demanded two months of data in every season.

  
 
}
\value{The function returns a mean for the year or NA if the criteria
  are not met. For the function \code{keepIf} on could specify
  the variable \code{cnt} to equal 6 and years with 6 or more months
  of data would have the mean calculated.
 
}
 
\author{Steven Mosher
 
}
\note{Thanks to Peter Oneill for the idea. 
 
}

 

\seealso{\code{\link{annualize}}
 
}
 
\keyword{ TimeSeries }
 
