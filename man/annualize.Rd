\name{annualize}
\alias{annualize}
 
\title{a function to create an annual mean from monthly data
 
}
\description{The  data used in RGhcnV3 has a monthly frequency.
  Creating yearly means from this is straightforward. However, the
  package supports three types of data structures, an Array, a zoo
  object and an mts object. This function takes any one of those
  objects and creates an annula series from it
}
\usage{annualize(Data, user.fun = NULL, na.rm = TRUE, ...)
}
 
\arguments{
  \item{Data}{A monthly series of data. It can be temperatures
  or anomalies. A check for the frequency of data is made. The data
  object can be any type of mts, zoo or Array. Internally, the
  function manages the change of object types.
 
}
  \item{user.fun}{The \code{user.fun} defaults to NULL. This variable
  is provided if the user wants to create their own "NA" handlers.
  A "NA" handler is a function that describes how to calculate a
  yearly mean in the presence of "NAs". It can, for example, give
  you the granular control to accept only a specified number of NAs
  or NAs in certain seasons. A sample of a user function can be
  seen in \code{\link{keepIf}}. If a user defined function is
  used, then \code{na.rm} must be set to NULL. Parameters for
  a user function are passed through the \code{,...} mechanism.
 
}
  \item{na.rm}{ \code{na.rm} is set to TRUE by default. This means
  that when annual means are calculated, all NAs will be removed.
  The annual mean will be calculated if ANY of the 12 months are
  present. If \code{na.rm} is set to FALSE, then annual averages
  will only be calculated if ALL 12 months are present. If a user
  function is provided, \code{na.rm} must be set to NULL. 
 
 
}
  \item{\dots}{ for passing parameters to the user defined function.
 
}
}
\details{The annualize function works by using the zoo \code{aggregate}
  function. That function is very straightforward. Like the
  \code{aggregate} function in the base package, a data structure, like
  a data.frame, is subdivided by an index function. In this case we
  map every month of a given year to a unique index, 1, 2, etc.
  Then, the \code{mean} function is applied using that index. So, one
  gets the mean by year. For most purposes setting \code{na.rm} to
  either TRUE or FALSE will suffice. However, in some cases the user
  may want finer control over how means are calculated in the presence
  of \code{NA}. For example, one may want the mean if and only if at
  least 10 months of data are present. Or, if all 4 four seasons have
  at least 2 months of data in each season. That can be accomplished by
  writing a user defined function and passing its handle to \code{user.fun}
  The user defined function describes how to calculate a mean and can
  take parameters which are passed through the \code{,...}
 
}
\value{Returns a zoo series of annual averages either by taking a
  simple mean or by using the user defined function for taking
  a mean. If a time series or mts object is passed in, one will be passed
  out
 
}
 
\author{Steven Mosher
 
}
\note{Thanks to Peter O'Neill for the suggestion
 
}

 

\seealso{\code{\link{keepIf}}
 
}
\examples{
 \dontrun{
  annual <- annualize(Anomalies)
  # get the averages but only if all 12 months have data
  annual2 <- annualize(Anomalies, na.rm = FALSE)
  # use a function called keepIf, set na.rm =NULL, pass cnt =10
  # to the user.fun. That function calculates a mean if there
  # are 10 or months of data in a year
  annual3 <- annualize(Anomalies,user.fun = keepIf, na.rm =NULL, cnt =10)
}
}
 
\keyword{ TimeSeries }
\keyword{ Zoo } 
