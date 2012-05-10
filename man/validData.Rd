\name{validData}
\alias{validData}
 
\title{ validData performs sanity checks on temperature data
 
}
\description{ During the course of creating temperature data objects,
 such as Arrays various sources can be merged. This can lead to
 situations were you have duplicate stations or stations with
 all the data missing or stations that do not start on whole years.
 validData  checks for a variety of issues and returns diagnostics.
 Internally other functions which depend upon valid data will
 call \code{validData}
 
}
\usage{
validData(Data)
}
 
\arguments{
  \item{Data}{ An Array or Mts or Zoo object
 
}
}
\details{ The function checks for the following conditions:
  duplicate station Ids in the temperature data. Objects
  with a number of months that is not a multiple of 12.
  Months where no station has a report. The issues are
  reported but not fixed.
 
}
\value{ Returns TRUE or returns FALSE with a diagnostic
 
}
 
\author{Steven Mosher
 
}
 

 

 
 
\keyword{ Timeseries }
 