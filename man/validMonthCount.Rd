\name{validMonthCount}
\alias{validMonthCount}
 
\title{ Checks an object for having all months complete
 
}
\description{ By definition a 3D Array has slots for every month of every
 year. Even if the data is NA. Some sources, however, may drop months
 if they have missing data. This function checks whether an Mts or
 Zoo object has all months present and checks that it begins with
 January and ends with december
 
}
\usage{
validMonthCount(Data)
}
 
\arguments{
  \item{Data}{ An Array, Mts or Zoo object
 
}
}
\details{ If you pass an Array it returns true by definition.
 Otherwise the number of months is checked for divisibility by
 12 and the cycle is checked to insure that the structure starts
 with january and ends with december
 
}
\value{ returns TRUE or FALSE
 
}
 
\author{Steven Mosher
 
}
 

 
 
    
           
\keyword{ Timeseries }
 