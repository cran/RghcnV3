\name{trimHeadTail}
\alias{trimHeadTail}
 
\title{ Trims the head and tail of a time series object
 
}
\description{ The functions in this package assume that time series
 start with january and end with december. This function will trim
 any object ( Array,Mts, Zoo) to remove all months before the first
 january and after the last december
 
}
\usage{
trimHeadTail(Data)
}
 
\arguments{
  \item{Data}{Any Array object, Mts Object or Zoo object
 
}
}
\details{ Different sources such as CRU or Berkeley supply data
  in formats that may include incomplete years at the head and tail.
  This function quickly trims the head and tail.
 
}
\value{Returns the same object only trimmed such that the first time
 is a january and the last time is a december. Every year
 will have 12 months.
 
}
 
\author{Steven Mosher
 
}
 

 
 
  
 
\keyword{ Timeseries }
 