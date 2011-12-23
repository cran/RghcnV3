\name{camMask}
\alias{camMask}
 
\title{ create a mask temperature data
 
}
\description{ The function returns a true/false mask for a dataset of
 station temperatures. If station records "pass" the cam test specified
 TRUE is returned otherwise false. 
 
}
\usage{
camMask(Data, criteria = list(Start = 1961, End = 1990, Years = 15, Threshold = 12))
}
 
\arguments{
  \item{Data}{ A data structure of stations and their temperatures.
   This can be an Array object, Zoo object or MTS
 
}
  \item{criteria}{ A list of criteria for passing a CAM test.
 
}
}
\details{ A data structure such as a MTS object will have station data
 in  columns with every row holding a month of data. In certain situations
 the user may wish to select those stations that have "complete" records.
The "camMask" is one approach to selecting stations. For example,
 you might select stations that had at least 10 complete years in the 1961
 to 1990 period. When a data structure is passed to camMask, the function
 will return a mask that can then be applied to the data structure
 to select only those stations that pass the test.
 
}
\value{ Returns a true/false mask as long as the data structure
 
}
 
\author{Steven Mosher
 
}
 

 

 
\examples{
 \dontrun{
 
  mask <- camMask(Data)
  Data <- Data[mask,]
 }
}
 
\keyword{ TimeSeries }
 