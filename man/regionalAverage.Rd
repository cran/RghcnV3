\name{regionalAverage}
\alias{regionalAverage}
 
\title{Determines a local or regional average temperature
 
}
\description{This function is based on the Berkeley Method of calculating
  an average temperature from a collection of stations. This particular
  variant was developed by the Blogger/Time series specialist Tamino.
  An improvement on the reference station method for combining stations
  in a close proximity, this method uses all the information provided
  by the stations to calculate an average for the region. See the post
  referenced in the notes below for a detailed mathematical theory
}
\usage{
regionalAverage(Data, offres = 0.001)
}
 
\arguments{
  \item{Data}{ Data must be a zoo series as created
  by the function \code{ v3ToZoo}. Support for 'mts' class objects
  has also been added. These can be created with \code{v3ToMts}
 
}
  \item{offres}{A threshold for the offset variable that determines
  when the iterative solution is completed.
 
}
}
\details{The function should be called with a zoo series of temperature
  data or mts of temperature data. The function finds a set of "offsets"
  that are used to estimate a mean temperature for the region. 
  
 
}
\value{The function has been modified somewhat from tamino's public
  release. The number of input parameters has been cut. Since the time
  data in a zoo series is already matched, there is no rounding required
  for time. The rounding variable for data has been removed, users can
  round the output if required. The flag that allowed for full output
  of data, including the temperature input dataset and the station Ids
  has been removed. The full dataset is the calling parameter and is
  not changed by the program.  The output consists of a list: The names of
  that list will depend upon the class of the object being worked on. If
  the function is fed a 'zoo' object it returns an item named "Zoo". Mts,
  returns "Mts"
  \item{Zoo}{A zoo object of data, stations, se and stdev}
  \item{Mts}{a "mts" object of data, station count, se,stdev}
  \item{offsets}{a vector of the offsets}
 
}
\references{url{http://tamino.wordpress.com/2011/07/06/aligning-station-records/}
 
}
\author{Tamino; adapted by Steven Mosher
 
}
\note{url{http://www.berkeleyearth.org/Resources/Berkeley_Earth_Summary.pdf}
   url{http://tamino.files.wordpress.com/2011/07/align.pdf}
}

 
\examples{
\dontrun{
  # get the data
  # get the inventory
  # crop the inventory to a region
  # window the data to a time
  # makeZoo series
  # intersect the two
  # call regional
  # plotthe answers
  v3Mean <- readV3Data(filename='yourfile')
  v3Inv  <- readInventory(filename = "invfilename")
  e      <- extent(-100,-90,30,40)
  inv    <- cropInv(v3Inv,e)
  v3late <- windowV3(v3Mean,start = 1950, end = 2010)
  v3Temp <- v3ToZoo(v3late)
  Data   <- intersectInvZoo(inv,v3Temp)
  REGION <- regionalAverage(Data=Data$Zoo)
  plot(REGION$Zoo$data)

}
 
}
 
\keyword{ TimeSeries }
\keyword{ Contributed } 
