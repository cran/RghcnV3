\name{readHadResults}
\alias{readHadResults}
 
\title{A function to read Monthly Hadley Temperature
 
}
\description{Every Month HadCru posts monthly updates of its
  temperature series. There are three monthly files all in the same
  format. This function reads and retruns that data directly from
  the supplied URL. 
 
}
\usage{ readHadResults(url = c(HADSST2.RESULTS.URL, 
                HADCRUT3.RESULTS.URL, CRUTEMP3.RESULTS.URL))
}
 
\arguments{
  \item{url}{Three options for the url are shown. Each corresponds to
  A different dataset. The SST series, The land/ocean series and the
  land series. In calling this function ONE series should be read.
  Reading all three series requires three calls to the function with
  a different URL sent each time. A local copy is not kept. If more
  than one series is included in the variable url, then a warning is
  thrown and the first in the list is read from the internet.
 
}
}
\details{Monthly data is updated every month for the three datasets
  Only monthly data is read in and the annual averages are recalculated.
  care should be taken in looking at the most recent months. The last line
  of data in the file will contain data for all the months in that year,
  even if that month hasn't happened. These are recorded as zeros, rather
  than NA. The output of this function thus must trim the time series
  to ensure that these zeros are dropped.
  
 
}
\value{The function returns a monthly zoo series of data from the 1850
  to the last month of the current Year. 
 
}
 
\author{Steven Mosher
 
}
 

 

 
\examples{
\dontrun{

  cruMonthly <- readHadResults(CRUTEMP3.RESULTS.URL)
  hadMonthly <- readHadResults(HADCRUT3.RESULTS.URL)
  sstMonthly <- readHadResults(HADSST2.RESULTS.URL)
}
 
}
 
\keyword{ file }
 
