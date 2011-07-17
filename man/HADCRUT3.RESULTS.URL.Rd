\name{HADCRUT3.RESULTS.URL}
\alias{HADCRUT3.RESULTS.URL}
\docType{data}
\title{A url to Hadcrut3 temperature series
 
}
\description{This is the url to the hadcrut3 land temperature record.
  The data consists of monthly anomalies relative to a 1961-1990
  baseline. The data is starts in 1850 and is updated monthly. This
  is a combined land/ocean dataset as opposed to crutem3 which
  is land only
 
}
\usage{ HADCRUT3.RESULTS.URL }
\format{
  The format is:
 chr "http://www.cru.uea.ac.uk/cru/data/temperature/hadcrut3gl.txt"
}
\details{The dataset contains monthly figures for anomalies and annual
  figures as well. In addition, a metric for the percentage of land
  sampled is included. It is up to the date. Care should be taken
  because of the way future months are included. For example,
  in June, there will be data elements for july through Dec. They
  will typically be zero. Future releases will use the system
  clock to trim the data to only read in data that is before clock time
 
}

\source{the data is described here
  \url{http://www.cru.uea.ac.uk/cru/data/temperature/}
 
}
\references{documentation for the data is contained the following paper
  \url{http://www.cru.uea.ac.uk/cru/data/temperature/HadCRUT3_accepted.pdf}
 
}
\examples{
print(HADCRUT3.RESULTS.URL)
 
}
\keyword{datasets}
