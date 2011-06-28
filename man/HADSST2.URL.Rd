\name{HADSST2.URL}
\alias{HADSST2.URL}
\docType{data}
\title{A Url to the HADSST2 dataset
 
}
\description{ A url that points to the dataset. This Url is 
  used to download the data
 
}
\usage{ HADSST2.URL }
\format{
  The format is:
 chr "http://hadobs.metoffice.com/hadsst2/data/HadSST2_1850on.nc.gz"
}
\details{The dataset is gz compressed. It is a 5 degree by 5 degree
  dataset, from -180 to +180, -90 to 90. The data is temperature
  anomalies. The base period is 1961-1990. The first date in the file
  is Jan 1850.
 
}
\source{From the website description:
  "The SST data are taken from the International Comprehensive 
  Ocean-Atmosphere Data Set,ICOADS,from 1850 to 1997 and from
  the NCEP-GTS from 1998 to the present. HadSST2 is produced by taking
  in-situ measurements of SST from ships and buoys, rejecting measurements
  which which fail quality checks, converting the measurements to anomalies
  by subtracting climatological values from the measurements, and calculating
  a robust average of the resulting anomalies on a 5 deg by 5 deg degree monthly 
  grid. After gridding the anomalies, bias corrections are applied to remove
  spurious trends caused by changes in SST measuring practices before 1942.
  The uncertainties due to under-sampling have been calculated for the 
  gridded monthly data as have the uncertainties on the bias corrections 
  following the procedures described in the paper."
 
}
\references{url{http://www.metoffice.gov.uk/hadobs/hadsst2/rayner_etal_2005.pdf}
  The above paper describes the dataset
}
\examples{
print(HADSST2.URL)
 
}
\keyword{datasets}
