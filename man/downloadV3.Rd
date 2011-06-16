\name{downloadV3}
\alias{downloadV3}
 
\title{ A function to download the version 3 Temperature data
 
}
\description{ The function downloads the datasets for the Global
  Historical Climate Network version 3, available on a public ftp.
  The function downloads the tar.gz data sets and decompresses the
  contents to a local directory.
 
}
\usage{
  downloadV3(url = V3.MEAN.ADJ.URL, directory = getwd(),
             overwrite = FALSE, remove = FALSE)
}
 
\arguments{
  \item{url}{ The url is defaulted to download the "adjusted" V3 'Tave' data.
  there are 6 different datasets that are updated on a near daily basis. Each
  has a different URL. This package provides the url or each in stored
  data. 
  \code{V3.MEAN.ADJ.URL},
  \code{V3.MEAN.RAW.URL},
  \code{V3.MAX.ADJ.URL},
  \code{V3.MAX.RAW.URL},
  \code{V3.MIN.ADJ.URL},
  \code{V3.MIN.RAW.URL}.
  The should be loaded when the package loads. If not, \code{data()} will
  load them.
 
}
  \item{directory}{The default directory is the working directory. After
  downloading the files are unpacked and stored under a dated directory.
  This directory will be written directly under the working directory or
  the directory provided by the user. If \code{directory} does not
  exist it will be created
 
}
  \item{overwrite}{Overwrites the compressed files (tar or gz) 
 
}
  \item{remove}{Removes the compressed files after decompression
 
}
}
\details{Using the URL provided the specified compressed files
  are downloaded to the directory provided. They are 
  \code{gunzipped} using the \code{R.utils} package and
  then \code{untar}. This process creates a subdirectory that
  is dated. The date is required as these files are updated on
  a daily basis. Within that subdirectory there will be two
  files: a data file and an inventory file. They must be used
  together. After download and decompression a typical set up
  would have a subdirectory named. \code{ghcnm.v3.0.0.20110612}
  with two files in it.
  \code{ghcnm.tavg.v3.0.0.20110612.qca.dat}
  \code{ghcnm.tavg.v3.0.0.20110612.qca.inv}
  Those files are the data files and the inventory files on 
  June 12th. Downloading on a different day would collect
  a different dataset.
 
}
\value{ The function returns a list with three values. \code{DataFilename}
  and \code{InventoryFile} and \code{Date}. These variables can
  be used to read in the local data \code{readV3Data()}.
  \item{DataFilename}{ a full path to the datafile just downloaded}
  \item{InventoryFile}{a full path to the inventory file just downloaded}
  \item{Date}{ a Posix date taken from the filenames}
 
}
 
\author{Steven Mosher
 
}
 

 
\examples{
\dontrun{
 files   <- downloadV3(url = V3.MEAN.RAW.URL)
 v3Mean  <- readV3Data(filename=files$DataFilename)
 Inv     <- readInventory(filename=files$InventoryFile)
 Anomaly <- createAnomaly(V3Data=v3Mean)


}
 
}
 
\keyword{ file }
 
