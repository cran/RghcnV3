\name{downloadSST}
\alias{downloadSST}

\title{A function to download Hadley SST
 
}
\description{The function locates the HADSST2 sea surface temperature
  data and downloads and unpacks it. This data set currently has a start
  date of 1850 and data is included for every month to the present. The
  data is downloaded to the working directory unles directory is explicitly
  set otherwise. If directory does not exist, it will be created.
  
 
}
\usage{
downloadSST(url = HADSST2.URL, directory = getwd(), 
            overwrite = FALSE, remove = FALSE)
}
 
\arguments{
  \item{url}{The url is predefined according to its published location
  at the time of testing the package. 
 
}
  \item{directory}{The directory where the files will be downloaded to.
  This defaults as the working directory. If directory does not exist,
  it will be created.
 
}
  \item{overwrite}{If overwrite is false and the file already exists
  an error will be thrown. To overwrite the file, set overwrite to
  TRUE. The download takes a while, so overwrite is set to false
  by default. If you want to re download the data, then set the flag
  to \code{TRUE}.
 
}
  \item{remove}{Remove the compressed file after decompression
 
}
}
\details{The file is compressed at a .gz file. That file is 
  \code{gunzipped} and a \code{ncdf} file is unpacked.
  
 
}
\value{The function returns the path name to the unzipped
  file. This is a complete path.

}
 
\author{Steven Mosher
 
}
 
\examples{
\dontrun{sstFile <- downloadSST()}
 
}
 
\keyword{ Download }
 
