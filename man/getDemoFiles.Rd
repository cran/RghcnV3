\name{getDemoFiles}
\alias{getDemoFiles}
 
\title{A utility used by demo programs
 
}
\description{A simple function to return downloaded file
  names required by various demo programs
 
}
\usage{
getDemoFiles(directory = GHCN.V3.DATA)
}
 
\arguments{
  \item{directory}{The data directory  
   
}
}
\details{Used by the demo programs to get the file names
  or to download the files if they are absent. 
 
}
\value{Returns a list of filenames that can be used in the read*
  functions. Gets, data and inventories, masks and SST}
 
\author{Steven Mosher
 
}
 
 
\keyword{ Internals }
 
