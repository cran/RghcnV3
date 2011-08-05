\name{readCruStations}
\alias{readCruStations}
 
\title{A function to read Cru Station data
 
}
\description{reads CRU station data and returns a user slected data format:
  Zoo, mts or Array
 
}
\usage{readCruStations(filename = "stations-data.txt", output = c("Array", "Zoo", "Mts"))
}
 
\arguments{
  \item{filename}{The default filename of CRU station data
 
}
  \item{output}{Select On of these three data output options
 
}
}
\details{CRU data and inventory is conatined in one monolithic file.
  The routine parses the data into two objects, an Inventory and a data
  set. The dataset returned is defined by the setting of the  "output"
  parameter
 
}
\value{The function returns a list with two objects and inventory
  objects and a dataset
  \item{Inventory}{ a dataframe is returned}
  \item{Zoo}{ if zoo is selected as output a list element named Zoo is returned}
  \item{Mts}{ if Mts is selected as output a list element named Mts is returned}
  \item{Array}{ if Array is selected as output a list element named Array is returned}
 
}
 
\author{Steven Mosher
 
}
 

 
\examples{
 \dontrun{
 
   Cru <- readCruStations()
 }
}
 
\keyword{ files }
 
