\name{isInventory}
\alias{isInventory}
 
\title{ A test function to tell if a dataframe is an inventory
 
}
\description{Since functions depend on an inventory having certain data
  This function checks inventories to ensure that they have a station
  Id, a latitude in the second column and Longitide in the thrid column
  
 
}
\usage{
isInventory(inv)
}
 
\arguments{
  \item{inv}{ The function is pass a data structure and it is tested
  to ensure that it has the right inventory information
 
}
}
\details{Because the inventory dataframe can be altered by programmers
  there is no way to ensure that the require information stays in the object.
  Until this package is ported to an OOP approach the code will make
  simple checks to determin if objects comply with certain specifications.
  An inventory must be a data.fraem with Id in column 1, Lat in column 2
  and Lon in column 3. 
 
}
\value{Returns true or false and throws a warning if false
 
}
 
\author{Steven Mosher
 
}
 
 
\examples{
 \dontrun{
  x <- isInventory(inv)
 } 
}
 
\keyword{ Internals }
 
