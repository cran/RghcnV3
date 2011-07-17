\name{intersectInvMts}
\alias{intersectInvMts}
 
\title{A function to reconcile or intersect an Mts dataset and inventory
 
}
\description{ During the course preparing temperature data or inventory
  data for use in a study it is a common practice to segment the data.
  For example, to select a subset of stations in an inventory or
  to selectcertain years of temperature data. When the inventory
  datasets are read in and when the temperature is read in every record
  in the temperature dataset has a matching inventory record. After
  stations have been removed from either the temperature data or inventory
  data these two must be reconciled. The stations must match. This
  function does that for inventories and for \code{mts} objects.
  
 
}
\usage{
intersectInvMts(inventory, Mts)
}
 
\arguments{
  \item{inventory}{An  inventory \code{data.frame} of stations.
    This is the typically output by \code{readInventory}
 
}
  \item{Mts}{A mts object of temperatures. This is created
    by \code{v3ToMts}}
 
 
}
\details{The function operates by taking the two datasets, and 
  finding the intersection of station Ids, those stations that are in 
  both datasets and returns a list containing two objects:
  an Inventory dataframe and the mts object. If the two
  datasets had different stations before the function call,
  they will be reconciled. This allows you to subset an inventory
  and then "match" that reduced inventory with its corresponding
  temperature data.
 
}
\value{The function returns a list of a data.frame and a mts object.
  $Inventory contains a data.frame of the station inventories.
  $Mts contains a \code{mts} object of temperatures.    
  Suppose the following call is made: 
  \code{Data <- intersectInvMts(inv, Mts)}.
  On return, the inventory can be addressed as
  \code{Data$Inventory} and the temperatures can be addressed
  as \code{Data$Mts} The stations in each of these data structures
  will match. Namely, column 1 of the \code{Data$Inventory} object
  will contain the same stations in the same order as the columns
  of \code{Data$Mts}
 
}
 
\author{Steven Mosher
 
}
 
 
\examples{
 \dontrun{
  files   <- downloadV3(url = V3.MEAN.RAW.URL)
  v3Mean  <- readV3Data(filename=files$DataFilename)
  Inv     <- readInventory(filename=files$InventoryFile)
  #  get the first 2000 stations
  Inv     <- Inv[1:2000,]
  Temps   <- v3ToMts(v3Mean)
  Data    <- intersectInvMts( Inv,Temps)
 
 }
}
 
\keyword{ spatial }
 
