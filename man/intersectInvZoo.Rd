\name{intersectInvZoo}
\alias{intersectInvZoo}
 
\title{ A function to reconcile or intersect an zoo dataset and inventory
 
}
\description{ When Temperature data is processed and turned into zoo anomalies
  a certain number of stations will be dropped in the process. Namely,
  those stations which do not meet the CAM criteria. So the initial dataset
  with 7280 stations may be reduced to 5000 or so stations. In order to
  merge this with the inventory of 7280 stations you have to find the
  intersection of the two sets of stations. Or, if you modify the inventory
  dataset and reduce it by selecting only rural stations, these have to selected
  from the zoo dataset. The intersection function is used to do this matching.
  It functions like a normal intersection function except it operates on temperature
  datasets, which contain Ids, and Inventories which contain Ids. The Id common
  to both are selected and then the function returns the two datasets reduced to
  their common denominators.  
 
}
\usage{
   intersectInvZoo(inventory, zooObj)
}
 
\arguments{
  \item{inventory}{ An  inventory \code{data.frame} of stations.
    This is the typically output by \code{readInventory}
 
}
  \item{zooObj}{ A zoo object of anomalies or temperatures. This is created
    by \code{createAnomaly}} or by \code{v3ToZoo}
 
 
}
\details{ The function operates by taking the two datasets, and 
  finding the intersection of station Ids, those stations that are in 
  both datasets and returns a list containing two objects:
  an Inventory dataframe and the  zoo object. If the two
  datasets had different stations before the function call,
  they will be reconciled. This allows you to subset an inventory
  and then "match" that reduced inventory with its corresponding
  temperature data.  
   
 
}
\value{ The function returns a list of a data.frame and a zoo object.
  $Inventory contains a data.frame of the station inventories.
  $Zoo contains a \code{zoo} object of Anomalies.    
  Suppose the following call is made: 
  \code{Data <- intersectInvZoo(inv, anomalies)}.
  On return, the inventory can be addressed as
  \code{Data$Inventory} and the anomalies can be addressed
  as \code{Data$Zoo} The stations in each of these data structures
  will match. Namely, column 1 of the \code{Data$Inventory} object
  will contain the same stations in the same order as the columns
  of \code{Data$Zoo}
 
}
 
\author{ Steven Mosher}
 
 
\examples{
\dontrun{
 files   <- downloadV3(url = V3.MEAN.RAW.URL)
 v3Mean  <- readV3Data(filename=files$DataFilename)
 Inv     <- readInventory(filename=files$InventoryFile)
 Inv     <- Inv[which(Inv$Airport == TRUE), ]
 Anomaly <- createAnomaly(V3Data=v3Mean)
 ## which stations in the inventory match those in the Anomaly
 ## dataset? find the intersection and adjust both datasets
 ## to the common denominator
 Data    <- intersectInvZoo(inv=Inv,anomalies=Anomaly)
 

}
 
}
 
\keyword{ spatial }