\name{intersectInvAnomalies}
\alias{intersectInvAnomalies}
 
\title{ A function to reconcile or intersect an anomaly dataset and inventory
 
}
\description{ When Temperature data is processed and turned into anomalies
  a certain number of stations will be dropped in the process. Namely,
  those stations which do not meet the CAM criteria. So the initial dataset
  with 7278 stations may be reduced to 5000 or so stations. In order to
  merge this with the inventory of 7278 stations you have to find the
  intersection of the two sets of stations. Or, if you modify the inventory
  dataset and reduce it by selecting only rural stations, these have to selected
  from the anomaly dataset. The intersection function is used to do this matching.
  It functions like a normal intersection function except it operates on temperature
  datasets, which contain Ids, and Inventories which contain Ids. The Id common
  to both are selected and then the function returns the two datasets reduced to
  their common denominators.
 
}
\usage{
   intersectInvAnomalies(inv, anomalies)
}
 
\arguments{
  \item{inv}{ An  inventory of stations
 
}
  \item{anomalies}{ A data.frame of anomalies }
 
 
}
\details{ The function operates by taking the two datasets, and 
  finding the intersection of station Ids, those stations are in 
  both datasets and returns a list containing two dataframes:
  an Inventory dataframe and the anomaly dataframe. If the two
  dataframes had different stations before the function call,
  they will be reconciled. This allows you to subset an inventory
  and then "match" that reduced inventory with its corresponding
  temperature data. On return Anomalies will be cast as a matrix
  and transposed. This eases calls into \code{raster} functions
  and now both the inventory and the anomalies will have stations
  in rows. In the inventory station ID will be in column 1 and
  in Anomalies the stations will be in the row names ( like column
  0)
   
 
}
\value{ The function returns a list of a data.frame and a matrix.
  $Inventory contains a data.frame of the station inventories.
  $Anomalies contains a \code{matrix}. This is important to understand.
  When the function is called a data.frame of Anomalies is fed into the function.
  On return, that data.frame is turned into a matrix with the rows and columns
  transposed. Stations are now in row names and dates are in column names. This
  transposition is key for interfacing with \code{raster} type functions.
  Suppose the following call is made 
  \code{Data <- intersectInvAnomalies(inv, anomalies)} 
  on return, the inventory can be addressed as
  \code{Data$Inventory} and the anomalies can be addressed
  as \code{Data$Anomalies} The stations in each of these data structures will
  match. \code{Data$Anomalies} will be a matrix at this point.
 
}
 
\author{ Steven Mosher}
 
 
\examples{
\dontrun{
 files   <- downloadV3(url = V3.MEAN.RAW.URL)
 v3Mean  <- readV3Data(filename=files$DataFilename)
 Inv     <- readInventory(filename=files$InventoryFile)
 Anomaly <- createAnomaly(V3Data=v3Mean)
 Data    <- intersectInvAnomaly(inv=Inv,anomalies=Anomaly)
 

}
 
}
 
\keyword{ spatial }