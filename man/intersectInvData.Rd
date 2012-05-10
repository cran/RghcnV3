\name{intersectInvData}
\alias{intersectInvData}
 
\title{A function to reconcile a collection of data and an Inventory
 
}
\description{The Inventory and temperature data are tied together by the 
  station Ids. If you edit the inventory to remove or select certain stations
  then the temperature data must contain the same stations. Typically, one would
  select a subset of the inventory, for example all rural stations, and then
  find those stations in the temperature data. This function accepts the three
  basic dataset as an input ( Array, Zoo and Mts) along with a inventory. 
  The input Inventory and the input data  are both reconciled and on output 
  both are adjusted to have matching stations. Stations are also sorted so
  that the Ids are in a common order. 
  
 
}
\usage{intersectInvData(inventory, Data)
}
 
\arguments{
  \item{inventory}{An inventory data.frame containing station Ids
 
}
  \item{Data}{The temperature or anomaly data  
 
}
}
\details{It's often quicker to prune the  data  prior to creating
  anomalies. So, for example one can "window" the
  data or you can select a subset of the stations before you turn the
  temperature data  into an anomaly series. For example, you could select
  only the "rural" stations from the inventory. Then call this function
  \code{intersectInvData()}. The advantage in this is that fewer stations
  will have to be run through the anomalize process.
 
}
\value{the function returns a list with two elements. An inventory
  element and a  data element
  \item{Inventory}{ a inventory with stations matching the temperature series}
  \item{Array}{ an Array object if an Array is input}
  \item{Zoo}{ a Zoo object if a Zoo is input}
  \item{Mts}{ a Mts object if a Mts is input}
 
}
 
\author{Steven Mosher
 
}
 

 

 
 
 
\examples{
 \dontrun{
  # we select only certain stations and time periods
  # then we reconcile the two data sources.
  vdata    <- readV3Data(filename="yourfile", output = "Array")
  ruralInv <- inv[which(inv$Rural == "R"),]
  vdata    <- windowArray(vdata, start = 1920, end =1996)
  DATA     <- intersectInvData(ruralInv,vdata)
  anomalies <- anomalize(DATA$Array)
  
 }
}
 
\keyword{ spatial }
 
