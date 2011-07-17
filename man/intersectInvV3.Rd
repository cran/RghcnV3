\name{intersectInvV3}
\alias{intersectInvV3}
 
\title{A function to reconcile a V3 dataframe and an Inventory
 
}
\description{The Inventory and temperature data are tied together by the 
  station Ids. If you edit the inventory to remove or select certain stations
  then the temperature data must conatin the same stations. Typically, one would
  select a subset of the inventory, for example all rural stations, and then
  find those stations in the temperature data. This function accepts a
  14 column V3 dataset as an input along with a inventory. the input Inventory
  and the input data.frame are both reconciled and on output both are adjusted
  to have matching stations. 
  
 
}
\usage{
intersectInvV3(inventory, V3data)
}
 
\arguments{
  \item{inventory}{An inventory data.frame containing station Ids
 
}
  \item{V3data}{The temperature data output by \code{readV3data}
 
}
}
\details{It's often quicker to prune the V3 data.frame prior to turning
  it into a zoo object or mts object. So, for example one can "window" the
  data or you can select a subset of the stations before you turn the
  temperature data.frame into a zoo series. For example, you could select
  only the "rural" stations from the inventory. Then call this function
  \code{intersectInvV3()}. The advantage in this is that fewer stations
  will have to be turned into zoo series with \code{v3ToZoo}
 
}
\value{the function returns a list with two elements. An inventory
  element and a V3 data element
  \item{Inventory}{ a inventory with stations matching the temperature series}
  \item{V3}{ a 14 column data.frame of temperatures}
 
}
 
\author{Steven Mosher
 
}
 

 

\seealso{\code{\link{intersectInvZoo}}, \code{\link{intersectInvMts}}
 
}
\examples{
 \dontrun{
  # we select only certain stations and time periods
  # then we reconcile the two data sources.
  vdata    <- readV3Data(filename="yourfile")
  ruralInv <- inv[which(inv$Rural == "R"),]
  vdata    <- windowV3(vdata, start = 1920, end =1996)
  DATA     <- intersectInvV3(ruralInv,vdata)
  
 }
}
 
\keyword{ spatial }
 
