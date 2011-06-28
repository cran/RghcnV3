\name{RghcnV3-package}
\alias{RghcnV3-package}
\alias{RghcnV3}
\docType{package}
\title{A package for downloading, importing and analyzing GHCN version 3
 
}
\description{The RghcnV3 package provides some basic tools for downloading
  formating and analyzing Global Historical Climate Network temperature
  data. It is targeted at the version 3 release. The core functions allow
  for the easy creation of temperature anomalies for land surface stations.
  In addition, land masks are provided. Together with the raster package
  these functions can be used to create a global temperature index using
  the Common Anomaly Method or CAM. The functions provided will create a
  data structure that can be directly used by raster functions to create
  spatially weighted maps of temperatures.
 
}
\details{
\tabular{ll}{
Package: \tab RghcnV3\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2011-06-28\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
LazyData: \tab no\cr
}
 
}
\section{I. Overview of Processing}{
  The goal of this package is to provide an API for the formating and processing
  of GHCN Temperature data. The API allows a analyst to read in GHCN data and
  cast it into two useful packages in R \code{zoo} and \code{raster}. A knowledge
  of those packages is essential to using this package. Maximum reuse of 
  \code{zoo} and \code{raster} was a design criteria. The purpose of this approach
  is to leverage code that is tested and known to work. Instead of creating a
  novel approach to calculating a global temperature index, the goal here is to
  re-use tried and tested algorithms. That requires some data reformating throughout
  the analysis process.\cr
    Essentially the package provides a series of functions 
  which put GHCN data into the various objects of \code{zoo} and \code{raster}.
  There is one primary data flows that brings GHCN data into the objects of
  the \code{raster} package. Data is brought in from GHCN and then it is 
  passed into \code{zoo} objects. This creates a specialized time series
  object, known as a zoo object.\cr 
    The zoo object is great for plotting the
  individual station data, either station temperatures or station anomalies.
  Moving from GHCN to \code{zoo} is basically moving from a \code{data.frame} 
  to a \code{zoo} object. The \code{zoo} object, like the data.frame is
  two dimensional. In \code{zoo} format the station's data is presented
  in column format. Columns are stations; rows are times. This organization
  is imposed by the \code{zoo} package. Prior rasterizing the zoo object
  the stations in the object must be paired with their Lat/Lon data in
  the inventory. That happens
  in the function \code{intersectInvAnomalies}. This function brings
  together the station inventories (Lat/Lon) and the station Anomalies
  (time). After that the data can be directly read into a 3D raster
  data structure called a \code{brick}. Essentially Lat/Lon/Time with
  measures ( temperature anomalies) in every cell.\cr
  By way of overview here are the two typical calling sequences
  \tabular{ll}{ 
    \code{readV3Data}\cr 
    \code{createAnomaly}\cr
    \code{intersectInvAnomalies}\cr
    \code{rasterizeAnomaly}\cr
    }
  This path is the most direct path and is the fastest processing. The
  only drawback is all the data is in anomalies. Inside the function
  \code{createAnomaly} there are three other functions called:
  \tabular{ll}{
    \code{createTemperature}\cr
    \code{anomalize}\cr
    \code{whichStationsCAM}\cr
  }
  In the second approach instead of calling \code{createAnomaly}
  the three functions inside that function can be called.
  The result is exactly the same, but in  this approach, you have access to
  temperature data for plotting if you like. Here is that approach:
  \tabular{ll}{
    \code{readV3Data}\cr 
    \code{createTemperature}\cr
    \code{anomalize}\cr
    \code{whichStationsCAM}\cr
    \code{intersectInvAnomalies}\cr
    \code{rasterizeAnomaly}\cr
  }
  As you can see the three calls inside of \code{createAnomaly} are
  now called directly.
  The main purpose of wrapping 3 functions into \code{createAnomaly} is
  that it speeds up processing and uses less memory. But, you lose access
  to the temperatures and have to work in anomalies, which is standard
  practice. The functions are described below in the links.
  
  
   \tabular{ll}{
    \code{\link{readV3Data}}\tab Read in V3 data\cr
    \code{\link{createTemperature}}\tab Create Temperature Series\cr
    \code{\link{anomalize}}\tab Create Anomalies\cr
    \code{\link{whichStationsCAM}}\tab select stations\cr
    \code{\link{createAnomaly}}\tab Create Anomalies\cr
    \code{\link{intersectInvAnomalies}}\tab Prepare for spatial Analysis\cr
    \code{\link{rasterizeAnomaly}}\tab spatial processing\cr
      
   }

}
\section{II. Downloading Data}{
  The data required for the RghcnV3 package is available for download
  from various urls. See the table below for documentation on the urls
  and the files at those locations. These urls are read into your work
  environment upon package loading. They are predefined and you
  can just use them as is. Each data type has its own specialized
  download function. The download function calls \code{download.file}
  and then decompresses the file using the appropriate tools. Programmers
  of course can choose to download the files manually or write their
  own download functions

    \tabular{ll}{
    \code{\link{V3.MEAN.RAW.URL}}\tab unadjusted Tave data\cr
    \code{\link{V3.MEAN.ADJ.URL}}\tab adjusted Tave data\cr
    \code{\link{V3.TMAX.RAW.URL}}\tab unadjusted Tmax data\cr
    \code{\link{V3.TMAX.ADJ.URL}}\tab adjusted Tmax data\cr
    \code{\link{V3.TMIN.RAW.URL}}\tab unadjusted Tmin data\cr
    \code{\link{V3.TMIN.ADJ.URL}}\tab  adjusted Tmin data\cr
    \code{\link{WATER.MASK.URL}}\tab land water 1/4 deg mask\cr
    \code{\link{OCEAN.MASK.URL}}\tab land ocean 1/4 deg mask\cr
    \code{\link{HADSST2.URL}}\tab sea surface temperature anomalies\cr
      
   }
   The download functions are named as follows
   
   \tabular{ll}{
    \code{\link{downloadV3}}\tab downloads most current V3\cr
    \code{\link{downloadMask}}\tab download land masks\cr
    \code{\link{downloadSST}}\tab download SST data\cr
    \code{\link{GHCN.V3.DATA}}\tab name of download directory\cr
    }
    In the \code{demo} programs there is a demo script that will
    download all the files and create a download directory named
    \code{"GhcnV3Data"} that is the value of \code{GHCN.V3.DATA}
    simply run \code{demo(DownloadAllV3)} and all the files
    download automagically
     
      
   
   

}
\section{III. Reading Data}{
  After data has been downloaded and decompressed the first task is to
  read that data into a R data object. Reading in data is relatively
  straightfoward. Use the functions below. Inventory data is the station
  metadata. Note that the names have been renamed to less "terse"
  names. Type \code{FILE.PARAMETERS} at the console for a look at them.
  \code{readV3Data} reads in only the temperature data\cr 
    The format it produces at output is exactly the same as
  GHCN version 2. 14 columns of data. Column 1 is the station Id,
  column 2 is the year, and columns 3:14 are months of the year. 
  Temperatures are rescaled to WHOLE degrees on return.
  GhcnV2 format is produced by this function so that other code based
  on that format will simply work.\cr
  With version 3 NCDC has added QC data flags for each and every 
  temperature measure. There are three types
  of flags: DMFLAGS, QCFLAGS, and DSFLAGS. Flags about missing data,
  quality control checks, and data sources. Consult the readme. The
  function \code{readQC} returns a list of three data frames, one for
  each flag type. Those three data.frames are each in 14 column format.\cr
    \code{readMask} will read either a water/land mask or ocean/land mask.
  The masks provided are 1/4 degree masks. The must be rescaled to
  the grid size you desire by the \code{raster} function 
  \code{aggregate}. Typically a 3 degree or 5 degree mask. The function
  returns a raster object for the entire globe. Every cell is given a
  value between 0 and 1 depending on the FRACTION of land in the cell.
  This can be used to weight cell temperature values by the fractional
  land area in the cell. The native file runs from 0 to 100 percent.
  This is rescaled to 0 to 1.\cr
    \code{readSST} reads a ncdf file of sea surface anomalies and returns
  a raster \code{brick}. A brick is a 3D raster object. Lat/Lon/Time.
  SST is in a 5 degree by 5 degree cell structure. For combining land
  and ocean, both must be at the same resolution. This can be handled
  by the raster function \code{aggregate}. The functions for reading
  are listed below

    \tabular{ll}{
    \code{\link{readInventory}}\tab Read in V3 station Inventory\cr
    \code{\link{readV3Data}}\tab read v3 data\cr
    \code{\link{readQC}}\tab read v3 Qc data\cr
    \code{\link{readMask}}\tab read land mask\cr
    \code{\link{readSST}}\tab read sst data\cr
    \code{\link{GHCN.V3.DATA}}\tab Directory for downloads\cr
    
      
   }
   
   The most important thing to realize is that after import V3 data is in a
   packed format. That is it is 14 columns and a single station will have
   data over several rows of the data.frame. Also, if a stations record
   has gaps in time, for example, 3 full years of data missing, then those
   years will be missing from the data.frame.\cr
      Turning the data.frame format into a series format where every 
   station is one row or one column of data  happens in the next stage 
   of processing. Because  \code{readV3} outputs a data.frame one can
   do some simple processing on it before proceeding to the next steps. 
   For example, you can drop certain years by working on the \code{Year} 
   variable of the data.frame. The function \code{windowV3} can be used for that.
   Inventories can also be operated on, selecting subsets, such as "airport"
   and "no airport". Similarly with the SST \code{brick} various time slices
   can be dropped by using the raster function \code{dropLayers}. For
   a \code{brick} the \code{layerNames} function will provide you with the
   date of every layer of the brick.
   
   
   
}
\section{IV. Creating Anomalies}{

  Once V3 data has been returned from \code{readV3Data} it is in
  a data.frame format. To recall that format has no records for
  years where all the data is missing. In order to create Time series
  we need to "fill in" those gaps with NA. We also need to create 
  anomalies from temperatures. As we do this the data will be taken out
  of the data.frame format and into a \code{zoo} regular time series
  format. A 2D zoo structure is used.\cr
    In that structure all time series are in columns and the rows 
  contain consequtive months of data, with NAs included. A zoo object 
  however can be accessed  just like a matrix or data.frame is accessed 
  by using  R indexing approaches \code{[,1:2]}.\cr
    There are two paths to creating zoo obejcts from the 14 column data.frame.
  Directly by using \code{createAnomaly} or STEP by STEP by using the 
  three functions that \code{createAnomaly}. To repeat, \code{createAnomaly}
  calls three function:
  \tabular{ll}{
  \code{createTemperature}\cr
  \code{anomalize} and then\cr
  \code{whichStationsCAM}.\cr
  }
  Here is what those 3 functions do. 
  \code{createTemperature} takes the 14 column dataframe and creates a 2D
  zoo object of temperatures. Column names hold the station Id and every
  row of data is a month. There are leading and trailing NA depending on the years
  you select. zoo also has a host of time series functions you can use on the data. 
  for example, \code{na.trim} for removing leading and trailing NA. So that, 
  \code{plot(na.trim(Temps[,1]))} would plot the very first station with all
  the leading and trailing NA removed.\cr
    After temperature series are created, then one can create
  anomalies by using \code{anomalize} . Supply that function with a base period
  for the Common Anomaly Method (CAM) and each station will be normalized to
  that base period. Finally, to complete the Common Anomaly Method, one typically
  specifies that a station must have a certain number of years in the 
  period, otherwise it is dropped. 1961 to 1990 is the standard period 
  selected. We also require
  that a station have 15 years or more years in that period or it is dropped from
  analysis. Also, one selects the number of months in a year that have to be present
  to "count" as a full year. values between 10 and 12 are typical. The function
  \code{whichStationsCAM} will take a series of anomalies ( or temperatures) and
  return the subset of valid station series.  

     \tabular{ll}{
    \code{\link{createTemperature}}\tab Read in V3 data\cr
    \code{\link{anomalize}}\tab Create Temperature Series\cr
    \code{\link{whichStationsCAM}}\tab Create Anomalies\cr
    \code{\link{createAnomaly}}\tab select stations\cr
    \code{\link{intersectInvAnomalies}}\tab Create Anomalies\cr
    \code{\link{getStations}}\tab Prepare for spatial Analysis\cr
          
   }
   
   The final step in processing anomalies is really a preparation step for
   spatial processing. Because the inventory data contains the spatial variables
   (Lat/Lon) it must be joined  with the anomaly data in order to proceed
   with spatial processing. One other requirement is that the stations in the 
   inventory must MATCH those in the anomaly data ( the 2D zoo object).\cr
       Since the programmer may edit or change the stations in the inventory
   ( selecting only rural one for example) and since they may drop stations
   by performing \code{whichStationsCAM} you have to insure that the anomaly
   stations and the inventory stations are identical, and that the right spatial
   information is associated with each series.\cr
      The function \code{intersectInvAnomalies} does that. It reconciles the
   Inventory to the Anomalies, ensuring that both conatin the same stations. 
   
}
\section{V. Spatial Processing}{

  After \code{intersectInvAnomalies} the anomaly data is ready to be projected
  directly into a raster \code{brick}. The function for that is 
  \code{rasterizeAnomaly}.  It takes an inventory with station Id and
   longitude and latitude and a 2D zoo object. The number
  of rows in the inventory data must equal the number of columns in the zoo
  object of anomalies. Transposing the zoo object happens within the
  \code{rasterizeAnomaly} function.
     
  
   \tabular{ll}{
    \code{\link{getLonLat}}\tab get the station Lon/Lat\cr
    \code{\link{rasterizeAnomaly}}\tab put spatial data in a raster\cr
    \code{\link[raster]{area}}\tab area function\cr
    \code{\link[raster]{stackApply}}\tab apply a function to a brick\cr
          
   }
   Once the data is in a raster \code{brick} processing it to obtain
   unweighted averages or area weighted averages is easy. Just use the
   \code{raster} functions. In future releases, demo code will be released
   to illustrate how that is done. Plotting raster data is easy using the standard
   \code{plot} commands. 
   
}

 
\author{Steven Mosher 
 

Maintainer: Steven Mosher <moshersteven@gmail.com>
 
}

 
\references{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README
 
}
 
\keyword{ package }
\keyword{spatial}
 
\examples{
\dontrun{

 # download the files
 files     <- downloadV3(url = V3.MEAN.ADJ.URL)
 # pass the file names to the read functions
 v3Mean    <- readV3Data(filename=files$DataFilename)
 inventory <- readInventory(filename=files$InventoryFile)
 Anomaly   <- createAnomaly(v3Mean)
 Data      <- intersectInvAnomalies(inventory,Anomaly)
 
 
 
}
}