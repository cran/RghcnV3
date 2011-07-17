\name{FILE.PARAMETERS}
\alias{FILE.PARAMETERS}
\docType{data}
\title{A dataset containing various constants required to read data
 
}
\description{The data files and inventory files of GHCN V3
  differ from previous versions. Each dataset now has different
  columns and different column descriptions. In order to
  read in these datafiles certian variables such as column widths
  and column names must be set. Those parameters are kept in
  a list that is loaded when the package loads.
 
}
\usage{FILE.PARAMETERS}
\format{
  The format is:
List of 9\cr
 $ InvWidths  : num [1:16] 11 9 10 7 31 5 1 5 2 2 ...\cr
 $ InvNames   : chr [1:16] "Id" "Lat" "Lon" "Elevation" ...\cr
 $ DataWidths : num [1:51] 11 4 4 5 1 1 1 5 1 1 ...\cr
 $ DataNames  : chr [1:51] "Id" "Year" "Element" "Jan" ...\cr
 $ DataColumns: chr [1:14] "Id" "Year" "Jan" "Feb" ...\cr
 $ QCColumns  : chr [1:38] "Id" "Year" "DMF1" "DMF2" ...\cr
 $ DMFlags    : chr [1:14] "Id" "Year" "DMF1" "DMF2" ...\cr
 $ QCFlags    : chr [1:14] "Id" "Year" "QCF1" "QCF2" ...\cr
 $ DSFlags    : chr [1:14] "Id" "Year" "DSF1" "DSF2" ...\cr
}
\details{the various elements of the list are used when reading in the files.
  "widths" and "names" are provided for reading in the temperature data and
  the inventory data. In addition, with this release of GHCN NCDC has supplied QC
  flags and source flags for the temperature elements. Data columns can be 
  extracted by using the \code{$DataColumns} vector, and QC flags can be 
  extracted by using vectors as well.  
 
}
\source{\url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/README}
  This readme contains all the information you need to understand the files
  The format of the files and the description of the various flags is contained
  there. The names for Inventory columns has been changed slightly to 
  be more expressive. 
 
}
 
\examples{
data(FILE.PARAMETERS)
str(FILE.PARAMETERS) 
}
\keyword{datasets}
