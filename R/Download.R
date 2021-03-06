GHCN.V3.DATA <- "GhcnV3Data"

downloadSST <- function(url = HADSST2.URL, directory = getwd(), overwrite = FALSE, remove = FALSE){
  require("ncdf")
  if (!file.exists(directory))dir.create(directory)
  dest <- file.path(directory, basename(url), fsep=.Platform$file.sep)
  download.file(url, dest, mode = "wb")
	gunzip(dest, overwrite = overwrite, remove = remove)
  fname <- sub(".gz", "", dest, fixed = TRUE)
  return(fname)
}



downloadMask <- function(url = OCEAN.MASK.URL, directory = getwd()){
  if (!file.exists(directory))dir.create(directory)
  dest <- file.path(directory, basename(url), fsep = .Platform$file.sep)
  download.file(url = url, dest, mode = "wb")
  return(dest) 
}


downloadV3 <- function(url = V3.MEAN.ADJ.URL, directory = getwd(), overwrite = TRUE, remove = FALSE) {
   
   if (!file.exists(directory)) dir.create(directory)
   fullDestination <- file.path(directory, basename(url), fsep = .Platform$file.sep)
   download.file(url, destfile = fullDestination, mode = "wb")
   gunzip(fullDestination, overwrite = overwrite, remove = remove)
   # replace  .gz  with nothing for untar
   tarName <- sub(".gz", "", fullDestination, fixed = TRUE)
   theseFiles <- untar(tarName, list = TRUE, tar = "internal")
   
   untar(tarName, exdir = directory, tar = "internal")
   theseFiles <- gsub("./", "", theseFiles, fixed = TRUE)
   theseFiles <- file.path(directory, theseFiles, fsep =.Platform$file.sep)
   localDir   <- dirname(theseFiles)
   localDir   <- localDir[1] 
   dateString <- substring(localDir, first = nchar(localDir) - 7)
     
   data       <- grep(x = theseFiles, pattern = ".dat", fixed = TRUE)
   inv        <- grep(x = theseFiles, pattern = ".inv", fixed = TRUE)
    
   
   return(list(DataFilename  = theseFiles[data],
               InventoryFile = theseFiles[inv],
               Date          = strptime(dateString, format = "%Y%m%d")
               )
          )
}

downloadHadMaps <- function(url = c(CRUTEMP3.MAPS.URL, HADCRUT3.MAPS.URL), directory = getwd()){
  require("ncdf")
  if (length(url) > 1){
       cat("PASS one url ONLY", url,"\n")
       warning(cat(" using the first element", url[1],"\n"))
    }
  if (!file.exists(directory))dir.create(directory)
  dest <-file.path(directory,basename(url[1]), fsep = .Platform$file.sep)
  download.file(url[1], dest, mode = "wb")   
  return(dest) 
}

downloadCRU <- function(url = CRU.STATIONS.URL,directory = getwd()){
  if (!file.exists(directory)) dir.create(directory)
  fullDestination <- file.path(directory, basename(url), fsep = .Platform$file.sep)
  fullDestination <- file.path(directory, basename(url), fsep = .Platform$file.sep)
  download.file(url, destfile = fullDestination, mode = "wb")
  unzip(fullDestination)
  fullDestination <- sub(".zip",".txt",fullDestination)
return(fullDestination)
  
}
