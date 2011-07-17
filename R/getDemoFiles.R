getDemoFiles <- function(directory = GHCN.V3.DATA){
   if (!file.exists(directory)) stop(cat(directory, " does not exists","\n"))
   pat  <- "(\\.qca\\.dat)$|(\\.qca\\.inv)$|(HadSST2_1850on\\.nc)|(land_percent2_qd\\.asc)$"
     
   allFiles <- listDirectory(directory,
                            fullNames = TRUE,
                            recursive = TRUE,
                            pattern = pat)
   
   oMasks     <-  grep("(land_percent2)[[:print:]].*(\\.asc)$", allFiles)
   if (length(oMasks) > 0 ) oMaskFiles <-  allFiles[oMasks] else oMaskFiles <- NULL   
   sst       <-  grep("(HadSST2_1850on\\.nc)$", allFiles)
   if (length(sst) > 0 )  sstFiles   <-  allFiles[sst] else sstFiles <- NULL
     
   v3Dat    <-  grep("(ghcnm\\.v3).*(tavg).*(\\.dat)$", allFiles)
    if (length(v3Dat) > 0){
     DataFile  <-   allFiles[v3Dat[1] ]
      
     } else DataFile  <- NULL
   v3Inv    <-  grep("(ghcnm\\.v3).*(tavg).*(\\.inv)$", allFiles)
    if (length(v3Inv) > 0){
     InvFile  <-   allFiles[v3Inv[1] ]
      
     } else InvFile  <- NULL
    GHCNFILES <- list(OCEANMASKS = oMaskFiles,
                       
                      SST = sstFiles,
                      Data = DataFile,
                      Inv = InvFile)
    return(GHCNFILES)        
}