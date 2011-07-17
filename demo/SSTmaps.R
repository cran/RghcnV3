require("maps")

if(!file.exists(GHCN.V3.DATA)){
   cat(GHCN.V3.DATA, "Doesnt Exist.. creating","\n")
   print("downloading SST.. this will take a while")
   dir.create(GHCN.V3.DATA)
   downloadSST(url = HADSST2.URL,directory = GHCN.V3.DATA)
}

targetDir <- file.path(getwd(), GHCN.V3.DATA, fsep = .Platform$file.sep)
files     <- list.files(targetDir, full.names = TRUE, pattern = "(1850on.nc)$"  )
if(length(files ) == 0)stop("HadSST2_1850on.nc is not found")
SST       <- readSST(files)
startYear <- 1900
dl        <- which(layerNames(SST) == as.character(startYear))
SST       <- dropLayer(SST,1:(dl-1))
endYear   <- 2010
Years     <- endYear + 1 - startYear 
Months    <- Years * 12
SST       <- dropLayer(SST,(Months + 1):nlayers(SST))

### Now we will just average the months of the year
### Note this takes no account of the area of grid cells
### This is merely to show the function stackApply
### we create an index for the years 1:nlayers long
### all months in 1900 map to 1
### all months in 1901 map to 2.. and so forth
### stackApply, works like apply

yearIndex   <- floor(as.numeric(layerNames(SST))) - (startYear - 1)
annualSST   <- stackApply(SST,
                     indices = yearIndex  ,
                     fun = mean, na.rm = TRUE)

layerNames(annualSST)  <- as.character(startYear:endYear)

layer2000              <- which(layerNames(annualSST) == "2000")

displayLayer  <- raster(annualSST,layer = layer2000)

plot(displayLayer, main = "2000")
map("world", add = TRUE)