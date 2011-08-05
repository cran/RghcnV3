##################################################
#  Demo script to download land masks
#  written by Steven Mosher
#  license GPL 2
#  Date: July 2 2001
#################################################
# first we check to see if the directory
#  has been created, and if not then we create it
#  and download the files.
#  if the directory has been created previously
#  we need to check for the presence of the mask files
#  if ocean mask is missing, we download both
#
#################################################

require("maps")

if (!file.exists(GHCN.V3.DATA)) {
  dir.create(GHCN.V3.DATA)
  oMaskFile <- downloadMask(url = OCEAN.MASK.URL)
   
} else {
 ### check to see what files exists using a utility function
demoFiles <- getDemoFiles()
if (is.null(demoFiles$OCEANMASKS)){
   oMaskFile <- downloadMask(url = OCEAN.MASK.URL)  
} else {
  oMaskFile <- demoFiles$OCEANMASKS
}
}
  
 
### We start by loading the mask data into rasters
#  we plot them and add a map outline
myCols <- c("blue","blue","blue","green","green", "green")

oceanRaster  <- readMask(filename = oMaskFile)
plot(oceanRaster, main = "1/4 degree Ocean/Land mask")
map("world", add = TRUE)
 
##################################
#
# next we will show a few raster functions used to create
# various masks 
# find the resolution of the raster
maskResolution <- res(oceanRaster)
if (maskResolution[1] != maskResolution[2]) stop( "non square  grids") 
targetRes <- 1
## since its square we can just use the xresolution
multiplier <- floor(targetRes/xres(oceanRaster))

ocean1x1 <- aggregate(oceanRaster, fact = multiplier)
plot(ocean1x1, main = " 1 degree ocean/land mask", col = myCols)
map("world", add = TRUE)

##########  testing other raster functions

cuba <- extent(-88, -65, 16, 30)
Cuba <- crop(oceanRaster, cuba)
plot(Cuba,main = "Cuba")
map("world", add = TRUE)

cubaValues <- values(Cuba)
 

#############  make and save a 3 degree and 5 degree binary mask

targetRes <- 3
multiplier <- floor(targetRes/xres(oceanRaster))
ocean3x3   <- aggregate(oceanRaster, fact = multiplier)
plot( ocean3x3)
binary3x3  <- reclass(ocean3x3, rcl = c(.5,1,1))
binary3x3  <- reclass(binary3x3, rcl = c(0,.5, 0))
plot(binary3x3, main = "binary 3 degree ocean/land", col=myCols)
fname <- file.path( getwd(),
                    GHCN.V3.DATA,
                    "BinaryMask3x3.grd",
                    fsep = .Platform$file.sep)

writeRaster(binary3x3,filename = fname, datatype = 'INT4S', overwrite = TRUE)

targetRes <- 5
multiplier <- floor(targetRes/xres(oceanRaster))
ocean5x5   <- aggregate(oceanRaster, fact = multiplier)
plot( ocean5x5)
binary5x5  <- reclass(ocean5x5, rcl = c(.5,1,1))
binary5x5  <- reclass(binary5x5, rcl = c(0,.5, 0))
plot(binary5x5, main = "binary 5 degree ocean/land", col=myCols)
fname <- file.path( getwd(),
                    GHCN.V3.DATA,
                    "BinaryMask5x5.grd",
                    fsep = .Platform$file.sep)

writeRaster(binary5x5,filename = fname, datatype = 'INT4S', overwrite = TRUE)
