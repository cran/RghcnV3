###############
#  create directory for data downloads and download the data
#################

######################################################################
# Downloads all the files required for demo programs and others bit
# 
# Author: Steven Mosher
# License:  GPL
# Date July 2, 2011
#
#
#
#
#####################################################################

#### check if the directory for demos has already been created.
#### if it has stop. otherwise create the directory and download the files
#### 
if(file.exists(GHCN.V3.DATA))stop(cat(GHCN.V3.DATA, " Already Exists ","\n"))
dir.create(GHCN.V3.DATA)
# download the files and save the file names to the variable

meanAdjFiles    <- downloadV3(url = V3.MEAN.ADJ.URL, directory = GHCN.V3.DATA)
meanRawFiles    <- downloadV3(url = V3.MEAN.RAW.URL, directory = GHCN.V3.DATA)
maxAdjFiles     <- downloadV3(url = V3.TMAX.ADJ.URL, directory = GHCN.V3.DATA)
maxRawFiles     <- downloadV3(url = V3.TMAX.RAW.URL, directory = GHCN.V3.DATA)
minAdjFiles     <- downloadV3(url = V3.TMIN.ADJ.URL, directory = GHCN.V3.DATA)
minRawFiles     <- downloadV3(url = V3.TMIN.RAW.URL, directory = GHCN.V3.DATA)
oMaskFile       <- downloadMask(url = OCEAN.MASK.URL,directory = GHCN.V3.DATA)
wMaskFile       <- downloadMask(url = WATER.MASK.URL,directory = GHCN.V3.DATA)
hadSSTFile      <- downloadSST(url = HADSST2.URL, directory = GHCN.V3.DATA)
######## Get the results from Hadley/CRU
CRUmapsFile     <- downloadHadMaps(url = CRUTEMP3.MAPS.URL, directory = GHCN.V3.DATA )
HADCRUTmapsFile <- downloadHadMaps(url = HADCRUT3.MAPS.URL, directory = GHCN.V3.DATA )
 
