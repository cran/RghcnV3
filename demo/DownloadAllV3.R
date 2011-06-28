###############
#  create directory for data downloads and download the data
#################

if(file.exists(GHCN.V3.DATA))stop(cat(GHCN.V3.DATA, " Already Exists ","\n"))
dir.create(GHCN.V3.DATA)
meanAdj <-downloadV3(url=V3.MEAN.ADJ.URL,directory=GHCN.V3.DATA)
meanRaw <-downloadV3(url=V3.MEAN.RAW.URL,directory=GHCN.V3.DATA)
maxAdj <-downloadV3(url=V3.TMAX.ADJ.URL,directory=GHCN.V3.DATA)
maxRaw <-downloadV3(url=V3.TMAX.RAW.URL,directory=GHCN.V3.DATA)
minAdj <-downloadV3(url=V3.TMIN.ADJ.URL,directory=GHCN.V3.DATA)
minRaw <-downloadV3(url=V3.TMIN.RAW.URL,directory=GHCN.V3.DATA)
ocean <- downloadMask(url=OCEAN.MASK.URL,directory=GHCN.V3.DATA)
water <- downloadMask(url=WATER.MASK.URL,directory=GHCN.V3.DATA)
hadSST <-downloadSST(url=HADSST2.URL,directory=GHCN.V3.DATA)