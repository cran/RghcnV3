##################################################
#  Demo script to download land masks
#  written by Steven Mosher
#  licence GPL 2
#################################################

ocean <- downloadMask(url=OCEAN.MASK.URL)
water <- downloadMask(url=WATER.MASK.URL)
Oraster <- readMask(filename=ocean)
plot(Oraster)
Wraster <- readMask(filename=water)
plot(Wraster)
mask5X5 <-aggregate(Wraster, fact=20)
plot(mask5X5)
##################################