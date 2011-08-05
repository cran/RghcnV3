####################################
#  test of New data structures
#
#
#
####################################

 TEXAS.DAT  <- system.file("external/Texas.dat", package = "RghcnV3")
 mts       <- readV3Data(filename=TEXAS.DAT, output = "Mts")
 Zoo       <- readV3Data(filename=TEXAS.DAT, output = "Zoo")
 Array     <- readV3Data(filename=TEXAS.DAT, output = "Array")
 i          <- system.file("external/Texas.inv", package = "RghcnV3")
 inv        <-readInventory(i)
 mts        <- window(mts,start =1900, end = 2010 +(11/12))
 Zoo        <- window(Zoo,start =1900, end = 2010 +(11/12))
 Array      <- windowArray(Array,start =1900, end = 2010) 

 T1      <-  averageStations(mts)
 T2      <-  averageStations(asMts(Zoo))
 T3      <- averageStations(asMts(Array))
 
 # offset the colors to see
 plot(window(T1$Average, start =1900, end =1940))
lines(window(T2$Average, start =1900, end =1940)+.5,col="red")
lines(window(T3$Average, start =1900, end =1940)-.5,col="blue")
