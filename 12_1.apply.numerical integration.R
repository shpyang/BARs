B=1000

source("C:/Users/shpry/Desktop/WFH/brown/ploris/R/FUN.numerical integration.R")


n1=22; n2=23;
mT=110; mX=110
sT= 20; sX=30
muT0=110; muX0=110; sig02=1000


dT=rnorm(n1, mT, sT)
dX=rnorm(n2, mX, sX)
 
  out.i=FUN.int2(dT, dX, muT0, muX0, sig02, printTF = FALSE, threshold=10)

 out.i 

 