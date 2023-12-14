B=1000
library("brms")
source("C:/Users/shpry/Desktop/WFH/brown/ploris/R/FUN.numerical integration.R")


n1=22; n2=23;
mT=120; mX=100
sT= 20; sX=30
muT0=110; muX0=110; sig02=1000

dT=rnorm(n1, mT, sT)
dX=rnorm(n2, mX, sX)
 
d=data.frame(y=c(dT, dX), Group=c(rep("A", n1),rep("B", n2)))

mod=brm(
  y~Group, data=d
)
mod



  out.i=FUN.int2(dT, dX, muT0, muX0, sig02, printTF = FALSE, threshold=0)

 out.i 

 