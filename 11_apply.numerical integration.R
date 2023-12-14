B=1000
#https://richarddmorey.github.io/BayesFactor/
#https://vasishth.github.io/bayescogsci/book/hypothesis-testing-using-the-bayes-factor.html
source("H:/WFH/brown/POLARIS/R/FUN.numerical integration.R")
int.p=t.p=t2.p=rep(NA, B)

n1=22; n2=23;
mT=110; mX=110
sT= 20; sX=30
muT0=110; muX0=110; sig02=1000

for (i in 1:B)
{

  out.i=FUN.int(n1, n2, mT, sT, mX, sX, muT0, muX0, sig02, printTF = FALSE)

t.p[i]  =out.i[[2]]
int.p[i]=out.i[[1]]

}


mean(t.p); mean(int.p)

