B=1000
#https://richarddmorey.github.io/BayesFactor/
#https://vasishth.github.io/bayescogsci/book/hypothesis-testing-using-the-bayes-factor.html


FUN.int=function(n1, n2, mT, sT, mX, sX, muT0, muX0, sig02, printTF)
{if (missing(printTF)) {printTF=TRUE}
#n1=n2=22; mX=25; mT=30; sX=9; sT=10
#muX0=26; muT0=30; sig02=sX^2  *1000

integrand = function(x, a, b)
{pnorm(x, a, b)*dnorm(x)}

#mT0=mT; sT0=sT;
T=rnorm(n1, mT, sT)
X=rnorm(n2, mX, sX)
tpp=round(t.test(T, X, alternative="greater")$p.value, 3)

ybarT=mean(T); sigT2=sd(T)^2
ybarX=mean(X); sigX2=sd(X)^2

meanT=(n1*ybarT/sigT2+muT0/sig02)/(n1/sigT2+1/sig02)
sdT=sqrt(1/(n1/sigT2+1/sig02))
meanX=(n2*ybarX/sigX2+muX0/sig02)/(n2/sigX2+1/sig02)
sdX=sqrt(1/(n2/sigX2+1/sig02))

#print(meanT); print(ybarT); print(sqrt(sigT2/n1)); print(sdT)
#print(meanX); print(ybarX); print(sqrt(sigX2/n2)); print(sdX)

if (printTF) {print(paste0("P for a one-sided t test is ", tpp))}
a=(meanT-meanX)/sdX
b=sdT/sdX
out=list(integrate(integrand, lower=-Inf, upper=Inf, a, b)$value, tpp)
names(out)[[1]]="The posterior probabilith that X is great than T (%)"
names(out[[2]])="The one-sided t test p value"
return(out)
}




FUN.int2=function(dT, dX, muT0, muX0, sig02, printTF)
{if (missing(printTF)) {printTF=TRUE}
  #n1=n2=22; mX=25; mT=30; sX=9; sT=10
  #muX0=26; muT0=30; sig02=sX^2  *1000

  integrand = function(x, a, b)
  {pnorm(x, a, b)*dnorm(x)}

  #mT0=mT; sT0=sT;
  T=dT; X=dX
  n1=length(dT); n2=length(dX)
  tpp=round(t.test(T, X, alternative="greater")$p.value, 3)

  ybarT=mean(T); sigT2=sd(T)^2
  ybarX=mean(X); sigX2=sd(X)^2

  meanT=(n1*ybarT/sigT2+muT0/sig02)/(n1/sigT2+1/sig02)
  sdT=sqrt(1/(n1/sigT2+1/sig02))
  meanX=(n2*ybarX/sigX2+muX0/sig02)/(n2/sigX2+1/sig02)
  sdX=sqrt(1/(n2/sigX2+1/sig02))

  #print(meanT); print(ybarT); print(sqrt(sigT2/n1)); print(sdT)
  #print(meanX); print(ybarX); print(sqrt(sigX2/n2)); print(sdX)

  if (printTF) {print(paste0("P for a one-sided t test is ", tpp))}
  a=(meanT-meanX)/sdX
  b=sdT/sdX
  out=list(integrate(integrand, lower=-Inf, upper=Inf, a, b)$value, tpp)
  names(out)[[1]]="The posterior probabilith that X is great than T (%)"
  names(out[[2]])="The one-sided t test p value"
  return(out)
}



