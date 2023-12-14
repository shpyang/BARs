B=1000
#https://richarddmorey.github.io/BayesFactor/
#https://vasishth.github.io/bayescogsci/book/hypothesis-testing-using-the-bayes-factor.html


FUN.int=function(n1, n2, mT, sT, mX, sX, muT0, muX0, sig02, printTF, threshold)
{if (missing(printTF)) {printTF=TRUE}
  if (missing(threshold)) {threshold=0} 
  
  integrand = function(x, a, b)   {pnorm(x, a, b)*dnorm(x)}

#mT0=mT; sT0=sT;
T=rnorm(n1, mT, sT)
X=rnorm(n2, mX, sX) 
if (mean(T)>mean(X)) {T=T-abs(threshold)}
if (mean(T)<=mean(X)) {T=T+abs(threshold)}

tpp=round(t.test(T, X, alternative="greater")$p.value, 3)

ybarT=mean(T); sigT2=sd(T)^2
ybarX=mean(X); sigX2=sd(X)^2

meanT=(n1*ybarT/sigT2+muT0/sig02)/(n1/sigT2+1/sig02)
sdT=sqrt(1/(n1/sigT2+1/sig02))
meanX=(n2*ybarX/sigX2+muX0/sig02)/(n2/sigX2+1/sig02)
sdX=sqrt(1/(n2/sigX2+1/sig02))

if (printTF) {print(paste0("P for a one-sided t test is ", tpp))}
a=(meanT-meanX)/sdX
b=sdT/sdX
out=list(integrate(integrand, lower=-Inf, upper=Inf, a, b)$value, tpp)
names(out)[[1]]="The posterior probabilith that X is great than T (%)"
names(out[[2]])="The one-sided t test p value"
return(out)
}




FUN.int2=function(dT, dX, muT0, muX0, sig02, printTF, threshold)
{if (missing(printTF)) {printTF=TRUE}
  if (missing(threshold)) {threshold=0} 

  integrand = function(x, a, b)   {pnorm(x, a, b)*dnorm(x)}
 
  T=dT; X=dX
  if (mean(T)>mean(X)) {T=T-abs(threshold)}
  if (mean(T)<=mean(X)) {T=T+abs(threshold)}
  
  n1=length(dT); n2=length(dX)
  tpp=round(t.test(T, X, alternative="greater")$p.value, 3)

  ybarT=mean(T); sigT2=sd(T)^2
  ybarX=mean(X); sigX2=sd(X)^2

  meanT=(n1*ybarT/sigT2+muT0/sig02)/(n1/sigT2+1/sig02)
  sdT=sqrt(1/(n1/sigT2+1/sig02))
  meanX=(n2*ybarX/sigX2+muX0/sig02)/(n2/sigX2+1/sig02)
  sdX=sqrt(1/(n2/sigX2+1/sig02)) 

  if (printTF) {print(paste0("P for a one-sided t test is ", tpp))}
  a=(meanT-meanX)/sdX
  b=sdT/sdX
  out=list(integrate(integrand, lower=-Inf, upper=Inf, a, b)$value, tpp)
  names(out)[[1]]="The posterior probabilith that X is great than T (%)"
  names(out[[2]])="The one-sided t test p value"
  return(out)
}



