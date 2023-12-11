B=1000
#https://richarddmorey.github.io/BayesFactor/
#https://vasishth.github.io/bayescogsci/book/hypothesis-testing-using-the-bayes-factor.html


FUN.int=function(n1, n2, mT, sT, mX, sX, muT0, muX0, sig02, printTF)
{if (missing(printTF)) {printTF=TRUE}
#n1=n2=22; mX=25; mT=30; sX=9; sT=10
#muX0=26; muT0=30; sig02=sX^2  *1000

integrand = function(x, a, b)
{pnorm(x, a, b)*dnorm(x)}

mT0=mT; sT0=sT;
T=rnorm(n1, mT, sT)
X=rnorm(n2, mX, sX)
t.test(T, X, alternative = "greater")
ybarT=mean(T); sigT2=sd(T)^2
ybarX=mean(X); sigX2=sd(X)^2

meanT=(n2*ybarT/sigT2+muT0/sig02)/(n2/sigT2+1/sig02)
sdT=sqrt(1/(n2/sigT2+1/sig02))
meanX=(n1*ybarX/sigX2+muX0/sig02)/(n1/sigX2+1/sig02)
sdX=sqrt(1/(n1/sigX2+1/sig02))

#print(meanT); print(ybarT); print(sqrt(sigT2/n1)); print(sdT)
#print(meanX); print(ybarX); print(sqrt(sigX2/n2)); print(sdX)

if (printTF) {print(paste0("P for a one-sided t test is ", round(t.test(T, X, alternative="greater")$p.value, 3)))}
a=(meanT-meanX)/sdX
b=sdT/sdX
return(list(integrate(integrand, lower=-Inf, upper=Inf, a, b)$value, round(t.test(T, X, alternative="greater")$p.value, 3)))
}



#a0=(mT-mX)/sX
#b0=sT/sX
#integrate(integrand, lower=-Inf, upper=Inf, a0, b0)$value


#FUN.int(n1=33, n2=33, mT=30, sT=9, mX=25, sX=9, muT0=30, muX0=25, sig02=1000)




