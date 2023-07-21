
i=1; kluster=8; sdd=rep(100, kluster); pii=rep(1, kluster); pii0=pii*99
n.n=nrow(data); inc=rep(NA,n.n);
pvar=matrix(rep(0, 3^2), 3); diag(pvar)=100
y=data$y
x=cbind(int=1,data[,which(names(data)%in%c("x", "x2"))])

cbeta=.5*(1:kluster)+mean(y)
cbeta=seq(min(y), max(y), length.out=kluster)
pBeta=Beta=t(cbind(cbeta, seq(0, -.00000000001, length.out=kluster), seq(0, -.000000000001, length.out=kluster)))

T=matrix(rep(NA,n.n*kluster),ncol=kluster)

for (i in 1:100)
{
  for (j in 1:kluster)
  {if (i!=1&j==2) {sdd[j]=sdd[j]/sqrt(2)}
    T[,j]=pii[j]*(dnorm((y-as.matrix(x)%*%as.matrix(Beta[,j])),0,sdd[j]))
  }

  T[rowSums(T)!=0,]=T[rowSums(T)!=0,]/rowSums(T[rowSums(T)!=0,])
  pii=colSums(T)/n.n
  print(sum(pii-pii0))
  if ( sum(pii-pii0) ==0 &i>3) {break}

  pii0=pii
  kluster0=kluster
  for (j in kluster:1)
  {
    for (w in 1:n.n) {inc[w]=T[w,j]==max(T[w,])}

    if (sum(inc)==1)
    {Beta[,j]=pBeta[,j]} else
    {Beta[,j]= solve((pvar)+as.matrix(t(x[inc,])) %*% as.matrix(x[inc,]))%*%((pvar)%*%pBeta[,j]   + as.matrix(t(x[inc,])) %*% as.matrix(y[inc]))

ww=rep(NA, n.n)
    if (i>5)
    {for (h in 1:n.n) {ww[h]=which(T[h,]==max(T[h,]))[1]}
      xx=x
      xx$ww=ww; xx$y=y
      lm=lm(y~x+x2+as.factor(ww), data=xx)
      sm=summary(lm)[[4]]
      Beta[2,]=sm[2,1]; Beta[3,]=sm[3,1]
      Beta[1,1]=sm[1,1]
        tbww=table(ww)
     if (nrow(sm)==(kluster+2))
      {Beta[1,2:kluster]=sm[,1][-c(1:3)]+sm[1,1]} else
      {if (j==kluster0)
      {kluster=kluster-1; grps=substr(row.names(sm),14,15)
      grps=grps[grps!=""]; grps=c(1,as.numeric(grps))
      pii=pii[grps]#sm[,1][-c(1:3)]+sm[1,1]
      Beta=Beta[,grps]
      T=T[,grps]}
        print(i)
        print(kluster)
        print(Beta[1, 2:kluster]); print(sm[, 1][-c(1:3)])
        Beta[1,2:kluster]=sm[,1][-c(1:3)]+sm[1,1]
      }

    }
  }
  }

  for (j in kluster:1)
  {
    for (w in 1:n.n) {inc[w]=T[w,j]==max(T[w,])}

    #if (sum(inc)==1)
    #{sdd[j]=0.0001} else
    {sdd[j]= apply((y[inc]-as.matrix((x[inc,]))%*%Beta[,j]), 2, sd)
    }
  }
  sdd=rep(mean(sdd[!is.na(sdd)&sdd!=0]), kluster)

  if (i<5)
  {Beta[2,]=mean(Beta[2,])#+Beta[2,]*pii#*pii
  Beta[3,]=mean(Beta[3,])#+Beta[3,]*pii#*pii
  }

}


plot(data$x, data$y)
for (j in 1:kluster) {
  points(data$x, Beta[1, j] + Beta[2, j] * data$x + Beta[3, j] * data$x2, cex = 0.6, col = j + 1)
}

T.max=ccol=rep(NA,n.n)

for (w in 1:n.n)
{
  ccol[w]=(which(T[w,]==max(T[w,])))+1
  T.max[w]=max(T[w,])
}


plot(data$x, data$y, col=ccol)


