
library('dplyr'); library('tidyr')

FUN.Beta.est=function(data, kluster)
{ outx = as.data.frame(data) %>% 
  pivot_wider(names_from = x, values_from = y)

  row.names(outx)=outx$id
  touts=as.data.frame(outx[,-1])  
  kmeans_result=kmeans(touts, center=kluster)
  data$kmean_cluster=rep(as.vector(kmeans_result$cluster), each=length(unique(data$x)))
  data$x2=data$x^2; data$x3=data$x^3
  
  for (k in 1:kluster)
  {data.i=data[data$kmean_cluster==k,]
   lm=lm(y~x+x2+x3, data=data.i)
   outlm=summary(lm)[[4]][,1]
   if (k==1) {Beta=outlm} else {Beta=cbind(Beta, outlm)}
  }
return(list(kmean_cluster=data$kmean_cluster, Beta=Beta))
}
 
 
 
FUN.cr=function(data, kluster, Beta, sdd, max.iter, threshold, equal.variance)
{ids=unique(data$id); data$x2=data$x^2; data$x3=data$x^3
x=as.matrix(cbind(int=1, data[,which(names(data)%in%c("int", "x", "x2", "x3"))]))
y=data$y
pii=pii0=rep(1, kluster); if (missing(threshold)) {threshold=1e-09}
 if (missing(equal.variance)) {equal.variance=TRUE}
 T = matrix(NA, nrow(data), kluster);  inc=T[,1]
 if (missing(sdd)) {sdd=rep(100, kluster)}
 if (missing(Beta)) {print("Please run the FUN.Beta.est function to get some initial values of Beta"); break}
 if (missing(max.iter)) {max.iter=30}
  for (iter in 1:max.iter)
  {T=pii*dnorm(y, mean=x%*%Beta, sd=sdd); T=T/rowSums(T); pii=colSums(T)/nrow(data)
    for (k in kluster:1)
    {for (w in 1:length(ids)) 
    {inc[data$id==ids[w]]=sum(T[data$id==ids[w],k])==max(colSums(T[data$id==ids[w],]))}
      if (sum(is.na(inc))==0) 
      {Beta[,k]=solve(t(x[inc,])%*%x[inc,])%*%(t(x[inc,])%*%y[inc])
      sdd[k]=sd(y[inc]-x[inc,]%*%Beta[,k])
      }
    }
    if (equal.variance) {sdd=rep(mean(sdd, na.rm=TRUE), kluster)}
 
  likelihoods <- rowSums(T * pii)
  log_likelihood <- sum(log(likelihoods), na.rm = TRUE)
  
  if (iter>1&(all(abs(pii-pii0)<threshold))) {break}
  pii0=pii
  }  
 print(paste0("STOPPED at iteration ", iter, " ."))
return(list(ids=ids, T=T, llk=log_likelihood))
}