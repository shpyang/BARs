
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
 
 
 
FUN.cr=function(data, kluster, Beta, sdd, contcov, catcov, max.iter, threshold, equal.variance)
{ids=unique(data$id);data$int=1
 data$x2=data$x^2; data$x3=data$x^3
if (missing(contcov)) {contcov=""}
if (missing(catcov)) {catcov=""}
 
# Initialize an empty list to hold interaction terms
interaction_terms <- list()

# Define the main continuous variables
x_vars <- c("x", "x2", "x3")

if (!all(catcov==""))
  # Create dummy variables for each categorical covariate with one less column
{dummy_covs <- lapply(catcov, function(cat) model.matrix(~ data[[cat]])[, -1])
names(dummy_covs) <- catcov
}
# Loop over continuous variables and create interaction terms with each continuous and categorical covariate
for (var in x_vars) {
  
  if (!all(contcov==""))
    # Interaction with each continuous covariate
  {for (cont in contcov) {
    interaction_terms[[paste0(var, "_", cont)]] <- data[[var]] * data[[cont]]
  }}
  
  if (!all(catcov==""))
  {# Interaction with each categorical covariate
    for (cat in catcov) {
      dummy_cat <- as.data.frame(dummy_covs[[cat]])  # Get the dummy variable matrix for this categorical covariate
      for (j in 1:ncol(dummy_cat)) {
        interaction_terms[[paste0(var, "_", cat, "_", colnames(dummy_cat)[j])]] <- data[[var]] * dummy_cat[, j]
      }
    }}
}

if (all(contcov=="")&all(catcov==""))
{X=as.matrix(cbind(int=1, data[,which(names(data)%in%c("x", "x2", "x3"))]))} 
if (all(contcov=="")&(!all(catcov=="")))
{X <- as.matrix(cbind(int=1, data[, c(x_vars, dummy_covs)],
                      do.call(cbind, interaction_terms)))} 
if (all(catcov=="")&(!all(contcov=="")))
                      {X <- as.matrix(cbind(int=1, data[, c(x_vars, contcov)],  
                                            do.call(cbind, interaction_terms)))}
if (!all(catcov=="")&(!all(contcov==""))) 
  {X <- as.matrix(cbind(int=1, data[, c(x_vars, contcov)], 
    do.call(cbind, dummy_covs),
    do.call(cbind, interaction_terms)))
  }

# Create the response vector
y <- data$y
x=X
print(head(x))
nvar=dim(x)[[2]]
nb=nrow(Beta)
print
if (nvar>nb) {B.add=matrix(rep(0, (nvar-nb)*kluster), ncol=kluster)
Beta=rbind(Beta, B.add)}

#x=as.matrix(cbind(int=1, data[,which(names(data)%in%c("int", "x", "x2", "x3", cov))]))
#y=data$y
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
      {dinc=data.frame(x[inc,], y=y[inc])
        slm=summary(lm(y~., data=dinc))[[4]]
        Beta[,k]=slm[,1] #solve(t(x[inc,])%*%x[inc,])%*%(t(x[inc,])%*%y[inc])
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