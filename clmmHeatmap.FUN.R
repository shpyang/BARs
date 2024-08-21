



clmmHeatmap=function(data, predictor, outcome, npoints, mapoutput)
{if (missing(npoints)) {npoints=100}
  if (missing(mapoutput)) {mapoutput="Class"}
  data$predictor=data[,predictor]
  data$y=as.factor(data[,outcome])
  data1=data[!is.na(data$y),]  
  
  
  fm1 <- clmm2(y ~ pcycle + predictor +
                 + cfemale + DT_KCAL + age_60 + crace + cintervention + csite   #  +anthro_Wt_BL + r(rand_tx_duration)
               , random=cpart_id, data=data1, Hess = TRUE)
  npred=seq(min(data$predictor, na.rm=TRUE), max(data$predictor, na.rm=TRUE), length.out=npoints)
  
  newdata=data.frame(pcycle=sort(rep(sort(unique(data$pcycle)), npoints)), predictor=rep(npred, length(unique(data$pcycle))))
  
  
  p3=cbind(plogis(fm1$Theta[1] - fm1$beta[which(names(fm1$beta)=="pcycle")]*newdata$pcycle
                  -fm1$beta[which(names(fm1$beta)=="predictor")]*newdata$predictor),
           plogis(fm1$Theta[2] - fm1$beta[which(names(fm1$beta)=="pcycle")]*newdata$pcycle
                  -fm1$beta[which(names(fm1$beta)=="predictor")]*newdata$predictor)-
             plogis(fm1$Theta[1] - fm1$beta[which(names(fm1$beta)=="pcycle")]*newdata$pcycle
                    -fm1$beta[which(names(fm1$beta)=="predictor")]*newdata$predictor),
           plogis(fm1$Theta[3] - fm1$beta[which(names(fm1$beta)=="pcycle")]*newdata$pcycle
                  -fm1$beta[which(names(fm1$beta)=="predictor")]*newdata$predictor)-
             plogis(fm1$Theta[2] - fm1$beta[which(names(fm1$beta)=="pcycle")]*newdata$pcycle
                    -fm1$beta[which(names(fm1$beta)=="predictor")]*newdata$predictor)
  )
  
  prob=data.frame(p3, 1-rowSums(p3))
  
  plot(newdata$pcycle, prob[,1], ylim=c(0, 1), type="p")
  for (k in 2:4)
  { points(newdata$pcycle, prob[,k], col=k, type="p")}
  
  
  ptb=table(data1$y, data1$pcycle)
  
  points(ptb[2,]/colSums(ptb),pch=15, cex=1.5, col="orange")
  points(ptb[2,]/colSums(ptb),pch=5, cex=1, col="cyan")
  
  names(prob)=0:3
  prob$class <- apply(prob, 1, function(x) names(prob)[which.max(x)])
  out=data.frame(newdata,prob)
  
  
  hout=matrix(rep(NA, length(unique(newdata$pcycle))*length(unique(newdata$predictor))), ncol=length(unique(newdata$predictor)))
  
  
  if (mapoutput=="No symptom") {outc="X0"}
  if (mapoutput=="Mild symptom") {outc="X1"}
  if (mapoutput=="Moderate symptom") {outc="X2"}
  if (mapoutput=="Severe symptom") {outc="X3"}
  if (mapoutput=="Class") {outc="class"}
  
  scycle=sort(unique(newdata$pcycle))
  
  for (i in 1:nrow(hout))
  { 
    hout[scycle[i],]=as.vector(as.numeric(out[out$pcycle==scycle[i], which(names(out)==outc)]))
    }
  
  hout=data.frame(hout)
  
  names(hout)=round(out$predictor[1:npoints], 1) 
  row.names(hout)=paste0("Cycle ", 1:length(unique(newdata$pcycle)))
  
  mapoutput2=paste0("(", mapoutput, ")")
  
  if (length(table(unlist(hout)))>1)
    
  {heatmap.2(as.matrix(hout), Rowv = FALSE, Colv = FALSE, dendrogram="none", trace='none', notecol=1,
             keysize = 1.0,         # Adjust the size of the color key
             key.title = NA,        # Remove title of the color key
             key.xlab = "Intensity",margins = c(5, 8),
             xlab=predictor, main=paste(toupper(outcome), mapoutput2, sep="\n"))
    
  } else {print ("ALL VALUES ARE THE SAME!")}
} 


