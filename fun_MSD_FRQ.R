
msd=function(data, var, rounding)
{if (missing(rounding)) {rounding=1}
  data.i=data[,which(names(data)%in%c("treatment", var))]
  mean.all=round(mean(data.i[,var]),rounding)  
  sd.all=round(sd(data.i[,var]),rounding)  
  msd.all=paste0(mean.all, "(", sd.all, ")")
  
  mean.sub=aggregate(data.i[,var], by=list(data.i$treatment), FUN=mean)
  sd.sub=aggregate(data.i[,var], by=list(data.i$treatment), FUN=sd)
  mean.sub$x=round(mean.sub$x, rounding)
  sd.sub$x=round(sd.sub$x, rounding)
  t.tp=round(t.test(data.i[,var]~data.i$treatment)$p.value, 3)
  out.i=t(c(var, msd.all, paste0(mean.sub$x, "(", sd.sub$x, ")"), t.tp))
  out.i=as.data.frame(out.i)
  nn=paste0(mean.sub$Group.1, " (n=",table(data.i$treatment), ")")
  names(out.i)=c("Variable", "All", nn, "P")
  
  return(out.i)
}

 
 

frq=function(data, var, rounding)
{if (missing(rounding)) {rounding=1}
  data.i=data[,which(names(data)%in%c("treatment", var))]
  
  dtb.all=table(data.i[,var])
  dout.all=paste0(dtb.all, "(", round(dtb.all/sum(dtb.all)*100,1), ")")
  
  dtb=table(data.i[,var], data.i$treatment)  
  ddtb=as.data.frame(matrix(paste0(dtb, "(", round(dtb/colSums(dtb)*100,1), ")"), 2) )
  names(ddtb)=paste0(colnames(dtb), " (n=", colSums(dtb), ")")
  
  c.p=round(chisq.test(data.i$treatment, data.i[,var])$p.value, 3)
  print(ddtb)
  out.i=data.frame(Variable=names(dtb.all), All=dout.all, ddtb, P="")  
  names(out.i)[3:4]=names(ddtb)
  out.isum=out.i[1,]
  out.isum=c(var, "", "", "", c.p)
  out.i=rbind(out.isum, out.i)
  return(out.i)  
}