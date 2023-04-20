
library(lmerTest)
require(lme4)
require(haven)
library(emmeans)
 

FUN.BA=function(data, outcome.name, outcome.before, outcome.after, id.name, trt.name, trt.levels, covariates)
{ 
data$outcome.before=data[,outcome.before]
data$outcome.after =data[,outcome.after] 
data$id=data[,id.name]

data$trt=factor(data[,trt.name], levels = trt.levels)

ms=aggregate(data$outcome.before, list(data$trt), FUN="mean", na.rm=TRUE)
sds=aggregate(data$outcome.before, list(data$trt), FUN="sd", na.rm=TRUE)

mnout=paste0(round(ms[,2],2), "(", round(sds[,2],2), ")")
mnout

dataa=data
dataa=data[,c("id", covariates, outcome.before, "trt")]
datab=data[,c("id", covariates, outcome.after, "trt")]

dataa$y=data$outcome.before
datab$y=data$outcome.after
dataa$time=0
datab$time=1
base=dataa
dataa=dataa[,-which(names(dataa)%in%c(outcome.before))]
datab=datab[,-which(names(datab)%in%c(outcome.after))]
base$base=base[,outcome.before]
base=base[,c("id", "base")]

mg2=rbind(dataa, datab)
mg3=merge(mg2, base, by="id")
 
covariates_formula <- paste(covariates, collapse="+")
modf=formula(paste0(" y ~ trt + time + trt:time + (1| id)+", covariates_formula))
m3x <- lmer(modf, mg3) 
#m3x <- lmer(y ~ trt + time + trt:time + (1| part_id) +base, mg3)
mout=summary(m3x)
anova(m3x)
#emmeans(m3x, ~trt)
#emmeans(m3x, ~trt:time)
 
diff <- contrast(emmeans(m3x, ~ trt:time),ref = trt.levels[1],method = "revpairwise")
sdiff=summary(diff)

mdiff=as.data.frame(rbind(sdiff[sdiff$contrast==paste0(trt.levels[1], " time1 - ", trt.levels[1], " time0"),],
      sdiff[sdiff$contrast==paste0(trt.levels[2], " time1 - ", trt.levels[2], " time0"),]))
mdout=paste0(round(mdiff[,2],2), " (", round(mdiff[,3],2), ")")

ddifft=t(as.matrix(mout[[10]][row.names(mout[[10]])==paste0("trt", trt.levels[2], ":time"),] ))
ddiff=rbind(t(c("0.00-Reference", "")), c(paste0(round(ddifft[1,1],3), "(", round((ddifft[1,1]-1.96*ddifft[1,2]),3), ", ",
                                               round((ddifft[1,1]+1.96*ddifft[1,2]),3), ")"), round(ddifft[1,5],3)))


out=as.data.frame(cbind(c(outcome.name, ""), mnout, mdout, ddiff))
names(out)=c("Outcome.var", "Baseline", "Change", "Difference", "P value")
return(out)
}
 