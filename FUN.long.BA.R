
library(lmerTest)
library(lme4)
library(haven)
library(emmeans)

FUN.BA=
function (data, outcome.name, time, time.levels, id.name, trt.name, trt.levels, covariates, rd, rd2, rd3) 
{ 
  if (missing(rd)) {
    rd = 2
  }
  if (missing(rd2)) {
    rd2 = 2
  }  
  if (missing(rd3)) {
    rd3 = 3
  }
  #data$outcome.before = data[, outcome.before]
  #data$outcome.after = data[, outcome.after]
  data$id = data[, id.name]
  data$trt = factor(data[, trt.name], levels = trt.levels)
  
  data$time[data[,time]==time.levels[1]]=0
  data$time[data[,time]==time.levels[2]]=1
  
  ms = aggregate(data[data$time==0,outcome.name], list(data$trt[data$time==0]), FUN = "mean", 
                 na.rm = TRUE)
  sds = aggregate(data[data$time==0,outcome.name], list(data$trt[data$time==0]), FUN = "sd", 
                  na.rm = TRUE)
  
  mnout = paste0(round(ms[, 2], rd), "(", round(sds[, 2], rd), 
                 ")")
  mnout
  #dataa = data[, c("id", covariates, outcome.before, "trt")]
  #datab = data[, c("id", covariates, outcome.after, "trt")]
  #dataa$y = data$outcome.before
  #datab$y = data$outcome.after
  #dataa$time = 0
  #datab$time = 1
  base = data[data$time==0,]
  base$base = base[, outcome.name]
  base = base[, c("id", "base")]
  #mg2 = rbind(dataa[, c("id", covariates, "trt", "time", "y")], 
  #            datab[, c("id", covariates, "trt", "time", "y")])
  #mg3 = merge(mg2, base, by = "id")
  mg3=data
  mg3$y=mg3[,outcome.name]
  if (missing(covariates)) {modf = formula(paste0(" y ~ trt + time + trt:time + (1| id) "))} else
  {covariates_formula <- paste(covariates, collapse = "+")
  modf = formula(paste0(" y ~ trt + time + trt:time + (1| id)+", 
                        covariates_formula))}
  m3x <- lmer(modf, REML = TRUE, data = mg3)
  mout = summary(m3x)
  anova(m3x)
  diff <- contrast(emmeans(m3x, ~trt:time), ref = trt.levels[1], 
                   method = "revpairwise")
  sdiff = summary(diff)
  mdiff = as.data.frame(rbind(sdiff[sdiff$contrast == paste0("trt", trt.levels[1], 
                                                             " time1 - ", "trt",trt.levels[1], " time0"), ], sdiff[sdiff$contrast == 
                                                                                                                     paste0("trt",trt.levels[2], " time1 - ", "trt",trt.levels[2], " time0"), 
                                                             ]))
  mdiff = as.data.frame(rbind(sdiff[sdiff$contrast == paste0( trt.levels[1], 
                                                              " time1 - ",  trt.levels[1], " time0"), ], sdiff[sdiff$contrast == 
                                                                                                                 paste0( trt.levels[2], " time1 - ", trt.levels[2], " time0"), 
                                                              ]))
  #wtrt = substr(mdiff[, 1], 1, max(nchar(trt.levels)))
  #wtrt = substr(mdiff[, 1], 4, 3+max(nchar(trt.levels)))
  n1=sum(data$time==0&data$trt==trt.levels[1])
  n2=sum(data$time==0&data$trt==trt.levels[2])
  wtrt=paste0(trt.levels, "(n=", c(n1,n2), ")")
  
  mdout = paste0(round(mdiff[, 2], rd2), " (", round(mdiff[, 
                                                           3], rd2), ")")
  ddifft = t(as.matrix(mout[[10]][row.names(mout[[10]]) == 
                                    paste0("trt", trt.levels[2], ":time1"), ]))
  ddiff = rbind(t(c("Reference", "")), c(paste0(round(ddifft[1, 
                                                             1], rd2), "(", round((ddifft[1, 1] - qt(0.975, ddifft[, 
                                                                                                                   3]) * ddifft[1, 2]), rd2), ", ", round((ddifft[1, 1] + 
                                                                                                                                                             qt(0.975, ddifft[, 3]) * ddifft[1, 2]), rd2), ")"), round(ddifft[1, 
                                                                                                                                                                                                                              5], 3)))
  
  out = as.data.frame(cbind(c(outcome.name, ""), wtrt, mnout, 
                            mdout, ddiff))
  names(out) = c("Outcome.var", "Treatment", "Baseline", "Change", 
                 "Difference", "P value")
  
  return(out)
}