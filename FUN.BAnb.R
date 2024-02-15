QT <- function(x) {
  # Define your custom computation here
  result <- quantile(x, c(.25, .5, .75), na.rm=TRUE)  # For example, compute the mean of x and multiply by 2
  return(result)
}

FUN.BAnb=
function (data, outcome.name, outcome.before, outcome.after, 
            id.name, trt.name, trt.levels, covariates, rd, rd2, rd3) 
  {
    if (missing(rd)) {rd = 2}
    if (missing(rd2)) { rd2 = 3 }
    if (missing(rd3)) { rd3 = 3 }
    
  data$outcome.before = data[, outcome.before]
  data$outcome.after = data[, outcome.after]
  
  print("XX")
  data$id = data[, id.name]
  data$trt = factor(data[, trt.name], levels = trt.levels)
  ms = aggregate(data$outcome.before, list(data$trt), FUN = "mean", 
                 na.rm = TRUE)
  sds = aggregate(data$outcome.before, list(data$trt), FUN = "sd", 
                  na.rm = TRUE)
  mnout = paste0(round((ms[, 2]), rd), "(", round((sds[, 2]), rd), 
                 ")")
  mnout
  
  

  
  qtl = aggregate(data$outcome.before, list(data$trt), FUN = "QT" )
  qmnout=paste0(round(qtl[,2][,2], rd), "(", round(qtl[,2][,1], rd), ", ", round(qtl[,2][,3], rd), ")")
  
  
  dataa = data[, c("id", covariates, outcome.before, "trt")]
  datab = data[, c("id", covariates, outcome.after, "trt")]
  dataa$y = data$outcome.before
  datab$y = data$outcome.after
  dataa$time = 0
  datab$time = 1
  base = dataa
  base$base = base[, outcome.before]
  base = base[, c("id", "base")]
  mg2 = rbind(dataa[, c("id", covariates, "trt", "time", "y")], 
              datab[, c("id", covariates, "trt", "time", "y")])
  mg3 = merge(mg2, base, by = "id")
  mg3=mg3[!is.na(mg3$y),]
  mg3=mg3[!is.na(mg3[,outcome.before]),]
  covariates_formula <- paste(covariates, collapse = "+")
  modf = formula(paste0(" y ~ trt + time + trt:time + (1| id)+",   covariates_formula))
  
  
  
  m3x <- glmmTMB(modf,   family = nbinom2,data = mg3)
  mout = summary(m3x)
  mout 
  
  
  diff <- contrast(emmeans(m3x, ~trt:time), ref = trt.levels[1], 
                   method = "revpairwise")
  sdiff = summary(diff)
  mdiff = as.data.frame(rbind(sdiff[sdiff$contrast == paste0("trt", trt.levels[1], 
                                                             " time1 - ", "trt", trt.levels[1], " time0"), ], sdiff[sdiff$contrast == 
                                                                                                                      paste0("trt",trt.levels[2], " time1 - ", "trt", trt.levels[2], " time0"), 
                                                             ]))
  
  table(dataa$trt)
  wtrt = substr(mdiff[, 1], 4, 3+max(nchar(trt.levels)))
  n1=sum(dataa$trt==wtrt[1])
  n2=sum(dataa$trt==wtrt[2])
  wtrt=paste0(wtrt, "(n=", c(n1,n2), ")")
  mdout = paste0(round(exp(mdiff[, 2]), rd2), " (", #round(exp(mdiff[, 3]), rd2), 
                 round(exp(mdiff[, 2]-qt(.975, mdiff[,4])*mdiff[,3]), rd2), ", ", 
                 round(exp(mdiff[, 2]+qt(.975, mdiff[,4])*mdiff[,3]), rd2),
                 ")")
  
  ddifft = t(as.matrix(mout[[6]][[1]][row.names(mout[[6]][[1]]) == 
                                        paste0("trt", trt.levels[2], ":time"), ]))
  ddiff = rbind(t(c("Reference", "")), c(paste0(round(exp(ddifft[1,  1]), rd2), 
                                                "(", round(exp(ddifft[1, 1] - 1.96 * ddifft[1, 2]), rd2), ", ",
                                                round(exp(ddifft[1, 1] + 1.96 * ddifft[1, 2]), rd2), ")"), 
                                         c(round(ddifft[1,  4], 3))))
  
  out = as.data.frame(cbind(c(outcome.name, ""), wtrt, mnout,  qmnout, mdout, ddiff))
  
  names(out) = c("Outcome.var", "Treatment", "Baseline", "Baseline QT", "Change", 
                 "Difference", "P value")
  
  out
  
  print("XXX")
  print(mg3[1,])
  library(MASS)
  
  model <- glm.nb(outcome.after ~ outcome.before + group + sex + assay_site, data=data)
  print(summary(model))
  return(out)
}
