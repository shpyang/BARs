FUN.BA=
function (data, outcome.name, outcome.before, outcome.after, 
          id.name, trt.name, trt.levels, covariates, rd, rd2, rd3) 
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
  data$outcome.before = data[, outcome.before]
  data$outcome.after = data[, outcome.after]
  data$id = data[, id.name]
  data$trt = factor(data[, trt.name], levels = trt.levels)
  ms = aggregate(data$outcome.before, list(data$trt), FUN = "mean", 
                 na.rm = TRUE)
  sds = aggregate(data$outcome.before, list(data$trt), FUN = "sd", 
                  na.rm = TRUE)
  mnout = paste0(round(ms[, 2], rd), "(", round(sds[, 2], rd), 
                 ")")
  mnout
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
  covariates_formula <- paste(covariates, collapse = "+")
  modf = formula(paste0(" y ~ trt + time + trt:time + (1| id)+", 
                        covariates_formula))
  m3x <- lmer(modf, REML = TRUE, data = mg3)
  mout = summary(m3x)
  
  
  
  wtrt=names(table(dataa$trt))
  n1=sum(dataa$trt==wtrt[1])
  n2=sum(dataa$trt==wtrt[2])
  wtrt=paste0(wtrt, "(n=", c(n1,n2), ")")
  
  fixed_effects <- fixef(m3x)
  
  # Extract the variance-covariance matrix of the fixed effects coefficients from the model summary
  vcov_matrix <- vcov(m3x)
  
  # Extract the standard errors of the fixed effects coefficients from the variance-covariance matrix
  fixed_effects_se <- sqrt(diag(vcov_matrix))
  
  # Calculate the estimated change within each group
  change_in_trt <- fixed_effects['time'] + fixed_effects[regexpr(":", names(fixed_effects))>0]  # Change in the 'trt' group
  change_in_ck <- fixed_effects['time']  # Change in the 'ck' group
  
  # Calculate the standard errors for the estimated change within each group
  se_change_in_trt <- sqrt(fixed_effects_se['time']^2 + 2 * vcov_matrix['time', regexpr(":", names(fixed_effects))>0] + fixed_effects_se[regexpr(":", names(fixed_effects))>0]^2)  # SE for change in the 'trt' group
  se_change_in_ck <- fixed_effects_se['time']  # SE for change in the 'ck' group
  
  mdout=c(paste0(round(change_in_ck, rd2), "(", round(se_change_in_ck,rd2), ")"), 
          paste0(round(change_in_trt, rd2), "(", round(se_change_in_trt,rd2), ")"))
  
  
  ddifft = t(as.matrix(mout[[10]][regexpr(":", row.names(mout[[10]]))>0, ]))
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