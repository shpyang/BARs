# Set the seed for reproducibility
#set.seed(123)
library(MASS)
# Add correlated repeated measures
library(nlme)

B=1000
genderg.p=aunpair.gender=unpair.gender=pairt.p=treat0.p1=treat.p=treat.p1=carry.p=pt.p=gender.p=TgenderB.p=TgenderC.p=dpt.p=genderAB.p=genderAC.p=rep(NA, B)
for (b in 1:B)
{
  n_subjects <- 270 
  n_treatments <- 3
  n_periods <- n_treatments  # Number of periods
  sds=1
  
  n_sequence=ifelse(round(n_treatments/2)==n_treatments/2, n_treatments, n_treatments*2)
  
  corr=0.5
  diffm=0.2
  mus=10+diffm*c(1,2,2)*0
  # Create an empty data frame to store the simulated data
response <- as.data.frame(mvrnorm(n = n_subjects, mu = mus, Sigma = sds^2*(matrix(rep(corr,n_treatments^2),n_treatments)+diag(1-corr,n_treatments))))
 
    response$p1=  c(c(rep(1,n_subjects/n_sequence),rep(2,n_subjects/n_sequence),rep(3,n_subjects/n_sequence)),
                    c(rep(1,n_subjects/n_sequence),rep(2,n_subjects/n_sequence),rep(3,n_subjects/n_sequence)))
    
    response$p2=  c(c(rep(2,n_subjects/n_sequence),rep(1,n_subjects/n_sequence),rep(1,n_subjects/n_sequence)),
                    c(rep(3,n_subjects/n_sequence),rep(3,n_subjects/n_sequence),rep(2,n_subjects/n_sequence)))
    
    response$p3=  c(c(rep(3,n_subjects/n_sequence),rep(3,n_subjects/n_sequence),rep(2,n_subjects/n_sequence)),
                    c(rep(2,n_subjects/n_sequence),rep(1,n_subjects/n_sequence),rep(1,n_subjects/n_sequence)))
    
    s1=NA
    for (i in 1:n_sequence)
    {s1i=rep(i, n_subjects/n_sequence)
      if (i==1) {s1=s1i} else {s1=c(s1, s1i)}
    }
    
    response$s2=response$s3=response$s1=s1
     
    datas0=datas=data.frame(Treatment=c(rep("A", n_subjects),rep("B", n_subjects),rep("C", n_subjects)), 
                     Period=c(response$p1,response$p2,response$p3), Response=c(response[,1], response[,2], response[,3]), 
                     Sequence=c(response$s1,response$s2,response$s3), Subject=rep(1:(nrow(response)),3))
  
    datas$ss=paste(datas$Subject, datas$Sequence)
    udata=datas[!duplicated(datas$ss),]
    #udata$gender=rep(c(sample(c(rep("M",15),rep("F",30)), 45, replace = FALSE)),6)
    udata$gender=rep(c(sample(c(rep("M",22),rep("F",23)), 45, replace = FALSE)),6)
    udata=udata[,which(names(udata)%in%c("ss", "gender"))]
    
    datas=merge(datas, udata, by="ss", all.x=TRUE)
    datas=datas[order(datas$Subject, datas$Sequence, datas$Period),]
    
    # datas$Response[datas$gender=="M"&datas$Treatment=="C"]=datas$Response[datas$gender=="M"&datas$Treatment =="C"]+rnorm(sum(datas$gender=="M"&datas$Treatment =="C"), diffm, sds)#*0
    datas$Response[datas$gender=="M" ]=datas$Response[datas$gender=="M" ]+diffm#*0
    #datas$Response[datas$gender=="M"&datas$Treatment=="C"]=datas$Response[datas$gender=="M"&datas$Treatment =="C"]+diffm#*0
    
    dij=response[,2]-response[,1]
    del=sum(dij)/length(dij)
    
    for (i in 1:n_sequence)
    {si=response[response$s1==i,]
    di=mean(si[,2]-si[,1])
    di2=(((si[,2]-si[,1]))-di)^2
    if (i==1) {sdi2=di2} else (sdi2=c(sdi2,  di2))
    }
    
    sd2=sum(sdi2)/n_sequence/(n_subjects/n_sequence-1)
    t=del/sqrt(sd2)*sqrt(n_sequence*n_subjects/n_sequence)
    ptp=(1-pt(t, n_sequence*(n_subjects/n_sequence-1)))*2
 
# Create a linear mixed-effects model with Sequence and Period as fixed effects
  # Do not include the treatment by period interaction term; otherwise the p values are very different.
    model0 <- lme(Response ~ Treatment + Period, random = ~ 1 | Subject , data = datas)
    modelg <- lme(Response ~ Treatment + Period +gender, random = ~ 1 | Subject , data = datas)
    
# The interaction term
  model <- lme(Response ~ Treatment +Period +gender+Treatment*gender, random = ~ 1 | Subject , data = datas)
    
# Summary of the model
anova_result0 <- anova(model0)
anova_result <- anova(model)
treat0.p1[b]=summary(model0)$tTable[2,5]

# note that the p below might not be correct, because of covariates.
treat.p1[b]=summary(model)$tTable[2,5]
treat.p[b]=anova_result[2,4]
carry.p[b]=anova_result[5,4]
pt.p[b]=ptp
pairt.p[b]=t.test(response[,1], response[,2], paired=TRUE)$p.value
gender.p[b]=summary(model)$tTable[5,5]
genderg.p[b]=summary(modelg)$tTable[5,5]

unpair.gender[b]=t.test(datas$Response~datas$gender)$p.value

d1=datas[datas$Treatment=="A",]
d2=datas[datas$Treatment=="B",]
d3=datas[datas$Treatment=="C",]

aunpair.gender[b]=t.test((d1$Response+d2$Response+d3$Response)/3~d1$gender)$p.value

TgenderB.p[b]=summary(model)$tTable[6,5]
TgenderC.p[b]=summary(model)$tTable[7,5]

dA=datas[datas$Treatment=="A",]
dB=datas[datas$Treatment=="B",]
dC=datas[datas$Treatment=="C",]
mg=merge(dA, dC, by="Subject")
mg2=merge(dA, dB, by="Subject")
mg$C_A=mg$Response.y-mg$Response.x
mg2$B_A=mg2$Response.y-mg2$Response.x

genderAB.p[b]=t.test(mg$C_A~mg$gender.x)$p.value
genderAC.p[b]=t.test(mg2$B_A~mg2$gender.x)$p.value
}


# overall treatment effect:
table(treat.p<0.05) 

# full model comparing Treatment B and A
table(treat.p1<0.05) 
 
#table(carry.p<0.05)

# This is the p value from PASS (see PASS documentation) Comparing Treatment B and A
table(pt.p<0.05)

# model without carryover effect - comparing Treatment B and A
# This is similar to the p value from PASS above pt.p
# note that there is no interaction term (carryover) in the mixed regression model;
table(treat0.p1<0.05)

table(gender.p<0.05)
table(genderg.p<0.05)
table(unpair.gender<0.05)
table(aunpair.gender<0.05)

table(TgenderB.p<0.05)
table(TgenderC.p<0.05)
 
table(genderAB.p<0.05)
table(genderAC.p<0.05)



# note that TgenderC.p and genderAB.p are very similar:
# the former is the treatment by gender interaction term from the mixed model regression;
# the latter is the different of two treatments as the outcome, gender is the predictor.



sqrt((3+3*2*corr)/9)

table(d1$gender)

# To calculate the power for testing main effect of gender is the below
 set everything else as 0

Group sample sizes of 132 and 138 achieve 43.383% power to reject the null hypothesis of equal
means when the population mean difference is μ1 - μ2 = 0.2 - 0.0 = 0.2 with a standard
deviation for both groups of 0.9 and with a significance level (alpha) of 0.050 using a
two-sided two-sample equal-variance t-test.

This equals to the power for the gender effect for model
modelg <- lme(Response ~ Treatment + Period +gender, random = ~ 1 | Subject , data = datas)

This also equal to the unpaired comparison of average of the 3 arms between the two groups
aunpair.gender[b]=t.test((d1$Response+d2$Response+d3$Response)/3~d1$gender)$p.value

The sd for (d1$Response+d2$Response+d3$Response)/3 is 
sqrt((3+3*2*corr)/9)


0.05/9