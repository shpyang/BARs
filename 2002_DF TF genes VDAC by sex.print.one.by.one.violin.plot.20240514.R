

library(ggplot2)

dat=read.csv("H:/WFH/manna/AD/data/logCPM.csv")

cov=read.csv("H:/WFH/manna/AD/data/cov.csv")
de=read.csv("H:/WFH/manna/AD/data/DEgene.csv")

vars=c("HDAC1", "HDAC2", "HDAC3", "HDAC8", "HDAC4", "HDAC5", "HDAC7", "HDAC9", "HDAC6",
       "HDAC10", "SIRT1", "SIRT2", "SIRT3", "SIRT4", "SIRT5", "SIRT6", "SIRT7", "HDAC11",
       "STAR", "APP", "MAPT", "VDAC1", "VDAC2", "VDAC3", "ACAT1", "HMGCR", "HDLR", "LDLR",
       "BACE1", "APOE", "PRES1", "PRES2"
)
vars2=toupper(c("STAR", "APP", "MAPT", "TSPO", "STARD3", "Cyp11a1", "Hsd3beta", "Cyp19A1",
                "Drp1", "Fis1", "Mfn1", "Opa1", "Pgc1a", "Nrf1", "Nrf2",
                "Tfam", "Psd9", "synap1", "Snap1", "Synap2", "Snap2", "Snap25",
                "Map2", "Lc3b", "Atg5", "Beclin1", "P62",
                "Pink1", "Parkin", "Tert", "Bnip3l", "Bcl2",
                "LIPE", "HSL", "VDAC1", "VDAC2", "VDAC3", "ACAT1", "HMGCR", "HDLR", "LDLR",
                "BACE1", "APOE", "PRES1", "PRES2"))#, "HSD-3beta", "HSD3B2", "CYP19A1", "aromatase"))
vars3=toupper(c("RARA", "RARB", "RARG", "RXRA", "RXRB", "RXRG", "LXRA", "LXRB", "NR1H3", "NR1H2", "VDAC1", "VDAC2", "VDAC3"))
vars3=toupper(c("MEG3", "PINT", "PANDA", "HOTAIR", "BACE1", "MALAT1", "START", "CCND1,2",
                "LET", "PRNCR1", "HIS", "SPHINX", "PNUTS", "HAL", "MIR155HG", "PPP1R10"))
vars3=c("CREB1", "CREM", "SF1", "NR4A1", "CEBPB", "GATA1", "SREBF1", "SREBF2",
"SP1", "FOS", "JUN", "NR0B1", "YY1", "VDAC1", "VDAC2", "VDAC3", "ACAT1", "HMGCR", "HDLR", "LDLR",
"BACE1", "APOE", "PRES1", "PRES2")

vars2=toupper(c("STAR", "APP", "MAPT", "TSPO", "LIPE", "ApoE", "SREBP", "Drp1", "Park2", vars3,
                "VDAC1", "VDAC2", "VDAC3", "ACAT1", "HMGCR", "HDLR", "LDLR",
                "BACE1", "APOE", "PRES1", "PRES2"))


vars=c(vars, vars2)

dev=de[toupper(de$hgnc_symbol)%in%vars,]

df=dat[dat$ensembl_gene_id%in%dev$ensembl_gene_id,]


adcov=cov[cov$Diagnosis%in%c("AD", "CONTROL"),]
adcov$age=adcov$age_death

dic=data.frame(gene=df$ensembl_gene_id)
deg=de[,c("ensembl_gene_id", "hgnc_symbol")]
deg=deg[!duplicated(deg$ensembl_gene_id),]

mdf=merge(deg, df, by="ensembl_gene_id")
row.names(mdf)=mdf$hgnc_symbol
mdf=mdf[,-(1:2)]


adc=adcov[,c("SampleID", "age", "Diagnosis", "msex")]
adc$SampleID=paste0("X", adc$SampleID)


i=1
dfi=as.data.frame(t(mdf[which(row.names(mdf)==vars[i]),]))
dfi$SampleID=row.names(dfi)

data.i=merge(dfi, adc, by="SampleID")

table(data.i$msex, data.i$Diagnosis)

fc=data.i[data.i$Diagnosis=="CONTROL"&data.i$msex==0,]
fa=data.i[data.i$Diagnosis=="AD"&data.i$msex==0,]
mc=data.i[data.i$Diagnosis=="CONTROL"&data.i$msex==1,]
ma=data.i[data.i$Diagnosis=="AD"&data.i$msex==1,]

range(fc$age)
range(fa$age)
range(mc$age)
range(ma$age)

i=1

vars=vars[vars%in%dev$hgnc_symbol ]
vars=c("VDAC1", "VDAC2", "VDAC3", "ACAT1", "HMGCR",  "LDLR",
       "BACE1", "APOE", "SREBF1", "SREBF2")

dfi=as.data.frame(t(mdf[which(row.names(mdf)==vars[i]),]))
dfi$SampleID=row.names(dfi)


my_list <- list()
for (i in 1:10) {
  my_list[[i]] <- i
}
my_list=list()

for (i in 1:length(vars))
{
  
  
  
  
  
  
  
  #i=1
  
  
  
  dfi=as.data.frame(t(mdf[which(row.names(mdf)==vars[i]),]))
  dfi$SampleID=row.names(dfi)
  
  data.i=merge(dfi, adc, by="SampleID") 
  data.i$sex="Female"
  data.i$sex[data.i$msex==1]="Male"
 
  fc=mean(data.i[data.i$msex==0&data.i$Diagnosis=="CONTROL",vars[i]])
  fa=mean(data.i[data.i$msex==0&data.i$Diagnosis=="AD",vars[i]])
  
  fp=wilcox.test(data.i[data.i$msex==0,vars[i]]~data.i$Diagnosis[data.i$msex==0])
  
  mc=mean(data.i[data.i$msex==1&data.i$Diagnosis=="CONTROL",vars[i]])
  ma=mean(data.i[data.i$msex==1&data.i$Diagnosis=="AD",vars[i]])
  
  mp=wilcox.test(data.i[data.i$msex==1,vars[i]]~data.i$Diagnosis[data.i$msex==1])
  
  mpfcmc=round(wilcox.test(data.i[data.i$msex==0&data.i$Diagnosis=="CONTROL",vars[i]],
                           data.i[data.i$msex==1&data.i$Diagnosis=="CONTROL",vars[i]])$p.value, 3)
  
  mpfama=round(wilcox.test(data.i[data.i$msex==0&data.i$Diagnosis=="AD",vars[i]],
                           data.i[data.i$msex==1&data.i$Diagnosis=="AD",vars[i]])$p.value, 3)
  
  out.i=data.frame(vars[i],data.frame(round(cbind(fc, fa, fp$p.value, mc, ma, mp$p.value),3)),mpfcmc, mpfama)
  
  names(out.i)=c("Gene", "Non-AD W", "AD W", "p W", "Non-AD M", "AD M", "p M", "C W-M", "A W-M")
  
  
  current_var=vars[i] 
  y_min <- min(data.i[, current_var])
  y_max <- 1.1 * max(data.i[, current_var])
  data.i$sex="Female"
  data.i$sex[data.i$msex==1]="Male"
  data.i$Diagnosis[data.i$Diagnosis=="CONTROL"]="Non-AD"
  
  fp0=round(fp$p.value,3); fp0[fp0==0]="<0.001"
  mp0=round(mp$p.value,3); mp0[mp0==0]="<0.001"
  
  #my_list[[i]]=
   
  pp=ggplot(data.i, aes(x = sex, y = !!sym(current_var), fill = Diagnosis)) + 
    geom_violin( ) + 
    geom_point(  col=2,  shape = 16,
                 position=position_dodge(.91))+
    scale_y_log10(limits = c(y_min, y_max)) +  # Set y-axis limits
    scale_fill_manual(values = c("lightblue", "lightgreen")) +  # Manually set fill colors to green and red
    theme_minimal() +  # Set plot theme to minimal
    coord_cartesian(ylim = c(y_min, y_max)) +  # Set y-axis limits
    geom_segment(aes(x = 0.77, xend = 1.23, y = 1.02 * max(data.i[, current_var]), 
                     yend = 1.02 * max(data.i[, current_var])), arrow = arrow(type = "open", angle=90, 
                                                                              length = unit(0.1, "inches")))+
    geom_segment(aes(x = 1.23, xend = 0.77, y = 1.02 * max(data.i[, current_var]), 
                     yend = 1.02 * max(data.i[, current_var])), arrow = arrow(type = "open", angle=90, 
                                                                              
                                                                              length = unit(0.1, "inches")))  +
    geom_text(aes(x = 1, y = 1.05 * max(data.i[, current_var]), label = fp0), color = "black", size = 4)+
  
  geom_segment(aes(x = 1.77, xend = 2.23, y = 1.02 * max(data.i[, current_var]), 
                   yend = 1.02 * max(data.i[, current_var])), arrow = arrow(type = "open", angle=90, 
                                                                            length = unit(0.1, "inches")))+
    geom_segment(aes(x = 2.23, xend = 1.77, y = 1.02 * max(data.i[, current_var]), 
                     yend = 1.02 * max(data.i[, current_var])), arrow = arrow(type = "open", angle=90, 
                                                                              
                                                                              length = unit(0.1, "inches")))  +
    geom_text(aes(x = 2, y = 1.05 * max(data.i[, current_var]), label = mp0), color = "black", size = 4)
  
  
   
  
  # Save as TIFF with adjusted dimensions and point size
  ggsave(filename = paste0("H:/WFH/manna/AD/plots/violin.p.", vars[i], ".tif"), 
         plot = pp, 
         width = 5.80, 
         height = 3.3,
         dpi = 300)  # Adjust dpi as needed
  
  
  
  
  
  
  
  
  if (i==1) {out=out.i} else {out=rbind(out, out.i)}
}
  
 
write.csv(out, "H:/WFH/manna/AD/out/Table 5.TF.VDAC.20240514.csv", row.names=FALSE)

 