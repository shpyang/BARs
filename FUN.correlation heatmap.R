

library('Hmisc')
cor=rcorr(as.matrix(datac), type="spearman")

corr=cor[[1]]
corp=cor[[3]]
diag(corr)=NA

diag(corp)=NA
ccorp=as.character(corp)
ccorp[corp>=.05]=""
ccorp[corp<.01]="**"
ccorp[corp<.05&corp>=.01]="*"
ccorp[corp<.001]="***"
mcorp=matrix(paste(round(corr,2), ccorp), nrow(corr))
mcorp[mcorp=="NA NA"]=""

notec=corr*0+4
notec[ccorp!=""]=3
notec=as.matrix(as.numeric(notec))

dmcorp=as.data.frame(mcorp)
row.names(dmcorp)=names(datac)
colnames(dmcorp)=names(datac)
write.csv(dmcorp, "H:/WFH/SPARC/johnston/pwv/out/corr table.20230314.csv")


library('gplots')

tiff("H:/WFH/SPARC/johnston/pwv/plots/Correlation map 20230314.tif",
     width = 2080, height = 1080, pointsize=30)
par(oma=c(5,1,1,8))
heatmap.2(corr, Rowv = TRUE, Colv = TRUE, dendrogram = 'both', trace='none', notecol=1, cellnote=mcorp, na.color='lightgray',
          key=TRUE, main="Correlation matrix", cexRow = 1.0, cexCol = 1.0,
          keysize=1.2)
dev.off()
