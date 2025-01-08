

library('Hmisc'); library('gplots')

FUN.correlation.heatmap=function (datac, table.out, plot.out, oma, type, width, height, pointsize,
                                  cexRow, cexCol, keysize, main.tit, lwdd)
{if (missing(type)) {type="spearman"}
  if (missing(width)) {width=5380}
  if (missing(height)) {height=3080}
  if (missing(pointsize)) {pointsize=50}
  if (missing(cexRow)) {cexRow=1.0}
  if (missing(cexCol)) {cexCol=1.0}
  if (missing(keysize)) {keysize=1.2}
  if (missing(oma)) {oma=c(5,1,1,8)}
  if (missing(main.tit)) {main.tit="Correlation matrix"}
  if (missing(lwdd)) {lwdd=2}
  
  cor=rcorr(as.matrix(datac), type=type)

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
write.csv(dmcorp, paste0(table.out, "/Corr table.", Sys.Date(),".csv"))



tiff(paste0(plot.out, "/Correlation map.", Sys.Date(), ".tif"),
     width = width, height = height, pointsize=pointsize, compression="zip")
par(oma=oma)
heatmap.2(corr, Rowv = TRUE, Colv = TRUE, dendrogram = 'both', trace='none', notecol=1, cellnote=mcorp, na.color='lightgray',
          key=TRUE, main=main.tit, cexRow = cexRow, cexCol = cexCol, lwd=lwdd,
          keysize=keysize)
dev.off()
}
