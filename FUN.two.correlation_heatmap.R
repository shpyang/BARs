library(gridGraphics)
library(grid) 
library(gridExtra)


grab_grob <- function(){
  grid.echo()
  grid.grab()
}
 
  
  FUN.correlation.heatmap2=function (datac1, datac2, plot.out, oma, type, width, height, pointsize,
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
  
  cor=rcorr(as.matrix(datac1), type=type)
  
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
  row.names(dmcorp)=names(datac1)
  colnames(dmcorp)=names(datac1) 
  
  
  
  cor2=rcorr(as.matrix(datac2), type=type)
  
  corr2=cor2[[1]]
  corp2=cor2[[3]]
  diag(corr2)=NA
  
  diag(corp2)=NA
  ccorp2=as.character(corp2)
  ccorp2[corp2>=.05]=""
  ccorp2[corp2<.01]="**"
  ccorp2[corp2<.05&corp2>=.01]="*"
  ccorp2[corp2<.001]="***"
  mcorp2=matrix(paste(round(corr2,2), ccorp2), nrow(corr2))
  mcorp2[mcorp2=="NA NA"]=""
  
  notec2=corr2*0+4
  notec2[ccorp2!=""]=3
  notec2=as.matrix(as.numeric(notec2))
  
  dmcorp2=as.data.frame(mcorp2)
  row.names(dmcorp2)=names(datac2)
  colnames(dmcorp2)=names(datac2) 
  
  
  
  arr <- list(corr, corr2)
  cp=list(mcorp, mcorp2) 
    
  mtt=list("Male", "Female")
  kys=list(TRUE, FALSE)
   
  gl <- lapply(1:2, function(i){
    heatmap.2(as.matrix(arr[[i]]), Rowv = TRUE, Colv = TRUE, dendrogram = 'both', trace='none', notecol=1, cellnote=cp[[i]], na.color='lightgray',
              key=kys[[i]], main=mtt[[i]], cexRow = cexRow, cexCol = cexCol, lwd=lwdd,
              keysize=1)
    
    grab_grob()
  })
  
   

    tiff(paste0(plot.out, "/Correlation map 2.", Sys.Date(), ".tif"),
         width = 2.5*width, height = 1.5*height, pointsize=pointsize, compression="zip")
    
    grid.newpage()
    grid.arrange(grobs=gl, ncol=2, clip=TRUE)
  
dev.off()
  }
  
  

 
