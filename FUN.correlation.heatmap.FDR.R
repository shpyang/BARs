# Load the correct libraries
library(gplots) # Corrected from 'gplot'
library(Hmisc)  # Required for rcorr()

adjust_p_matrix <- function(pmat) {
  pvec <- pmat[upper.tri(pmat)]
  padj <- p.adjust(pvec, method = "BH")
  
  pmat_adj <- matrix(NA, nrow = nrow(pmat), ncol = ncol(pmat))
  pmat_adj[upper.tri(pmat_adj)] <- padj
  
  # FIX: Transpose a temporary matrix or reflect the upper tri correctly
  pmat_adj[lower.tri(pmat_adj)] <- t(pmat_adj)[lower.tri(pmat_adj)] 
  
  diag(pmat_adj) <- NA
  rownames(pmat_adj) <- rownames(pmat)
  colnames(pmat_adj) <- colnames(pmat)
  
  return(pmat_adj)
}

FUN.correlation.heatmap = function (datac, table.out, plot.out, oma, type, width, height, pointsize,
                                    cexRow, cexCol, keysize, main.tit, lwdd, to.file, FDR) # Added FDR here
{
  # Set defaults
  if (missing(type)) {type="spearman"}
  if (missing(to.file)) {to.file="."}
  if (missing(width)) {width=5380}
  if (missing(height)) {height=3080}
  if (missing(pointsize)) {pointsize=50}
  if (missing(cexRow)) {cexRow=1.0}
  if (missing(cexCol)) {cexCol=1.0}
  if (missing(keysize)) {keysize=1.2}
  if (missing(oma)) {oma=c(5,1,1,8)}
  if (missing(main.tit)) {main.tit="Correlation matrix"}
  if (missing(lwdd)) {lwdd=2}
  if (missing(FDR)) {FDR=TRUE} # Now this works because FDR is a parameter
  
  # Compute correlation
  cor=rcorr(as.matrix(datac), type=type)
  corr=cor[[1]]
  corp=cor[[3]]
  
  if (FDR==TRUE) {
    corp = adjust_p_matrix(corp)
  }
  
  diag(corr)=NA
  diag(corp)=NA
  
  # FIX: Clean and bulletproof p-value categorization
  ccorp <- character(length(corp))
  ccorp[corp >= 0.05] <- ""
  ccorp[corp < 0.05 & corp >= 0.01] <- "*"
  ccorp[corp < 0.01 & corp >= 0.001] <- "**"
  ccorp[corp < 0.001] <- "***"
  ccorp <- matrix(ccorp, nrow=nrow(corp))
  
  mcorp=matrix(paste(round(corr,2), ccorp), nrow(corr))
  mcorp[mcorp=="NA NA" | mcorp == "NA "]=""
  
  # Create data frame for saving
  dmcorp=as.data.frame(mcorp)
  row.names(dmcorp)=names(datac)
  colnames(dmcorp)=names(datac)
  
  # FIX: Used file.path for robust path building
  write.csv(dmcorp, file.path(table.out, paste0(main.tit,".",, Sys.Date(),".csv")))
  
  # Plotting
  tiff(file.path(plot.out, paste0(main.tit, to.file, "_", Sys.Date(), ".tif")),
       width = width, height = height, pointsize=pointsize, compression="zip")
  
  par(oma=oma)
  
  heatmap.2(corr, Rowv = TRUE, Colv = TRUE, dendrogram = 'both', trace='none', 
            notecol=1, cellnote=mcorp, na.color='lightgray',
            key=TRUE, main=main.tit, cexRow = cexRow, cexCol = cexCol, lwd=lwdd,
            keysize=keysize)
  
  dev.off()
}