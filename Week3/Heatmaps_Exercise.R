library(GSE5859Subset)
data(GSE5859Subset)

library(matrixStats)

#q1
?rowMads
rv <- rowMads(geneExpression)
idx <- order(-rv)[1:25]

library(gplots)
hmcol <- colorRampPalette(brewer.pal(9,"GnBu"))(25)
cols <- palette(brewer.pal(7, "Dark2"))[as.numeric(sampleInfo$group)]
cbind(colnames(e),cols)
#it is also resizable
gcol=brewer.pal(3,"Dark2")
gcol=gcol[sampleInfo$g+1]
labcol= gsub("2005-","",sampleInfo$date)  
heatmap.2(geneExpression[idx,],
          col=hmcol,
          trace="none",
          scale="row",
          labRow=geneAnnotation$CHR[idx],
          labCol=labcol,
          ColSideColors=gcol,
          key=FALSE)

#q2
set.seed(17)
m = nrow(geneExpression)
n = ncol(geneExpression)
x = matrix(rnorm(m*n),m,n)
g = factor(sampleInfo$g )
cols = colorRampPalette(rev(brewer.pal(11,"RdBu")))(25)

t_test <- rowttests(x,g)
rv <- rowSds(geneExpression)
idx <- order(-rv)[1:50]

indexes <- list(t=order(t_test$p.value)[1:50], s=idx)
for (i in indexes)
  {
  heatmap.2(x[i,],col = cols,trace="none",scale="row", labCol = g,key=FALSE)
}
