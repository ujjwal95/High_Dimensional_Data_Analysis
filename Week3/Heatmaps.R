library(tissuesGeneExpression)
data(tissuesGeneExpression)
library(rafalib)

image(e[1:100,])

#we cannot show all the genes ie 22000 genes
#so, we can pick up the genes which vary the most
#so, we can pick up the first 40
library(genefilter)
rv <- rowVars(e)
idx <- order(-rv)[1:40]

heatmap(e[idx,])
#not very infromative

#changing colors
library(RColorBrewer)
hmcol <- colorRampPalette(brewer.pal(9,"GnBu"))(100)
#better to look at
heatmap(e[idx,], col = hmcol)

#heatmap.2 is better package tham heatmap
library(gplots)
cols <- palette(brewer.pal(7, "Dark2"))[as.fumeric(tissue)]
cbind(colnames(e),cols)
#it is also resizable
heatmap.2(e[idx,],labCol = tissue,trace = "none",ColSideColors = cols, col = hmcol)
