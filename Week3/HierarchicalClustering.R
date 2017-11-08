library(tissuesGeneExpression)
data(tissuesGeneExpression)

d <- dist(t(e))

#package for hierarchial clustering
hc <- hclust(d)
class(hc)

#tissue is in the correct order
plot(hc, cex = 0.5, label = tissue)

#coloring using rafalib package
library(rafalib)
myplclust(hc,label = tissue, cex = 0.5, lab.col = as.fumeric(tissue))

#doesnt really give us clusters, so we have to cut the tree
abline(h =120)

#all below the above line are in the same group
# we can use the cutree function 

cl <- cutree(hc, h = 120)
# now we have a cluster for each sample

#if we didnt know that the clustering could be done using tissue value,
#find clustering
table(true = tissue, cluster = cl)
