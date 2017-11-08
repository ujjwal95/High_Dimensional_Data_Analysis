library(tissuesGeneExpression)
data(tissuesGeneExpression)
library(rafalib)

#kmeans clustering
set.seed(1)
kmeans <- kmeans(t(e), centers = 7)
table(tissue, cluster = kmeans$cluster)

#visualize it
d <- dist(t(e))
mds <- cmdscale(d)
plot(mds[,1], mds[,2], col = kmeans$cluster)

#running it again will give it a different answer