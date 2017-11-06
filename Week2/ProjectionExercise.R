library(Biobase)
library(GSE5859Subset)
data(GSE5859Subset)

#q1
y = geneExpression[,1:2]
maplot(y[,1], y[,2])
x <- matrix( c(1, 1, 1, -1), nrow=2, ncol=2) 
y <- y %*% x
maplot(y[,1], y[,2])
