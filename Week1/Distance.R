install.packages('devtools')
library(devtools)
install_github("genomicsclass/tissuesGeneExpression")
library(tissuesGeneExpression)
data(tissuesGeneExpression)
table(tissue)
dim(e)

x <- e[,1]
y <- e[,2]
z <- e[,87]

print(tissue[c(1,2,87)])

#distance
sqrt(sum((x-y)^2))
sqrt(sum((x-z)^2))
sqrt(sum((y-z)^2))

#using matrix algebra
sqrt(crossprod(x-y))
sqrt(crossprod(z-y))
sqrt(crossprod(x-z))

#distance function
d <- dist(t(e),method = 'euclidean')
class(d)
#it is of class dist, so it is easier to use it as a matrix instead of a dist
as.matrix(d)[1,2]

#for visualization
image(as.matrix(d))

