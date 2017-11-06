library(devtools)
install_github("genomicsclass/tissuesGeneExpression")
library(tissuesGeneExpression)
data(tissuesGeneExpression)
head(e)
head(tissue)

#q1
length(tissue[tissue == 'hippocampus'])

#q2
d <- dist(t(e),method = 'euclidean')
as.matrix(d)[3,45]

#q3
x <- e['210486_at',]
y <- e['200805_at',]
sqrt(sum((x-y)^2))

#q5
d <- as.matrix(d)
(length(d)-length(diag(d)))/2

