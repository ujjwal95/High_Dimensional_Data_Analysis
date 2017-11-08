library(tissuesGeneExpression)
data(tissuesGeneExpression)

#q1
y = e - rowMeans(e)
s = svd(y)
z = s$d * t(s$v)
library(rafalib)
ftissue = factor(tissue)
mypar(1,1)
plot(z[1,],z[2,],col=as.numeric(ftissue))
legend("topleft",levels(ftissue),col=seq_along(ftissue),pch=1)
d = dist(t(e))
mds = cmdscale(d)
cor(mds[,1],z[1,])

#q2
cor(mds[,2],z[2,])

#q3
library(rafalib)
ftissue = factor(tissue)
mypar(1,2)
z[1,] <- z[1,]* -1
z[2,] <- z[2,]* -1
plot(z[1,],z[2,],col=as.numeric(ftissue))
legend("topleft",levels(ftissue),col=seq_along(ftissue),pch=1)
plot(mds[,1],mds[,2],col=as.numeric(ftissue))

#q4 and q5
library(GSE5859Subset)
data(GSE5859Subset)
s = svd(geneExpression-rowMeans(geneExpression))
z = s$d * t(s$v)
index_max <- vector('numeric', length= 24)

for (i in 1:dim(z)[1]){
  index_max[i] <- cor(z[i,],sampleInfo$group)
}
index_max <- abs(index_max)
which.max(index_max)

#q6
max(index_max)

#q7
month = format( sampleInfo$date, "%m")
month = factor( month)
month <- as.numeric(month)
which.max(cor(month,t(z)[,1]))
cor(month,t(z)[,1])

#q8
U <- s$u
mypar(1,1)
strat <- split(U[,5],  geneAnnotation$CHR)
boxplot(strat)
