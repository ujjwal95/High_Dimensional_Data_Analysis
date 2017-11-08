library(tissuesGeneExpression)
data(tissuesGeneExpression)

colind <- tissue%in%c('kidney','colon','liver')

mat <- e[,colind]
ftissue <- factor(tissue[colind])
dim(mat)

#remove mean because they are cancelled out when calculating distances anyways
s <- svd(mat-rowMeans(mat))
z <- diag(s$d[1:2]) %*% t(s$v[,1:2])
dim(z)
#used to columns - instructor
z <- t(z)
# we can visualize the data in 2 dimensions
plot(z)
plot(z[,1],z[,2],pch=21,bg=as.numeric(ftissue))


#explains 52 percent of the data
sum(s$d[1:2]^2)/sum(s$d^2)

#we can look at the 3rd and the 4th dimension
z <- diag(s$d[3:4]) %*% t(s$v[,3:4])
z <- t(z)
plot(z[,1],z[,2],pch=21,bg=as.numeric(ftissue))

#cmdscale for mds plots, k gives number of dimensions
d <- dist(t(mat))
mds <- cmdscale(d, k= 2)
library(rafalib)
mypar(1,1)
plot(mds[,1],mds[,2],bg=as.numeric(ftissue),pch=21,xlab="First dimension",ylab="Second dimension")
legend("bottomleft",levels(ftissue),col=seq(along=levels(ftissue)),pch=15)
