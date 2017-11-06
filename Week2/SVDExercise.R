library(tissuesGeneExpression)
data(tissuesGeneExpression)

s = svd(e)
signflips = sample(c(-1,1),ncol(e),replace=TRUE)
signflips
newu= sweep(s$u,2,signflips,FUN="*")
newv= sweep(s$v,2,signflips,FUN="*" )
all.equal( s$u %*% diag(s$d) %*% t(s$v), newu %*% diag(s$d) %*% t(newv))

#q1
s = svd(e)
m = rowMeans(e)
cor(s$u[,1], m)

#q2
newmeans = rnorm(nrow(e)) ##random values we will add to create new means
newe = e+newmeans ##we change the means
sqrt(crossprod(e[,3]-e[,45]))
sqrt(crossprod(newe[,3]-newe[,45])) 
y = e - rowMeans(e)
s = svd(y)
resid = y - s$u %*% diag(s$d) %*% t(s$v)
max(abs(resid))

#q4
z = s$d * t(s$v)
sqrt(crossprod(e[,3]-e[,45]))
sqrt(crossprod(y[,3]-y[,45]))
sqrt(crossprod(z[,3]-z[,45]))
sqrt(crossprod(e[,3]-e[,45]))-sqrt(crossprod(z[1:2,3]-z[1:2,45]))

#q5
for (i in range(1:185)){
dist <-  sqrt(crossprod(e[,3]-e[,45]))-sqrt(crossprod(z[1:i,3]-z[1:i,45]))
 if(dist < sqrt(crossprod(e[,3]-e[,45]))*0.1)
    {
      break;
 }
}
print(i)
