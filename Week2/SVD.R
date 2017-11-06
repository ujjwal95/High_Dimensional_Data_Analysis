library(rafalib)
library(MASS)
n <- 100
set.seed(1)
y <- t(mvrnorm(n, c(0,0), matrix(c(1,0.95,0.95,1),2.2)))

mypar()
LIM <- c(-3.5,3.5)
plot(y[1,],y[2,],xlim = LIM, ylim = LIM)

s <- svd(y)
PC1 <- s$d[1]*s$v[,1]
PC2 <- s$d[2]*s$v[,2]
plot(PC1,PC2,xlim = LIM, ylim = LIM)

library(tissuesGeneExpression)
data(tissuesGeneExpression)

dim(e)

set.seed(1)
#took sample of 500
ind <- sample(nrow(e),500)
#scaled matrix
Y <- t(apply(e[ind,],1,scale))

s <- svd(Y)

U <- s$u
V <- s$v
D <- diag(s$d)
#recompose the matrix Y
Yhat <- U%*% D %*% t(V)
#we can find the residual to be extremely low
resid <- (Yhat - Y)
max(abs(resid))
#we can plot to see which values are very low or zero and remove them
plot(s$d)
#some are exactly 0
#so we can reduce the dimensions
k <- ncol(U)-4
Yhat <- U[,1:k]%*% D[1:k,1:k] %*% t(V[,1:k])
resid <- (Yhat - Y)
max(abs(resid))
#residual is still the same ie very low
#d shows us the amount of variance each column is contributing to the original value
#we can see by the below command that vairance explained becomes 0 soon enough
plot(s$d^2/ sum(s$d^2)*100)

#we can try to remove half of the data

k <- ncol(U)-95
Yhat <- U[,1:k] %*% D[1:k,1:k] %*% t(V[,1:k])
resid <- (Yhat - Y)
max(abs(resid))
#we can see by making a boxlpot that the residuals are very low
boxplot(resid, ylim = LIM)

#we can see what percent we actually keep
# we can explain 95 percent of data
var(as.vector(Yhat))/var(as.vector(Y))
# we also have the variance in s$d
sum(s$d[1:k]^2)/sum(s$d^2)

#we can also try out for highly correlated data
m <- 100
n <- 2
x <- rnorm(m)
e <- rnorm(n*m,0,0.01)
Y <- cbind(x,x)+e
cor(Y)
d <- svd(Y)$d
d[1]^2/sum(d^2)
#now we can try for 25 dimensions
m <- 100
n <- 25
x <- rnorm(m)
e <- rnorm(n*m,0,0.01)
Y <- replicate(n,x)+e
d <- svd(Y)$d
d[1]^2/sum(d^2)
