#q1

set.seed(1)
m = 10000
n = 24
x <- matrix(rnorm(m*n),m,n)
colnames(x) <- 1:n
x <- t(x)
d <- dist(x)
hc <- hclust(d)
plot(hc)

#q2
set.seed(1)
m = 10000
n = 24
number_of_clusters <- vector('numeric', length = 100)
for (i in 1:100)
{
  x = matrix(rnorm(m*n),m,n)
  hc = hclust( dist( t(x)))
  cl <- cutree(hc, h = 143)
  number_of_clusters[i] <- length(unique(cl))
}
popsd(number_of_clusters)
