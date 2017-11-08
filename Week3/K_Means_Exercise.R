#q1
library(GSE5859Subset)
data(GSE5859Subset)

set.seed(10)

km <- kmeans(t(geneExpression), centers = 5)

table(sampleInfo$date, km$cluster)
