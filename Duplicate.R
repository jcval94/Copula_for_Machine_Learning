#Reproducing iris DataSet

data(iris)

#Primero convertiremos la tabla a números
#First, we have to transform the data into numeric variables

library(FitUltD)
library(copula)

set.seed(31109)
X<-c(rnorm(100),rnorm(100,4,1))
plot(density(X))

kmeans(X,2)
