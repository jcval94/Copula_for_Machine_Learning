#Reproducing iris DataSet

data(iris)

#Primero convertiremos la tabla a números
#First, we have to transform the data into numeric variables

library(FitUltD)
library(copula)
library(MASS)
library(psych)

set.seed(31109)
#Creamos una V.A.
X<-c(rnorm(100),rnorm(100,4,1))







set.seed(100)

m <- 3
n <- 2000
sigma <- matrix(c(1, 0.4, 0.2,
                  0.4, 1, -0.8,
                  0.2, -0.8, 1), 
                nrow=3)
z <- mvrnorm(n,mu=rep(0, m),Sigma=sigma,empirical=T)
cor(z,method='spearman')
pairs.panels(z)


u <- pnorm(z)
pairs.panels(u)

x1 <- qgamma(u[,1],shape=2,scale=1)
x2 <- qbeta(u[,2],2,2)
x3 <- qt(u[,3],df=5)
df <- cbind(x1,x2,x3)
pairs.panels(df)
cor(df,meth='spearman')

#Incluimos las cópulas
#Ingreso la correlación actual entre variables
myCop <- normalCopula(param=c(0.4,0.2,-0.8), dim = 3, dispstr = "un")

#Creo la cópula
myMvd <- mvdc(copula=myCop, margins=c("gamma", "beta", "t"),
              paramMargins=list(list(shape=2, scale=1),
                                list(shape1=2, shape2=2), 
                                list(df=5)) )
#Uno de los requisitos para que funcione es que existan las funciones ddist y pdist


#Hora de reproducir mi base de datos
Z2 <- rMvdc(2000,myMvd)
colnames(Z2) <- c("x1", "x2", "x3")
pairs.panels(Z2)





