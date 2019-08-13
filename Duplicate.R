#Reproducing iris DataSet

data(iris)

#Primero convertiremos la tabla a números
#First, we have to transform the data into numeric variables

library(FitUltD)
library(copula)
library(MASS)
library(psych)
#Sumar y restar
library(VaRES)
#El mero mero sabor ranchero
library(distr)

#https://stackoverflow.com/questions/23569133/adding-two-random-variables-via-convolution-in-r

N <- Norm(mean=1, sd=0.5)          # N is signature for normal dist
L <- Lnorm(meanlog=1.5,sdlog=0.75) # same for log-normal
conv <- convpow(L+N,1)             # object of class AbscontDistribution
f.Z  <- d(conv)                    # distribution function
F.Z <- p(conv)

z <- seq(0,50,0.01)

hist(Z,freq=F,breaks=50, xlim=c(0,30))
z <- seq(0,50,0.01)
lines(z,f.Z(z),lty=2,col="red")


set.seed(31109)
#Creamos una V.A.
X<-c(rnorm(100),rnorm(100,4,1))
Y<-c(rnorm(100)+rnorm(100,4,1))

par(mfrow=c(1,2))
plot(density(X))
plot(density(Y))

http://www.di.fc.ul.pt/~jpn/r/distributions/distr.html
https://cran.r-project.org/web/packages/distrDoc/vignettes/distr.pdf

#Construyamos a X a partir de la info que tenemos
X1<-Norm(mean=0, sd=1)

Indicadora_Norm<-function(x){ifelse(x>.5,X1,0)}

X2<-Norm(mean=4, sd=1)

#Try this: sample(c(rnorm(700,0,1),rgamma(700,9,2)),1200)

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


###############################
#Creo la cópula
myMvd <- mvdc(copula=myCop, margins=c("gamma", "beta", "pois"),
              paramMargins=list(list(shape=2, scale=1),
                                list(shape1=2, shape2=2), 
                                list(lambda=5)) )
#Uno de los requisitos para que funcione es que existan las funciones ddist y pdist
library(DISTRIB)
#Hora de reproducir mi base de datos
Z2 <- rMvdc(2000,myMvd)
colnames(Z2) <- c("x1", "x2", "x3")
pairs.panels(Z2)

##############################################################
##############################################################

library(distr)

B <- Binom(5,0.5)
p(B)(3)
## [1] 0.8125
p.l(B)(3)
## [1] 0.5
distr::q(B)(.5)
## [1] 2
q.r(B)(0.5)
## [1] 3
B0 <- as(Binom(5,0.5),"DiscreteDistribution")
## coercion necessary:
## otherwise slot "prob" of B0 will be returned
prob(B0)
## 0 1 2 3 4 5
## 0.03125 0.15625 0.31250 0.31250 0.15625 0.03125
HN <- Huberize(N, -2,1)
prob(HN)
## -2 1
## cond 0.12541045 0.8745895
## abs 0.02275013 0.1586553
par(mfrow=c(2,3))
plot(makeAbscontDistribution(Nbinom(5,.5)),mfColRow=FALSE)
plot(makeAbscontDistribution(HN),mfColRow=FALSE)

plot(makeAbscontDistribution(Huberize(N, -0,1)),mfColRow=FALSE)

##################################################
####################La respuesta##################
##################################################
#Primero ver que se puedan crear las cópulas asociadas

#Todas las componentes deben ser contínuas
mylist <- UnivarMixingDistribution(Norm(5), Norm(), 
                                   mixCoeff=c(1/4,3/4))

dplot<-distr::d(mylist)
pplot<-distr::p(mylist)
rplot<-distr::r(mylist)
qplot<-distr::q(mylist)

mylist <- UnivarMixingDistribution(Norm(15), Norm(10), 
                                   mixCoeff=c(1/4,3/4))

dplott<-distr::d(mylist)
pplott<-distr::p(mylist)
rplott<-distr::r(mylist)
qplott<-distr::q(mylist)

plot(density(rplot(1000)))
myCop <- normalCopula(param=c(0.4), dim = 2, dispstr = "un")
#myCop <- ellipCopula(param=c(0.4), dim = 3, dispstr = "ex")
#Creo la cópula
myMvd <- mvdc(copula=myCop, margins=c("plott", "plot"),
              paramMargins=list(list(),
                                list()))

Z2 <- rMvdc(2000,myMvd)
colnames(Z2) <- c("x1", "x2")
pairs.panels(Z2)



#Iniciamos el ejemplo:

data(iris)

#Las variables numéricas las conservamos y las categóricas las transformamos en dummies
library(purrr)
library(tibble)
All_number<-function(df){
  df<-as_tibble(df)
  classes<-purrr::map_chr(df,class);nnm<-!classes %in% "numeric"
  no_num<-df[,names(df)[nnm]]
  #Ahora aplicamos la función categorizar
  no_mun_2<-categorizar(no_num,ALL = F)
  #Ahora combinamos las tablas para que todo sea numérico
  
  df_n<-cbind(df[,!nnm],no_mun_2)
  df_n
}

ls("package:data")

All_number()



