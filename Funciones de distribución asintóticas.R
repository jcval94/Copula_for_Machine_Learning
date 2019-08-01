#Fuera de la caja

#Sabemos que existen funciones cuya 

eq = function(x){
  ifelse(x> -1,1/(x^-2),
         ifelse(x<1,1/(x^-2),0))/2
  }
x<-seq(-3,3,.1)

plot(eq(x),x, type='l')

