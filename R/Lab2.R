# Laboratorio 2

##Ejercicio 1.
###Transformada inversa
library("tidyverse")
inv.d<-function(n=10000){
  U<-runif(n)
  X<-case_when(
    U < 0.30 ~ 1,
    U < 0.20 ~ 2,
    U < 0.35 ~ 3,
    U < 0.15 ~ 4,
    TRUE ~ 5
  )
}
siminv.d <- inv.d()
table(siminv.d)
100*prop.table(table(siminv.d))
qplot(siminv.d,binwidth=1,fill=I("gray"),col=I("black"))

##Ejercicio 2.
###Monte Carlo
# Gráfico
library(DescTools)
f <- function(x, y){
  exp((x+y)^2)
}
x<-y<-seq(0,1, length=1000)
z<-outer(x,y, f)
persp(x,y,z)

#Integral
n=1000
f <- function(x, y){
  exp((x+y)^2)
}
x<-y<-seq(0,1, length=n)
Monte.Carlo <- function(f,a,b,n){
  k <- 0
  for(i in 1:n){
    k <- k+f(a+(b-a)*runif(1,0,1))
  }
  return(((b-a)/n)*k)
}

Monte.Carlo(f,0,1,n)



### Con función de Rstudio
library("pracma")
f <- function(x, y){
  exp((x+y)^2)
   }
xmin <- 0; xmax <- 1
ymin <- 0; ymax <- 1
I <- integral2(f, xmin, xmax, ymin, ymax)
I$Q 


##Ejercicio 3.
### Proceso Poisson No Homogeneo
#función de intensidad

lambda<-function(x)((1/5)*x)*I(x<=5)+(1+5*(x-5))*I(x>=5&x<=10)
curve(lambda(x),xlim=c(0,10),ylab="lambda(x)")

#algoritmo

S<-vector()
u1<-vector()
u2<-vector()
procpois<-function(T) {t<-0;I<-0  
      for (i in 1:999) {u1[i]<-runif(1); t<-t-((1/20)*log(u1[i]))
      if (t>T) {print(i);break}
      if (t<=T) u2[i]<-runif(1)
      if(u2[i]<=(lambda(t)/20)) {I<-I+1;S[I]<-t}}
print("S")
print(S)
print("tasa de aceptación")
print(length(S)/i)
print("número medio de realizaciones")
print(i/length(S))
hist(S)
curve(lambda(x),xlim=c(0,10),ylab="lambda(x)",col="red",add=TRUE)
}
procpois(10)

#tiempo de ejecución

system.time(procpois(10))
