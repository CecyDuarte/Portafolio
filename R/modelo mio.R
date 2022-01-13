rm(list=ls())
library(car)
mydata<-read.csv("D:/P2_2.csv")
colnames(mydata)<-c("Y","x")

modelo<-lm(Y ~poly(x, degree=6),data=mydata)
summary(modelo)
coefficients(modelo) #valores de coeficientes
confint(modelo,level=0.95) #intervalos de confianza
fitted(modelo) #y pronosticado
residuals(modelo) #residuales
anova(modelo) #analisis de varianza

car::vif(modelo)

modelonuevo<-lm(Y~ x,data=mydata)
summary(modelonuevo)


