library("quantmod")

prices = getSymbols("AC.MX", from = "2018-06-01", to = "2021-06-01")

head(`AC.MX`,n=3)

tail(`AC.MX`,n=3)

chartSeries(`AC.MX`,
            type="line",
            subset='2018-06::2021-06',
            theme=chartTheme('black'))

chartSeries(`AC.MX`,
            type="bar",
            subset='2018-06::2021-06',
            theme=chartTheme('black'))

chartSeries(`AC.MX`,
            type="candlesticks",
            subset='2021-05',
            up.col = 'white',
            down.col = 'black',
            theme=chartTheme('black'))

chartSeries(`AC.MX`,
            subset='2018-06::2021-06',
            theme=chartTheme('black')
            )
addSMA(n=200,on=1,col = "red")

myBBands <- function (price,n,sd){
  mavg <- SMA(price,n)
  sdev <- rep(0,n)
  N <- nrow(price)
  for (i in (n+1):N){
    sdev[i]<- sd(price[(i-n+1):i])
  }
  sdev <- sqrt((n-1)/n)*sdev
  up <- mavg + sd*sdev
  dn <- mavg - sd*sdev
  pctB <- (price - dn)/(up - dn)
  output <- cbind(dn, mavg, up, pctB)
  colnames(output) <- c("dn", "mavg", "up", 
        "pctB")
  return(output)
}
p<-na.omit(`AC.MX`)
bb <-myBBands(Cl(p),n=20,sd=2)
tail(bb,n=5)

chartSeries(`AC.MX`,
            subset='2018-06::2021-06',
            theme=chartTheme('black'))
addBBands(n=20,sd=2)

library("PerformanceAnalytics")

priceAC<-AC.MX$AC.MX.Close

length(priceAC)

min(priceAC)

max(priceAC)

retorno<-Return.calculate(priceAC)

tail(retorno, n=10)

library(tseries)
library(TSA)

priceSerie <- ts(priceAC, start= c(2018,104), end = c(2021, 103), frequency = 251)

head(priceSerie)

tail(priceSerie)

start(priceSerie)
end(priceSerie)
class(priceSerie)
tail(time(priceSerie),12)

plot(priceSerie)

tend <- lm(priceSerie~time(priceSerie), data = priceSerie)

summary(tend)

tc <- as.numeric(time(priceSerie))
tenc<-lm(priceSerie~poly(tc, degree=4))
summary(tenc)

logtime <-log(time(priceSerie))
tlog<-lm(priceSerie~logtime)
summary(tlog)

logPrice <- log(priceSerie)
te <- lm(logPrice~time(priceSerie))
summary(te)

adf.test(priceSerie, alternative="stationary")

C1 <- diff(log(priceSerie))

plot(C1)

tc <- as.numeric(time(C1))
tenc<-lm(C1~poly(tc, degree=4))
summary(tenc)

adf.test(C1, alternative="stationary")

acf(C1) 
pacf(C1) 

ar(C1)

AIC(arima(C1, order = c(1,0,0), method = 'ML'))

aic0<-Inf # le das un numero muy grande
for (i in 1:20)
{
  aic1<-AIC(arima(C1, order = c(0,0,i), method ="ML" ))
  if(aic1<aic0)
  {
    aic0<-aic1
    MA.mejor<-i
  }
}
MA.mejor
aic0 

arma.aic<-Inf
for(j in 1:8)
{
  for (i in 1:15)
  {
    aux<-AIC(arima(C1, order = c(j,0,i), method ="ML" ))
    if(aux<arma.aic)
    {
      arma.aic<-aux
      ARMA.mejor<-c(j,i)
    }
  }
}
ARMA.mejor
arma.aic

res<-residuals(arima(C1, order=c(1,1,1), method = 'ML'))

mean(res)

plot(res, type="p")

acf(res) 
pacf(res) 

Box.test(res)

shapiro.test(res)

tail(priceSerie)

Pred <- predict(arima(priceSerie, order=c(6, 1,9), method="ML"), n.ahead=20)$pred
head(Pred)

plot(priceSerie,
    main = 'Predicciones a 20 dias al futuro',
    xlab = 'Fecha',
    ylab = 'Precio',
    xlim = c(2018.4, 2021.5))
lines(Pred, 
      col="red")
legend('topleft', 
       legend = c('Predicción'),
        col = c('red'),
      lty = 1)

plot(priceSerie,
    main = 'Predicciones a 20 dias al futuro. Gráfico aumentado.',
    xlab = 'Fecha',
    ylab = 'Precio',
    xlim = c(2021.35, 2021.5),
    ylim = c(100, 115))
lines(Pred, 
      col="red")
legend('bottomright', 
       legend = c('Predicción'),
        col = c('red'),
      lty = 1)

