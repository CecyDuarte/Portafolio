########################################################################
# FUNCIONES/// NO MODIFICAR
library(FinancialMath)

D.unif <- function(n,min,max){
  floor(max-min*runif(n)) + 1
}

VPN <- function(tasa, variacion){
  
  variacion <- 0.02
  tasa <- 0.0705
  
  Mercado <- 20000
  ParticipacionMercado <- rnorm(5,0.08, 0.03)
  ParticipacionMercado
  Ventas <- Mercado * ParticipacionMercado
  Ventas
  CostosVentas <- (1.05/(10^ParticipacionMercado))*.46*Ventas
  CostosVentas
  CostosOperacionales <- (0.60+2*ParticipacionMercado)*.40*Ventas
  CostosOperacionales
  
  aumento_precio <- rep(0.02,4)
  aumento_precio <- aumento_precio * (1+(runif(4,0,variacion*2)-variacion))
  aumento_precio <- c(0, aumento_precio)
  aumento_precio
  
  aumento_costo <- rep(0.05,4)
  aumento_costo <- aumento_costo * (1+(runif(4,0,variacion*2)-variacion))
  aumento_costo <- c(0, aumento_costo)
  
  Ventas <- Ventas*(1+aumento_precio)
  CostosVentas <- CostosVentas*(1+aumento_costo)
  
  Ventas
  CostosVentas
  CostosOperacionales
  
  flujo_total <- Ventas - CostosVentas - CostosOperacionales 
  flujo_total
  
  flujo_total/Ventas
  
  Resul <- c(flujo_total, Ventas, CostosVentas, CostosOperacionales)
  return(Resul)
}

Escenarios <- function(n,t,v){
  Optimista <- 0
  Pesimista <- Inf
  Mult_VPN <- c()
  for (i in 1:n) {
    Resultados <- VPN(t,v)
    VPNC <- 0
    for (j in 1:5) {
      VPNC <- VPNC + (Resultados[j] / (1+t)^j)
    }
    VPNC <- VPNC - 2000
    Mult_VPN[i] <- VPNC
    if(VPNC >= max(Mult_VPN)){
      Optimista <- c(Resultados, VPNC)
    }
    if(VPNC <= min(Mult_VPN)){
      Pesimista <- c(Resultados, VPNC)
    }
  }
  
  plot(Mult_VPN, type = 'l', col = 'blue', main = "VPN simulados")
  abline( mean(Mult_VPN), c(0,n),
          col = 'red')
  
  hist(Mult_VPN)
  
  Analisis <- c(mean(Mult_VPN),  mean(Optimista[6:10]), mean(Optimista[11:15]), mean(Optimista[16:20]), mean(Optimista[1:5]), mean(Pesimista[6:10]), mean(Pesimista[11:15]), mean(Pesimista[16:20]),  mean(Pesimista[1:5]), Optimista[21], Pesimista[21])
  return(Analisis)
}


###########################################################################
# EJECUCION DEL PROGRAMA/// MODIFICAR VARIABLES PARA OBTENER CAMBIOS


Simulacion <- Escenarios(1000,.0705,0.02)
Estados <- data.frame(
  'Conceptos' = c('VPN', 'Ventas', 'Costos de ventas', 'Costos operativos', 'Ebitda'),
  "Pesimista" = c(Simulacion[11], Simulacion[6],Simulacion[7],Simulacion[8],Simulacion[9]),
  "Neutral" = c((Simulacion[10]+Simulacion[11])/2, (Simulacion[6]+Simulacion[2])/2,(Simulacion[7]+Simulacion[3])/2,(Simulacion[8]+Simulacion[4])/2,(Simulacion[9]+Simulacion[5])/2),
  "Optimista" = c(Simulacion[10], Simulacion[2],Simulacion[3],Simulacion[4],Simulacion[5])
)
Estados



