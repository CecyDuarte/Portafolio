library(readr)
library(BoutrosLab.plotting.general)
library(KScorrect)
library(dplyr)

# DEFINICION DE LAS FUNCIONES

# Obtener el VC de Lilifors
VCLillifors <- function(alfa=.95){
  nreps <- 9999
  x <- rnorm(25)
  Lc <- LcKS(mydata$datos, "pnorm", nreps = nreps)
  sim.Ds <- sort(Lc$D.sim)
  crit <- round(c(alfa) * nreps, 0)
  # Lilliefors' (1967) critical values, using improved values from
  #   Parsons & Wirsching (1982) (for n = 25):
  # 0.141 0.148 0.157 0.172 0.201
  VCLillie <- round(sim.Ds[crit], 3)
  return(VCLillie)
}

# Crear la tabla y estadistico para Lilliefors
Lilliefors <- function(dp, a){
  dp <- arrange(dp, datos) # Ordena los datos por la columna datos
  xbarra = mean(dp$datos) # Calcular la media
  de = sd(dp$datos) # Calcular lo desviacion estandar
  dp$indice = which(dp$datos == dp$datos) # Crea una columna con el index del numero
  dp$Fn = dp$indice/length(dp$datos) # Crea la columna de la probabilidad esperada
  dp$Zi = (dp$datos - xbarra)/de # Crea la columna de Zi
  dp$Fo = pnorm(dp$Zi) # Crea la columna de probabilidad observada
  dp$dif = abs(dp$Fn - dp$Fo) # Crea la columna de diferencia de FN-F0
  Estimador <- max(dp$dif) # Encuentro el mayor valor de las diferencias
  ValorCritico <- VCLillifors((1-a))
  if (Estimador > ValorCritico){
    Conclusion <- "Se rechaza H0"
  }else{
    Conclusion <- "No se rechaza H0"
  }
  Resultados <- data.frame(Estimador, ValorCritico, Conclusion)
  writeLines("\nEstos son los resultados de la prueba:\n")
  print(Resultados)
}

# Crear la tabla y estadistico para Kolmogorov
Kolmogorov <- function(dp, xbarra, de, a){
  dp <- arrange(dp, datos) # Ordena los datos por la columna datos
  dp$indice = which(dp$datos == dp$datos) # Crea una columna con el index del numero
  dp$Fn = dp$indice/length(dp$datos) # Crea la columna de la probabilidad esperada
  dp$Zi = (dp$datos - xbarra)/de # Crea la columna de Zi
  dp$Fo = pnorm(dp$Zi) # Crea la columna de probabilidad observada
  dp$dif = abs(dp$Fn - dp$Fo) # Crea la columna de diferencia de FN-F0
  Estimador <- max(dp$dif) # Encuentro el mayor valor de las diferencias
  ValorCritico <- ks.test.critical.value(15, (1-a), alternative = "two-sided")
  if (Estimador > ValorCritico){
    Conclusion <- "Se rechaza H0"
  }else{
    Conclusion <- "No se rechaza H0"
  }
  Resultados <- data.frame(Estimador, ValorCritico, Conclusion)
  writeLines("\nEstos son los resultados de la prueba:\n")
  print(Resultados)
}

# Crear la tabla para Chi cuadrada
ChiCuadrada <- function(dp, xbarra, de, PC, a){
  # xbarra = mean(dp$datos) # Calcular la media
  # de = sd(dp$datos) # Calcular lo desviacion estandar
  
  # Crear tabla de intervalos
  k <- length(dp$datos)/5
  intervalos <- c(1:(k-1))
  intervalos <- data.frame(intervalos)
  intervalos$Acum <- intervalos$intervalos/k
  intervalos$Z <- qnorm(intervalos$Acum)
  intervalos$X <- intervalos$Z*de + xbarra
  
  # Crear tabla de frecuencias observadas y esperadas
  interX <- c(1:k)
  frecuencias <- data.frame(interX)
  frecuencias$Ei <- rep(5,k)
  frecuencias$Oi <- rep(0,k)
  # Categoriza las observaciones en los intervalos correspondientes
  for (i in dp$datos){
    if (i < intervalos[1,4]){
      frecuencias[1,3] <- frecuencias[1,3] + 1
    }
  }
  
  for (j in 2:(k-1)){
    for (i in dp$datos){
      if (i > intervalos[(j-1),4] & i<intervalos[j,4]){
        frecuencias[j,3] <- frecuencias[j,3] + 1
      }
    }
  }
  for (i in dp$datos){
    if (i > intervalos[(k-1),4]){
      frecuencias[k,3] <- frecuencias[k,3] + 1
    }
  }
  # (Oi-Ei)^2 / Ei
  frecuencias$est <- ((frecuencias$Oi - frecuencias$Ei)^2) / frecuencias$Ei
  Estimador <- sum(frecuencias$est) # Estadistico
  ValorCritico <- qchisq((1-a), (k-1-PC)) # Obtener el VC de Chi Cuadrada
  if (Estimador > ValorCritico){
    Conclusion <- "Se rechaza H0"
  }else{
    Conclusion <- "No se rechaza H0"
  }
  Resultados <- data.frame(Estimador, ValorCritico, Conclusion)
  writeLines("\nEstos son los resultados de la prueba:\n")
  print(Resultados)  
}


# MENU
print("Programa para elaborar pruebas de normalidad\nA continuación presione ENTER y seleccione el archivo CSV con los datos a analizar")
mydata <- read_csv(file.choose())
mydata <- data.frame(mydata) # Convierte los datos en un data frame


while(TRUE){
  writeLines("\n\nSe realizará una prueba para normalidad.\nIngresa el método que deseas utilizar o 0 para terminar\n1 Lilliefors\n2 Kolmogorov\n3 Chi cuadrada\n0 Terminar programa")
  op <- readline(prompt = "Opcion: ")
  op <- as.integer(op)
  if (op == 1){
    alfa <- readline(prompt = "Ingresa el valor de alfa para la prueba: ")
    alfa <- as.double(alfa)
    Lilliefors(mydata, alfa)
  }else if (op == 2){
    media <- as.double(readline(prompt = "Ingresa la media de los datos: "))
    Desv <- as.double(readline(prompt = "Ingresa la desviación estandar: "))
    alfa <- readline(prompt = "Ingresa el valor de alfa para la prueba: ")
    alfa <- as.double(alfa)
    Kolmogorov(mydata, media, Desv, alfa)
  }else if (op == 3){
    writeLines("¿Conoces la media y desviación estandar?\n1 Si\n2 No")
    op2 <- (readline(prompt = ": "))
    op2 <- as.integer(op2)
    if (op2 == 1){
      media <- as.double(readline(prompt = "Ingresa la media de los datos: "))
      Desv <- as.double(readline(prompt = "Ingresa la desviación estandar: "))
      parametros <- 0
      alfa <- readline(prompt = "Ingresa el valor de alfa para la prueba: ")
      alfa <- as.double(alfa)
      ChiCuadrada(mydata, media, Desv, parametros, alfa)
    }else{
      media <- mean(mydata$datos)
      Desv <- sd(mydata$datos)
      parametros <- 2
      alfa <- readline(prompt = "Ingresa el valor de alfa para la prueba: ")
      alfa <- as.double(alfa)
      ChiCuadrada(mydata, media, Desv, parametros, alfa)
    }
  }else{
    print("Programa terminado")
    break
  }
}

