## Importación de los datos
# Cargamos las bibliotecas necesarias

library(dplyr)
library(readr)

# Leemos el archivo "Verizon.csv" y filtramos los datos para ILEC y CLEC

Verizon <- read_csv("Verizon.csv")

ILEC<- Verizon %>%
  filter(Group == "ILEC") %>%
  select(Time) %>%
  as.vector()
ILEC<-ILEC$Time

CLEC<- Verizon %>%
  filter(Group == "CLEC") %>%
  select(Time) %>%
  as.vector()
CLEC<-CLEC$Time

#Punto 1.1

# Definimos los hiperparámetros del modelo y calculamos s_k y n para cada población

ak=3
bk=17

sk1<-sum(ILEC)
n1<-length(ILEC)

sk2<-sum(CLEC)
n2<-length(CLEC)

# Gráfico de las distribuciones posteriores lambda_1 y lambda_2

par(mfrow = c(1, 2), mar = c(3, 3, 1.5, 1.5), mgp = c(1.75, 0.75, 0))

# Curva posterior lambda_1 (ILEC)
curve(expr = dgamma(1/x, shape = ak + n1, rate = bk + sk1),
      from = 0, to = 45, n = 1000, col = 3, lwd = 2, ylim = c(0, 150),
      xlab = expression(lambda), ylab = "Densidad", main = "")

# Curva posterior lambda_2 (CLEC)
curve(expr = dgamma(1/x, shape = ak + n2, rate = bk + sk2),
      from = 0, to = 45, n = 1000, col = 2, lwd = 2, ylim = c(0, 150),
      xlab = expression(lambda), ylab = "Densidad", add = TRUE)

# Leyendas
legend("topright", legend = c(expression(paste("Posterior (ILEC)")),
                              expression(paste("Posterior (CLEC)"))),
       col = c(3, 2), lty = 1, lwd = 2, bty = "n")

# Curva previa
curve(expr = dgamma(1/x, shape = ak , rate = bk ),
      from = 0, to = 60, n = 1000, col = 1, lwd = 2, ylim = c(0, 35),
      xlab = expression(lambda), ylab = "Densidad", main = "")

# Curva posterior lambda_2
curve(expr = dgamma(1/x, shape = ak + n2, rate = bk + sk2),
      from = 0, to = 60, n = 1000, col = 2, lwd = 2, ylim = c(0, 35),
      xlab = expression(lambda), ylab = "Densidad", add = TRUE)

# Añadimos leyendas
legend("topright", legend = c("Previa", expression(paste("Posterior (CLEC)"))),
       col = c(1, 2), lty = 1, lwd = 2, bty = "n")

# Cargar el paquete invgamma
library("invgamma")

# Calcular Estadisticas
Media_ILEC <- (bk + sk1) / (ak + n1)
intervalo1 <- qinvgamma(c(0.025, 1 - 0.025), ak + n1, bk + sk1)
Media_CLEC <- (bk + sk2) / (ak + n2)
intervalo2 <- qinvgamma(c(0.025, 1 - 0.025), ak + n2, bk + sk2)

# Crear un marco de datos
tabla <- data.frame(
  Grupo = c("ILEC", "CLEC"),
  Media = c(Media_ILEC, Media_CLEC),
  Intervalo_Inf = c(intervalo1[1], intervalo2[1]),
  Intervalo_Sup = c(intervalo1[2], intervalo2[2])
)

# Formatear la tabla para LaTeX
tabla_latex <- xtable(tabla, caption = "Valores para ILEC y CLEC", label = "tabla:valores")

# Imprimir la tabla en formato LaTeX
print(tabla_latex, caption.placement = "top", include.rownames = FALSE, hline.after = c(-1, 0, nrow(tabla)))


# Número de muestras 
B=10000

# semilla
set.seed(1234)

#Generacion de B muestras de gammas inversas
lambda1=rgamma(B,ak+n1,bk+sk1)
lambda1=1/lambda1

lambda2=rgamma(B,ak+n2,bk+sk2)
lambda2=1/lambda2


# Calcular eta como la diferencia entre lambda1 y lambda2
eta<-lambda1-lambda2

# gráfico distribución posterior
par(mfrow = c(1,1), mar = c(3,3,1.4,1.4), mgp = c(1.75,0.75,0))
hist(x = eta, freq = FALSE, col = "gray95", border = "gray95", xlab = expression(eta), ylab = "Densidad", main = expression(paste("Distribución posterior de ", eta)))
lines(density(eta), col = "gray", lwd = 2)
# Línea vertical en la media
abline(v = mean(eta), col = 2, lty = 3)
# Intervalo de credibilidad al 95%
abline(v = quantile(eta, c(.025,.975)), col = 4, lty = 3)
#Leyenda
legend("topleft", legend = c("Posterior", "Media", "Intervalo al 95%"), col = c("gray",2,4), lwd = 2, bty = "n")


# Calcular estadísticas de interés
mean(eta)
median(eta)
quantile(eta, c(0.025, 0.975))
sd(eta) / abs(mean(eta))
mean(eta > 0)


#Punto 1.2

# Hiperparámetros
a<-c(3,2,3,2)
b1<-c(17,8.5,16.8,8.4)
b2<-c(17,8.5,33,16.5)

# Almacenar resultados
Media<-NULL
CV<-NULL
SD<-NULL
P25<-NULL
P975<-NULL



# Realizar el procedimiento varias veces y almacenar los resultados 
for (i in 1:4){
  set.seed(1234)
  lambda1 = rgamma(B, a[i] , b1[i] )
  lambda1 = 1 / lambda1
  set.seed(1234)
  lambda2 = rgamma(B, a[i], b2[i] )
  lambda2 = 1 / lambda2
  
  eta = lambda1 - lambda2
  Media[i] <- mean(eta)
  CV[i] <- sd(eta) / abs(mean(eta))
  SD[i]<-sd(eta)
}

# Resultados
CV
Media
SD

# Almacenar resultados de sensibilidad en una matriz
sensibilidad<-matrix(,nrow=0,ncol=B)
CV<-NULL
Media<-NULL
for (i in 1:4){
  set.seed(1234)
  lambda1 = rgamma(B, a[i] + n1, b1[i] + sk1)
  lambda1 = 1 / lambda1
  set.seed(1234)
  lambda2 = rgamma(B, a[i] + n2, b2[i] + sk2)
  lambda2 = 1 / lambda2
  
  eta = lambda1 - lambda2
  sensibilidad<-rbind(sensibilidad,eta)
  Media[i] <- mean(eta)
  CV[i] <- sd(eta) / abs(mean(eta))
  P25[i]<-quantile(eta,0.025)
  P975[i]<-quantile(eta,1-0.025)
  
}

# Resultados
Media
CV
P25
P975

# Gráficos de histogramas de sensibilidad
par(mfrow = c(2, 2), mar = c(3, 3, 1.4, 1.4), mgp = c(1.75, 0.75, 0))

hist(x = sensibilidad[1, ], freq = FALSE, col = "gray95", border = "gray95",
     xlab = expression(eta), ylab = "Densidad", main = expression(paste(a[k] == 3, " y ", b[k] == 17)))
lines(density(sensibilidad[1, ]), col = "gray", lwd = 2)
abline(v = mean(sensibilidad[1, ]), col = 2, lty = 3)
abline(v = quantile(sensibilidad[1, ], c(0.025, 0.975)), col = 4, lty = 3)


hist(x = sensibilidad[2, ], freq = FALSE, col = "gray95", border = "gray95",
     xlab = expression(eta), ylab = "Densidad", main = expression(paste(a[k] == 2, " y ", b[k] == 8.5)))
lines(density(sensibilidad[2, ]), col = "gray", lwd = 2)
abline(v = mean(sensibilidad[2, ]), col = 2, lty = 3)
abline(v = quantile(sensibilidad[2, ], c(0.025, 0.975)), col = 4, lty = 3)


hist(x = sensibilidad[3, ], freq = FALSE, col = "gray95", border = "gray95",
     xlab = expression(eta), ylab = "Densidad", main = expression(paste(a[k] == 3, " , ", b[1] == 16.8, " y ", b[2] == 33)))
lines(density(sensibilidad[3, ]), col = "gray", lwd = 2)
abline(v = mean(sensibilidad[3, ]), col = 2, lty = 3)
abline(v = quantile(sensibilidad[3, ], c(0.025, 0.975)), col = 4, lty = 3)


hist(x = sensibilidad[4, ], freq = FALSE, col = "gray95", border = "gray95",
     xlab = expression(eta), ylab = "Densidad", main = expression(paste(a[k] == 1, " , ", b[1] == 8.4, " y ", b[2] == 16.5)))
lines(density(sensibilidad[4, ]), col = "gray", lwd = 2)
abline(v = mean(sensibilidad[4, ]), col = 2, lty = 3)
abline(v = quantile(sensibilidad[4, ], c(0.025, 0.975)), col = 4, lty = 3)



B <- 10000
# Generar B muestras de la posterior para ILEC y CLEC

#Punto 1.3
set.seed(1234)
lambda1=rgamma(B,ak+n1,bk+sk1)
lambda1=1/lambda1
set.seed(1234)
lambda2=rgamma(B,ak+n2,bk+sk2)
lambda2=1/lambda2
outILEC<-NULL
outCLEC<-NULL


# Calcular estadísticas observadas para ILEC y CLEC
tobsILEC <- c(mean(ILEC), sd(ILEC))
tobsCLEC <- c(mean(CLEC), sd(CLEC))

# Tomar las B muestras de la posterior y evaluarlas en los datos observados
set.seed(1234)
for (i in 1:B) {
  # Predicción
  yILEC_rep <- rexp(n = n1, 1/lambda1[i])
  yCLEC_rep <- rexp(n = n2, 1/lambda2[i])
  # Calcular estadísticas de prueba
  outILEC <- rbind(outILEC, c(mean(yILEC_rep), sd(yILEC_rep)))
  outCLEC <- rbind(outCLEC, c(mean(yCLEC_rep), sd(yCLEC_rep)))
}

# Calcular ppp
mean(outILEC[,1]>mean(ILEC))
mean(outILEC[,2]>sd(ILEC))

mean(outCLEC[,1]>mean(CLEC))
mean(outCLEC[,2]>sd(CLEC))

# Gráficos de histogramas y dispersión
par(mfrow = c(2,2), mar = c(3,3,1.4,1.4), mgp = c(1.75,0.75,0))
tipo <- c("ILEC","CLEC")
nombres <- c("Media","D. Estandar")
colores1 <- c("mistyrose","lightblue1")
colores2 <- c(2, 4)
for (s in tipo) {
  tobs <- get(x = paste0("tobs", s))
  out  <- get(x = paste0("out",  s))
  for (j in 1:2) {
    ppp <- mean(out[,j] > tobs[j])
    hist(x = out[,j], freq = F, col = colores1[j], border = colores1[j], xlab = nombres[j], ylab = "Densidad", main = paste0("", s, ", ppp = ", round(ppp, 2)))
    abline(v = tobs[j], lty = 2)
    abline(v = quantile(out[,j], c(.025,.975)), lty = 1, col = colores2[j])
  }
}

# Gráficos de dispersión para ILEC y CLEC
par(mfrow = c(1,2), mar = c(3,3,1.4,1.4), mgp = c(1.75,0.75,0))


# Diagrama de dispersión para ILEC
plot(outILEC[,1], outILEC[,2], 
     pch = 19,               
     col = "#CCCCCC",       
     xlab = "Media",     
     ylab = "D. Estandar",     
     main = "Diagrama de Dispersión - ILEC",
     ylim = c(min(outILEC[, 2]), sd(ILEC))
     
)
# Intervalos de credibilidad
abline(v = quantile(outILEC[,1], c(.025,.975)), lty = 1, col = 2)
abline(h = quantile(outILEC[,2], c(.025,.975)), lty = 1, col = 4)

# Media y desviación estándar real
points(mean(ILEC), sd(ILEC), pch = 19, col = "darkblue")

# Diagrama de dispersión para CLEC
plot(outCLEC[,1], outCLEC[,2], 
     pch = 19,               
     col = "#CCCCCC",       
     xlab = "Media",     
     ylab = "D. Estandar",     
     main = "Diagrama de Dispersión - CLEC"
)
# Intervalos de credibilidad

abline(v = quantile(outCLEC[,1], c(.025,.975)), lty = 1, col = 2)
abline(h = quantile(outCLEC[,2], c(.025,.975)), lty = 1, col = 4)

# Media y desviación estándar real
points(mean(CLEC), sd(CLEC), pch = 19, col = "darkblue")


# Diagrama de dispersión con histogramas a los lados
library(ggplot2)
library(ggExtra)

outILEC<-as.data.frame(outILEC)

str(outILEC)

# Guardar el gráfico de dispersión en una variable
p <- ggplot(outILEC, aes(x = V1, y = V2)) +
  geom_point()+labs(title="Dispersograma - ILEC",x="Media",
                   y="Des. Estandar",color="City")+ 
  geom_hline(yintercept=quantile(outILEC$V2,0.025), linetype="dashed", color = "blue")+
  geom_hline(yintercept=quantile(outILEC$V2,1-0.025), linetype="dashed", color = "blue")+
  geom_vline(xintercept=quantile(outILEC$V1,0.025), linetype="dashed", color = "red")+
  geom_vline(xintercept=quantile(outILEC$V1,1-0.025), linetype="dashed", color = "red")

# Argumentos para cada histograma marginal
plot2<-ggMarginal(p, type = "histogram", 
           xparams = list(fill = 2),
           yparams = list(fill = 4))


outCLEC<-as.data.frame(outCLEC)

str(outCLEC)

str(outCLEC)

# Guarda el gráfico de dispersión en una variable
p <- ggplot(outCLEC, aes(x = V1, y = V2)) +
  geom_point()+labs(title="Dispersograma - CLEC",x="Media",
                    y="Des. Estandar",color="City")+ 
  geom_hline(yintercept=quantile(outCLEC$V2,0.025), linetype="dashed", color = "blue")+
  geom_hline(yintercept=quantile(outCLEC$V2,1-0.025), linetype="dashed", color = "blue")+
  geom_vline(xintercept=quantile(outCLEC$V1,0.025), linetype="dashed", color = "red")+
  geom_vline(xintercept=quantile(outCLEC$V1,1-0.025), linetype="dashed", color = "red")
  
# Argumentos para cada histograma marginal
plot1<-ggMarginal(p, type = "histogram", 
           xparams = list(fill = 2),
           yparams = list(fill = 4))

library("gridExtra")
grid.arrange(plot1, plot2, ncol=2)
###########################################################################################################################
# Punto 2
 
# Estimación de eta, varianza de eta y coeficiente de variación de eta
Eta<-mean(ILEC)-mean(CLEC)
Vareta<-((mean(ILEC)^2/length(ILEC))+(mean(CLEC)^2/length(CLEC)))
CV<-((Vareta)^{0.5})/abs(Eta)

# Intervalo de confianza MLE asintótico
Intervalo<-c(Eta-(1.96*(Vareta^0.5)),Eta+(1.96*(Vareta^0.5)))

# Bootstrap no paramétrico
set.seed(1234)
NoParametrico<-NULL
for (i in 1:10000){
  # Calcular B muestras de la diferencia de medias de muestras de ILEC y CLEC 
  NoParametrico[i]<-mean(sample(ILEC,replace = T))-mean(sample(CLEC,replace = T))
}

par(mfrow = c(1,2), mar = c(3,3,1.4,1.4), mgp = c(1.75,0.75,0))

hist(x = NoParametrico, freq = FALSE, col = "gray95", border = "gray95", xlab = expression(eta), ylab = "Frecuencia", main = 'Boostrap No Parametrico')
lines(density(NoParametrico), col = "gray", lwd = 2)
abline(v = mean(NoParametrico), col = 2, lty = 3)
abline(v = quantile(NoParametrico, c(.025,.975)), col = 4, lty = 3)
legend("topleft", legend = c("Posterior", "Media", "Intervalo al 95%"), col = c("gray",2,4), lwd = 2, bty = "n")

# Bootstrap paramétrico

ybarra1<-mean(ILEC)
ybarra2<-mean(CLEC)
set.seed(1234)
Parametrico<-NULL
for (i in 1:10000){
  # Calcular B muestras de la diferencia de medias de muestras de ILEC y CLEC 
  # generadas por distribuciones exponenciales de parámetro ybarra
  Parametrico[i]<-mean(rexp(length(ILEC),1/ybarra1))-mean(rexp(length(CLEC),1/ybarra2))
}
hist(x = Parametrico, freq = FALSE, col = "gray95", border = "gray95", xlab = expression(eta), ylab = "Frecuencia", main =  'Boostrap Parametrico')
lines(density(Parametrico), col = "gray", lwd = 2)
abline(v = mean(Parametrico), col = 2, lty = 3)
abline(v = quantile(Parametrico, c(.025,.975)), col = 4, lty = 3)
legend("topleft", legend = c("Posterior", "Media", "Intervalo al 95%"), col = c("gray",2,4), lwd = 2, bty = "n")

# Resultados

Eta
CV
Intervalo

mean(NoParametrico)
sd(NoParametrico)/abs(mean(NoParametrico))
quantile(NoParametrico, c(.025,.975))
mean(Parametrico)
sd(Parametrico)/abs(mean(Parametrico))
quantile(Parametrico, c(.025,.975))


############################################################################################################

#Punto 3


# Carga de bibliotecas necesarias
library(foreach)
library(doParallel)
library(dplyr)
library(readr)

# Carga de datos desde un archivo CSV llamado "Verizon.csv"
Verizon <- read_csv("Verizon.csv")

# Filtrado y selección de datos para el grupo "ILEC"
ILEC<- Verizon %>%
  filter(Group == "ILEC") %>%
  select(Time) %>%
  as.vector()
ILEC<-ILEC$Time


# Filtrado y selección de datos para el grupo "CLEC"
CLEC<- Verizon %>%
  filter(Group == "CLEC") %>%
  select(Time) %>%
  as.vector()
CLEC<-CLEC$Time

# Definimos los tamaños de muestra (n) que queremos evaluar
n <- c(10, 20, 50, 100)

# Declaración de variables
l1 <- mean(ILEC)
l2 <- mean(CLEC)
etaT <- l1 - l2
l1 <- mean(ILEC)
l2 <- mean(CLEC)
l1 <- 1 / l1
l2 <- 1 / l2
# Detectamos el número de núcleos disponibles en el sistema
cores <- detectCores()

#Creamos un clúster de núcleos para el procesamiento paralelo
cl <- makeCluster(cores-1)
registerDoParallel(cl)
m=100000
B=10000
# Cambiamos de n cada vez que termine el proceso 
n=n[3]

#Inicialización de matrices para muestras
y1<-matrix(,m,n)
y2<-matrix(,m,n)

# Generación de muestras exponenciales para y1 y y2 
set.seed(1234)
y1<-foreach(j = 1:m,.combine=rbind) %dopar% {rexp(n, l1)}
set.seed(1234)
y2<-foreach(j = 1:m,.combine=rbind) %dopar% {rexp(n, l2)}


# Cálculo de estadísticas suficientes para y1 y y2
s_y1<-foreach(j = 1:m,.combine=rbind) %dopar% {
  sum(y1[j,])
}
s_y2<-foreach(j = 1:m,.combine=rbind) %dopar% {
  sum(y2[j,])
}

# Cálculo de intervalos de credibilidad
set.seed(1234)
eta__B <- foreach(j = 1:m, .combine = rbind) %dopar% {
  eta_b <- 1/rgamma(B, 3 + n, 17 + s_y1[j,]) - 1/rgamma(B, 3 + n, 17 + s_y2[j,])
  quantiles <- quantile(eta_b, c(0.025, 0.975))
  return(quantiles)
}


# Verificar si el parámetro está dentro de los intervalos
PB <- foreach(j = 1:m, .combine = rbind) %dopar% {
  (eta__B[j, 1] <= etaT & etaT <= eta__B[j, 2])
}
# Calcular el promedio de PB
mean_PB <- apply(PB, 2, mean)

# Liberar memoria eliminando variables innecesarias 
rm(eta__B,PB,s_y2,s_y1)

# Cálculo de intervalos asintóticos mediante MLE
E_MLE <- foreach(j = 1:m, .combine = rbind) %dopar% {
  m__y1 <- mean(y1[j,])
  m__y2 <- mean(y2[j,])
  vareta <- sqrt((m__y1^2 + m__y2^2) / n)
  LI<-m__y1-m__y2-1.96*vareta
  LS<-m__y1-m__y2+1.96*vareta
  return(list(LI=LI,LS=LS))
}

# Verificar si el parámetro está dentro de los intervalos MLE 

PMLE <- foreach(j = 1:m, .combine = rbind) %dopar% {
  (E_MLE[j, 1] <= etaT & etaT <= E_MLE[j, 2])
  
}
# Calcular el promedio de PMLE
mean_MLE <- apply(PMLE, 2, mean)

# Liberar memoria eliminando variables innecesarias
rm(PMLE,E_MLE)

# Función para realizar bootstrap No paramétrico y obtener cuantiles
calcularBootstrapNNP <- function(y1, y2, B) {
  NNP <- numeric(B)
  
  for (k in 1:B) {
    NNP[k] <- mean(sample(y1, replace = TRUE)) - mean(sample(y2, replace = TRUE))
  }
  quantiles <- quantile(NNP, c(0.025, 0.975))
  return(quantiles)
}

# Calcular cuantiles del bootstrap No paramétrico utilizando la función
set.seed(1234)
E_BNP <- foreach(j = 1:m, .combine = rbind) %dopar% {
  calcularBootstrapNNP(y1[j,], y2[j,], B = B)
}


# Verificar si el parámetro está dentro de los intervalos BNP
BNP <- foreach(j = 1:m, .combine = rbind) %dopar% {
  (E_BNP[j, 1] <= etaT & etaT <= E_BNP[j, 2])
}

# Calcular el promedio de BNP
mean_BNP <- apply(BNP, 2, mean)

# Liberar memoria eliminando variables innecesarias
rm(E_BNP,BNP)

# Función para realizar bootstrap paramétrico (otra variante)
calcularBootstrapPP <- function(y1, y2, B) {
  PP <- numeric(B)
  
  for (k in 1:B) {
    PP[k] <- mean(rexp(n, 1 / mean(y1))) - mean(rexp(n, 1 / mean(y2)))
  }
  quantiles <- quantile(PP, c(0.025, 0.975))
  return(quantiles)
}

set.seed(1234)
# Calcular cuantiles del bootstrap paramétrico 
E_BP <- foreach(j = 1:m, .combine = rbind) %dopar% {
  calcularBootstrapPP(y1[j,], y2[j,], B = B)
}

# Verificar si el parámetro está dentro de los intervalos BPP
BPP <- foreach(j = 1:m, .combine = rbind) %dopar% {
  (E_BP[j, 1] <= etaT & etaT <= E_BP[j, 2])
}
#Resultados
mean_BPP <- apply(BPP, 2, mean)
rm(E_BP ,BPP)
rm(y1,y2)
mean_PB
mean_MLE
mean_BNP
mean_BPP
