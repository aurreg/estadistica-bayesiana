# libreria para generar muestras de la distribucion dirilecht
library(LaplacesDemon)
# resultados de la votaciones en Bogota
nk<-c(493,257,227,48,41,38,28,11,3,54)

# vector de unos para la Dirichlet
I<-rep(1,length(nk))
# Definición de la función MCMC para el muestreo de Gibbs
MCMC <- function(B, nk, delta) {
  acs <- 0
  alpha <- 1
  
  # Almacenamiento
  THETA <- matrix(data = NA, nrow = B, ncol = length(nk) + 1)  # Matriz para almacenar resultados
  LL <- matrix(data = NA, nrow = B, ncol = 1)  # Matriz para almacenar log-verosimilitud
  j <- 1  # Contador
  
  # Cadena MCMC
  for (b in 1:B) {
    
    # Muestreo de theta_k mediante Dirichlet
    thetak <- rdirichlet(1, alpha * I + nk)
    
    # Muestreo de alpha mediante Metropolis
    alpha.star <- exp(rnorm(1,log(alpha), sd = delta))
    
    # Cálculo de la función de aceptación y rechazo
    log.r <- ddirichlet(thetak, alpha.star * I, TRUE) + dgamma(alpha.star, 1, 1, log = TRUE) -
      ddirichlet(thetak, alpha * I, TRUE) - dgamma(alpha, 1, 1, log = TRUE) + alpha.star - alpha
    
    # Aceptación o rechazo del nuevo valor de alpha
    if (runif(1) < exp(log.r)) { 
      alpha <- alpha.star 
      acs <- acs + 1 
    }
    
    if (b >= 1000 & ((b - 1) %% 10) == 0) {
      # Muestreo sistemático
      THETA[j, ] <- c(thetak, alpha)
      
      # Cálculo de log-verosimilitud
      LL[j] <- sum(dmultinom(nk, prob = thetak, log = TRUE))
      j <- j + 1
    }
  }
  
  # Asignar nombres de columnas a las matrices resultantes
  colnames(THETA) <- c(paste0("theta", 1:length(nk)), "alpha")
  colnames(LL) <- c("ll")
  
  # Crear data frames con los resultados
  THETA <- as.data.frame(THETA[1:j - 1, ])
  LL <- as.data.frame(LL[1:j - 1])
  
  # Calcular la tasa de aceptación corregida
  acs <- acs / B
  
  # Devolver resultados como una lista
  return(list(THETA = THETA, LL = LL, acs = acs))
}

# Configuración de la semilla aleatoria
set.seed(123)

# Ejecución de la función MCMC con 110,000 iteraciones
# para obtener 100,000 muestras (descartando las primeras 10,000)
Dirichlet <- MCMC(110000, nk, 1)
Dirichlet$acs
#######################################resultados#######################################################
Nombres<-c('C. F. Galan','G. Bolivar','J. D. Oviedo')
Estimacion<-apply(Dirichlet$THETA[,1:3],2,mean)
Q.25<-apply(Dirichlet$THETA[,1:3],2,function(x) quantile(x,0.025))
Q.975<-apply(Dirichlet$THETA[,1:3],2,function(x) quantile(x,0.975))
Registaduria<-c(49.02,20.10,18.71)
resultados <- as.data.frame(cbind(Candidato = Nombres, 
                                  Estimacion = round(Estimacion, 3)*100, 
                                  Q.25 = round(Q.25, 3)*100, 
                                  Q.975 = round(Q.975, 3)*100, 
                                  Registaduria = round(Registaduria, 3)))
resultados
###############################grafico#####################################################
plot(x = c(16,50), y = c(1, 3), type = "n", xlab = "Porcentaje", ylab ='',
     main = "Estimación Bayesiana", yaxt = "n")
abline(h = 1:3, col = "lightgray", lwd = 1)
for (l in 1:3) {
  j <- order(nk[1:3])[l]
  points(x = Registaduria[j], y = l)
  if (resultados$Q.25[j] > Registaduria[j] | resultados$Q.975[j] < Registaduria[j]) {
    lines(x = c(resultados$Q.25[j], resultados$Q.975[j]), y = rep(l, 2), col = 'red')
  }
  if (resultados$Q.25[j] <= Registaduria[j] & resultados$Q.975[j] >=Registaduria[j]) {
    lines(x = c(resultados$Q.25[j], resultados$Q.975[j]), y = rep(l, 2), col = 'green')
  }
}
axis(side = 2, at = 1:3, labels = c('Galan','Bolivar','	Oviedo'), las = 2)
#################################tamaños efectivos####################################################################
library(coda)
Neff_chain <- effectiveSize(Dirichlet$THETA)

se_chain <- apply(Dirichlet$THETA, 2, sd) / sqrt(Neff_chain)

round(cbind(Neff_chain,se_chain),3)
#########################cadenas##############################################
par(mfrow=c(2,2)) 

plot(Dirichlet$THETA$alpha, type = 'l', col = 'gray', xlab='iteracion', ylab = expression(sigma))
plot(Dirichlet$THETA$theta1, type = 'l', col = 'lightblue', xlab='iteracion', ylab = expression(theta[1]))
plot(Dirichlet$THETA$theta2, type = 'l', col = 'lightgreen', xlab='iteracion', ylab = expression(theta[2]))
plot(Dirichlet$THETA$theta3, type = 'l', col = '#D2B48C', xlab='iteracion', ylab = expression(theta[3]))
par(mfrow=c(1,1)) 
plot(Dirichlet$LL$`LL[1:j - 1]`, type = 'l', xlab='iteracion', ylab = 'Log-verosimilitud')
######################validacion##########################################
S<-NULL
validacion<-matrix(,10000,10)
set.seed(123)
for (i in 1:10000){
  validacion[i,]<-(rmultinom(1,1200,Dirichlet$THETA[1,1:10]))
}
######################histogramas ppp#######################################
hist(
  x = validacion[,1], freq = F, col = 'peachpuff', border = 'peachpuff', 
  xlab = '', ylab = "Densidad",
  main = paste0("ppp Galan= ", round(mean(validacion[,1] > nk[1]), 2))
)

abline(v = nk[1], col = "red")
hist(
  x = validacion[,2], freq = F, col = 'lavender', border = 'lavender', 
  xlab = '', ylab = "Densidad",
  main = paste0("ppp  Bolivar= ", round(mean(validacion[,2] > nk[2]), 2))
)
  
  abline(v = nk[2], col = "red")
hist(
  x = validacion[,3], freq = F, col = 'lightpink', border = 'lightpink', 
  xlab = '', ylab = "Densidad",
  main = paste0("ppp Oviedo= ", round(mean(validacion[,3] > nk[3]), 2))
)
  
  abline(v = nk[3], col = "red")

  
  

########################punto 2###################################################

yX.diabetes.train<-as.data.frame(readRDS(file = "yX.diabetes.train"))
yX.diabetes.test<-as.data.frame(readRDS(file='yX.diabetes.test'))


# Unitaria

fit<-lm(y~age + sex + bmi + map + tc + ldl + hdl + tch + ltg + glu +
          I(age^2) + I(bmi^2) + I(map^2) + I(tc^2) + I(ldl^2) + I(hdl^2) + I(tch^2) + I(ltg^2) + I(glu^2) + age:sex + age:bmi +
          age:map + age:tc + age:ldl + age:hdl + age:tch + age:ltg + age:glu + sex:bmi + sex:map + sex:tc +
          sex:ldl + sex:hdl + sex:tch + sex:ltg + sex:glu + bmi:map + bmi:tc + bmi:ldl + bmi:hdl + bmi:tch +
          bmi:ltg + bmi:glu + map:tc + map:ldl + map:hdl + map:tch + map:ltg + map:glu + tc:ldl + tc:hdl +
          tc:tch + tc:ltg + tc:glu + ldl:hdl + ldl:tch + ldl:ltg + ldl:glu + hdl:tch + hdl:ltg + hdl:glu +
          tch:ltg + tch:glu + ltg:glu-1, data=yX.diabetes.train)

library(mvtnorm)


X<-as.matrix(yX.diabetes.train[,2:65])
n<-length(yX.diabetes.train[,1])
y<-as.matrix(yX.diabetes.train[,1])
B.ols<-as.vector(fit$coefficients)
sigma2.ols<-(sum(fit$residuals^2)/fit$df.residual)
Beta.0=B.ols
v.0=1
#####################
# previa u jags
library(R2jags)

# convertir vectores a matrices


y <- c(as.matrix(y))

model.u <- function() {
  for (i in 1:n) {
    y[i] ~ dnorm(inprod(X[i,], beta), phi) # jags trabaja con la dispersion
  }
  beta[1:p] ~ dmnorm(beta0[1:p], SIGMA[1:p,1:p])
  phi ~ dgamma(a0, b0)
}

# previa

beta0 <- B.ols
SIGMA<-n*sigma2.ols*t(X)%*%X
nu0   <- 1
s20   <- sigma2.ols
a0    <- nu0/2
b0    <- nu0*s20/2


# input
model_data.u <- list(y = y, X = X, n = n, p = p, beta0 = beta0, SIGMA = SIGMA, a0 = a0, b0 = b0)
model_parameters.u <- c("beta", "phi")
initial_values.u <- list(list("beta" = beta0, "phi" = 1/s20))
# iteraciones
niter  <- 101000
nburn  <- 1000
nthin  <- 10
nchain <- length(initial_values)

set.seed(123)
fit.u <- jags(data = model_data.u, inits = initial_values.u, 
            parameters.to.save = model_parameters.u, model.file = model.u, 
            n.chains = nchain, n.iter = niter, n.thin = nthin, n.burnin = nburn)
############################
# previa g jags
# modelo
model.g <- function() {
  for (i in 1:n) {
    y[i] ~ dnorm(inprod(X[i,], beta), phi)
  }
  beta[1:p] ~ dmnorm(beta0[1:p], (phi/g)*XtX[1:p,1:p])
  phi ~ dgamma(a0, b0)
}
# previa
beta0 <- c(rep(0,p))
XtX   <- t(X)%*%X
g     <- n
nu0   <- 1
s20   <- sigma2.ols
a0    <- nu0/2
b0    <- nu0*s20/2

model_data.g <- list(y = y, X = X, n = n, p = p, g = g, beta0 = beta0, XtX = XtX, a0 = a0, b0 = b0)


model_parameters.g <- c("beta", "phi")

initial_values.g <- list(list("beta" = beta0, "phi" = 1/s20))
# iteraciones
niter  <- 101000
nburn  <- 1000
nthin  <- 10
nchain <- length(initial_values)

set.seed(123)
fit.g <- jags(data = model_data.g, inits = initial_values.g, 
            parameters.to.save = model_parameters.g, model.file = model.g, 
            n.chains = nchain, n.iter = niter, n.thin = nthin, n.burnin = nburn)

#################################
# regresion rigida jags
model.r <- function() {
  for (i in 1:n) {
    y[i] ~ dnorm(inprod(X[i,], beta), phi)
  }
  beta[1:p] ~ dmnorm(beta0[1:p], (1/(phi*lambda))*I.p[1:p,1:p])
  phi ~ dgamma(a0, b0)
  lambda ~ dgamma(a.l,b.l)
}
beta0<-c(rep(0,p))
I.p<-diag(rep(1,dim(X)[2]))
nu0   <- 1
s20   <- sigma2.ols
a0    <- nu0/2
b0    <- nu0*s20/2
a.l<-1
b.l<-2

model_data.r <- list(y = y, X = X, n = n, p = p, beta0=beta0,I.p=I.p,a0 = a0, b0 = b0, a.l=a.l, b.l=b.l)

model_parameters.r <- c("beta", "phi",'lambda')

initial_values.r <- list(list("beta" = B.ols, "phi" = 1/s20))

# iteraciones
niter  <- 101000
nburn  <- 1000
nthin  <- 10
nchain <- length(initial_values.r)

set.seed(123)
fit.r <- jags(data = model_data.r, inits = initial_values.r, 
              parameters.to.save = model_parameters.r, model.file = model.r, 
              n.chains = nchain, n.iter = niter, n.thin = nthin, n.burnin = nburn)



##################################################
# errores correlacionados jags

n  <- dim(X)[1]
p  <- dim(X)[2] 
DY <- abs(outer( (1:n),(1:n) ,"-")) # para construir la matriz de correlacion



model <- function() {
  y[1:n] ~ dmnorm(X[1:n,1:p]%*%beta[1:p], phi*inverse(ilogit(rho)^DY[1:n,1:n]))
  beta[1:p] ~ dmnorm(beta0[1:p], Omega0[1:p,1:p])
  phi ~ dgamma(a0, b0)
  rho ~ dunif(a, b)
}

# previa
beta0  <- c(rep(0,p))
t2.0<-50^2
Omega0 <- diag(1/t2.0,nrow=p)
a0     <- v.0/2
b0     <- v.0*sigma2.ols/2
a<-0
b<-1

# input
model_data <- list(y = y, X = X, DY = DY, n = n, p = p, beta0 = beta0, Omega0 = Omega0, a0 = a0, b0 = b0, a=a,b=b) 

# parameters
model_parameters <- c("beta","phi","rho")

# initial values
initial_values <- list(list("beta" =beta0 , "phi" = 1/sigma2.ols, "rho" = 0.5))
                       

# mcmc settings
niter  <- 101000
nburn  <- 1000
nthin  <- 10
nchain <- length(initial_values)

# mcmc
set.seed(123)
fit.EC <- jags(data = model_data, inits = initial_values, 
            parameters.to.save = model_parameters, model.file = model, 
            n.chains = nchain, n.iter = niter, n.thin = nthin, n.burnin = nburn)


##########################Graficos y ppp##################################

Theta.u<-fit.u$BUGSoutput$sims.matrix
Theta.g<-fit.g$BUGSoutput$sims.matrix
Theta.r<-fit.r$BUGSoutput$sims.matrix
Theta.Ec<-fit.EC$BUGSoutput$sims.matrix


library(doParallel)
library(foreach)

cores <- detectCores()

cl <- makeCluster(cores-3)

n<-dim(X)[1]
set.seed(24113)
media.u<- foreach(i =1:10000,.combine = cbind)%dopar%{
  Beta<-(Theta.u[i,1:64])
  sigma<-rep(1/Theta.u[i,66],n)
  
  m<-mean(rmvnorm(1,X%*%Beta,diag(sigma)))
  return(m)
}
set.seed(24113)
media.g<- foreach(i =1:10000,.combine = cbind)%dopar%{
  Beta<-(Theta.g[i,1:64])
  sigma<-rep(1/Theta.g[i,66],n)
  
  m<-mean(rmvnorm(1,X%*%Beta,diag(sigma)))
  return(m)
}

set.seed(24113)
media.r<- foreach(i =1:10000,.combine = cbind)%dopar%{
  Beta<-(Theta.r[i,1:64])
  sigma<-rep(1/Theta.r[i,67],n)
  
  m<-mean(rmvnorm(1,X%*%Beta,diag(sigma)))
  return(m)
}

set.seed(24113)
media.Ec<- foreach(i =1:10000,.combine = cbind)%dopar%{
  Beta<-(Theta.Ec[i,1:64])
  sigma<-1/Theta.Ec[i,66]
  SIGMA<-sigma*Theta.Ec[i,67]^DY
  m<-mean(rmvnorm(1,as.vector(X%*%Beta),SIGMA))
  return(m)
}



stop(cluster)
pdf("Validacion.pdf", width=12, height=8)
par(mfrow = c(2, 2))
hist(media.u, main=paste0('Previa unitaria ~ ', round(mean(media.u>mean(y)),3)),xlab='Media', col='lavender',freq=F,ylim= c(0, 11),xlim=c(-0.2,0.3))
abline(v=mean(y),col='red')
hist(media.g, main=paste0('Previa g ~ ', round(mean(media.g>mean(y)),3)),xlab='Media', col= "peachpuff",freq=F, ylim= c(0, 11),xlim=c(-0.2,0.3))
abline(v=mean(y),col='red')
hist(media.r, main=paste0('Rígida ~ ',round(mean(media.r>mean(y)),3)  ),xlab='Media', col="lightblue1",freq=F,ylim= c(0, 11),xlim=c(-0.2,0.3))
abline(v=mean(y),col='red')
hist(media.Ec, main=paste0('Errores Correlacionados ~ ', round(mean(media.Ec>mean(y)),3)),xlab='Media', col="lightgreen",freq=F,ylim= c(0, 11),xlim=c(-0.2,0.3))
abline(v=mean(y),col='red')
dev.off()

yX.diabetes.test<-as.data.frame(readRDS(file='yX.diabetes.test'))
y.test<-as.matrix(yX.diabetes.test[,1])
X.test<-as.matrix(yX.diabetes.test[,2:65])
# Calcular los errores absolutos medios (EAM)

EAM.u <- mean(abs(y.test - X.test %*% fit.u$BUGSoutput$mean$beta))
EAM.g <- mean(abs(y.test - X.test %*% fit.g$BUGSoutput$mean$beta))
EAM.r <- mean(abs(y.test - X.test %*% fit.r$BUGSoutput$mean$beta))
EAM.Ec <- mean(abs(y.test - X.test %*% fit.EC$BUGSoutput$mean$beta))


pdf("ValidacionC.pdf", width=12, height=8)
par(mfrow = c(2, 2))

# Gráfico para Beta.u
plot(y.test, X.test %*% fit.u$BUGSoutput$mean$beta, main = paste("Previa unitaria ~ ", round(EAM.u, 3)),
     ylab = expression(hat(y)), xlab = "y",ylim= c(-4, 3))
lines(y.test, y.test, col = "red", lty = 2)


# Gráfico para Beta.g
plot(y.test, X.test %*% fit.g$BUGSoutput$mean$beta, main = paste("Previa g ~ :", round(EAM.g, 3)),
     ylab = expression(hat(y)), xlab = "y",ylim= c(-4, 3))
lines(y.test, y.test, col = "red", lty = 2)


# Gráfico para Beta.r
plot(y.test, X.test %*% fit.r$BUGSoutput$mean$beta, main = paste("Rígida ~ ", round(EAM.r, 3)),
     ylab = expression(hat(y)), xlab = "y",ylim= c(-4, 3))
lines(y.test, y.test, col = "red", lty = 2)

# Gráfico para Beta.Ec
plot(y.test, X.test %*% fit.EC$BUGSoutput$mean$beta, main = paste("Errores Correlacionados ~ ", round(EAM.Ec, 3)), 
     ylab = expression(hat(y)), xlab = "y",ylim= c(-4, 3))
lines(y.test, y.test, col = "red", lty = 2)
dev.off()


Neff_chain <- effectiveSize(Theta.u)
Neff_chain2 <- effectiveSize(Theta.g)
Neff_chain3 <- effectiveSize(Theta.r)
Neff_chain4 <- effectiveSize(Theta.Ec)


se_chain <- apply(Theta.u, 2, sd) / sqrt(Neff_chain)
se_chain2 <- (apply(Theta.g, 2, sd) / sqrt(Neff_chain2))
se_chain3 <- (apply(Theta.r, 2, sd) / sqrt(Neff_chain3))
se_chain4 <-(apply(Theta.Ec, 2, sd) / sqrt(Neff_chain4))


# Imprimir resumen de errores estándar
summary(se_chain)
summary(se_chain2)
summary(se_chain3)
summary(se_chain4)
pdf("Devianza.pdf", width=12, height=8)
par(mfrow = c(2, 2))
plot(x=(1:10000), y=Theta.u[,65], type= 'l', col='mistyrose', xlab= 'Iteración' , ylab='Devianza', main= 'Modelo 1')
plot(x=(1:10000), y=Theta.g[,65], type= 'l', col='lightblue', xlab= 'Iteración' , ylab='Devianza', main= 'Modelo 2')
plot(x=(1:10000), y=Theta.r[1:10000,65], type= 'l', col='lightgreen', xlab= 'Iteración' , ylab='Devianza', main= 'Modelo 3')
# se tomaron un poco mas de muestras 
plot(x=(1:10000), y=Theta.Ec[1:10000,65], type= 'l', col='lightgrey', xlab= 'Iteración' , ylab='Devianza', main= 'Modelo 4')
dev.off()


