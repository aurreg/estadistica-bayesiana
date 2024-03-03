# Importacion de datos----


library(readr)
library(dplyr)
# Importamos el archivo SB11_20222.TXT 
SB11_20222 <- read_delim("C:/Users/Pc/Desktop/Bayesiana/segundo corte/Estudio caso/SB11_20222.TXT",
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)


# Filtramos los datos de estudiantes colombianos con estado de investigación 'PUBLICAR' que no esten en 88 
SABER_1 <- SB11_20222 %>%
  filter(ESTU_NACIONALIDAD == "COLOMBIA" & ESTU_ESTADOINVESTIGACION == "PUBLICAR" &
           COLE_COD_DEPTO_UBICACION != "88" & ESTU_PAIS_RESIDE == "COLOMBIA") %>%
  arrange(COLE_COD_DEPTO_UBICACION, COLE_COD_MCPIO_UBICACION) %>%
  select(COLE_COD_DEPTO_UBICACION, COLE_COD_MCPIO_UBICACION, PUNT_GLOBAL) %>%
  as.data.frame()

# Creamos un nuevo DataFrame con estadísticas a nivel de municipio
SABER_2 <- SABER_1 %>%
  arrange(COLE_COD_DEPTO_UBICACION) %>%
  group_by(COLE_COD_MCPIO_UBICACION, COLE_COD_DEPTO_UBICACION) %>%
  summarise(njk = n(), s2jk = var(PUNT_GLOBAL), yjkb = mean(PUNT_GLOBAL)) %>%
  as.data.frame()

# Calculamos estadísticas a nivel de departamento
SABER_3 <- SABER_2 %>%
  group_by(COLE_COD_DEPTO_UBICACION) %>%
  summarise(sk2 = var(yjkb), ykb = mean(yjkb), nk = n_distinct(COLE_COD_MCPIO_UBICACION)) %>%
  arrange(COLE_COD_DEPTO_UBICACION) %>%
  as.data.frame()

# Unimos SABER_3 con estadísticas de estudiantes
SABER_3 <- SABER_1 %>%
  group_by(COLE_COD_DEPTO_UBICACION) %>%
  summarise(sk2_estudiantes = var(PUNT_GLOBAL), ykb_estudiantes = mean(PUNT_GLOBAL), nk_estudiantes = n()) %>%
  arrange(COLE_COD_DEPTO_UBICACION) %>%
  left_join(SABER_3, by = c("COLE_COD_DEPTO_UBICACION" = "COLE_COD_DEPTO_UBICACION")) %>%
  as.data.frame()

# Importamos la base de datos de cobertura neta secundaria en 2022
library(readr)
Neta <- read_csv("C:/Users/Pc/Desktop/Bayesiana/segundo corte/Estudio caso/estadísticas educación.csv")

# Filtramos los datos para el año 2022 y seleccionamos las columnas relevantes
SABER_2 <- Neta %>%
  filter(AÑO == 2022) %>%
  select(COLE_COD_MCPIO_UBICACION = CÓDIGO_MUNICIPIO, COBERTURA_NETA=COBERTURA_NETA_SECUNDARIA, MUNICIPIO) %>%
  right_join(SABER_2, by = c('COLE_COD_MCPIO_UBICACION' = 'COLE_COD_MCPIO_UBICACION')) %>%
  arrange(COLE_COD_DEPTO_UBICACION, COLE_COD_MCPIO_UBICACION) %>%
  as.data.frame()

# Creación de la base de datos de nombres de departamentos
codigo <- c("05", "08", "11", "13", "15", "17", "18", "19", "20", "23", "25", "27", "41", "44", "47", "50", "52", "54", "63", "66", "68", "70", "73", "76", "81", "85", "86", "88", "91", "94", "95", "97", "99")
nombre <- c("ANTIOQUIA", "ATLANTICO", "BOGOTA", "BOLIVAR", "BOYACA", "CALDAS", "CAQUETA", "CAUCA", "CESAR",
            "CORDOBA", "CUNDINAMARCA", "CHOCO", "HUILA", "LA GUAJIRA", "MAGDALENA", "META", "NARINO", "N. SANTANDER", 
            "QUINDIO", "RISARALDA", "SANTANDER", "SUCRE", "TOLIMA", "VALLE DEL CAUCA", "ARAUCA", "CASANARE", "PUTUMAYO", 
            "SAN ANDRES", "AMAZONAS", "GUAINIA", "GUAVIARE", "VAUPES", "VICHADA")
deptos <- data.frame(codigo, nombre)

# Creación de la base de datos de pobreza monetaria 
Monetaria <- data.frame(
  nombre = c(
    "ANTIOQUIA", "ATLANTICO", "BOGOTA", "BOLIVAR", "BOYACA",
    "CALDAS", "CAQUETA", "CAUCA", "CESAR", "CHOCO", "CORDOBA",
    "CUNDINAMARCA", "HUILA", "LA GUAJIRA", "MAGDALENA", "META",
    "NARINO", "N. SANTANDER", "QUINDIO", "RISARALDA",
    "SANTANDER", "SUCRE", "TOLIMA", "VALLE DEL CAUCA"
  ),
  Y = c(
    21.2, 24.2, 12.4, 36.2, 26.6, 22.1, 40.1, 50.5, 42.9, 61.1,
    44.2, 16.4, 37.0, 53.7, 46.6, 25.4, 41.4, 41.7, 24.1, 17.7,
    20.1, 41.0, 31.0, 20.4
  )
)


Monetaria <- left_join(x = Monetaria, y = deptos, by = "nombre")
Monetaria <- Monetaria %>% select(codigo, Y) %>% as.data.frame()

# Unimos Monetaria con SABER_3
SABER_3 <- SABER_3 %>% left_join(Monetaria, by = c('COLE_COD_DEPTO_UBICACION' = "codigo")) %>% as.data.frame()

# Unimos SABER_3 con los nombres de los departamentos
SABER_3 <- SABER_3 %>% left_join(deptos, by = c('COLE_COD_DEPTO_UBICACION' = 'codigo')) %>% as.data.frame()

SABER_3

# Definición de hiperparámetros para las MCMC
xi0 = 1
k20 = 50 * 50
mu0 = 250
g20 = 50 * 50
eta0 = 1
t20 = 50 * 50
nu0 = 1
s20 = 50 * 50
al0 = 1
be0 = (1 / (50 * 50))

# Definición de estadísticas para las MCMC
sk2_estudiantes <- SABER_3$sk2_estudiantes
ykb_estudiantes <- SABER_3$ykb_estudiantes
nk_estudiantes <- SABER_3$nk_estudiantes
yjkb <- SABER_2$yjkb
sjk2 <- SABER_2$s2jk
njk <- SABER_2$njk
ykb <- SABER_3$ykb
sk2 <- SABER_3$sk2
nk <- SABER_3$nk
y <- SABER_1$PUNT_GLOBAL
neta <- SABER_2$COBERTURA_NETA
monetaria <- SABER_3$Y

# Definición de Beta para las simulaciones
B = 101000

# Analisis Exploratorio-----

library(ggplot2)
library(sf)
# Cargamos el mapa de los departamentos de Colombia (shapefile)

deptoshp <- st_read("MGN_DPTO_POLITICO.shp",quiet=TRUE)
str(deptoshp)

# Unimos el mapa de departamentos con el DataFrame SABER_3 usando el código del departamento
mapdeptos <- deptoshp %>% left_join(SABER_3,by=c("DPTO_CCDGO"="COLE_COD_DEPTO_UBICACION"))
str(mapdeptos)

# Cargamos el mapa del mundo para mostrar los países vecinos de Colombia
mundoshp <- st_read("admin00.shp",quiet=TRUE)
# mapa del mundo para que aparezcan los vecinos de colombia
mundocol <- mundoshp %>%
  filter(CNTRY_NAME %in% c("Peru","Brazil","Venezuela","Ecuador","Panama"))

# Definimos las coordenadas de nuestro mapa
box <- st_bbox(mapdeptos)


# Creación del primer mapa (ICFES)
ICFES<-ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapdeptos,aes(fill=ykb),col="darkgray",linetype="solid") + 
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=mapdeptos,aes(label=ifelse(ykb< 220,nombre,"")),col="black",
               fontface="bold",size=4,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitud",y="Latitud",title="Media Muestral Del\nPuntaje Global",fill="Puntaje") +
  scale_fill_gradient(low="white",high="purple",n.breaks=5) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) +
  theme(panel.background=element_rect(fill="lightblue"),
        plot.title = element_text(hjust = 0.5))
        
# Creación del segundo mapa (MONETARIA)
MONETARIA<-ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapdeptos,aes(fill=Y),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Incidencia De La Pobreza\nMonetaria En 2018 ",fill="Incidencia") +
  scale_fill_gradient(low="white",high="green",n.breaks=5) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) +
  theme(panel.background=element_rect(fill="lightblue"),
        plot.title = element_text(hjust = 0.5))


ggsave("MONETARIA.png", plot = MONETARIA, width = 8, height = 8)
ggsave("ICFES.png", plot = ICFES, width = 8, height = 8)


# Municipios
# Cargamos el mapa de los municipios de Colombia (shapefile)
mpioshp <- st_read("MGN_MPIO_POLITICO.shp", quiet = TRUE)

# Unimos el mapa de municipios con el DataFrame SABER_2 utilizando el código del municipio
mpioshp <- mpioshp %>% left_join(SABER_2, by = c("MPIO_CCNCT" = "COLE_COD_MCPIO_UBICACION"))

# Creación del primer mapa (ICFES2)
ICFES2 <- ggplot() +
  geom_sf(data = mundocol) +
  geom_sf(data = mpioshp, aes(fill = yjkb), col = "darkgray", linetype = "solid") +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax), expand = FALSE) +
  labs(x = "Longitud", y = "Latitud", title = "Media Muestral Del\nPuntaje Global", fill = "Puntaje") +
  scale_fill_gradient(low = "white", high = "purple", n.breaks = 5) +
  annotate("text", x = c(-74.5, -68, -78, -69, -78.5), y = c(-2.5, 0, -1, 9, 9), colour = "blue",
           label = c("Perú", "Brasil", "Ecuador", "Venezuela", "Panamá")) +
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.title = element_text(hjust = 0.5))

# Creación del segundo mapa (cobertura)
cobertura <- ggplot() +
  geom_sf(data = mundocol) +
  geom_sf(data = mpioshp, aes(fill = COBERTURA_NETA), col = "darkgray", linetype = "solid") +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax), expand = FALSE) +
  labs(x = "Longitud", y = "Latitud", title = "Cobertura Neta\nSecundaria En 2022", fill = "Cobertura") +
  scale_fill_gradient(low = "white", high = "red", n.breaks = 5) +
  annotate("text", x = c(-74.5, -68, -78, -69, -78.5), y = c(-2.5, 0, -1, 9, 9), colour = "blue",
           label = c("Perú", "Brasil", "Ecuador", "Venezuela", "Panamá")) +
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.title = element_text(hjust = 0.5))



ggsave("ICFES2.png", plot = ICFES2, width = 8, height = 8)
ggsave("cobertura.png", plot = cobertura, width = 8, height = 8)

# Modelos MCMC----

MCMC <- function(mu0, g20, s20, nu0, y, B) {
  n <- length(y)  # Tamaño de la muestra de datos
  PHI <- matrix(data = NA, nrow = B, ncol = 2)  # Matriz para almacenar los resultados
  colnames(PHI) <- c("theta", "isig2")  # Nombres de las columnas en la matriz PHI
  isig2 <- rgamma(n = 1, shape = nu0/2, rate = nu0*s20/2)  # Inicialización de isig2
  LL <- numeric(B)  # Vector para almacenar la log-verosimilitud
  j <- 1  # Inicialización de un contador
  
  for (b in 1:B) {
    t2n <- 1 / (1/g20 + n * isig2)  # Cálculo de t2n
    mun <- t2n * (mu0/g20 + isig2 * sum(y))  # Cálculo de mun
    theta <- rnorm(n = 1, mean = mun, sd = sqrt(t2n))  # Actualización de theta
    nun <- nu0 + n  # Actualización de nun
    s2n <- (nu0 * s20 + sum((y - theta)^2)) / nun  # Cálculo de s2n
    isig2 <- rgamma(n = 1, shape = nun/2, rate = nun * s2n/2)  # Actualización de isig2
    
    if (b >= 1000 & ((b - 1) %% 10) == 0) {
      #Muestreo sistematico
      PHI[j, ] <- c(theta, isig2)  # Almacenar valores de theta e isig2 en PHI
      LL[j] <- sum(dnorm(x = y, mean = theta, sd = sqrt(1/isig2), log = T))  # Cálculo de log-verosimilitud
      # se trabaja con la precision
      j <- j + 1  # Incrementar contador
    }
  }
  
  # Eliminar filas vacías y almacenar los resultados
  PHI <- as.data.frame(PHI[1:j-1,])
  LL <- as.data.frame(LL[1:j-1])
  
  return(list(THETA = PHI, LL = LL))  # Devolver resultados en una lista
}
MCMC2 <- function(B, nk, ykb, sk2, mu0, g20, eta0, t20, nu0, s20, y, n) {
  # Tamaños
  n <- n  
  m <- length(nk)  
  
  # Valores iniciales
  theta <- ykb  # Valores iniciales de theta
  sig2 <- mean(sk2)  # Valor inicial de sig2 
  mu <- mean(theta)  # Valor inicial de mu 
  tau2 <- var(theta)  # Valor inicial de tau2 
  
  # Almacenamiento
  THETA <- matrix(data = NA, nrow = B, ncol = m + 3)  # Matriz para almacenar resultados
  LL <- matrix(data = NA, nrow = B, ncol = 1)  # Matriz para almacenar log-verosimilitud
  j <- 1  # Contador
  
  # Cadena MCMC
  for (b in 1:B) {
    # Actualizar sigma^2
    sig2 <- 1 / rgamma(n = 1, shape = 0.5 * (nu0 + n), rate = 0.5 * (nu0 * s20 + sum((nk - 1) * sk2 + nk * (ykb - theta)^2, na.rm = TRUE)))
    
    # Actualizar theta
    vtheta <- 1 / (1 / tau2 + nk / sig2)
    theta <- rnorm(n = m, mean = vtheta * (mu / tau2 + nk * ykb / sig2), sd = sqrt(vtheta))
    
    # Actualizar mu
    vmu <- 1 / (1 / g20 + m / tau2)
    mu <- rnorm(n = 1, mean = vmu * (mu0 / g20 + m * mean(theta) / tau2), sd = sqrt(vmu))
    
    # Actualizar tau^2
    tau2 <- 1 / rgamma(n = 1, shape = 0.5 * (eta0 + m), rate = 0.5 * (eta0 * t20 + (m - 1) * var(theta) + m * (mean(theta) - mu)^2))
    
    if (b >= 1000 & ((b - 1) %% 10) == 0) {
      # Muestreo sistematico
      THETA[j,] <- c(theta, sig2, mu, tau2)
      
      # Calcular log-verosimilitud
      LL[j] <- sum(dnorm(x = y, mean = rep(theta, nk), sd = sqrt(sig2), log = TRUE))
      j <- j + 1
    }
  }
  
  # Asignar nombres de columnas a las matrices resultantes
  colnames(THETA) <- c(paste0("theta", 1:m), "sig2", "mu", "tau2")
  colnames(LL) <- c("ll")
  
  # Crear data frames con los resultados
  THETA <- as.data.frame(THETA[1:j - 1, ])
  LL <- as.data.frame(LL[1:j - 1])
  
  # Devolver resultados como una lista
  return(list(THETA = THETA, LL = LL))
}
MCMC3 <- function(B, nk, ykb, sk2, mu0, g20, eta0, t20, lam0, al0, be0, nus0, y, n) {
  # Tamaños
  n <- n  
  m <- length(nk)  #
  
  # Valores iniciales
  theta <- ykb  # Valores iniciales de theta
  sig2 <- sk2  # Valores iniciales de sigma_j^2
  mu <- mean(theta)  # Valor inicial de mu 
  tau2 <- var(theta)  # Valor inicial de tau2 
  nu <- 1  # Valor inicial de nu
  ups2 <- 50 * 50  # Valor inicial de sigma^2
  
  # Almacenamiento
  THETA <- matrix(data = NA, nrow = B, ncol = 2 * m + 3)  # Matriz para almacenar resultados
  LL <- matrix(data = NA, nrow = B, ncol = 1)  # Matriz para almacenar log-verosimilitud
  I <- 1  # Contador
  
  # Cadena MCMC
  for (b in 1:B) {
    # Actualizar sigma_j^2
    sig2 <- 1 / rgamma(n = m, shape = 0.5 * (nu + nk), rate = 0.5 * (nu * ups2 + (nk - 1) * sk2 + nk * (ykb - theta)^2))
    
    # Actualizar theta
    vtheta <- 1 / (1 / tau2 + nk / sig2)
    theta <- rnorm(n = m, mean = vtheta * (mu / tau2 + nk * ykb / sig2), sd = sqrt(vtheta))
    
    # Actualizar mu
    vmu <- 1 / (1 / g20 + m / tau2)
    mu <- rnorm(n = 1, mean = vmu * (mu0 / g20 + m * mean(theta) / tau2), sd = sqrt(vmu))
    
    # Actualizar tau2
    tau2 <- 1 / rgamma(n = 1, shape = 0.5 * (eta0 + m), rate = 0.5 * (eta0 * t20 + (m - 1) * var(theta) + m * (mean(theta) - mu)^2))
    
    # Actualizar sigma^2
    ups2 <- rgamma(n = 1, shape = (al0 + m * nu) * 0.5, rate = (be0 + nu * sum(1 / sig2)) * 0.5)
    
    # Muestreo sistematico
    if (b >= 1000 & ((b - 1) %% 10) == 0) {
      THETA[I,] <- c(theta, sig2, mu, tau2, ups2)
      
      # Calcular log-verosimilitud
      LL[I] <- sum(dnorm(x = y, mean = rep(theta, nk), sd = sqrt(rep(sig2, nk)), log = TRUE))
      I <- I + 1
    }
  }
  
  # Asignar nombres de columnas a las matrices resultantes
  colnames(THETA) <- c(paste0("theta", 1:m), paste0("sig2", 1:m), "mu", "tau2", "ups2")
  colnames(LL) <- c("ll")
  
  # Crear data frames con los resultados
  THETA <- as.data.frame(THETA[1:I - 1, ])
  LL <- as.data.frame(LL[1:I - 1])
  
  # Devolver resultados como una lista
  return(list(THETA = THETA, LL = LL))
}
MCMC4 <- function(B, y, yjkb, sjk2, njk, ykb, sk2, nk, xi0, k20, mu0, g20, eta0, t20, nu0, s20) {
  
  # Inicialización de variables y cálculo de algunas estadísticas iniciales
  kappa2 <- var(yjkb)  # Varianza de yjkb
  zeta <- yjkb  # Valores iniciales de zeta
  thetak <- ykb  # Valores iniciales de thetak
  sigma2 <- var(thetak)  # Varianza de thetak
  mu <- mean(ykb)  # Media de ykb
  tau2 <- var(thetak)  # Varianza de thetak
  no_zetas <- length(zeta)  # Número de zetas
  no_theta <- length(thetak)  # Número de thetak
  n <- sum(njk)  # Suma de njk
  zetab <- NULL  # Vector para almacenar zetas agregadas
  indicador <- NULL  # Vector para indicar posiciones 
  
  # Calculamos una suma acumulada para conocer los índices que definen los límites del primer 
  #y último elemento de zeta en cada subgrupo definido por nk
  for (i in 1:length(nk)) {
    indicador[i] <- sum(nk[1:i])
  }
  i <- 1
  indicador <- c(0, indicador)
  
  # Almacenamiento de resultados
  THETA <- matrix(data = NA, nrow = 10000, ncol = no_zetas + no_theta + 4)
  LL <- matrix(data = NA, nrow = 10000, ncol = 1)
  j <- 1
  
  # Ciclo MCMC
  for (b in 1:B) {
    # Actualización de zeta (media de los municipios)
    zeta <- rnorm(no_zetas, mean = (1 / (1 / sigma2 + njk / kappa2)) * (rep(thetak, nk) / sigma2 + njk * yjkb / kappa2), sd = sqrt(1 / (1 / sigma2 + njk / kappa2)))
    
    # Actualización de kappa (varianza de los municipios)
    kappa2 <- 1 / rgamma(1, shape = 0.5 * (xi0 + n), rate = 0.5 * (xi0 * k20 + sum((njk - 1) * sjk2 + njk * (yjkb - zeta)^2, na.rm = TRUE)))
    
    for (i in 1:length(nk)) {
      zetab[i] <- mean(zeta[( (indicador[i]) + 1) :indicador[i+1] ])  # Calculamos zetas barra
    }
    
    thetak <- rnorm(no_theta, mean = (1 / (1 / tau2 + nk / sigma2)) * (mu / tau2 + nk * zetab / sigma2), sd = sqrt(1 / (1 / tau2 + nk / sigma2)))
    
    # Actualización de sigma2 (varianza de los departamentos)
    sigma2 <- 1 / rgamma(1, shape = 0.5 * (nu0 + sum(nk)), rate = 0.5 * (nu0 * s20 + sum((zeta - rep(thetak, nk))^2)))
    
    # Actualización de mu (media de las medias de los departamentos)
    vmu <- 1 / (1 / g20 + no_theta / tau2)
    mu <- rnorm(1, mean = vmu * (mu0 / g20 + no_theta * mean(thetak) / tau2), sd = sqrt(vmu))
    
    # Actualización de tau2 (varianza de las medias de los departamentos)
    tau2 <- 1 / rgamma(1, shape = 0.5 * (eta0 + no_theta), rate = 0.5 * (eta0 * t20 + (no_theta - 1) * var(thetak) + no_theta * (mean(thetak) - mu)^2))
    
    # Muestreo sistematico
    if (b >= 1000 & ((b - 1) %% 10) == 0) {
      THETA[j,] <- c(zeta, kappa2, thetak, sigma2, mu, tau2)
      LL[j] <- sum(dnorm(y, mean = rep(zeta, njk), sd = sqrt(kappa2), log = TRUE))
      j <- j + 1
    }
  }
  
  # Asignar nombres de columnas a las matrices resultantes
  colnames(THETA) <- c(paste0("zeta", 1:no_zetas), 'kappa2', paste0('theta', 1:no_theta), 'sigma2', "mu", "tau2")
  colnames(LL) <- c("ll")
  
  # Crear data frames con los resultados
  THETA <- as.data.frame(THETA[1:j - 1, ])
  LL <- as.data.frame(LL[1:j - 1])
  
  # Devolver resultados como una lista
  return(list(THETA = THETA, LL = LL))
}
MCMC5 <- function(B, y, yjkb, sjk2, njk, ykb, sk2, nk, xi0, k20, mu0, g20, eta0, t20, s20, al0, be0, neta, monetaria) {
  # Inicialización de las variables y parámetros
  kappa2 <- var(yjkb)  # Varianza de yjkb
  zeta <- yjkb  # Vector de medias de los municipios
  thetak <- ykb  # Vector de medias de los departamentos
  sigma2_k <- sk2  # Vector de varianzas de los departamentos (sigma2_k)
  sigma2_k[is.na(sigma2_k)] <- mean(sigma2_k, na.rm = TRUE)  # Reemplazar NA con la media de sigma2_k
  mu <- mean(ykb)  # Media de ykb
  tau2 <- var(thetak)  # Varianza de thetak
  no_zetas <- length(zeta)  # Número de elementos en zeta
  no_theta <- length(thetak)  # Número de elementos en thetak
  n <- sum(njk)  # Número total de observaciones
  zetab <- NULL  # Vector para almacenar medias de zeta
  zetas2 <- NULL  # Vector para almacenar varianzas de zeta
  indicador <- NULL  # Vector para indicar posiciones de elementos
  sigma <- 50 * 50  # Valor inicial de sigma^2
  neta <- neta  # Valores de neta para municipios
  monetaria <- monetaria  # Valores de monetaria para departamentos
  NASneta <- which(is.na(neta))  # Índices de valores faltantes en neta
  NASmonetaria <- which(is.na(monetaria))  # Índices de valores faltantes en monetaria
  Estimacion_neta <- NULL  # Matriz para almacenar estimaciones de neta
  Estimacion_monetaria <- NULL  # Matriz para almacenar estimaciones de monetaria
  ClusterM <- NULL  # Matriz para almacenar resultados de clustering de zeta
  ClusterD <- NULL  # Matriz para almacenar resultados de clustering de thetak
  j <- 1  # Contador de iteraciones
  
  # Calcular sumas acumuladas para conocer las posiciones del primer y último elemento de zeta
  for (i in 1:length(nk)) {
    indicador[i] <- sum(nk[1:i])
  }
  i <- 1
  indicador <- c(0, indicador)
  
  # Crear matrices vacías para almacenar los resultados
  THETA <- matrix(data = NA, nrow = 10000, ncol = no_zetas + 2 * (no_theta) + 4)
  LL <- matrix(data = NA, nrow = 10000, ncol = 1)
  Estimacion_neta <- matrix(data = NA, ncol = length(NASneta), nrow = 10000)
  Estimacion_monetaria <- matrix(data = NA, ncol = length(NASmonetaria), nrow = 10000)
  ClusterM <- matrix(data = NA, ncol = no_zetas, nrow = 10000)
  ClusterD <- matrix(data = NA, ncol = no_theta, nrow = 10000)
  
  # Ciclo MCMC
  for (b in 1:B) {
    # Actualización de zeta (media de los municipios)
    zeta <- rnorm(no_zetas, mean = (1 / (1 / rep(sigma2_k, nk) + njk / kappa2)) * (rep(thetak, nk) / rep(sigma2_k, nk) + njk * yjkb / kappa2), sd = sqrt(1 / (1 / rep(sigma2_k, nk) + njk / kappa2)))
    
    # Actualización de kappa (varianza de los municipios)
    kappa2 <- 1 / rgamma(1, shape = 0.5 * (xi0 + n), rate = 0.5 * (xi0 * k20 + sum((njk - 1) * sjk2 + njk * (yjkb - zeta)^2, na.rm = TRUE)))
    
    # Calcular medias y varianzas de zeta para cada grupo de departamentos
    for (i in 1:length(nk)) {
      zetab[i] <- mean(zeta[( (indicador[i]) + 1) :indicador[i+1] ])
      zetas2[i] <- var(zeta[( (indicador[i]) + 1) :indicador[i+1] ])
    }
    
    # Actualización de sigma2 (varianza de los departamentos)
    sigma2_k <- 1 / rgamma(no_theta, shape = 0.5 * (1 + nk), rate = 0.5 * (1 * sigma + ((nk - 1) * zetas2 + nk * (zetab - thetak)^2)))
    
    # Reemplazar varianza para bogota
    sigma2_k[is.na(zetas2)] <- 1 / rgamma(1, shape = 1, # shape es uno 
                                            rate = 0.5 * (1 * sigma+(zetab[which(is.na(zetas2))] - thetak[which(is.na(zetas2))])^2))
    # La media de los grupos con un solo individuo es igual al individuo, identificamos su correspondiente theta 
    
    # Actualización de theta (medias de los departamentos)
    thetak <- rnorm(no_theta, mean = (1 / (1 / tau2 + nk / sigma2_k)) * (mu / tau2 + nk * zetab / sigma2_k), sd = sqrt(1 / (1 / tau2 + nk / sigma2_k)))
    
    # Actualización de mu (media de las medias de los departamentos)
    vmu <- 1 / (1 / g20 + no_theta / tau2)
    mu <- rnorm(1, mean = vmu * (mu0 / g20 + no_theta * mean(thetak) / tau2), sd = sqrt(vmu))
    
    # Actualización de tau2 (varianza de las medias de los departamentos)
    tau2 <- 1 / rgamma(1, shape = 0.5 * (eta0 + no_theta), rate = 0.5 * (eta0 * t20 + (no_theta - 1) * var(thetak) + no_theta * (mean(thetak) - mu)^2))
    
    # Actualización de sigma (varianza de las medias de los departamentos)
    sigma <- rgamma(n = 1, shape = (al0 + no_theta) * 0.5, rate = 0.5 * (be0 + sum(1 / sigma2_k)))
    
    # Almacenamiento de resultados si se cumple una condición específica
    if (b >= 1000 & ((b - 1) %% 10) == 0) {
      # Realizar regresiones para estimar neta y monetaria
      reg <- lm(neta[-c(NASneta)] ~ zeta[-c(NASneta)])
      B_0 <- reg$coefficients[1]
      B_1 <- reg$coefficients[2]
      Estimacion_neta[j, ] <- B_0 + B_1 * zeta[c(NASneta)]
      
      reg <- lm(monetaria[-c(NASmonetaria)] ~ thetak[-c(NASmonetaria)])
      B_0 <- reg$coefficients[1]
      B_1 <- reg$coefficients[2]
      Estimacion_monetaria[j, ] <- B_0 + B_1 * thetak[c(NASmonetaria)]
      
      # Realizar clustering en zeta y thetak
      ClusterM[j, ] <- kmeans(zeta, 8)$cluster
      ClusterD[j, ] <- kmeans(thetak, 5)$cluster
      
      # Almacenar resultados
      THETA[j, ] <- c(zeta, kappa2, thetak, sigma2_k, sigma, mu, tau2)
      LL[j] <- sum(dnorm(y, mean = rep(zeta, njk), sd = sqrt(kappa2), log = TRUE))
      j <- j + 1
    }
  }
  # Asignar nombres a las columnas de las matrices
  colnames(THETA) <- c(paste0("zeta", 1:no_zetas), 'kappa2', paste0('theta_k', 1:no_theta), paste0('sigma2_k', 1:no_theta), 'sigma2', "mu", "tau2")
  colnames(LL) <- c("ll")
  
  # Convertir los resultados en data frames
  THETA <- as.data.frame(THETA[1:j - 1, ])
  LL <- as.data.frame(LL[1:j - 1])
  Estimacion_monetaria <- as.data.frame(Estimacion_monetaria[1:j - 1, ])
  Estimacion_neta <- as.data.frame(Estimacion_neta[1:j - 1, ])
  ClusterM <- as.data.frame(ClusterM[1:j - 1, ])
  ClusterD <- as.data.frame(ClusterD[1:j - 1, ])
  
  # Devolver resultados
  return(list(THETA = THETA, LL = LL, Estimacion_neta = Estimacion_neta, Estimacion_monetaria = Estimacion_monetaria, ClusterD = ClusterD, ClusterM = ClusterM))
}

B=101000
# Ejecutar cinco cadenas MCMC con diferentes modelos
set.seed(1)
chain<- MCMC(mu0, g20, s20, nu0, y, B=B)
set.seed(2)
chain2 <-MCMC2(B=B, nk_estudiantes, ykb_estudiantes, sk2_estudiantes , mu0, g20, eta0, t20, nu0, s20,y,length(y))
set.seed(3)
chain3 <-MCMC3(B=B,nk_estudiantes, ykb_estudiantes, sk2_estudiantes, mu0, g20, eta0, t20, lam0, al0, be0, 1,y, length(y))
set.seed(4)
chain4 <- MCMC4(B=B, y, yjkb , sjk2, njk ,ykb ,sk2 ,nk , xi0, k20, mu0, g20, eta0, t20, nu0, s20)
set.seed(5)
chain5 <- MCMC5(B=B, y, yjkb , sjk2, njk ,ykb ,sk2 ,nk,xi0, k20, mu0, g20, eta0, t20, s20,al0,be0,neta,monetaria)


library(coda)
# Calcular el tamaño efectivo (Neff) y errores estándar (se) de las cadenas MCMC
Neff_chain <- effectiveSize(chain$THETA)
Neff_chain2 <- effectiveSize(chain2$THETA)
Neff_chain3 <- effectiveSize(chain3$THETA)
Neff_chain4 <- effectiveSize(chain4$THETA)
Neff_chain5 <- effectiveSize(chain5$THETA)

se_chain <- apply(chain$THETA, 2, sd) / sqrt(Neff_chain)
se_chain2 <- (apply(chain2$THETA, 2, sd) / sqrt(Neff_chain2))
se_chain3 <- (apply(chain3$THETA, 2, sd) / sqrt(Neff_chain3))
se_chain4 <-(apply(chain4$THETA, 2, sd) / sqrt(Neff_chain4))
se_chain5 <-(apply(chain5$THETA, 2, sd) / sqrt(Neff_chain5))

# Imprimir resumen de errores estándar
summary(se_chain)
summary(se_chain2)
summary(se_chain3)
summary(se_chain4)
summary(se_chain5)
# Guardar log-verosimilitud
L<-chain$LL$`LL[1:j - 1]`
L2<-chain2$LL$`LL[1:j - 1]`
L3<-chain3$LL$`LL[1:I - 1]`
L4<-chain4$LL$`LL[1:j - 1]`
L5<-chain5$LL$`LL[1:j - 1]`

# Identificar errores estándar de Monte Carlo mayores a 1
se_chain[se_chain > 1]
se_chain2[se_chain2 > 1]
se_chain3[se_chain3 > 1]
se_chain4[se_chain4 > 1]
se_chain5[se_chain5 > 1]

# Guardar los resultados
saveRDS(chain, file = "chain")
saveRDS(chain2, file = "chain2")
saveRDS(chain3, file = "chain3")
saveRDS(chain4, file = "chain4")
saveRDS(chain5, file = "chain5")





# Grafico LL-----
pdf("LL.pdf",width=8, height=6)

par(mfrow=c(2,2)) 

plot(x=(1:10000), y=L2, type= 'l', col='mistyrose', xlab= 'Iteración' , ylab='Log-verosimilitud', main= 'Modelo 2',
     cex.axis=0.7, xlim=c(1,10000)) 
plot(x=(1:10000), y=L3, type= 'l', col='lightblue', xlab= 'Iteración' , ylab='Log-verosimilitud', main= 'Modelo 3',
     cex.axis=0.7, xlim=c(1,10000), ylim = range(L3)) 
plot(x=(1:10000), y=L4, type= 'l', col='lightgreen', xlab= 'Iteración' , ylab='Log-verosimilitud', main= 'Modelo 4',
     cex.axis=0.7, xlim=c(1,10000)) 
plot(x=(1:10000), y=L5, type= 'l', col='lightgrey', xlab= 'Iteración' , ylab='Log-verosimilitud', main= 'Modelo 5',
     cex.axis=0.7, xlim=c(1,10000)) 

dev.off()

# Media posterior e intervalos miu-----

mu<-c(quantile(chain$THETA$theta,0.025), mean(chain$THETA$theta), quantile(chain$THETA$theta, 0.975))
mu2<-c(quantile(chain2$THETA$mu,0.025), mean(chain2$THETA$mu), quantile(chain2$THETA$mu, 0.975))
mu3<-c(quantile(chain3$THETA$mu,0.025), mean(chain3$THETA$mu), quantile(chain3$THETA$mu, 0.975))
mu4<-c(quantile(chain4$THETA$mu,0.025), mean(chain4$THETA$mu), quantile(chain4$THETA$mu, 0.975))
mu5<-c(quantile(chain5$THETA$mu,0.025), mean(chain5$THETA$mu), quantile(chain5$THETA$mu, 0.975))

Mus<-rbind(mu,mu2,mu3,mu4,mu5)
colnames(Mus)<-c('IC_0.025','Media', 'IC_0.975')
round(Mus,3)
# Ranking Deptos------
library(dplyr)
# Cálculo de intervalos de confianza frecuentista 
ICF <- SABER_3 %>% 
  mutate(C_0.025 = ykb - sqrt(sk2) / nk,
         C_0.975 = ykb + sqrt(sk2) / nk) %>%
  select(Codigo = COLE_COD_DEPTO_UBICACION, nombre, C_0.025, ykb, C_0.975) %>%
  as.data.frame()

# Cálculo de intervalos bayesianos 
ICF <- cbind(ICF,
             CB_0.025 = apply(chain5$THETA[,(sum(nk)+2):(sum(nk)+1+32)], 2, function(x) quantile(x, 0.025)),
             EB = apply(chain5$THETA[,(sum(nk)+2):(sum(nk)+1+32)], 2, mean),
             CB_0.975 = apply(chain5$THETA[,(sum(nk)+2):(sum(nk)+1+32)], 2, function(x) quantile(x, 0.975))
)


m = 32  # Número de departamentos
pdf("ranking.pdf", width=12, height=8)
par(mfrow = c(1,2), mar = c(4,10,1.5,1), mgp = c(2.5,0.75,0))

# Crear un gráfico de puntos y líneas para el ranking bayesiano
plot(x = c(min(SABER_3$ykb), max(SABER_3$ykb)), y = c(1, m), type = "n", xlab = "Puntaje", ylab ='',
     main = "Ranking Bayesiano", yaxt = "n")
abline(h = 1:m, col = "lightgray", lwd = 1)
abline(v = 250, col = "gray", lwd = 3)

# Iterar a través de los departamentos para agregar puntos y líneas al gráfico bayesiano
for (l in 1:m) {
  j <- order(ICF$EB)[l]
  points(x = ICF$EB[j], y = l)
  if (ICF$CB_0.975[j] < 250) {
    lines(x = c(ICF$CB_0.025[j], ICF$CB_0.975[j]), y = rep(l, 2), col = 'red')
  }
  if (ICF$CB_0.025[j] <= 250 & 250 <= ICF$CB_0.975[j]) {
    lines(x = c(ICF$CB_0.025[j], ICF$CB_0.975[j]), y = rep(l, 2))
  }
  if (ICF$CB_0.025[j] >= 250) {
    lines(x = c(ICF$CB_0.025[j], ICF$CB_0.975[j]), y = rep(l, 2), col = 'green')
  }
}

# Etiquetas para el eje Y basadas en el ranking bayesiano
axis(side = 2, at = 1:m, labels = ICF$nombre[order(ICF$EB)], las = 2)


# Crear un gráfico de puntos y líneas para el ranking bfrecuentista 
plot(x = c(min(SABER_3$ykb), max(SABER_3$ykb)), y = c(1, m), type = "n", xlab = "Puntaje", ylab ='',
     main = "Ranking Frecuentista", yaxt = "n")
abline(h = 1:m, col = "lightgray", lwd = 1)
abline(v = 250, col = "gray", lwd = 3)


# Gráfico de puntos y líneas para el ranking frecuentista
for (l in 1:m) {
  j <- order(ICF$ykb)[l]
  points(x = ICF$ykb[j], y = l)
  if (ICF$C_0.975[j] < 250) {
    lines(x = c(ICF$C_0.025[j], ICF$C_0.975[j]), y = rep(l, 2), col = 'red')
  }
  if (ICF$C_0.025[j] <= 250 & 250 <= ICF$C_0.975[j]) {
    lines(x = c(ICF$C_0.025[j], ICF$C_0.975[j]), y = rep(l, 2))
  }
  if (ICF$C_0.025[j] >= 250) {
    lines(x = c(ICF$C_0.025[j], ICF$C_0.975[j]), y = rep(l, 2), col = 'green')
  }
}

# Etiquetas para el eje Y basadas en el ranking frecuentista
axis(side = 2, at = 1:m, labels = ICF$nombre[order(ICF$ykb)], las = 2)

dev.off()

# K-medias Departamentos------
# Realizar el agrupamiento K-Means para el ranking de departamentos
set.seed(555)
kmeans_D <- kmeans(ICF$EB, centers = 5) 
par(mfrow = c(1,1))
# Agregar la información del clúster al conjunto de datos ICF
ICF <- cbind(ICF, cluster = as.factor(kmeans_D$cluster))

# Cargar las bibliotecas necesarias
library(ggplot2)
library(sf)
# Cargar los datos de los departamentos y países
deptoshp <- st_read("MGN_DPTO_POLITICO.shp", quiet = TRUE)
mundoshp <- st_read("admin00.shp", quiet = TRUE)
mundocol <- mundoshp %>%
  filter(CNTRY_NAME %in% c("Peru", "Brazil", "Venezuela", "Ecuador", "Panama"))

# Realizar el mapeo de departamentos y países
mapdeptos <- deptoshp %>% right_join(ICF, by = c("DPTO_CCDGO" = "Codigo"))

box <- st_bbox(mapdeptos)

# Definir colores pálidos utilizando la paleta "Pastel 1"
library(RColorBrewer)
library(scales)

order(kmeans_D$centers)
# "#FED9A6" Amarillo 1
# "#CCEBC5"Verde 3
# "#DECBE4" Mordao 4
# "#B3CDE3" Azul 2
#  "#FBB4AE" rojo 5
cluster_colors <- c("#FED9A6" ,"#B3CDE3", "#CCEBC5", "#DECBE4",  "#FBB4AE")

show_col(cluster_colors)

MediasD<-round(kmeans_D$centers,2)

# Asignar colores a los departamentos en el mapa
# Crear el gráfico con colores pálidos
KMEDIASD<-ggplot() +
  geom_sf(data = mundocol) +
  geom_sf(data = mapdeptos, aes(fill = cluster), col = "darkgray", linetype = "solid") +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax), expand = FALSE) +
  labs(x = "Longitud", y = "Latitud", title = "Cluster Departamentos", fill = "Media Cluster") +
  scale_fill_manual(values = cluster_colors, breaks = 1:5, labels = MediasD) +
  annotate("text", x = c(-74.5, -68, -78, -69, -78.5), y = c(-2.5, 0, -1, 9, 9), colour = "blue",
           label = c("Perú", "Brasil", "Ecuador", "Venezuela", "Panamá")) +
  theme(panel.background = element_rect(fill = "lightblue"),
plot.title = element_text(hjust = 0.5))

ggsave("KMEDIASD.png", plot = KMEDIASD, width = 8, height = 8)

# Calcular la matriz de incidencia
B <- nrow(chain5$ClusterD)
n=32
A <- matrix(data = 0, nrow = n, ncol = n)
colnames(A)<-c(seq(1:32))
rownames(A)<-c(seq(1:32))

for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    A[i,j]=sum(chain5$ClusterD[,i]==chain5$ClusterD[,j])
  } 
}

A=A*1/B

A <- A + t(A)
diag(A) <- 1

# se organizan las observaciones de acuerdo a la partición verdadera

# Organizar las observaciones 
indices <- order(ICF$EB)
A <- A[indices,indices]

# Visualización de la matriz de incidencia
par(mar = c(2.75,2.75,0.5,0.5), mgp = c(1.7,0.7,0))
corrplot::corrplot(corr = A,method = 'color', is.corr = FALSE,tl.col = "black")

# Estimación monetaria por departamentos----

# Calcular las estimaciones monetarias a partir de los resultados de la cadena de Markov
Estimaciones_Monetaria <- cbind(
  media = apply(chain5$Estimacion_monetaria, 2, mean),
  C_0.025 = apply(chain5$Estimacion_monetaria, 2, function(x) quantile(x, 0.025)),
  C_0.975 = apply(chain5$Estimacion_monetaria, 2, function(x) quantile(x, 0.975))
)
Estimaciones_Monetaria <- as.data.frame(Estimaciones_Monetaria)

# Combinar las estimaciones monetarias con los datos existentes
Valores <- c(SABER_3$Y, Estimaciones_Monetaria$media)
Valores <- Valores[!(is.na(Valores))]
SABER_3 <- cbind(SABER_3, Valores)

# Cargar los datos geoespaciales de los departamentos
deptoshp <- st_read("MGN_DPTO_POLITICO.shp", quiet = TRUE)
str(deptoshp)

# Combinar los datos geoespaciales con los datos de SABER_3
mapdeptos <- deptoshp %>% left_join(SABER_3, by = c("DPTO_CCDGO" = "COLE_COD_DEPTO_UBICACION"))
str(mapdeptos)

# Cargar datos geoespaciales globales
mundoshp <- st_read("admin00.shp", quiet = TRUE)
mundocol <- mundoshp %>%
  filter(CNTRY_NAME %in% c("Peru", "Brazil", "Venezuela", "Ecuador", "Panama"))

# Crear un gráfico que muestre la pobreza monetaria
EMMAPA<-ggplot() +
  geom_sf(data = mundocol) +
  geom_sf(data = mapdeptos, aes(fill = Valores), col = "darkgray", linetype = "solid") +
  geom_sf_text(
    data = mapdeptos,
    aes(label = ifelse(is.na(Y), nombre, "")), # etiquetas de texto con los nombres de los departamentos que no fueron medidos
    col = "black",
    fontface = "bold",
    size = 4,
    fun.geometry = function(x) sf::st_centroid(x)
  ) +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax), expand = FALSE) +
  labs(x = "Longitud", y = "Latitud", title = "Incidencia De La Pobreza\nMonetaria En 2018", fill = "Incidencia") +
  scale_fill_gradient(low = "white", high = "green", n.breaks = 5) +
  annotate("text", x = c(-74.5, -68, -78, -69, -78.5), y = c(-2.5, 0, -1, 9, 9), colour = "blue",
           label = c("Perú", "Brasil", "Ecuador", "Venezuela", "Panamá")) +
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.title = element_text(hjust = 0.5))

ggsave("EMMAPA.pdf", plot = EMMAPA, width = 8, height = 8)


pdf("Ranking Monetario.pdf", width = 8, height = 10)

# Ajustar los márgenes y el tamaño de las etiquetas
par(mar = c(5, 8, 4, 2))  # Izquierda, Derecha, Abajo, Arriba
las <- 2  # Rotar etiquetas verticalmente
cex.axis <- 0.7  # Tamaño de fuente más pequeño para etiquetas del eje y

# Crear un gráfico de puntos y líneas para el ranking bayesiano
plot(x = c(min(SABER_3$Valores), max(SABER_3$Valores)), y = c(1, m), type = "n", xlab = "", ylab ='',
     main = "Ranking Pobreza Monetaria", yaxt = "n")
abline(h = 1:m, col = "lightgray", lwd = 1)
abline(v = mean(SABER_3$Y, na.rm = T), col = "gray", lwd = 3)

# Iterar a través de los departamentos para agregar puntos y líneas al gráfico bayesiano
for (l in 1:m) {
  j <- order(SABER_3$Valores)[l]
  points(x = SABER_3$Valores[j], y = l)
  if (is.na(SABER_3$Y[j])) {
    lines(x = c(Estimaciones_Monetaria$C_0.025[(j-32+8)], Estimaciones_Monetaria$C_0.975[(j-32+8)]), y = rep(l, 2))
  }
}
axis(side = 2, at = 1:m, labels = SABER_3$nombre[order(SABER_3$Valores)], las = las, cex.axis = cex.axis)


dev.off()

Estimaciones_Monetaria<-cbind(Estimaciones_Monetaria,SABER_3[25:32,'nombre'])

Estimaciones_Monetaria%>%arrange(desc(media))

# k-medias Municipios------

# Realizar el agrupamiento K-Means de los estimadores puntuales de zeta
set.seed(123454)
kmediasm <- kmeans(apply(chain5$THETA[, 1:1112], 2, mean), 8)

# Agregar los resultados al conjunto de datos
SABER_2 <- cbind(SABER_2,
                 BayesM = apply(chain5$THETA[, 1:1112], 2, mean),
                cluster= kmediasm$cluster)

# Cargar el archivo shapefile de municipios
mpioshp <- st_read("MGN_MPIO_POLITICO.shp", quiet = TRUE)
str(mpioshp)
# Combinar datos del shapefile con los resultados del análisis
mpioshp <- mpioshp %>% left_join(SABER_2, by = c("MPIO_CCNCT" = "COLE_COD_MCPIO_UBICACION"))

# Definir el número de clústeres para el gráfico
mpioshp$cluster <- factor(mpioshp$cluster)

order(kmediasm$centers)
# Definir una paleta de colores
pale_colors <- brewer.pal(n = nlevels(mpioshp$cluster), name = "Pastel1")
show_col(pale_colors)


# 
# "#CCEBC5"Verde 7
# "#DECBE4" Mordao 4 
# "#B3CDE3" Azul 5  
#  "#FBB4AE" rojo 2
# '#FED9A6' Naranja3
# '#FFFFCC' Amarillo 8
# '#E5D8BD'Cafe 6
# '#FDDAEC' Rosado 1
cluster_colors <- c( '#FDDAEC', "#FBB4AE", "#FED9A6","#DECBE4","#B3CDE3",'#E5D8BD',"#CCEBC5",'#FFFFCC')


MediasM<-round(kmediasm$centers,2)

# Crear un gráfico de mapas con colores para los clústeres

KMEDIASM<-ggplot() +
  geom_sf(data = mundocol) +
  geom_sf(data = mpioshp, aes(fill =cluster), col = "darkgray", linetype = "solid") +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax), expand = FALSE) +
  labs(x = "Longitud", y = "Latitud", title = "Cluster Municipios", fill = "Media Cluster") +
  scale_fill_manual(values = cluster_colors, breaks = 1:8, labels = MediasM) +
  annotate("text", x = c(-74.5, -68, -78, -69, -78.5), y = c(-2.5, 0, -1, 9, 9), colour = "blue",
           label = c("Perú", "Brasil", "Ecuador", "Venezuela", "Panamá")) +
  theme(panel.background = element_rect(fill = "lightblue"),
plot.title = element_text(hjust = 0.5))
ggsave("KMEDIASM.png", plot = KMEDIASM, width = 8, height = 8)

# Calcular la matriz de incidencia para los clústeres
n = 1112
A <- matrix(data = 0, nrow = n, ncol = n)
        
# Sumar las veces en las que i coincide con j
for (i in 1:(n - 1)) {
for (j in (i + 1):n) {
    A[i, j] = sum(chain5$ClusterM[, i] == chain5$ClusterM[, j])
   } 
}
        
 # Normalizar la matriz de incidencia
A = A * 1 / B
          
# Sumar la transpuesta para obtener la matriz simétrica
A <- A + t(A)
          
# Establecer la diagonal principal de la matriz como 1
diag(A) <- 1
          
# Organizar las observaciones de acuerdo a la partición verdadera
indices <- order(SABER_2$BayesM)
A <- A[indices, indices]
# Visualización de la matriz de incidencia
par(mar = c(2.75, 2.75, 0.5, 0.5), mgp = c(1.7, 0.7, 0))
corrplot::corrplot(corr = A, is.corr = FALSE, addgrid.col = NA, method = "color", tl.pos = "n")
# Estimacion Neta por Municipios----
Estimaciones_Neta <- cbind(
  media = apply(chain5$Estimacion_neta, 2, mean),
  C_0.025 = apply(chain5$Estimacion_neta, 2, function(x) quantile(x, 0.025)),
  C_0.975 = apply(chain5$Estimacion_neta, 2, function(x) quantile(x, 0.975))
)
Estimaciones_Neta <- as.data.frame(Estimaciones_Neta)

# Combinar las estimaciones monetarias con los datos existentes
Valores <- c(SABER_2$COBERTURA_NETA)
Valores[(is.na(Valores))] <-  c(Estimaciones_Neta$media)
SABER_2 <- cbind(SABER_2, Valores)
mpioshp <- st_read("MGN_MPIO_POLITICO.shp", quiet = TRUE)
# Combinar datos del shapefile con las estimaciones de cobertura neta

# Como unicamente no se midieron 2 municipios vamos a ver cuales son y en que departamento se encuentran para hacer 2 mapas
# de los respctivos departamentos y sus municipios ya con la estimacion 

colnames(SABER_2)
SABER_3
SABER_2[is.na(SABER_2$COBERTURA_NETA),]
unique(SB11_20222$COLE_MCPIO_UBICACION[SB11_20222$COLE_COD_MCPIO_UBICACION=='27086'])
unique(SB11_20222$COLE_MCPIO_UBICACION[SB11_20222$COLE_COD_MCPIO_UBICACION=='94663'])
Estimaciones_Neta



# podemos ver que los municipios son del departamento del Choco y Guainia
choco <- mpioshp %>%
  left_join(SABER_2,by=c("MPIO_CCNCT"="COLE_COD_MCPIO_UBICACION")) %>%
  filter(DPTO_CCDGO =="27")
str(choco)


Choco<-ggplot() +
  geom_sf(data = choco, aes(fill = Valores), col = "darkgray", linetype = "solid") +
  geom_sf_text(data = choco[choco$MPIO_CCNCT == '27086', ], aes(label = "BELÉN DE BAJIRÁ"), col = "black",
               fontface = "bold", size = 3.5, fun.geometry = function(x) sf::st_centroid(x)) +
  labs(x = "Longitud", y = "Latitud", title = "Chocó", fill = "Cobertura Neta") +
  scale_fill_gradient(low = "white", high = "red", n.breaks = 5)

guainia <- mpioshp %>%
  left_join(SABER_2,by=c("MPIO_CCNCT"="COLE_COD_MCPIO_UBICACION")) %>%
  filter(DPTO_CCDGO =="94")

Guainia<-ggplot() +
  geom_sf(data = guainia , aes(fill = Valores), col = "darkgray", linetype = "solid") +
  geom_sf_text(data = guainia [choco$MPIO_CCNCT == '94663', ], aes(label = "BELÉN DE BAJIRÁ"), col = "black",
               fontface = "bold", size = 3.5, fun.geometry = function(x) sf::st_centroid(x)) +
  labs(x = "Longitud", y = "Latitud", title = "Guainía", fill = "Cobertura Neta ") +
  scale_fill_gradient(low = "white", high = "red", n.breaks = 5)

ggsave("Choco.pdf", plot = Choco, width = 8, height = 8)
ggsave("Guainia.pdf", plot = Guainia, width = 8, height = 8)



#DIC, WAIC y Validación   con foreach y doParallel---------
library(foreach)
library(doParallel)

# 32 Departamentos
# 1112 Municipios
n=length(y)
# Calculamos los estimadores bayesianos puntuales
# Modelo 1
ThetaB<-mean(chain$THETA$theta) ; sigmaB<-mean(chain$THETA$isig2)
# Modelo 2
ThetaBj<-apply(chain2$THETA[,1:32],2,mean) ; sigmaB2<-mean(chain2$THETA$sig2)
# Modelo 3
ThetaBj2<-apply(chain3$THETA[,1:32],2,mean) ; sigmaBj2<-apply(chain3$THETA[,33:64],2,mean)
# Modelo 4
ZetaB<-apply(chain4$THETA[,1:1112],2,mean) ; KappaB<-mean(chain4$THETA$kappa2)
# Modelo 5
ZetaB2<-apply(chain5$THETA[,1:1112],2,mean) ; KappaB2<-mean(chain5$THETA$kappa2)

# DIC

# -2 log( p | thetaB )+ 2 p_DIC,   PDIC= 2 (log( p | thetaB) - 1/B sum ( log (y | theta^(b) ) )
# En el modelo uno se trabajo con la precision 
DIC_1 <- -2 * sum(dnorm(x = y, mean = ThetaB, sd = sqrt(1/sigmaB), log = T)) + 
  2 * 2 * (sum(dnorm(x = y, mean = ThetaB, sd = sqrt(1/sigmaB), log = T) )- mean(L))

DIC_2 <- -2 * sum(dnorm(x = y, mean = mean(rep(ThetaBj, nk_estudiantes)), sd = sqrt(sigmaB2), log = TRUE)) + 2 *
  2 * (sum(dnorm(x = y, mean = mean(rep(ThetaBj, nk_estudiantes)), sd = sqrt(sigmaB2), log = T) )- mean(L2))


DIC_3 <- -2 * sum(dnorm(x = y, mean = mean(rep(ThetaBj2, nk_estudiantes)), sd = sqrt(rep(sigmaBj2, nk_estudiantes)), log = T)) + 
  2 * 2 * (sum(dnorm(x = y, mean = mean(rep(ThetaBj2, nk_estudiantes)), sd = sqrt(rep(sigmaBj2, nk_estudiantes)), log = T) )- mean(L3))


DIC_4 <- -2 * sum(dnorm(x = y, mean = mean(rep(ZetaB, njk)), sd = sqrt(KappaB), log = T)) + 2 * 
  2 * (sum(dnorm(x = y, mean = mean(rep(ZetaB, njk)), sd = sqrt(KappaB), log = T) )- mean(L4))

DIC_5 <- -2 * sum(dnorm(x = y, mean = mean(rep(ZetaB2, njk)), sd = sqrt(KappaB2), log = TRUE)) + 2 * 
  2 * (sum(dnorm(x = y, mean = mean(rep(ZetaB2, njk)), sd = sqrt(KappaB2), log = T) )- mean(L5))
#WAIC

library(doParallel)
library(foreach)

cores <- detectCores()

cl <- makeCluster(cores-1)

#WAIC = -2lppd + 2 pWAIC ; lppd = sum log ( 1/B sum p (y_i | theta^(b) ) ) ; 
# pWAIC = 2 sum ( log ( 1/B sum p (y_i | theta^(b) ) ) - 1/B sum log p (y_i | theta^(b) ) )

WAIC_1<- foreach(i =1:n,.combine = cbind)%dopar%{
  # lppd
  tmp <- dnorm(x = y[i], mean = chain$THETA$theta, sd = sqrt(1/chain$THETA$isig2))
  lppd <-log(mean(tmp))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain$THETA$theta, sd =  sqrt(1/chain$THETA$isig2), log = T)
  pWAIC <- 2*(log(mean(tmp)) - mean(tmp2))
  
  r<- -2*lppd+ 2 *pWAIC
  return(r)
}
WAIC_1<-sum(WAIC_1)

# creamos un indice que nos indique en cual departamento se encuentra cada niño

g<-rep(seq(1:32), nk_estudiantes)

WAIC_2<- foreach(i =1:n,.combine = cbind)%dopar%{
  # lppd
  tmp <- dnorm(x = y[i], mean = chain2$THETA[,g[i]], sd = sqrt(chain2$THETA$sig2))
  lppd <-log(mean(tmp))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain2$THETA[,g[i]], sd =  sqrt(chain2$THETA$sig2), log = T)
  pWAIC <- 2*(log(mean(tmp)) - mean(tmp2))
  
  r<- -2*lppd+ 2 *pWAIC
  return(r)
}
WAIC_2<-sum(WAIC_2)

WAIC_3<- foreach(i =1:n,.combine = cbind)%dopar%{
  # lppd
  tmp <- dnorm(x = y[i], mean = chain3$THETA[,g[i]], sd = sqrt(chain3$THETA[,32+g[i]]))
  lppd <-log(mean(tmp))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain3$THETA[,g[i]], sd =  sqrt(chain3$THETA[,32+g[i]]), log = T)
  pWAIC <- 2*(log(mean(tmp)) - mean(tmp2))
  
  r<- -2*lppd+ 2 *pWAIC
  return(r)
}
WAIC_3<-sum(WAIC_3)

# Ahora creamos un indice que indique a que municipio pertenece el niño

g2<- rep(seq(1:1112), njk)

WAIC_4<- foreach(i =1:n,.combine = cbind)%dopar%{
  # lppd
  tmp <- dnorm(x = y[i], mean = chain4$THETA[,g[i]], sd = sqrt(chain4$THETA$kappa2))
  lppd <-log(mean(tmp))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain4$THETA[,g[i]], sd = sqrt(chain4$THETA$kappa2), log = T)
  pWAIC <- 2*(log(mean(tmp)) - mean(tmp2))
  
  r<- -2*lppd+ 2 *pWAIC
  return(r)
}
WAIC_4<-sum(WAIC_4)


WAIC_5<- foreach(i =1:n,.combine = cbind)%dopar%{
  # lppd
  tmp <- dnorm(x = y[i], mean = chain5$THETA[,g[i]], sd = sqrt(chain5$THETA$kappa2))
  lppd <-log(mean(tmp))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain5$THETA[,g[i]], sd = sqrt(chain5$THETA$kappa2), log = T)
  pWAIC <- 2*(log(mean(tmp)) - mean(tmp2))
  
  r<- -2*lppd+ 2 *pWAIC
  return(r)
}
WAIC_5<-sum(WAIC_5)


D<-c(DIC_1,DIC_2,DIC_3,DIC_4,DIC_5)
W<-c(WAIC_1,WAIC_2,WAIC_3,WAIC_4,WAIC_5)
DIC_5
round(D,3);round(W,3)

x<-NULL
M<-matrix(NA,10000,6)


set.seed(19991027)

indicador<-NULL
for (i in 1:length(njk)) {
  indicador[i] <- sum(njk[1:i])
}

indicador <- c(0, indicador)
# funcion indicadora para calcular las estadisticas de los Municipios
media<-NULL
mediana<-NULL
sd<-NULL
riq<-NULL
max<-NULL
min<-NULL
# Calculasmos las estadisticas de los municipios
for (i in 1:length(njk)) {
  media[i]<-mean(y[( (indicador[i]) + 1) :indicador[i+1] ])
  mediana[i]<-median(y[( (indicador[i]) + 1) :indicador[i+1] ])
  sd[i]<-sd(y[( (indicador[i]) + 1) :indicador[i+1] ])
  riq[i]<-IQR(y[( (indicador[i]) + 1) :indicador[i+1] ])
  max[i]<-max(y[( (indicador[i]) + 1) :indicador[i+1] ])
  min[i]<-min(y[( (indicador[i]) + 1) :indicador[i+1] ])
}


Municipios<-cbind(njk,ZetaB2,KappaB2,media,mediana,sd,riq,max,min)
Municipios<-as.data.frame(Municipios)
MI<-rep(0,1112)

# calculamos vectores de 
# ppp media
MI<-foreach(i =1:10000,.combine = rbind)%dopar%{
  
  vec<-apply(Municipios,1,function(x) mean(rnorm(n=x[1], mean=(x[2]), sd= sqrt(x[3]))) > x[4] )
  return(vec)

}

pppmedia<-apply(MI,2,mean)
rm(MI)

# ppp mediana
MI<-foreach(i =1:10000,.combine = rbind)%dopar%{
  
  vec<-apply(Municipios,1,function(x) median(rnorm(x[1], mean=(x[2]), sd= sqrt(x[3]))) > x[5] )
  return(vec)
  
}

pppmediana<-apply(MI,2,mean)
rm(MI)

# ppp sd
MI<-foreach(i =1:10000,.combine = rbind)%dopar%{
  
  vec<-apply(Municipios,1,function(x) sd(rnorm(x[1], mean=(x[2]), sd= sqrt(x[3]))) > x[6] )
  return(vec)
  
}

pppsd<-apply(MI,2,mean)
rm(MI)


# ppp riq
MI<-foreach(i =1:10000,.combine = rbind)%dopar%{
  
  vec<-apply(Municipios,1,function(x) IQR(rnorm(x[1], mean=(x[2]), sd= sqrt(x[3]))) > x[7] )
  return(vec)
  
}

pppriq<-apply(MI,2,mean)
rm(MI)


# ppp max
MI<-foreach(i =1:10000,.combine = rbind)%dopar%{
  
  vec<-apply(Municipios,1,function(x) max(rnorm(x[1], mean=(x[2]), sd= sqrt(x[3]))) > x[8] )
  return(vec)
  
}

pppmax<-apply(MI,2,mean)
rm(MI)

# ppp min
MI<-foreach(i =1:10000,.combine = rbind)%dopar%{
  
  vec<-apply(Municipios,1,function(x) min(rnorm(x[1], mean=(x[2]), sd= sqrt(x[3]))) > x[9] )
  return(vec)
  
}

pppmin<-apply(MI,2,mean)
rm(MI)


stop(cluster)

pdf("Validacion.pdf", width=12, height=8)


# Colores
colores <- c("mistyrose", "lightblue1", "lavender", "peachpuff", "lightgreen", "lightpink")

# Crear un nuevo gráfico
par(mfrow = c(1, 1))

data <- as.data.frame(cbind(pppmedia, pppmediana, pppsd, pppriq, pppmax, pppmin))
colnames(data) <- c("Media", "Mediana", "Sd", "RIQ", "Max", "Min")

boxplot(data, col = colores, main='ppp Municipios',ylab='ppp')

# Crear los boxplots uno al lado del otro



dev.off()


# Código realizado con la ayuda de las notas de clase de estadística bayesiana y computación estadística
# de la Universidad Nacional de Colombia.