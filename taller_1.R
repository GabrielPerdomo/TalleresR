##punto1
#1.1
set.seed(145)


#1.2
lista_1 <- list(1:50)
lista_1.2 <- as.list(c(1:50))


lista_2 <- lapply(1:50, function(x) {
  sample(5:50, 1)
})
lista_2.2 <- list(sample(5:50,50, replace = TRUE))


lista_3 <- list(rep("Años", 50))
lista_3.2 <- as.list(rep("Años",50))

#cuarta lista
nombres <- c("Juan", "María", "Pedro", "Ana", "Luis", "Sofía", "Jorge", "Carlos", "Laura", "David", "Elena","Daniela","Gabriel","Daniel","Julio","Julian","carla","Diana")

lista_4<- list()
for (i in 1:50) {
  nombre_aleatorio <- sample(nombres, 1)
  lista_4[[paste("Persona", i)]] <- nombre_aleatorio
}
lista_4.2 <- list(sample(nombres, 50, replace=TRUE))

#1.3
lista_concatenada <- list(paste(lista_1.2,lista_4,"tiene",lista_2,lista_3.2))

#1.4
length(lista_1.2)


for (i in 1:length(lista_1.2)){ 
  inicial <- substr(lista_4[[i]], 1, 1)
  if (inicial != "J" & ((lista_2[[i]]) %% 2) == 0 ){ 
    
    print(lista_concatenada[[1]][i])
  }
}
print(i)
print(inicial)
lista_4[[1]]
print(lista_concatenada)
#1.5


promedio_y_desviacion <- function(lista) {
  
  promedio <- mean(lista)
  
  desviacion_estandar <- sd(lista)
  
  h = (list(promedio = promedio, desviacion_estandar = desviacion_estandar))
  return=h 
}


promedio_sd <- promedio_y_desviacion(lista_2.2[[1]])
print(promedio_sd)
unlist(lista_2)

#1.6 
#estandarizar = (xi-media)/(sd)

estandarizacion <- function(lista){
  m <- promedio_sd[[1]]
  v <- promedio_sd[[2]]
  
  datos_normalizados <- (lista - m) / v
  
}
std <- estandarizacion(lista_2.2[[1]])
print(std)

#1.7


# Generar datos para cada vector
salario <- rnorm(50, mean = 0, sd = 1)
indice_salud <- rnorm(50, mean = 0, sd = 1)
experiencia_laboral <- rnorm(50, mean = 0, sd = 1)


outcomes_nominales <- list("salario" = salario, "indice_salud" = indice_salud, "experiencia_laboral" = experiencia_laboral)

# Mostrar la lista de listas generada
print(outcomes_nominales)

#1.8

lista_b_matriz <- function(lista){

n = length(lista)

matriz_1 <- matrix(nrow = n, ncol = 2)

matriz_1[,1] <- 1

matriz_1[,2] <- unlist(lista)

return(matriz_1)
  
}




#1.9

edad_std <- as.list(std)
matriz_x <- lista_b_matriz(edad_std)
matriz_prueba <- matriz_x[,1]
matriz_prueba2 <- matriz_x[,2]

print(matriz_x)




##punto2 

###Punto 2.1 
calcular_MCO <- function(X, y) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  
  beta_gorro <- solve(t(X) %*% X) %*% t(X) %*% y
  
  residuos <- y - X %*% beta_gorro
  
  se <- sqrt(diag(solve(t(X) %*% X) * sum(residuos^2) / (n - p)))
  
  return(list(estimador = beta_gorro, error_estandar = se))
}


lista_prueba <- unlist(lista_1)

calcular_MCO(matriz_x, lista_prueba)


modelo <- lm(lista_prueba ~ matriz_x)

#### Punto 2.2 

matriz_MCO <- matrix(nrow = 3, ncol = 3)
nombres <- c('salario', 'salud_index', 'experiencia')

for (i in 1:length(outcomes_nominales)) {
  
  estimadores = calcular_MCO(lista_prueba, lista_b_matriz(outcomes_nominales[[i]]))

  
  # La primera tiene el nombre del outcome
  matriz_MCO[i, 1] <- nombres[i]
  
  # La segunda columna tiene el beta_hat
  matriz_MCO[i, 2] <- estimadores$estimador[1,1]
  
  # La tercera columna tiene el sigma
  matriz_MCO[i, 3] <- estimadores$error_estandar[1]
  
}

print(matriz_MCO)


#### 2.3 

for (i in 1:nrow(matriz_MCO)) {

  P1 = sprintf('cuando %s aumente en una unidad', matriz_MCO[i,1])
  P2 = sprintf(' lista1.2 %s, en promedio', matriz_MCO[i,2])
  print(paste(P1, P2))
}


### 2.4

calcular_MSE <- function(y, xi, beta0, beta1) {
  n <- length(y)
  y_pred <- beta0 + beta1 * xi
  mse <- sum((y - y_pred)^2) / n
  return(mse)
}

calcular_MSE(unlist(lista_1), unlist(edad_std), 0, 2)


encontrar_beta_minimo <- function(y, xi, beta0, beta1_inicial, factor_aumento) {
  mse_actual <- calcular_MSE(y, xi, beta0, beta1_inicial) 
  beta1_actual <- beta1_inicial
  
  while(TRUE) {
    beta1_siguiente <- beta1_actual + factor_aumento 
    mse_siguiente <- calcular_MSE(y, xi, beta0, beta1_siguiente)  
    
    if (mse_siguiente < mse_actual) {
      mse_actual <- mse_siguiente
      beta1_actual <- beta1_siguiente
    } else {
      break 
    }
  }
  
  return(beta1_actual)
}

### 2.6

beta0 <- 0
beta1_inicial <- -2
factor_aumento <- 0.1

resultados <- matrix(NA, nrow = length(outcomes_nominales), ncol = 2,
                     dimnames = list(NULL, c("Nombre del outcome", "Beta_min_1")))

for (i in seq_along(outcomes_nominales)) {
  outcome <- outcomes_nominales[[i]]
  beta1_minimo <- encontrar_beta_minimo(outcomes_nominales[[i]], unlist(edad_std), beta0, beta1_inicial, factor_aumento)
  resultados[i, ] <- c(names(outcomes_nominales)[i], beta1_minimo)
}


print(resultados)