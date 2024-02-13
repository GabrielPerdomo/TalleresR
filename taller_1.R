
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


lista_3 <- list(rep("AÃ±os", 50))
lista_3.2 <- as.list(rep("AÃ±os",50))

#cuarta lista
nombres <- c("Juan", "MarÃ­a", "Pedro", "Ana", "Luis", "SofÃ­a", "Jorge", "Carlos", "Laura", "David", "Elena","Daniela","Gabriel","Daniel","Julio","Julian","carla","Diana")

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


promedio_y_desviacion <- function(valores) {
  
  promedio <- mean(valores)
  
  desviacion_estandar <- sd(valores)
  
  h = (list(promedio = promedio, desviacion_estandar = desviacion_estandar))
  return=h 
}


promedio_sd <- promedio_y_desviacion(lista_2.2[[1]])
print(promedio_sd)
unlist(lista_2)

#1.6 
#estandarizar = (xi-media)/(sd)

estandarizacion <- function(valores){
  m <- promedio_sd[[1]]
  v <- promedio_sd[[2]]
  
  datos_normalizados <- (valores - m) / v
  
}
std <- estandarizacion(lista_2.2[[1]])
print(std)

#1.7


# Generar datos para cada vector
salario <- rnorm(50, mean = 0, sd = 1)
indice_salud <- rnorm(50, mean = 0, sd = 1)
experiencia_laboral <- rnorm(50, mean = 0, sd = 1)


outcomes_nominales <- list(salario, indice_salud, experiencia_laboral)

# Mostrar la lista de listas generada
print(outcomes_nominales)

#1.8

lista_a_matriz <- function(lista) {
  
  num_elementos <- length(lista)
  
  
  max_longitud <- max(sapply(lista, length))
  
  
  matriz <- matrix(1, nrow = max_longitud, ncol = num_elementos + 1)
  
  
  for (i in 1:num_elementos) {
    matriz[1:length(lista[[i]]), i] <- lista[[i]]
  }
  
  
  return(matriz)
}

#1.9

edad_std <- list(std)
matriz_x <- lista_a_matriz(edad_std)
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

k <- calcular_MCO(matriz_x, lista_prueba)

#### Punto 2.2 
 
resultados <- matrix(NA, nrow = 3, ncol = 3)

for (i in outcomes_nominales){
  valores <- calcular_MCO(unlist(outcomes_nominales[[i]]))
  
  resultados[i, 1] <- names(outcomes_nominales)[i]
  
  resultados[i, 2] <- valores[[1]]
  
  resultados[i, 3] <- valores[[2]]
  
}
  
unlist(outcomes_nominales[[1]])

### punto 2.3

for (i in 1:nrow(resultados)) {
  
  print(resultados[i, 1])
  
  cat("Coeficiente estimado (??1):", resultados[i, 2], "\n")
  
  cat("Error estándar (?? ??):", resultados[i, 3], "\n")
  
  cat("Interpretación económica:\n")
  
  cat("Un incremento unitario en", resultados[i, 1], "se asocia con un cambio de", resultados[i, 2], "en la variable de respuesta, con un error estándar de", resultados[i, 3], "\n\n")
}

###2.4








