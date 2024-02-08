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
lista_concatenada <- list(paste(lista_1.2,lista_4,lista_2,lista_3.2))

#1.4
length(lista_1.2)


for (i in length(lista_1.2)){ 
  inicial <- substr(lista_4[i], 1, 1)
  if (inicial != "J" & as.integer(lista_2[i]) %% 2){ 
    print("la edad de", lista_2[i], "es", lista_2[i])
  }
}

#1.5


promedio_y_desviacion <- function(valores) {
  
  promedio <- mean(valores)
  
  desviacion_estandar <- sd(valores)
  
  return(list(promedio = promedio, desviacion_estandar = desviacion_estandar))
}


Promedio_sd <- promedio_y_desviacion(lista_2)
print(Promedio_sd)

##punto2 
x <- as.integer(lista_2[20])

is.integer(lista_2[20])
is.integer(x)



























