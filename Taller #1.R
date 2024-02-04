##punto1
#1.1
set.seed(132)


#1.2
lista_1 <- list(1:50)
lista_1.2 <- as.list(c(1:50))


lista_2 <- lapply(1:50, function(x) {
  sample(5:50, 1)
})

lista_2.2 <- list()
for (i in 1:50) {
  lista_2.2[[i]] <- sample(5:50, 1)
}

lista_3 <- list(rep("Años", 50))
lista_3.2 <- as.list(rep("Años",50))

#cuarta lista
nombres <- c("Juan", "María", "Pedro", "Ana", "Luis", "Sofía", "Carlos", "Laura", "David", "Elena","Daniela","Gabriel","Daniel","Julio","Julian","carla","Diana")

lista_4<- list()
for (i in 1:50) {
  nombre_aleatorio <- sample(nombres, 1)
  lista_4[[paste("Persona", i)]] <- nombre_aleatorio
}


#1.3
lista_concatenada <- c(lista_1.2,lista_4,lista_2.2,lista_3.2)

long <- length(lista_concatenada)

##punto2 