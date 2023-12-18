cereais <- read.table('C:/Users/Juliana Rosa/OneDrive/Documents/UnB - Disciplinas/Semestre 10/Multi/Lista 7/Brands-of-Cereal-T11-9.txt')
names(cereais) <- c("brand", "manufacturer", "calories", "protein", "fat",
                    "sodium", "fiber", "carbhydrates", "sugar", "potassium", "group")

#letra a

dists_euclidianas <- c()

#for (i in 1:42){
#  for (j in (i+1):43){
#    d <- sqrt(sum((cereais[i, 3:10]-cereais[j, 3:10])^2))
#    dists_euclidianas <- append(dists_euclidianas, d)
#  }
#}
#versão abaixo é mais demorada, mas fica mais fácil para montar a matriz

for (i in 1:43){
  for (j in 1:43){
    d <- sqrt(sum((cereais[i, 3:10]-cereais[j, 3:10])^2))
    dists_euclidianas <- append(dists_euclidianas, d)
  }
}

D <- matrix(dists_euclidianas, 43)

D

dists <- dist(cereais[, 3:10]) #para letra b

#letra b

ligacao_unica <- hclust(dists, method="single")

plot(ligacao_unica, main="Dendograma", xlab="Ligação Simples", ylab="Distânica")

ligacao_completa <- hclust(dists, method="complete")

plot(ligacao_completa, main="Dendograma", xlab="Ligação Completa", ylab="Distânica")

#questão 10

kmedias2 <- kmeans(cereais[, 3:10], centers=2)

plot(cereais[, 3:10], col=kmedias2$cluster, main="KMeans para 2 Grupos")

kmedias3 <- kmeans(cereais[, 3:10], centers=3)

plot(cereais[, 3:10], col=kmedias3$cluster, main="KMeans para 3 Grupos")

kmedias4 <- kmeans(cereais[, 3:10], centers=4)

plot(cereais[, 3:10], col=kmedias4$cluster, main="KMeans para 4 Grupos")
