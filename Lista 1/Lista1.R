### Lista 1 ###

## Questão 4 - 1.1 do livro ##

x1 <- c(3, 4, 2, 6, 8, 2, 5)
x2 <- c(5, 5.5, 4, 7, 10, 5, 7.5)

# Média

x1_barra <- sum(x1)/length(x1)
x1_barra #manual
mean(x1) #automático

x2_barra <- sum(x2)/length(x2)
x2_barra #manual
mean(x2) #automático

# Variância

s11 <- sum((x1-x1_barra)^2)/(length(x1)-1)
s11 #manual
var(x1) #automático

s22 <- sum((x2-x2_barra)^2)/(length(x2)-1)
s22 #manual
var(x2) #automático

# Covariância

s12 <- sum((x1-x1_barra)*(x2-x2_barra))/(length(x1)-1)
s12 #manual
cov(x1, x2) #automático

## Questão 5 - 1.2 do livro ##

# letra a #

x1 <- c(1, 2, 3, 3, 4, 5, 6, 8, 9, 11)
x2 <- c(18.95, 19, 17.95, 15.54, 14, 12.95, 8.94, 7.49, 6, 3.99)

library(ggplot2)
library(ggExtra)
library(gridExtra)

dd <- ggplot()+
  geom_point(aes(x1, x2))+
  theme_light()+
  labs(title="Preços de Carros Usados por Idade",
       x="Idade do carro (anos)",
       y="Preço de venda do carro (milhares de dólares)")

ggMarginal(dd, type="histogram")

# letra b #

#negativa

# letra c #

# Média

x1_barra <- sum(x1)/length(x1)
x1_barra #manual
mean(x1) #automático

x2_barra <- sum(x2)/length(x2)
x2_barra #manual
mean(x2) #automático

# Variância

s11 <- sum((x1-x1_barra)^2)/(length(x1)-1)
s11 #manual
var(x1) #automático

s22 <- sum((x2-x2_barra)^2)/(length(x2)-1)
s22 #manual
var(x2) #automático

# Covariância

s12 <- sum((x1-x1_barra)*(x2-x2_barra))/(length(x1)-1)
s12 #manual
cov(x1, x2) #automático

# Coeficiente de correlação

r12 <- s12/sqrt(s11*s22)
r12 #manual
cor(x1, x2) #automático

# letra d #

matrix(c(x1_barra, x2_barra), 2)
matrix(c(s11, s12, s12, s22), 2, byrow=T)
matrix(c(1, r12, r12, 1), 2, byrow=T)

## Questão 6 - 1.6 do livro ##


poluicao <- c(8, 98, 7, 2, 12, 8, 2,
              7, 107, 4, 3, 9, 5, 3,
              7, 103, 4, 3, 5, 6, 3,
              10, 88, 5, 2, 8, 15, 4,
              6, 91, 4, 2, 8, 10, 3,
              8, 90, 5, 2, 12, 12, 4,
              9, 84, 7, 4, 12, 15, 5,
              5, 72, 6, 4, 21, 14, 4,
              7, 82, 5, 1, 11, 11, 3,
              8, 64, 5, 2, 13, 9, 4,
              6, 71, 5, 4, 10, 3, 3,
              6, 91, 4, 2, 12, 7, 3,
              7, 72, 7, 4, 18, 10, 3,
              10, 70, 4, 2, 11, 7, 3,
              10, 72, 4, 1, 8, 10, 3,
              9, 77, 4, 1, 9, 10, 3,
              8, 76, 4, 1, 7, 7, 3,
              8, 71, 5, 3, 16, 4, 4,
              9, 67, 4, 2, 13, 2, 3,
              9, 69, 3, 3, 9, 5, 3,
              10, 62, 5, 3, 14, 4, 4,
              9, 88, 4, 2, 7, 6, 3,
              8, 80, 4, 2, 13, 11, 4,
              5, 30, 3, 3, 5, 2, 3,
              6, 83, 5, 1, 10, 23, 4,
              8, 84, 3, 2, 7, 6, 3,
              6, 78, 4, 2, 11, 11, 3,
              8, 79, 2, 1, 7, 10, 3,
              6, 62, 4, 3, 9, 8, 3,
              10, 37, 3, 1, 7, 2, 3,
              8, 71, 4, 1, 10, 7, 3,
              7, 52, 4, 1, 12, 8, 4,
              5, 48, 6, 5, 8, 4, 3,
              6, 75, 4, 1, 10, 24, 3,
              10, 35, 4, 1, 6, 9, 2,
              8, 85, 4, 1, 9, 10, 2,
              5, 86, 3, 1, 6, 12, 2,
              5, 86, 7, 2, 13, 18, 2,
              7, 79, 7, 4, 9, 25, 3,
              7, 79, 5, 2, 8, 6, 2,
              6, 68, 6, 2, 11, 14, 3,
              8, 40, 4, 3, 6, 5, 2)
poluicao <- matrix(poluicao, 42, 7, byrow=T)
poluicao <- data.frame(poluicao)
names(poluicao) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")

# letra a #

ggplot(poluicao)+
  geom_dotplot(aes(x1))+
  labs(title="Distribuição de Frequências da Variável Vento", x="Vento", y="Contagem")
ggplot(poluicao)+
  geom_dotplot(aes(x2))+
  labs(title="Distribuição de Frequências da Variável Radiação Solar", x="Radiação Solar", y="Contagem")
ggplot(poluicao)+
  geom_dotplot(aes(x3))+
  labs(title="Distribuição de Frequências da Variável CO", x="CO", y="Contagem")
ggplot(poluicao)+
  geom_dotplot(aes(x4))+
  labs(title="Distribuição de Frequências da Variável NO", x="NO", y="Contagem")
ggplot(poluicao)+
  geom_dotplot(aes(x5))+
  labs(title="Distribuição de Frequências da Variável NO2", x="NO2", y="Contagem")
ggplot(poluicao)+
  geom_dotplot(aes(x6))+
  labs(title="Distribuição de Frequências da Variável O3", x="O3", y="Contagem")
ggplot(poluicao)+
  geom_dotplot(aes(x7))+
  labs(title="Distribuição de Frequências da Variável HC", x="HC", y="Contagem")

# letra b #

# Médias

matrix(colMeans(poluicao), ncol=1)

#representam os valores médios de vento, radiação solar, etc.

# Variância-Covariancia

cov(poluicao)

#na diagonal principal estão as variâncias, sendo a maior delas a da radiação solar.
#porém, a variância reflete a escala média, então não serve para comparações.
#a variável radiação solar assume valores bem maiores do que as demais, o que é refletido na sua variância
#para comparar, seria necessário usar os coeficientes de variação

# Coeficientes de correlação

cor(poluicao)

#O vento tem correlação negativa com todas as variáveis menos a emissão de HC
#A emissão de HC tem correlação positiva com todas as variáveis
#As emissões de CO e NO2 têm correlação positiva com todas as variáveis menos o vento


sum(cor(poluicao)==1) #somente a diagonal principal é =1
cor(poluicao)>0.5 & cor(poluicao)!=1

#as correlações mais fortes são entre (x3, x4) e (x3, x5)
#ou seja, entre emissão de CO e emissão de NO/NO2

## Questão 7 - 1.22 do livro ##

oxigenio <- c(0.34, 3.71, 2.87, 30.87, 0.29, 5.04, 1.93, 33.85,
              0.39, 5.08, 3.38, 43.85, 0.28, 3.95, 2.51, 35.82,
              0.48, 5.13, 4.13, 44.51, 0.31, 4.88, 2.31, 36.40,
              0.31, 3.95, 3.60, 46.00, 0.30, 5.97, 1.90, 37.87,
              0.36, 5.51, 3.11, 47.02, 0.28, 4.57, 2.32, 38.30,
              0.33, 4.07, 3.95, 48.50, 0.11, 1.74, 2.49, 39.19,
              0.43, 4.77, 4.39, 48.75, 0.25, 4.66, 2.12, 39.21,
              0.48, 6.69, 3.50, 48.86, 0.26, 5.28, 1.98, 39.94,
              0.21, 3.71, 2.82, 48.92, 0.39, 7.32, 2.25, 42.41,
              0.32, 4.35, 3.59, 48.38, 0.37, 6.22, 1.71, 28.97,
              0.54, 7.89, 3.47, 50.56, 0.31, 4.20, 2.76, 37.80,
              0.32, 5.37, 3.07, 51.15, 0.35, 5.10, 2.10, 31.10,
              0.40, 4.95, 4.43, 55.34, 0.29, 4.46, 2.50, 38.30,
              0.31, 4.97, 3.56, 56.67, 0.33, 5.60, 3.06, 51.80,
              0.44, 6.68, 3.86, 58.49, 0.18, 2.80, 2.40, 37.60,
              0.32, 4.80, 3.31, 49.99, 0.28, 4.01, 2.58, 36.78,
              0.50, 6.43, 3.29, 42.25, 0.44, 6.69, 3.05, 46.16,
              0.36, 5.99, 3.10, 51.70, 0.22, 4.55, 1.85, 38.95,
              0.48, 6.30, 4.80, 63.30, 0.34, 5.73, 2.43, 40.60,
              0.40, 6.00, 3.06, 46.23, 0.30, 5.12, 2.58, 43.69,
              0.42, 6.04, 3.85, 55.08, 0.31, 4.77, 1.97, 30.40,
              0.55, 6.45, 5.00, 58.80, 0.27, 5.16, 2.03, 39.46,
              0.50, 5.55, 5.23, 57.46, 0.66, 11.05, 2.32, 39.34,
              0.34, 4.27, 4.00, 50.35, 0.37, 5.23, 2.48, 34.86,
              0.40, 4.58, 2.82, 32.48, 0.35, 5.37, 2.25, 35.07)
oxigenio <- matrix(oxigenio, 50, 4, byrow=T)
oxigenio <- data.frame(oxigenio)
names(oxigenio) <- c("x1", "x2", "x3", "x4")
oxigenio$sexo <- rep(c("Masculino", "Feminino"), 25)

# letra a #

library(rgl)


oxigenio$cores <- rep(c("green", "purple"))

plot3d( 
  x=oxigenio$x1, y=oxigenio$x2, z=oxigenio$x3,
  col = oxigenio$cores,
  xlab="x1", ylab="x2", zlab="x3",
  type = 's',
  radius = .1)

plot3d( 
  x=oxigenio$x1, y=oxigenio$x2, z=oxigenio$x4,
  col = oxigenio$cores,
  xlab="x1", ylab="x2", zlab="x4",
  type = 's',
  radius = .5)

plot3d( 
  x=oxigenio$x1, y=oxigenio$x3, z=oxigenio$x4,
  col = oxigenio$cores,
  xlab="x1", ylab="x3", zlab="x4",
  type = 's',
  radius = .5)

plot3d( 
  x=oxigenio$x2, y=oxigenio$x3, z=oxigenio$x4,
  col = oxigenio$cores,
  xlab="x2", ylab="x3", zlab="x4",
  type = 's',
  radius = .5)

# letra b #

library(dplyr)

oxigenio$id <- 1:50

#x1

ggplot(oxigenio)+
  geom_boxplot(aes(x1, fill=sexo))+
  labs(title="Busca por outliers em x1", "x1", "")+
  coord_flip()

oxigenio%>%
  filter(x1>0.5 | x1<0.15, sexo=="Feminino")

#x2

ggplot(oxigenio)+
  geom_boxplot(aes(x2, fill=sexo))+
  labs(title="Busca por outliers em x2", "x2", "")+
  coord_flip()

oxigenio%>%
  filter(x2>7 | x2<3, sexo=="Feminino")

#x3

ggplot(oxigenio)+
  geom_boxplot(aes(x3, fill=sexo))+
  labs(title="Busca por outliers em x3", "x3", "")+
  coord_flip()

#x4

ggplot(oxigenio)+
  geom_boxplot(aes(x4, fill=sexo))+
  labs(title="Busca por outliers em x4", "x4", "")+
  coord_flip()

oxigenio%>%
  filter(x4>45 | x4<30, sexo=="Feminino")

oxigenio%>%
  filter(x4<40, sexo=="Masculino")

#existem 2 outliers em x1, ambos mulheres
#os mesmos 2 são outliers também em x2. Além disso, mais 2 observações são outliers em x2. Os 4 são mulheres
#x3 não apresenta outliers
#x4 apresenta 2 outliers homens e 3 mulheres (das quais nenhuma é uma das mencionadas antes)

#ou seja, existe um total de 9 outliers (2 homens e 7 mulheres)
