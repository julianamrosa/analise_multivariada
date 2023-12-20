## lista 6 ##

#questão 1

gera_normal <- function(n, mu, sigma){
  p <- length(mu)
  decomp <- eigen(sigma)
  lambda <- decomp$values
  e <- decomp$vectors
  D <- e%*%diag(sqrt(lambda))%*%t(e)
  Y <- matrix(rnorm(n*p), n)
  X <- Y%*%D+matrix(mu, n, p, byrow=T)
  X
}

S <- matrix(c(1, -1.5, -1.5, 4), 2)
MU <- c(3, 2)
N <- 1000

set.seed(49058767)
x <- gera_normal(N, MU, S)

x <- data.frame(x)

library(car)

dataEllipse(x$X1, x$X2, levels = c(0.5, 0.95), 
             center.pch = 19, col = "blue", 
             xlab = "X1", ylab = "X2", 
             main = "Elipses de Contorno para Dados Normais Multivariados")

qqplot(x$X1, x$X2, xlab = "X1", ylab = "X2", main = "Q-Q Plot para Dados Simulados")

c(mean(x$X1), mean(x$X2))

cov(x)

#questão 2

MU <- c(1, 2)

##a=0

S <- matrix(c(2, 0, 2, 0), 2)

set.seed(4987456)
x <- gera_normal(N, MU, S)

x <- data.frame(x)

dataEllipse(x$X1, x$X2, levels = c(0.5, 0.95), 
            center.pch = 19, col = "blue", 
            xlab = "X1", ylab = "X2", 
            main = "Elipses de Contorno para Dados Normais Multivariados", sub="a=0", col.sub="blue")

##a=-0.5

S <- matrix(c(2, -0.5, 2, -0.5), 2)

set.seed(84765473)
x <- gera_normal(N, MU, S)

x <- data.frame(x)

dataEllipse(x$X1, x$X2, levels = c(0.5, 0.95), 
            center.pch = 19, col = "blue", 
            xlab = "X1", ylab = "X2", 
            main = "Elipses de Contorno para Dados Normais Multivariados", sub="a=-1/2", col.sub="blue")

##a=0.5

S <- matrix(c(2, 0.5, 2, 0.5), 2)

set.seed(98437876)
x <- gera_normal(N, MU, S)

x <- data.frame(x)

dataEllipse(x$X1, x$X2, levels = c(0.5, 0.95), 
            center.pch = 19, col = "blue", 
            xlab = "X1", ylab = "X2", 
            main = "Elipses de Contorno para Dados Normais Multivariados", sub="a=1/2", col.sub="blue")

##a=1

S <- matrix(c(2, 1, 2, 1), 2)

set.seed(934765874)
x <- gera_normal(N, MU, S)

x <- data.frame(x)

dataEllipse(x$X1, x$X2, levels = c(0.5, 0.95), 
            center.pch = 19, col = "blue", 
            xlab = "X1", ylab = "X2", 
            main = "Elipses de Contorno para Dados Normais Multivariados", sub="a=1", col.sub="blue")

#questão 3

A <- matrix(c(1, 1), 1)
B <- matrix(c(1, -1), 1)

S <- diag(1, 2)

A%*%S%*%t(B)

#questão 12

dados <- read.table("C:/Juliana/Multi/lista6_tabela.txt")
names(dados) <- c('density', 'machine_direction', 'cross_direction')

##densisty

boxplot(dados$density, main="Density")

qqnorm(dados$density, xlab="Quantis da Normal", ylab="Density")

shapiro.test(dados$density)

##machine direction

boxplot(dados$machine_direction, main="Machine Direction")

qqnorm(dados$machine_direction, xlab="Quantis da Normal", ylab="Machine Direction")

shapiro.test(dados$machine_direction)

##cross direction

boxplot(dados$cross_direction, main="Cross Direction")

qqnorm(dados$cross_direction, xlab="Quantis da Normal", ylab="Cross Direction")

shapiro.test(dados$cross_direction)

##pares de variáveis

dataEllipse(dados$density, dados$machine_direction, levels = c(0.5, 0.95), 
            center.pch = 19, col = "blue", 
            xlab = "Density", ylab = "Machine Direction", 
            main = "Elipses de Contorno para X1 e X2")

dataEllipse(dados$density, dados$cross_direction, levels = c(0.5, 0.95), 
            center.pch = 19, col = "blue", 
            xlab = "Density", ylab = "Cross Direction", 
            main = "Elipses de Contorno para X1 e X3")

dataEllipse(dados$machine_direction, dados$cross_direction, levels = c(0.5, 0.95), 
            center.pch = 19, col = "blue", 
            xlab = "Machine Direction", ylab = "Cross Direction", 
            main = "Elipses de Contorno para X2 e X3")

qqplot(dados$density, dados$machine_direction, xlab = "Density", ylab = "Machine Direction", main = "Q-Q Plot para X1 e X2")

qqplot(dados$density, dados$cross_direction, xlab = "Density", ylab = "Cross Direction", main = "Q-Q Plot para X1 e X3")

qqplot(dados$machine_direction, dados$cross_direction, xlab = "Machine Direction", ylab = "Cross Direction", main = "Q-Q Plot para X2 e X3")


