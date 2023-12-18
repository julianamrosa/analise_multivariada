## LISTA 5 ##

#questão 4

Rr <- matrix(c(0.81, 0.63, 0.45, 0.63,
               0.49, 0.35, 0.45, 0.35, 0.25), 3, byrow=T)
eigen(Rr)

#questão 7

L <- matrix(c(0.602, 0.2, 0.467, 0.154,
              0.926, 0.143, 1, 0, 0.874,
              0.476, 0.894, 0.327), 6, byrow=T)
LLt <- L%*%t(L)

R <- matrix(c(1, 0.505, 0.569, 0.602, 0.621, 0.603,
              0.505, 1, 0.422, 0.467, 0.482, 0.450,
              0.569, 0.422, 1, 0.926, 0.877, 0.878,
              0.602, 0.467, 0.926, 1, 0.874, 0.894,
              0.621, 0.482, 0.877, 0.874, 1, 0.937,
              0.603, 0.450, 0.878, 0.894, 0.937, 1), 6, byrow=T)

R-LLt

#questão 8

L_rotacionado <- matrix(c(0.484, 0.411, 0.375,
                          0.319, 0.603, 0.717,
                          0.519, 0.855, 0.861,
                          0.499, 0.744, 0.594), 6, byrow=T)

h <- c(0.602^2+0.2^2, 0.467^2+0.154^2, 0.926^2+0.143^2,
       1^2+0^2, 0.874^2+0.476^2, 0.894^2+0.327^2)

#h_rotacionado <- c(0.484^2+0.411^2, 0.375^2+0.319^2, 0.603^2+0.717^2,
#                   0.519^2+0.855^2, 0.861^2+0.499^2, 0.744^2+0.594^2)

#varimax para loadings originais
sum(apply(L^2/h, 2, sum)-apply(L^2/h, 2, sum)^2/6)/6

#varimax para loadings rotacionados
sum(apply(L_rotacionado^2/h, 2, sum)-apply(L_rotacionado^2/h, 2, sum)^2/6)/6

psi <- diag(c(0.598, 0.758, 0.122, 0, 0.01, 0.094))

qui_quadrado100 <- (100-1 -(2*6+4*2+5)/6)*log(det(L%*%t(L)+psi)/det(R))

qui_quadrado50 <- (50-1 -(2*6+4*2+5)/6)*log(det(L%*%t(L)+psi)/det(R))

qui_quadrado25 <- (25-1 -(2*6+4*2+5)/6)*log(det(L%*%t(L)+psi)/det(R))

qui_quadrado12 <- (12-1 -(2*6+4*2+5)/6)*log(det(L%*%t(L)+psi)/det(R))

gl <- ((6-2)^2-6-2)/2

pchisq(qui_quadrado100, gl)

pchisq(qui_quadrado50, gl)

pchisq(qui_quadrado25, gl)

pchisq(qui_quadrado12, gl)

#questão 9
##letra a

R <- matrix(c(1, 0.738, 0.731, 0.828, 0.681, 0.712, 0.625, 0.604,
              0.738, 1, 0.520, 0.688, 0.831, 0.543, 0.322, 0.303,
              0.731, 0.520, 1, 0.652, 0.513, 0.826, 0.579, 0.617,
              0.828, 0.688, 0.652, 1, 0.887, 0.867, 0.639, 0.563,
              0.681, 0.831, 0.513, 0.887, 1, 0.692, 0.419, 0.352,
              0.712, 0.543, 0.826, 0.867, 0.692, 1, 0.608, 0.610,
              0.625, 0.322, 0.579, 0.639, 0.419, 0.608, 1, 0.937,
              0.604, 0.303, 0.617, 0.563, 0.352, 0.610, 0.937, 1), 8, byrow=T)
R

L <- matrix(c(0.433, 0.612, 0.499,
              0.125, 0.892, 0.234,
              0.296, 0.238, 0.887,
              0.406, 0.708, 0.483,
              0.198, 0.895, 0.283,
              0.331, 0.414, 0.789,
              0.928, 0.160, 0.294,
              0.910, 0.079, 0.355), 8, byrow=T)
L

#comunalidades
h2 <- apply(L^2, 1, sum)
h2

#variâncias únicas
1-h2

##letra b

#matriz de resíduos
R-L%*%t(L)-diag(1-h2)

#proporção da variância explicada
apply(L^2, 2, sum)/sum(L^2)

#questão 10

dados <- read.table("C:/Users/Juliana Rosa/OneDrive/Documents/UnB - Disciplinas/Semestre 10/Multi/Lista 5/table_9_12_salespeople.txt")

#matriz de correlação
R <- cor(dados)
R

##letra a

##m=2

#autovalores
D <- diag(eigen(R)$values[1:2])
D

#autovetores
C <- eigen(R)$vectors[, 1:2]
C

#estimativas dos loadings
L_m2 <- C%*%sqrt(D)
L_m2

#comunalidades estimadas
h2_m2 <- apply(L_m2^2, 1, sum)
h2_m2

#variâncias únicas
psi_m2 <- diag(1-h2_m2)
psi_m2

##m=3

#autovalores
D <- diag(eigen(R)$values[1:3])
D

#autovetores
C <- eigen(R)$vectors[, 1:3]
C

#estimativas dos loadings
L_m3 <- C%*%sqrt(D)
L_m3

#comunalidades estimadas
h2_m3 <- apply(L_m3^2, 1, sum)
h2_m3

#variâncias únicas
psi_m3 <- diag(1-h2_m3)
psi_m3

##letra b

library(psych)
principal(R, nfactors=2, rotate="varimax")
principal(R, nfactors=3, rotate="varimax")

##letra c

R-L_m2%*%t(L_m2)+diag(1-h2_m2)
R-L_m3%*%t(L_m3)+diag(1-h2_m3)

apply(L_m2^2, 2, sum)/8
apply(L_m3^2, 2, sum)/8

##letra d

qui_quadrado_m2 <- (50-1 -(2*7+4*2+5)/6)*log(det(L_m2%*%t(L_m2)+psi_m2)/det(R))

gl_m2 <- ((7-2)^2-7-2)/2

pchisq(qui_quadrado_m2, gl_m2)

qui_quadrado_m3 <- (50-1 -(2*7+4*3+5)/6)*log(det(L_m3%*%t(L_m3)+psi_m3)/det(R))

gl_m3 <- ((7-3)^2-7-3)/2

pchisq(qui_quadrado_m3, gl_m3)

##letra e

#x <- (c(110, 98, 105, 15, 18, 12, 35)-c(mean(dados$V1),
#                                       mean(dados$V2),
#                                      mean(dados$V3),
#                                       mean(dados$V4),
#                                       mean(dados$V5),
#                                       mean(dados$V6),
#                                       mean(dados$V7)))/c(sd(dados$V1),
#                                                          sd(dados$V2),
#                                                          sd(dados$V3),
#                                                          sd(dados$V4),
#                                                          sd(dados$V5),
#                                                          sd(dados$V6),
#                                                          sd(dados$V7))

x <- c(110, 98, 105, 15, 18, 12, 35)

solve(t(L_m2)%*%solve(psi_m2)%*%L_m2)%*%t(L_m2)%*%solve(psi_m2)%*%(x-c(mean(dados$V1),
                                                                       mean(dados$V2),
                                                                       mean(dados$V3),
                                                                       mean(dados$V4),
                                                                       mean(dados$V5),
                                                                       mean(dados$V6),
                                                                       mean(dados$V7)))

#questão 11

dados <- read.table("C:/Users/Juliana Rosa/OneDrive/Documents/UnB - Disciplinas/Semestre 10/Multi/Lista 5/table1_5-air-pollution.txt")

S <- cov(dados[, 1:6])
S

##letra a

pr_m1 <- principal(S, nfactors=1, rotate="none", covar=T)
pr_m1

pr_m2 <- principal(S, nfactors=2, rotate="none", covar=T)
pr_m2

##letra b

mv_m1 <- factanal(covmat=S, factors=1, rotation="none")
mv_m1

mv_m2 <- factanal(covmat=S, factors=2, rotation="none")
mv_m2

##letra c

S-pr_m1$loadings%*%t(pr_m1$loadings)-diag(pr_m1$uniquenesses)
apply(pr_m1$loadings^2, 2, sum)/sum(diag(S))

S-pr_m2$loadings%*%t(pr_m2$loadings)-diag(pr_m2$uniquenesses)
apply(pr_m2$loadings^2, 2, sum)/sum(diag(S))

R-mv_m1$loadings%*%t(mv_m1$loadings)-diag(mv_m1$uniquenesses)
apply(mv_m1$loadings^2, 2, sum)/6

R-mv_m2$loadings%*%t(mv_m2$loadings)-diag(mv_m2$uniquenesses)
apply(mv_m2$loadings^2, 2, sum)/6

#questão 12

pr_rot <- principal(S, nfactors=2, rotate="varimax", covar=T)
pr_rot

mv_rot <- factanal(covmat=S, factors=2, rotation="varimax")
mv_rot

#questão 13

##letra a

mv_mq <- factanal(x=dados[, 1:6], factors=2, rotation="none", scores="Bartlett")
principal(dados[, 1:6], nfactors=2, rotate="none")$scores

mv_reg <- factanal(x=dados[, 1:6], factors=2, rotation="none", scores="regression")
mv_reg$scores

##letra b

pr_m2$scores

#questão 14

R <- cor(dados[, 1:6])
R

##letra a

pr_m1 <- principal(R, nfactors=1, rotate="none", covar=T)
pr_m1

pr_m2 <- principal(R, nfactors=2, rotate="none", covar=T)
pr_m2

##letra b

mv_m1 <- factanal(covmat=R, factors=1, rotation="none")
mv_m1

mv_m2 <- factanal(covmat=R, factors=2, rotation="none")
mv_m2

##letra c

R-pr_m1$loadings%*%t(pr_m1$loadings)-diag(pr_m1$uniquenesses)
apply(pr_m1$loadings^2, 2, sum)/6

R-pr_m2$loadings%*%t(pr_m2$loadings)-diag(pr_m2$uniquenesses)
apply(pr_m2$loadings^2, 2, sum)/6

R-mv_m1$loadings%*%t(mv_m1$loadings)-diag(mv_m1$uniquenesses)
apply(mv_m1$loadings^2, 2, sum)/6

R-mv_m2$loadings%*%t(mv_m2$loadings)-diag(mv_m2$uniquenesses)
apply(mv_m2$loadings^2, 2, sum)/6

#questão 15

dados <- read.table("C:/Users/Juliana Rosa/OneDrive/Documents/UnB - Disciplinas/Semestre 10/Multi/Lista 5/table_4_3_stiffness.txt")

S <- cov(dados)
S

principal(S, nfactors=2, rotate="none", covar=T)

principal(dados, nfactors=2, rotate="none")$scores

principal(dados, nfactors=2, rotate="none")$scores>3

#questão 16

dps <- c(32.9909, 33.5918, 36.5534, 37.3517)
S <- matrix(c(dps[1]^2, 0.7501*dps[1]*dps[2],
              0.6329*dps[1]*dps[3],
              0.6363*dps[1]*dps[4],
              0.7501*dps[1]*dps[2], dps[2]^2,
              0.6925*dps[2]*dps[3],
              0.7386*dps[2]*dps[4],
              0.6329*dps[1]*dps[3],
              0.6925*dps[2]*dps[3],
              dps[3]^2,
              0.6625*dps[3]*dps[4],
              0.6363*dps[1]*dps[4], 
              0.7386*dps[2]*dps[4],
              0.6625*dps[3]*dps[4],
              dps[4]^2), 4, byrow=T)
S

##letra a

pr_m1 <- principal(S, nfactors=1, rotate="none", covar=T)
pr_m1

pr_m2 <- principal(S, nfactors=2, rotate="none", covar=T)
pr_m2

##letra b

mv_m1 <- factanal(covmat=S, factors=1, rotation="none")
mv_m1

mv_m2 <- factanal(covmat=S, factors=2, rotation="none")
mv_m2

##letra c

pr_rot <- principal(S, nfactors=2, rotate="varimax", covar=T)
pr_rot

mv_rot <- factanal(covmat=S, factors=2, rotation="varimax")
mv_rot

#questão 17

dados <- read.table("C:/Users/Juliana Rosa/OneDrive/Documents/UnB - Disciplinas/Semestre 10/Multi/Lista 5/table_7_7_pulp_fiber.txt")

R <- cor(dados)
R

pr_cor <- principal(R, nfactors=1, rotate="none", covar=T)
pr_cor

mv_cor <- factanal(covmat=R, factors=1, rotation="none")
mv_cor

S <- cov(dados)
S

pr_cov <- principal(S, nfactors=1, rotate="none", covar=T)
pr_cov

mv_cov <- factanal(covmat=S, factors=1, rotation="none")
mv_cov
