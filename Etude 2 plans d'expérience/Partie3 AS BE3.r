# ANALYSE DE SENSIBILITE
library(sensitivity)

# ----- --------------------------------------------------------
# Etude des plans 
# Methode sobolEff et Methode sobolroalhs (indice d'ordre 1) 
# -------------------------------------------------------------
# question 1
###################
# Methode sobolEff
###################
# Nombre d'evaluations par plan
n <- 10

# Dimension
d <- 2

set.seed(245)
# Construction des deux echantillons PX1 et PX2 (loi uniforme)
PX1 <- data.frame(matrix(runif(d * n), nrow = n))
PX2 <- data.frame(matrix(runif(d * n), nrow = n))

# Obtention des plans
res_SobolEff <- sobolEff(model=NULL,PX1,PX2)

# Composantes de l’objet fourni en retour de sobolEff
names(res_SobolEff)
str(res_SobolEff)

# Taille total du plan d’experience
dim(res_SobolEff$X)

# on gele suivant la premiere dimension
plot(res_SobolEff$X[1:n,],xlim=c(0,1),ylim=c(0,1),col='red',xlab='X1',ylab='X2') 
points(res_SobolEff$X[(n+1):(2*n),],col='blue',pch=3) 
abline(v=res_SobolEff$X[1:n,1],lty=2,col='darkgrey')

# on gele suivant la deuxième dimension
points(res_SobolEff$X[(2*n+1):(3*n),],col='green',pch=3) 
abline(h=res_SobolEff$X[1:n,2],lty=2,col='darkgrey')

# question 2
###################
# Methode sobolroals
###################

# Obtention des plans
set.seed(245)
res_Sobolroalhs <- sobolroalhs(model=NULL,factors=d,n,order=1)

# Composantes de l’objet fourni en retour de sobolEff
names(res_Sobolroalhs)
str(res_Sobolroalhs)

# Taille total du plan d’experience
dim(res_Sobolroalhs$X)

# on observe un alignement dans chaque dimension
plot(res_Sobolroalhs$X[1:n,],xlim=c(0,1),ylim=c(0,1),col='red',xlab='X1',ylab='X2') 
points(res_Sobolroalhs$X[(n+1):(2*n),],col='blue',pch=3) 
abline(v=res_Sobolroalhs$X[1:n,1],h=res_Sobolroalhs$X[1:n,2],lty=2,col='darkgrey')

# -------------------------------------------------------------
# Estimation des indices f(x1,x2) = x1 * x2 + x3
# Methode sobolEff 
# -------------------------------------------------------------

#taille du plan d'expériences
n = 1000
d = 3

functionf <- function(X){return(X[,1]*X[,2] + X[,3])}


# cas où X1 Unif[-1,1] , X2 Unif[0,2] et X3 N(0,1/sqrt(3))
############################
PX1 <- data.frame(cbind(matrix(runif( n,-1,1), nrow = n),matrix(runif( n,0,2), nrow = n),matrix(rnorm( n,0,1/sqrt(3)), nrow = n)))
PX2 <- data.frame(cbind(matrix(runif( n,-1,1), nrow = n),matrix(runif( n,0,2), nrow = n),matrix(rnorm( n,0,1/sqrt(3)), nrow = n)))

#indices ordre 1
res_soboleff <- sobolEff(model = functionf,PX1, PX2,order = 1, nboot=100,conf=0.95)
print(res_soboleff)
plot(res_soboleff)

#indices closed ordre 2 (pour le couple (1,2), l'indice closed correspond à S1 + S2 + S12)
res_soboleff <- sobolEff(model = functionf,PX1, PX2,order = 2, nboot=100,conf=0.95)
print(res_soboleff)
plot(res_soboleff)

#indices totaux
res_soboleff <- sobolEff(model = functionf,PX1, PX2,order = 0, nboot=100,conf=0.95)
print(res_soboleff)
plot(res_soboleff)


