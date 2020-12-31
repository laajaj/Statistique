dir="C:/Users/LAAJAJ/Desktop/Cours 3A/MOD 2.3 statistique/BE3"
setwd(dir)
getwd()

# installation des packages

## libraries et fonctions √† charger 
library(DiceDesign)
library(pracma)
library(geometry)
library(sfsmisc)
library(randtoolbox)
source('functions/Minimax.R')

# ------------------------------------------------------------
# 1 - G√©n√©ration des LHS et calcul des crit√®res 
#     maximin, minimax, Discr√©pance centr√©e
# ------------------------------------------------------------

# dimension 2
n = 10
dimension = 2
X_init <- lhsDesign(n,dimension) # plan LHS non optimisÈ
plot(X_init$design[,1],X_init$design[,2], xlab="", ylab="", pch = 21, bg = "red",col  = "red")

# plan LHS optimisÈ par le maximin
X_opt_Mn <- maximinSA_LHS(X_init$design)
plot(X_opt_Mn$design[,1],X_opt_Mn$design[,2],xlab="", ylab="", pch = 21, bg = "red",col  = "red")

# plan LHS optimisÈ par la discrepency
X_opt_Disc <- discrepSA_LHS(X_init$design)
plot(X_opt_Disc$design[,1],X_opt_Disc$design[,2],xlab="", ylab="",pch = 21, bg = "red",col  = "red")

# Comparaison des diffÈrents plans 

Tableau_comparaison = cbind(t(cbind(mindist(X_init$design), Minimax(X_init$design)[1], discrepancyCriteria(X_init$design)$DisC2)),
                            t(cbind(mindist(X_opt_Mn$design), Minimax(X_opt_Mn$design)[1], discrepancyCriteria(X_opt_Mn$design)$DisC2)),
                            t(cbind(mindist(X_opt_Disc$design), Minimax(X_opt_Disc$design)[1], discrepancyCriteria(X_opt_Disc$design)$DisC2)))
Tableau_comparaison


# dimension 5
n = 50
dimension = 5
X_init <- lhsDesign(n,dimension) # plan LHS non optimisÈ
pairs(X_init$design)

# plan LHS optimisÈ par le maximin
X_opt_Mn <- maximinSA_LHS(X_init$design)
X_opt_Mn1 <- maximinSA_LHS(X_opt_Mn$design)
X_opt_Mn2 <- maximinSA_LHS(X_opt_Mn1$design)
pairs(X_opt_Mn2$design)

# plan LHS optimisÈ par la disc. centrÈe
X_opt_Disc <- discrepSA_LHS(X_init$design)
X_opt_Disc1 <- discrepSA_LHS(X_opt_Disc$design)
X_opt_Disc2 <- discrepSA_LHS(X_opt_Disc1$design)
pairs(X_opt_Disc2$design)

# Comparaison des diffÈrents plans pour diffÈrents critËres

Tableau_comparaison = cbind(t(cbind(mindist(X_init$design), Minimax(X_init$design), discrepancyCriteria(X_init$design)$DisC2)),
                            t(cbind(mindist(X_opt_Mn2$design), Minimax(X_opt_Mn2$design), discrepancyCriteria(X_opt_Mn2$design)$DisC2)),
                            t(cbind(mindist(X_opt_Disc2$design), Minimax(X_opt_Disc2$design), discrepancyCriteria(X_opt_Disc2$design)$DisC2)))

Tableau_comparaison

# distribution

nb_simulation = 20

maximin_init = zeros(1,nb_simulation)
maximin_mn = zeros(1,nb_simulation)
maximin_disc = zeros(1,nb_simulation)

disc_init = zeros(1,nb_simulation)
disc_mn = zeros(1,nb_simulation)
disc_disc = zeros(1,nb_simulation)

for (i in 1:nb_simulation) {
  X_init <- lhsDesign(n,dimension)
  
  X_opt_Mn <- maximinSA_LHS(X_init$design)
  X_opt_Mn1 <- maximinSA_LHS(X_opt_Mn$design)
  X_opt_Mn2 <- maximinSA_LHS(X_opt_Mn1$design)
  
  X_opt_Disc <- discrepSA_LHS(X_init$design)
  X_opt_Disc1 <- discrepSA_LHS(X_opt_Disc$design)
  X_opt_Disc2 <- discrepSA_LHS(X_opt_Disc1$design)
  
  maximin_init[i] = mindist(X_init$design)
  maximin_mn[i] = mindist(X_opt_Mn2$design)
  maximin_disc[i] = mindist(X_opt_Disc2$design)
  disc_init[i] = discrepancyCriteria(X_init$design)$DisC2
  disc_mn[i] = discrepancyCriteria(X_opt_Mn2$design)$DisC2
  disc_disc[i] = discrepancyCriteria(X_opt_Disc2$design)$DisC2
  
  print(i)
}

maximin_dataframe = cbind(t(maximin_init), t(maximin_mn), t(maximin_disc))
colnames(maximin_dataframe) = c("Plan initial", "OptimisÈ maximin", "OptimisÈ discrepancy")
boxplot(maximin_dataframe, main = "Distribution du critËre maximin")

disc_dataframe = cbind(t(disc_init), t(disc_mn), t(disc_disc))
colnames(disc_dataframe) = c("Plan initial", "OptimisÈ maximin", "OptimisÈ discrepancy")
boxplot(disc_dataframe, main = "Distribution du critËre discrepancy ")


# # ------------------------------------------------------------
# # 2 - Planification adaptative par MSE
# # ------------------------------------------------------------

######################## exemple en dimension 1 ##########################

#Question 1
N_BA = 5                ## Nombre de points de la base d'apprentissage (BA)
d = 1                    ## Dimension de l'espace des entrees
X_init <- lhsDesign(N_BA,d) # plan LHS non optimisÈ
PX_BA <- maximinSA_LHS(X_init$design)$design
plot(PX_BA[,1], c(0,0,0,0,0), xlab="", ylab="", pch = 21, bg = "red",col  = "red")

#Question 2
N_BT = 101        ### Nombre de points de la base de test
PX_BT = seq(0,1,length.out=N_BT)    ### Simulation d'une premiere base de test

coef.cov <- c(0.2)
sigma <- 1
trend <- c(intercept <- 0)

metamodel <- km(formula=~1,design=data.frame(x=PX_BA),response=rep(1,N_BA),
                covtype="matern5_2",coef.trend=trend, coef.cov=coef.cov, 
                coef.var=sigma^2) 

#Question 3
#Visualisation du modËle

n=10000
x_absc=seq(0,1,length=n)
outputs <- predict(metamodel, newdata=data.frame(x=x_absc), type="SK")
var_model = outputs$sd
matplot(x_absc, var_model, col =1, type = 'l', ylab = "Variance",
        main = "ModËle de krigeage")

mean_model = outputs$mean
upper_model = outputs$upper95
lower_model = outputs$lower95
matplot(x_absc, cbind(mean_model, upper_model, lower_model), col = c(2,1,1), type = 'l', ylab = "Moyenne et seuils ‡ 95%",
        main = "ModËle de krigeage")

# Identifier le point o˘ la variance est maximale

variance=outputs$sd
plot(x_absc,variance,type="l")

maximum_variance = max(outputs$sd)
index = which(outputs$sd==maximum_variance)

points(x_absc[index], maximum_variance, pch = 16, col = 'red')


#Question 4

for (i in 1:6) {
  metamodel <- km(formula=~1,design=data.frame(x=PX_BA),response=rep(1,N_BA+i-1),covtype="matern5_2",
                  coef.trend=trend, coef.cov=coef.cov, coef.var=sigma^2) # Creation du metamodele processus gaussien 
  
  n=10000
  x_absc=seq(0,1,length=n)
  outputs <- predict(metamodel, newdata=data.frame(x=x_absc), type="SK")
  variance=outputs$sd
  #plot(absc,variance,type="l")
  
  maximum_variance = max(outputs$sd)
  index = which(outputs$sd==maximum_variance)
  PX_BA = rbind(PX_BA, x_absc[index])
}

# modËle final

outputs <- predict(metamodel, newdata=data.frame(x=x_absc), type="SK")
mean_model = outputs$mean
upper_model = outputs$upper95
lower_model = outputs$lower95
matplot(x_absc, cbind(mean_model, upper_model, lower_model), col = c(2,1,1), type = 'l',xlab="plan", ylab = "Moyenne et seuils ‡ 95%",
        main = "ModËle de krigeage")
