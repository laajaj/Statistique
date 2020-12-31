# Partie 4 Exercice 2

set.seed(12)
# Question 1  
h = 0.01
absc = (0:(1/h))*h
funct = 10*(absc*sin(10*absc) + absc*cos(20*absc))
plot(absc, funct, type = 'l', main = "", ylab = "", xlab = "")

# Question 2

n = 4
x_init = runif(n)
y_init = 10*(x_init*sin(10*x_init) + x_init*cos(20*x_init))
points(x_init,  y_init, col = 'red', pch = 16)

# Question 3

# Modèle de krigeage "classique" 

metamodel <- km(formula=~1,design=data.frame(x=x_init),response=y_init,covtype="matern5_2",
                lower = 0.05) # Creation du metamodele processus gaussien 

outputs <- predict(metamodel, newdata=data.frame(x=absc), type="UK")
mean_model = outputs$mean
upper_model = outputs$upper95
lower_model = outputs$lower95
matplot(absc, cbind(funct, mean_model, upper_model, lower_model), xlab="x", col = c(1,2,4,4), type = "l", ylab = "y (à 95%)",
        main = "")
points(x_init, 10*(x_init*sin(10*x_init) + x_init*cos(20*x_init)), col = 'red', pch = 16)

# Modèle de krigeage avec algorithme génétique"

metamodel_gen <- km(formula=~1,design=data.frame(x=x_init),response=y_init,covtype="matern5_2",
                    lower = 0.05, optim.method = "gen") # Creation du metamodele processus gaussien 

outputs_gen <- predict(metamodel_gen, newdata=data.frame(x=absc), type="UK")
mean_model = outputs_gen$mean
upper_model = outputs_gen$upper95
lower_model = outputs_gen$lower95
matplot(absc, cbind(funct, mean_model, upper_model, lower_model), col = c(1,2,4,4), type = 'l',xlab="x", ylab = "y (à 95%)",
        main = " Modèle avec l'algorithme génétique")
points(x_init, 10*(x_init*sin(10*x_init) + x_init*cos(20*x_init)), col = 'red', pch = 16)

# Question 4

n_grille = 1000
grille = t(t(seq(0,1,length=n_grille)))
EI_grille <- apply(grille, 1, EI, metamodel)
plot(grille, EI_grille, type = 'l', main = "Grille d'évaluation de l'EI ", ylab = "EI", xlab = "")

# Question 5

absc_to_add = max_EI(metamodel, lower = 0, upper = 1)$par[1]
points(absc_to_add, max_EI(metamodel, lower = 0, upper = 1)$value[1], col = 'red', pch = 16)

x_init = c(x_init, absc_to_add)
y_init = y_init = 10*(x_init*sin(10*x_init) + x_init*cos(20*x_init))

metamodel <- km(formula=~1,design=data.frame(x=x_init),response=y_init,covtype="matern5_2",
                lower = 0.05) # Creation du metamodele processus gaussien 

outputs <- predict(metamodel, newdata=data.frame(x=absc), type="UK")
mean_model = outputs$mean
upper_model = outputs$upper95
lower_model = outputs$lower95
matplot(absc, cbind(funct, mean_model, upper_model, lower_model), col = c(1,2,4,4), type = 'l', xlab="x", ylab = "y (à 95%)",
        main = "Modèle de krigeage ")
points(x_init, 10*(x_init*sin(10*x_init) + x_init*cos(20*x_init)), col = 'red', pch = 16)


# Question 6

epsilon = 0.02
set.seed(12)
x_init = runif(n)
EI_previous = 0
ecart = 1
count = 0
while (ecart > epsilon) {
  y_init = 10*(x_init*sin(10*x_init) + x_init*cos(20*x_init))
  metamodel <- km(formula=~1,design=data.frame(x=x_init),response=y_init,covtype="matern5_2",
                  lower = 0.05) # Creation du metamodele processus gaussien 
  EI_grille <- apply(grille, 1, EI, metamodel)
  absc_to_add = max_EI(metamodel, lower = 0, upper = 1)$par[1]
  EI_max = max_EI(metamodel, lower = 0, upper = 1)$value[1]
  x_init_previous = x_init
  x_init = c(x_init, absc_to_add)
  count = count + 1
  ecart = abs(EI_max - EI_previous)
  EI_previous = EI_max
}

outputs <- predict(metamodel, newdata=data.frame(x=absc), type="UK")
mean_model = outputs$mean
upper_model = outputs$upper95
lower_model = outputs$lower95
matplot(absc, cbind(funct, mean_model, upper_model, lower_model), col = c(1,2,4,4), type = 'l', xlab="x", ylab = "y (à 95%)",
        main = "Modèle de krigeage")
points(x_init_previous, 10*(x_init_previous*sin(10*x_init_previous) + x_init_previous*cos(20*x_init_previous)), col = 'red', pch = 16)

y_min = min(y_init)
index = which(y_init == min(y_init))
x_min = x_init_previous[index]
c(x_min, y_min)

