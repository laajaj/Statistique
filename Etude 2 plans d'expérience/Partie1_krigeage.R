library(DiceKriging)

#Question 1

n=100
x=seq(0,1,length=n)
response <- rep(0,length(x))
covtype="matern5_2"
coef.cov=0.1
sigma2=1
trend=2
model <- km(formula=~1, design=data.frame(x=x), response=response, 
            covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
            coef.var=sigma2)

y <- simulate(model, nsim=5, newdata=data.frame(x=x))

nsim=5
plot(x, y[1,], type="l", col="black",
     ylim=c(min(y), max(y)), ylab="y")
for (i in 2:nsim) {
  lines(x, y[i,], col=i)
}

# Question 2
n=100
x=seq(0,1,length=n)
response <- rep(0,length(x))
covtype="matern5_2"
coef.cov=0.1
sigma2=1
trend=2
model <- km(formula=~1, design=data.frame(x=x), response=response, 
            covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
            coef.var=sigma2)


y <- simulate(model, nsim=5, newdata=data.frame(x=x))

plot(x, y[1,], type="l", col="black",
     ylim=c(min(y), max(y)), ylab="y")

# choix de 5 points
indice=c(10,20,50,60,80)
x2=x[indice]
trajectoire=y[1,]
y2=trajectoire[indice]
points(x2, y2, col = "red")

model2 <- km(formula=~1, design=data.frame(x=x2), response=y2, 
             covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
             coef.var=sigma2)

p <- predict(model2, newdata=data.frame(x=x), type="SK")
plot(x, p$mean, type="l", col="red", 
     ylim=c(min(p$lower95), max(p$upper95)), ylab="y")
lines(x, p$lower95, col="blue")
lines(x, p$upper95, col= "brown")
lines(x, y[1,], type="l", col="black",ylim=c(min(y), max(y)), ylab="y")

# Question 3

n=100
x=seq(0,1,length=n)
response <- rep(0,length(x))
covtype="matern5_2"
coef.cov=0.2
sigma2=1
trend=2
model <- km(formula=~1, design=data.frame(x=x), response=response, 
            covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
            coef.var=sigma2)


y <- simulate(model, nsim=5, newdata=data.frame(x=x))

# choix des points
indice=c(2,15,20,35,40,50,55,60,70,80,90)
x2=x[indice]
trajectoire=y[1,]
y2=trajectoire[indice]
points(x2, y2, col = "red")

#Prédicition
model2 <- km(formula=~1, design=data.frame(x=x2), response=y2, 
             covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
             coef.var=sigma2)

p <- predict(model2, newdata=data.frame(x=x), type="SK")
plot(x, p$mean, type="l", col="red", 
     ylim=c(min(p$lower95), max(p$upper95)), ylab="y")
lines(x, p$lower95, col="blue")
lines(x, p$upper95, col= "brown")
lines(x, y[1,], type="l", col="black",ylim=c(min(y), max(y)), ylab="y")

# bonus : noyaux de type matern3_2, exp, gauss, powexp

n=100
x=seq(0,1,length=n)
response <- rep(0,length(x))
covtype="powexp"
coef.cov=0.2
sigma2=1
trend=2
model <- km(formula=~1, design=data.frame(x=x), response=response, 
            covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
            coef.var=sigma2)


y <- simulate(model, nsim=5, newdata=data.frame(x=x))

# choix des points
indice=c(2,15,20,35,40,50,55,60,70,80,90)
x2=x[indice]
trajectoire=y[1,]
y2=trajectoire[indice]
points(x2, y2, col = "red")

#prédiction
model2 <- km(formula=~1, design=data.frame(x=x2), response=y2, 
             covtype=covtype, coef.trend=trend, coef.cov=coef.cov, 
             coef.var=sigma2)

p <- predict(model2, newdata=data.frame(x=x), type="SK")
plot(x, p$mean, type="l", col="red", 
     ylim=c(min(p$lower95), max(p$upper95)), ylab="y")
lines(x, p$lower95, col="blue")
lines(x, p$upper95, col= "brown")
lines(x, y[1,], type="l", col="black",ylim=c(min(y), max(y)), ylab="y")


