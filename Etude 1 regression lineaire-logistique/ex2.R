dir="C:/Users/LAAJAJ/Desktop/Cours 3A/MOD 2.3 statistique/BE2"
setwd(dir)
getwd()
hormones <-read.table("hormones.txt", header=T, dec= ',')
head(hormones)

# transformer en variables adimensionnées
hormones$X1 <- (hormones$X1- 85)/ 35
hormones$X2 <- (hormones$X2- 20)/ 5
#
summary(hormones)
plot(hormones)

m1 <- lm(Y1~((X1+X2)^2 + I(X1^2) + I(X2^2)), data = hormones)
summary(m1)

# chercher le couple optimale
X_1=seq(-1,1,0.1)
X_2=seq(-1,1,0.1)
grid <- expand.grid(X1=X_1 ,X2=X_2)
predict1=predict(m1,grid,interval="prediction",level=0.95)
ypred=matrix(predict1,21,21)
contour(X_1,X_1,ypred)

# indice du max
indi=which.max( predict1[,1])
grid[indi,]

