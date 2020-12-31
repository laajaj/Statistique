
dir="C:/Users/LAAJAJ/Desktop/Cours 3A/MOD 2.3 statistique/BE2"
setwd(dir)
getwd()
silicium <-read.table("silicium.txt",header=TRUE)
head(silicium)

summary(silicium)
plot(silicium)

silicium$Ctime
silicium$Ltemp*silicium$Ltime*silicium$Lpress
silicium$Catmos
silicium$Ltemp*silicium$Lpress*silicium$Ctemp

# produit de tX*X
silic_X = as.matrix(silicium[, -7])
silictXX = t(silic_X) %*% silic_X


#analayse de modèle
silicLm1 = lm(Camber ~ Ltemp + Ltime + Lpress + Ctemp + Ctime + Catmos, data = silicium)
summary(silicLm1)


#modèle sans les facteurs non significatif
silicLm2 = lm(Camber ~ Ltemp + Lpress + Ctime + Catmos, data = silicium)
summary(silicLm2)

#interval de confiance pour la courbure 
attach(silicLm2)
ymoy <- coefficients[2]*(-1)+coefficients[3]*(-1)+coefficients[4]*1+coefficients[5]*1+coefficients[1]
new_data=data.frame(Ltemp = -1, Lpress = -1, Ctime =1, Catmos =1)
predict(silicLm2,new_data,interval = "confidence", level= 0.95)

#vérification des hypothèses du modèle
par(mfrow=c(2,2))
plot(silicLm2)
