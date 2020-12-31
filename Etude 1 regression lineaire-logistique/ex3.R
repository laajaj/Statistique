dir="C:/Users/LAAJAJ/Desktop/Cours 3A/MOD 2.3 statistique/BE2"
setwd(dir)
getwd()
neur <-read.table("neuralgia.txt",header=TRUE)
head(neur)


# répartition des données d'apprentissage et de test
u <- sample(1:60,48)
apprenti <- neur[u,]
test <- neur[-u,]

#régression logistique 
apprenti$Treatment = as.factor(apprenti$Treatment)
apprenti$Sex = as.factor(apprenti$Sex)
neur_logit=glm(Pain ~ Treatment + Sex + Age + Duration,family=binomial(link="logit"),data=apprenti)

#installation du package car
install.packages("car")
library("car")

#Analyser par Anova
anova(neur_logit,test="Chisq")
Anova(neur_logit,test.statistic = "LR", type= 'III')
Anova(neur_logit,test.statistic = "Wald", type= 'III')

#méthode AIC
neu_logit2=glm(Pain ~ 1,family=binomial,data=apprenti)
forward <- step(neu_logit2, direction="forward", scope=list(upper=~(Treatment + Sex + Age + Duration)), trace = TRUE)

#modèle amélioré
neu_logit4=glm(Pain ~ Treatment +sex + Age ,family=binomial(link="logit"),data=apprenti)

#predictions et matrice de confusion
predict_init <- exp(predict(neur_logit,newdata =test))/(1+exp(predict(neur_logit,newdata =test)))
predictreduit <- exp(predict(neu_logit4,newdata =test))/(1+exp(predict(neu_logit4,newdata =test)))
table(predict_init>0.5,test$Pain)
table(predictreduit>0.5,test$Pain)

# calcul de precision pour le modèle réduit
prec <- vector("numeric",50) 
for (i in 1:50) {
  v<- sample(1:60,48)
  app5 <- neur[v,]
  test5 <- neur[-v,]
  app5$Treatment = as.factor(app5$Treatment)
  app5$Sex = as.factor(app5$Sex)
  neu_logit5=glm(Pain ~ Treatment + Sex + Age ,family=binomial(link="logit"),data=app5)
  predicti <- exp(predict(neu_logit5,newdata =test5))/(1+exp(predict(neu_logit5,newdata =test5)))
  table5 <- table(predicti>0.5,test5$Pain)
  prec[i]= (table5[1,1]+table5[2,2])/(table5[1,1]+table5[2,2]+table5[1,2]+table5[2,1])
}
summary(prec)
plot(seq(1,50,1),prec)
plot(prec)

# on varie le pourcentage des données d'apprentissage
pr=40
prec1 <- vector("numeric",50) 
for (i in 1:50) {
  v<- sample(1:60,ceiling(60*pr/100))
  app5 <- neur[v,]
  test5 <- neur[-v,]
  app5$Treatment = as.factor(app5$Treatment)
  app5$Sex = as.factor(app5$Sex)
  neu_logit5=glm(Pain ~ Treatment + Sex + Age ,family=binomial(link="logit"),data=app5)
  predicti <- exp(predict(neu_logit5,newdata =test5))/(1+exp(predict(neu_logit5,newdata =test5)))
  table5 <- table(predicti>0.5,test5$Pain)
  prec1[i]= (table5[1,1]+table5[2,2])/(table5[1,1]+table5[2,2]+table5[1,2]+table5[2,1])
}
summary(prec1)
