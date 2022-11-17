setwd("F:/R")

TABLE=read.table('hepatitis.txt', header=F, dec='.', na.strings=('?'),sep=',')
TABLE
dim(TABLE)
summary(TABLE)
edit(TABLE)
library(shiny)
library(arules)


#########################PRETRAITEMENT########################################r


       ##########REMPLACEMENT DES VALEURS MANQUANTES########
####PAR LA MOYENNE###

Remplacement=function(TABLE){
 
 classes=dim(TABLE)[2]
 for (i in 1:classes){
   if TABLE[i]=1 then{
    
   
  TABLE[is.na(TABLE$A7), 'A7'] <-mean(TABLE$A7, na.rm = T)

     }
    return(TABLE)
}
Remplacement(TABLE)

 
TABLE[is.na(TABLE$V4), 'V4'] <-median(TABLE$V4, na.rm = T)
TABLE[is.na(TABLE$V6), 'V6'] <-median(TABLE$V6, na.rm = T)
TABLE[is.na(TABLE$V7), 'V7'] <-median(TABLE$V7, na.rm = T)
TABLE[is.na(TABLE$V8), 'V8'] <-median(TABLE$V8, na.rm = T)
TABLE[is.na(TABLE$V9), 'V9'] <-median(TABLE$V9, na.rm = T)
TABLE[is.na(TABLE$V10), 'V10'] <-median(TABLE$V10, na.rm = T)
TABLE[is.na(TABLE$V11), 'V11'] <-median(TABLE$V11, na.rm = T)
TABLE[is.na(TABLE$V12), 'V12'] <-median(TABLE$V12, na.rm = T)
TABLE[is.na(TABLE$V13), 'V13'] <-median(TABLE$V13, na.rm = T)
TABLE[is.na(TABLE$V14), 'V14'] <-median(TABLE$V14, na.rm = T)
TABLE[is.na(TABLE$V15), 'V15'] <-median(TABLE$V15, na.rm = T)
TABLE[is.na(TABLE$V16), 'V16'] <-median(TABLE$V16, na.rm = T)
TABLE[is.na(TABLE$V17), 'V17'] <-median(TABLE$V17, na.rm = T)
TABLE[is.na(TABLE$V18), 'V18'] <-median(TABLE$V18, na.rm = T)
TABLE[is.na(TABLE$V19), 'V19'] <-median(TABLE$V19, na.rm = T)

####PAR LA M0YENNE###

TABLE[is.na(TABLE$V4), 'V4'] <-mean(TABLE$V4, na.rm = T)
TABLE[is.na(TABLE$V6), 'V6'] <-mean(TABLE$V6, na.rm = T)
TABLE[is.na(TABLE$V7), 'V7'] <-mean(TABLE$V7, na.rm = T)
TABLE[is.na(TABLE$V8), 'V8'] <-mean(TABLE$V8, na.rm = T)
TABLE[is.na(TABLE$V9), 'V9'] <-mean(TABLE$V9, na.rm = T)
TABLE[is.na(TABLE$V10), 'V10'] <-mean(TABLE$V10, na.rm = T)
TABLE[is.na(TABLE$V11), 'V11'] <-mean(TABLE$V11, na.rm = T)
TABLE[is.na(TABLE$V12), 'V12'] <-mean(TABLE$V12, na.rm = T)
TABLE[is.na(TABLE$V13), 'V13'] <-mean(TABLE$V13, na.rm = T)
TABLE[is.na(TABLE$V14), 'V14'] <-mean(TABLE$V14, na.rm = T)
TABLE[is.na(TABLE$V15), 'V15'] <-mean(TABLE$V15, na.rm = T)
TABLE[is.na(TABLE$V16), 'V16'] <-mean(TABLE$V16, na.rm = T)
TABLE[is.na(TABLE$V17), 'V17'] <-mean(TABLE$V17, na.rm = T)
TABLE[is.na(TABLE$V18), 'V18'] <-mean(TABLE$V18, na.rm = T)
TABLE[is.na(TABLE$V19), 'V19'] <-mean(TABLE$V19, na.rm = T)



####DISCRETISATION######
library(plyr)

TABLE$V1 <- revalue(TABLE$V1,c("2"="die"))
TABLE$V1 <- revalue(TABLE$V1,c("yes"=1))

TABLE[,1] <- discretize(TABLE[,1], breaks=2, quantile=FALSE, labels=c("Die", "Live"))
TABLE[,2] <- discretize(TABLE[,2], breaks=3, quantile=FALSE, labels=c("petit", "moyen", "grand"))
TABLE[,3] <- discretize(TABLE[,3], breaks=2, quantile=FALSE, labels=c("Homme", "Femme"))
TABLE[,4] <- discretize(TABLE[,4], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,5] <- discretize(TABLE[,5], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,6] <- discretize(TABLE[,6], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,7] <- discretize(TABLE[,7], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,8] <- discretize(TABLE[,8], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,9] <- discretize(TABLE[,9], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,10] <- discretize(TABLE[,10], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,11] <- discretize(TABLE[,11], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,12] <- discretize(TABLE[,12], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,13] <- discretize(TABLE[,13], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,14] <- discretize(TABLE[,14], breaks=2, quantile=FALSE, labels=c("yes", "no"))
TABLE[,15] <- discretize(TABLE[,15], breaks=3, quantile=FALSE, labels=c("faible", "moyen", "eleve"))
TABLE[,16] <- discretize(TABLE[,16], breaks=3, quantile=FALSE, labels=c("faible", "moyen", "eleve"))
TABLE[,17] <- discretize(TABLE[,17], breaks=3, quantile=FALSE, labels=c("faible", "moyen", "eleve"))
TABLE[,18] <- discretize(TABLE[,18], breaks=3, quantile=FALSE, labels=c("faible", "moyen", "eleve"))
TABLE[,19] <- discretize(TABLE[,19], breaks=3, quantile=FALSE, labels=c("faible", "moyen", "eleve"))
TABLE[,20] <- discretize(TABLE[,20], breaks=2, quantile=FALSE, labels=c("no", "yes"))




######################### REGLES D'ASSOCIATIONS ########################################

trans <- as(TABLE, "transactions")
rules <- apriori(trans,parameter = list(supp = 0.5, conf = 0.9,target = "rules"))
inspect(rules)



####################### CLASSIFICATION SUPERVISEE ################################

library(rpart)


ArbreDecision=rpart(V1~.,TABLE) #Construction de l'arbre
plot(ArbreDecision)
text(ArbreDecision,use.n=TRUE)
Predire=predict(ArbreDecision,TABLE[,-1],type=c('class'))
Predire

V=sample(1:nrow(TABLE), 2*nrow(TABLE)/3)
V
summary(V)
length(V)

APP=TABLE[V,]
TEST=TABLE[-V,]
ArbreDecision2=rpart(V1~.,APP)
plot(ArbreDecision2)
text(ArbreDecision2,use.n=TRUE)

PredireTest=predict(ArbreDecision2,TEST[,-1],type=c('class'))
PredireTest


 ######## MATRICE DE CONFUSION ##########

MatConfusion= table(predTEST, TEST[,5])
MatConfusion
#####DEVOIR ; calculer la précision, la fmesure, le rappel, accurency

library(tree)



############CALCUL DE LA PRECISION###########

Precision=function(MatConfusion){
 p=0
 classes=dim(MatConfusion)[1]
 for (i in 1:classes){
    p<-p+MatConfusion[i,i]/sum(MatConfusion[,i])
 }
  p<-p/classes
  r<-0
  return(c(p))
}
Precision(MatConfusion)
P<-Precision(MatConfusion)



############CALCUL DU RAPPEL###########

Rappel=function(MatConfusion){
 p=0
 classes=dim(MatConfusion)[1]
 for (i in 1:classes){
    p<-p+MatConfusion[i,i]/sum(MatConfusion[i,])
 }
  p<-p/classes
  r<-0
  return(c(p))
}

Rappel(MatConfusion)
R<-Rappel(MatConfusion)


############CALCUL DE LA FMESURE###########

F=2*R*P/R+P
F

############CALCUL DE L'ACCURACY########### NOMBRE DE VRAIS PREDITS DIVISE PAR LE NOMBRE TOTAL PREDIT

Accuracy=function(MatConfusion){
 p=0
 classes=dim(MatConfusion)[1]
 for (i in 1:classes){
         p<-p+MatConfusion[i,i]/sum(MatConfusion)
 
}
   return(c(p))
}

Accuracy(MatConfusion)


########LES RESEAUX DE NEURONES#############

library(nnet)
ArbreDecision <- nnet(V1~.,TABLE, size = 10)  #construction de l'arbre
ArbreDecision
plot(ArbreDecision)
text(ArbreDecision,use.n=TRUE)
classe <- predict(ArbreDecision,TABLE[,-1],type=c("class"))
v <- sample(1:nrow(Idata), 2*nrow(Idata)/3)
APP <- Idata[v,]
TEST <- Idata[-v,]
TEST
ArbreDecision = rpart(Species~.,APP)
plot(ArbreDecision)
text(ArbreDecision,use.n=TRUE)

predTEST <- predict(ArbreDecision,TEST[,-5],type=c("class"))
predTEST


####CONSTRUCTION DU MODELE DE NEURONES A PARTIR DU JEU D'APPRENTISSAGE ET AFFICHAGE######

v1 <- sample(1:nrow(Idata1), 2*nrow(Idata1)/3)#SEPARATION DU JEU DE DONNEES
APP <- Idata1[v,] #PARTIE APPRENTISSAGE
TEST <- Idata1[-v,] #PARTIE TEST

nn <- neuralnet(setosa+versicolor+virginica ~ Sepal.Length+Sepal.Width +Petal.Length +Petal.Width, data=APP, hidden=c(3))
plot(nn) 


#####PREDICTION AVEC NEURALNET##########

PREDICTION<- compute(nn, Idata[-5])$net.result 
PREDICTION

maxidx <- function(arr) { return(which(arr == max(arr))) } #retourne l'indice de la valeur maximale
maxidx
idx <- apply(PREDICTION, c(1), maxidx) #J'APPLIQUE LA FONCTION MAXIDX LIGNE PAR LIGNE
idx
PREDICTION <- c('setosa', 'versicolor', 'virginica')[idx] 
table(PREDICTION, Idata$Species)

