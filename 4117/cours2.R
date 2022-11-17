cc = read.table('fichier/crx.data.txt', 
                dec='.', sep=',', 
                na.string=c('?'), header=F, 
                col.name=paste("A", 1:16, sep="")
                )
cc

head(cc) #affiche les 6 premières valeurs

dim(cc) #Affiche les dimensions de CC dans un vecteur

summary(cc) #Détail de chaque colonne


#Accès à une colonne particulière soit via $ doit les 
cc$A1
cc[,'A1']

hist(cc$A11, main="A11")

boxplot(cc$A3) #boxplot colonne A3
abline(h=mean(cc$A3), col="red")

plot(cc$A3)
abline(h = mean(cc$A3, na.rm = T), col='yellow') #moyenne
#Zone de concentration des données
abline(h = mean(cc$A3, na.rm = T) + sd(cc$A3,na.rm = T), col='blue') #moyenne + écart type
abline(h = mean(cc$A3, na.rm = T) - sd(cc$A3,na.rm = T), col='blue') #moyenne - écart type
abline(h = mean(cc$A3, na.rm = T) + 2*sd(cc$A3,na.rm = T), col='green') #moyenne + écart type
abline(h = mean(cc$A3, na.rm = T) - 2*sd(cc$A3,na.rm = T), col='green') #moyenne - écart type
abline(h = median(cc$A3, na.rm = T), col='red') #mediane
A=identify(cc$A3)
A

#Element de cc$A3 qui ne sont pas NA et supérieur à 5
cc$A3[!is.na(cc$A3) & cc$A3>5] 

dim(cc) #dimension du dataFrame
dim(na.omit(cc)) #dimension du dataFrame sans les valeurs manquantes //Supprimer ces valeurs
cc$A14

#On remplace les données manquantes de A14 par la médiane
cc[is.na(cc$A14), 'A14'] = median(cc$A14, na.rm=T) 
summary(cc) #affichage des détail

#On remplace les données manquantes de A2 par la moyenne
cc[is.na(cc$A2), 'A2'] = mean(cc$A2, na.rm=T) 
summary(cc)

#Ajout de 10 valeur NA aléatoire dans la colonne A14
cc[sample(1:length(cc$A14), 10), "A14"] = NA

# Suppossons qu'il y'ait une corrélation forte entre A14 et A15
# C'est à dire A14 = f(A15) = a + b*A14
lm(A14~A15, data=cc)

#Suppression des données manquante en les remplaçant par les données prédites par régression linéaire simple avec A15
cc[is.na(cc$A14), 'A14'] = 1.803e+02 + 2.168e-03*cc[is.na(cc$A14), 'A15']
summary(cc$A14)

#diviser un vecteur en intervalle
cc$A14 = cut(cc$A14, breaks=3, labels=c("petit", "moyen", "grand")) 
typeof(cc$A14)

summary(cc$A14)

scale(cc$A2)