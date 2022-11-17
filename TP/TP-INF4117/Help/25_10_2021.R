# lecture de fichier
var = read.table("crx.data.txt", header=F, dec=".", sep=",", na.strings = "?")
# redefinition des noms de colonnes
colnames(var) = c("a1","a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16")
# les cinq premiere lignes
head(var)
# dimension du data frame
dim(var)
# resume du jeu de donnees
S = summary(var)
# trace de l'historgramme
hist(var$a2)
# plot
plot(var$a3)
# plot de la boite a moustache
boxplot(var$a3, col="blue")
# ajout d'un trace horizontal
plot(var$a3)
abline(h = mean(var$a3), col="red", lty = 1) # moyenne
abline(h = mean(var$a3, na.rm = T) + sd(var$a3, na.rm = T), lty = 2, col="green")
abline(h = mean(var$a3, na.rm = T) - sd(var$a3, na.rm = T), lty = 3, col="green")
abline(h = mean(var$a3, na.rm = T) + 2*sd(var$a3, na.rm = T), lty = 4, col="cyan")
abline(h = mean(var$a3, na.rm = T) - 2*sd(var$a3, na.rm = T), lty = 5, col="cyan")
abline(h = median(var$a3, na.rm = T), lty = 6, col="yellow")

# identification des outliers
# outliers = identify(var$a3)


# na.omit permet d'eliminer objects compotant des valeurs manquantes

dim(var)
dim(na.omit(var))

# remplacement des valeur manquantes dans la colonne a14 par la medianne
var[is.na(var$a14), "a14"] <- median(var$a14, na.rm = T)
dim(var)
dim(na.omit(var))

# remplacement des valeur manquantes dans la colonne a2 par la medianne
var[is.na(var$a2), "a2"] <- median(var$a2, na.rm = T)
dim(var)
dim(na.omit(var))

# exercice: une fonction qui prends un vecteur de modalite qui retourne la valeur la plus frequentes dans le vecteurs.

# creation de valeur manquante
var[sample(1:nrow(var), 10), "a14"] <- NA

# regresion dans R
lm(a14 ~ a15, data = var)

#
var[is.na(var$a14), "a14"] <- median(var$a14, na.rm = T)
var[sample(1:nrow(var), 10), "a14"] <- NA
var[is.na(var$a14), "a14"] <- 1.796e+02 + 2.204e-03*var[sample(1:nrow(var), 10), "a15"]
# best way
var[is.na(var$a14), "a14"] <- 1.796e+02 + 2.204e-03*var[is.na(var$a14), "a15"]

# discretisation de donnees
discret = cut(var$a14, breaks=3, labels = c("petit","moyen","grand"))
#normalisation
dis = scale(var$a14, center = TRUE, scale = TRUE)

graphy <- function(x,y){
  if(length(x) == length(y)){
    plot(x,y,type="l")
    point <- locator(4,type = "p", col = "blue")
  }
}
V[is.na(V)]<-sample(V[c(1:3,5:8)],1)
V
c = graphy(1:10,2:11)
