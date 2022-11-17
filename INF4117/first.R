x = a+b

mode(a)

mode(b)

mode(x)

attributes(x)

V = c(1,3,2,0)

is.na(V)

mode(V)

length(V)

mean(V)

median(V)

var(V)

V1 = c(5,7,9,4)

#covariance 
cov(V1,V)

#sample(V,m) retourne aleatoirement m valeurs qui sont dans V1 (m<=length(V))
sample(V1,4)

#n1:n2 cree un vecteur de nombre compris entre n1 et n2 avec le pas 1

# application de sample
sample(1:50, 5)

#rep(val,n) cree un vect de n elt initialise a val

#seq(from = VI, to = VF, by = pas) donne la liste des nombres allant de VI a VF avec le pas 'pas'
Vp = seq(from = 2, to = 1000, by = 2)

#seq(VI, VF, length=nombre) permet de creer un vecteur dont les valeurs partent de VI a VF et on la taille nombre

#letters est un vect avk les lettres de l'alphabet en miniscule
letters[1]

#LETTERS meme chose que letters mais en majuscule
LETTERS[1]


#on peu conserver les positions d'un vecteur dans un autre vecteur
 t = Vp[c(1:3,5,7,10)]
t

#on peut prendre des elts sauf certains indices
p = Vp[-c(1:3,5,7,10)]
p

#extraire des elts qui verifient une condition
Vp[Vp<= 500]

#multiple de 10
Vp[Vp%%10==0]

#concatenation de deux vecteurs
concatVect = c(Vp,V)
concatVect

#pour ajouter un elt a un vecteur
V = c(V,9)
V = c(V,2)
V

#afficher les elements sans les doublons
unique(V)

# creation d'une matrice
M = matrix(c(1:6), ncol=3, byrow=T)
M

#connaitre les dimensions resultatt (ligne colonne)
dim(M)

#donner des noms aux colonnes
colnames(M)=c("a","b","c")

#voir les noms des colonnes
colnames(M)

#donner des noms aux lignes
rownames(M)=c("ligne1","ligne2")

#voir les noms des lignes
rownames(M)

#prend un element de chaque vecteur et concatene a celui de la meme position dans les autres vecteurs
paste("attr",1:5,seq="")

#ajouter une ligne a une matrice
M = rbind(M,c(7:9))

#ajouter une colonne a une matrice
M = cbind(M,c(10:12))
M

rownames(M)=c("ligne1","ligne2","ligne3")
colnames(M)=c("a","b","c","d")
M

#extraction de sous matrice (deuxieme ligne et colonne de 2 a 4
M[2,c(2:4)]

#extraction de toute la ligne 1
M[1,]

#extraction de toute la colonne 1
M[,1]

#transpose
t(M)

#appliquer une fonction a notre matrice a une position donnee
#
apply(M,2,sum)

#multiplication elt par elt
M*M

#pour faire le produit mathematiquement on uitlise
M%*%M

#solve(A,b): resout le sys lineaire Ax = b;

#diag(M): diagonale de M

#eigen(M): calcul les valeurs et vecteurs propres

#les listes sont caracterisées par le symbole [[]]
#les listes sont semblables aux enregistrements/par contre ici on ne declare pas la structure a l'avance

#creation d'une liste en precisant les attributs
l = list(A=V,B=V1)
l

#creation d'une liste sans preciser les attributs
l1 = list(V,V1)
l1

#creation d'une liste de liste
l3 = list(l,l1)
l3

#dataframe qui est une matrice (ressamblant a une feuille excel) dont les colonnes representes differents elements
#ex: prenons une liste d'etudiant avec trois colonnes matricule, nom, telephone

#il contient des vecteurs qui representent les colonnes, ces vecteurs doivent etre de meme taille
df = data.frame(V,V)
df

#nombre de lignes
nrow(df)

#nombre de colonnes
ncol(df)

#liste des noms de lignes
row.names(df)

#liste des noms de colonnes
col.names(df)

#------ LES FONCTIONS ------# 

#creation d'une fonction
puissance=function(x,n)
{
 return (x^n)
}

#execution sur different type de donnnees
puissance(5,2)

V = c(3,-1,4,5)

puissance(V,2)

puissance(M,2)

#structure conditionnelle
#if(condition)
#{
#instructions
#.
#.
#.
#else
#{
#instructions
#.
#.
#.
#}
# ou encore X=if(condition){instructions}else{instructions}
#les boucles for et while.
#dans la boucle for on utilise plutot un vecteur pour contenir les valeurs de notre indice
# ex: for(i in t) {}

#boucle while exercice programme qui convertis un nombre n en binaire

binaire = function(n){
	res = vector()
	while(n>0){
		r<- n%%2
		res = c(r,res)
		n = n%/%2
	}
	return (res)
}

binaire(10)

#pour afficher un graphique (image, graphe, courbe, etc)
#on utilise la fonction plot(liste de vecteur) par defaut si on donne un vecteur,
# il prendra les indices du vecteur comme valeur des abscisses et le vecrteur pour les ordonnees

x = seq(-2*pi, 2*pi, length=100)

y = x*sin(x)
y
plot(x,y,type='l')
title("fonction sinus")
#mettre en rouge tous les points dont les cordonnees x et y dont y>0
points(x[y>0],y[y>0],col="red")
#permet d'aller recuperer sur le graphique les indices du nombre de 
#points qu'on aura definit et sur lesquels on aura cliqué et
#de les attribuer une couleur
#t = locator(5,type="p", col="blue")

#affichage de la distance entre deux points donnee
#chercher l'aide sur cette commande
#identify(20,col="yellow")
#?identify
#la commande points permet de tracer des lignes ou courbe sur le graphe
#abline(h=0)
#abline(v=0)
#abline(1,2)

#barplot
height = c(1:10)
h = c(0:99)
barplot(height , space = 1.5, axisnames = FALSE, sub = "Barplot")
boxplot(x)

#installation du package RMySQL dans R a travers l'interface graphique
# a installer
# arules
# rpart
# nnet
# neuralnet
# DMwR
# FactoMineR
# e17
# party
# clusty

#connexion a la base de donnees
DB <- dbConnect(MySQL(), user="root", host="localhost", password="", dbname="r_pratique")

#ecriture dans la base de donnees
df1 <- data.frame(
Name = c("Joe", "Joe", "Bill", "Jim", "Kate"),
Value = c(10.1, 13, 9, 7.7, -3),
Indic = c(0L, 1L, 1L, 2L, 2L),
Status = c("A", "A", "A", "B", "B"))
dbWriteTable(DB, "dat1", df1)

#lecture des donnnees dans la bd
dbReadTable(DB,"dat1")

#execution d'une requete dans mysql
#requete de selection
dbGetQuery(DB, "SELECT * FROM dat1 WHERE Name='Joe';")

#requete de suppression d'une table
dbRemoveTable(DB, "dat1") #suppression de la table

#liste des champs
dbGetQuery(DB, "DESCRIBE dat1")

#Annuler les modif
dbRollback(DB)

#validation des modifications
dbCommit(DB)

# déconnexion
dbDisconnect(DB)


##25/10/21

# chargement d'un fichier de donnees
cr<- read.table("crx.data.txt",header=F, dec='.',na.strings=c('?'),sep=',')

# definition des colonnes pour les donnees de mon fichier precedemment chargé
colnames(cr)= c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','A16')

#voir les 5 first ligne du fichier
head(cr)

#voir le fichier sous forme de tableau et pouvoir modifier les donnees
edit(cr)

#donne des statistiques suur nos donnees en fonctin de leur type
#lorsque c'est un vecteur avec plusieurs colonnes, on a la description des colonnes

summary(cr)

#pour acceder a une colonne precise, on a deux methodes:
# soit on utilise monVecteur[,i] avec i l'indice de la colonne en question
#ou on utilise monVecteur$nomColonne
cr$A1

# hist() permet de construire l'histogramme d'une variable
hist(cr$A3)

#par() pour passer les paramètres à R
#exmple par(mfrow=c(1,2)) pour mettre la fenêtre graphique sur une ligne et deux colonnes
par(mfrow=c(1,2))

#construire la boite a moustache d'une variable
boxplot(cr$A3)

#tracer sur un graphe une ligne representant la moyenne de la variable donnee
abline(h = mean(cr$A3), col="blue")

plot(cr$A3, xlab = "")
abline(h = mean(cr$A3, na.rm = T), lty = 1)
abline(h = mean(cr$A3, na.rm = T) + sd(cr$A3,
na.rm = T), lty = 2)
abline(h = median(cr$A3, na.rm = T), lty = 3)
