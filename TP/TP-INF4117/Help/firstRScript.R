x2 = a+b # affectation avec operation arithmetique
# (#) materialise le commantaire.
mode(x2) # le type d'une variable
o = attributes(x2) # attribut d'un type ( leur mode)
!a # non a
a&b # a et b
x|a # xor(x,a)
# (na) veut dire valeur manquante
is.na(x) # x est il du type na 
# creation de vecteur
V <- c(1,23,4,4,5,6,7,8, NA)
is.na(V)
V[9] <- 0
mode(V)

vect <- c(att1 = 1, att2=2)
attributes(vect)
names(vect)

V1 <- c(1,23,4,4,5,6,7,8, 9)

2*V # multiplication

V + V1 # addition

V1 * V # produit scalaire


mean(V) # moyenne
median(V) # mediane
var(V) # variance
cov(V, V1) # covariance
cor(V, V1) # correlation
sample(V, 2) # tirage aleatoire sans remise de n elements parmi ceux de V
sd(V) # ecart-type

rep(28, 1000) # creer un vecteur de 1000 occurances de 28
1:19 # les nombres  entier de 1 a 19
GV = seq(from=0, to= 100, by = 2) # tous les nombres paires compris entre 0 et 100
seq(1, 4, length = 5)


sample(1:50,5)
letters # toutes les lettres de l'alphabet miniscule
letters[27] 
sGV = GV[c(1:3,5,7,10)] # extraire les elements d'indices present dans 1,2,3,5,7,10
eGV = GV[-c(1:3,5,7,10)] # extraire les elements d'indices present dans 1,2,3,5,7,10
cGV = GV[GV<=50] # extraire les elements respectant une condition (<=50)
mGV10 = GV[GV%%10 == 0] # (multiple de 10)
cc = c(sGV,eGV,cGV, mGV10) # concatener n vecteurs


letters # vecteur contenant tout les lettres de l'alphabet en miniscule
LETTERS # vecteur contenant tout les lettres de l'alphabet en majiscule
c(letters,"ö") # ajouter le caractere ö dans le vecteur


# Matrices
M = matrix(c(1:6), ncol=3, byrow=T)
dim(M)
rownames(M) # le nom des lignes
colnames(M) # le nom des colonnes
colnames(M) = c("a","b","c") # renommage des colonnes
colnames(M) 
rownames(M) = c("i","j") # renommage des lignes
rownames(M)
paste("attr", 1:3, sep="")

M = rbind(M,c(8:9))
M = cbind(M, c(10:12))

FD = M[2, 2:4]

apply(M, 2, sum)
M%*%M # produit de deux matrices au sens de l'informaticien
M*M # produit de deux matrices au sens du mathematicien
solve(M%*%M) # inverse le produit AB
solve(M) # l'inverse de la matice M
solve(M, cc) # resolution de l'equation Mx =cc ou M est Matrix et cc un vecteur
eigen(M) # calcul les valeurs et les vecteurs propres de M



# Liste
L1 = list(a = cc,b = cGV, c = eGV,d = FD, e = mGV10, f = GV, g = V, h = V1, i = vect, j = M)
L2 = list(c(1:29, paste('ele',1:5, sep="")))
L3 = list(L1,L2)


# DataFrame
Df = data.frame(c(1:100),c(2:101),c(3:102))
Df1 = data.frame(M,row.name = c("a", "b", "c"))

# Fonctions
puissance = function(a,n){
  return (a^n) # a puissance n
}
puissance(2,4)

Vx = c(3,-1,4,5)
puissance(Vx,2)

# If
