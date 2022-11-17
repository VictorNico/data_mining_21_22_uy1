#Règles d'association
library(arules)
dataIris = iris
dataIris

l = matrix(c(1,1,1,0,0,1,1,0,0,0,1,1,0,0,1), ncol = 5)
trans = as(l, "transactions")

#Discretisation des 4 premières colonnes
dataIris[,4] = discretize(dataIris[,4], breaks=3, 
                          quantile=FALSE, 
                          labels=c("short", "medium", "long"))

dataIris[,3] = discretize(dataIris[,3], breaks=3, 
                          quantile=FALSE, 
                          labels=c("short1", "medium1", "long1"))

dataIris[,2] = discretize(dataIris[,2], breaks=3, 
                          quantile=FALSE, 
                          labels=c("short1", "medium1", "long1"))

dataIris[,1] = discretize(dataIris[,1], breaks=3, 
                          quantile=FALSE, 
                          labels=c("short", "medium", "long"))

summary(dataIris)
mode(dataIris)
l <- data.frame(dataIris)
mode(l)
trans = as(dataIris, "transactions")
trans
inspect(trans)

#Génération des règles d'associations en spécifiant les paramètres
rules = apriori(l, parameter = list(supp=0.25, conf=0.9, target="rules"))

#affichage des différentes règles
inspect(rules[1:10])

rules1 = apriori(
  trans, 
  parameter = list(supp=0.25, conf=0.9, target="closed frequent itemsets"),
  appearance = list(rhs=c("Species=setosa"))
)
inspect(rules1)

# Classification supervisée
library(rpart)

dataIris2 = iris
summary(dataIris2)

# Construction d'un arbre de décision
AD = rpart(Species ~ ., dataIris2)

prp(AD)

plot(AD)
text(AD, use.n=TRUE)

predict(AD, dataIris2[-5], type=c("class"))

V = sample(nrow(dataIris2), 2*nrow(dataIris2)/3)
train = dataIris2[V,]
test = dataIris2[-V,]

AD1 = rpart(Species ~ ., train)
pred = predict(AD1, test, type=c("class"))
M = table(test[,5], pred)
M

classes = unique(c(pred, test[,5]))

Mesure = function(M) {
  prec_model = 0
  nbreClass = 0
  rappel_model = 0
  classes = dim(M)[1]
  for(class in 1:classes){
    prec = M[class, class]/sum(M[,class])
    rappel = M[class, class]/sum(M[class,])
    prec_model = prec + prec_model
    rappel_model = rappel + rappel_model
  }
  prec_model = prec_model/classes
  rappel_model = rappel_model/classes
  f_mesure <- 2*prec_model*rappel_model/(prec_model+rappel_model
  mes <- data.frame(precision=prec_model, rappel=rappel_model, FMesure=f_mesure)
  return (mes)
}

Mesure(M)

# Réseau de neurones
library(nnet)
model = nnet(Species ~ ., train, size=10)
pred2 = predict(model, test, type=c("class"))

M2 = table(test[,5], pred2)
Mesure(M2)
Mesure(M)


library(neuralnet)
neuralnetIris = iris
neuralnetIris = cbind(neuralnetIris, neuralnetIris$Species == "setosa")
neuralnetIris = cbind(neuralnetIris, neuralnetIris$Species == "versicolor")
neuralnetIris = cbind(neuralnetIris, neuralnetIris$Species == "virginica")
neuralnetIris

names(neuralnetIris)[6] = "setosa"
names(neuralnetIris)[7] = "versicolor"
names(neuralnetIris)[8] = "virginica"
summary(neuralnetIris)

V = sample(nrow(neuralnetIris), 2*nrow(neuralnetIris)/3)
train = neuralnetIris[V,]
test = neuralnetIris[-V,]

nn = neuralnet(setosa+versicolor+virginica ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=train, hidden=c(3))
plot(nn)
pred_nn = compute(nn, neuralnetIris[1:4])$net.result
pred_nn
maxidx= function(arr){
  return (which(arr==max(arr)))
}
idx = apply(pred_nn, c(1), maxidx)
pred_neuralnet = c("setosa", "versicolor", "verginica")[idx]
M3 = table(pred_neuralnet, neuralnetIris$Species)
M3




library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
# but probably one of the best R packages ever. 
data(segmentationData)				# Get some data
data <- segmentationData[,-c(1,2)]

# Make big tree
form <- as.formula(Class ~ .)
tree.1 <- rpart(form,data=data,control=rpart.control(minsplit=20,cp=0))
# 
plot(tree.1)					# Will make a mess of the plot
text(tree.1)
# 
prp(tree.1)					# Will plot the tree
prp(tree.1,varlen=3)				# Shorten variable names

# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1) # display the new tree
#
#-------------------------------------------------------------------
tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle
#
#-------------------------------------------------------------------
# Plot a tree built with RevoScaleR
# Construct a model formula
sdNames <- names(segmentationData)
X <- as.vector(sdNames[-c(1,2,3)])
form <- as.formula(paste("Class","~", paste(X,collapse="+")))
# Run the model
rx.tree <- rxDTree(form, data = segmentationData,maxNumBins = 100,
                   minBucket = 10,maxDepth = 5,cp = 0.01, xVal = 0)
# Plot the tree						
 #prp(rxAddInheritance(rx.tree))
# fancyRpartPlot(rxAddInheritance(rx.tree))
