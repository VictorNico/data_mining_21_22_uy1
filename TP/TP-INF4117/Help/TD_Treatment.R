# made function for categoriale data
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(c("a","a","b"))

# exercice 1
exo1_var <- c(13,15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)
# Quelle est la moyenne de cette distribution, quelle est la médiane ?
exo1_mean <- mean(exo1_var)
exo1_sd <- sd(exo1_var)
# Quel est le mode ? Commenter ce mode (bimodal, trimodal, etc)
exo1_mode <- mode(exo1_var)
# Peut trouver le premier quartile, et le troisième quartile de ces données ?
  # oui en utilistant la methode summary
exo1_sum <- summary(exo1_var)
  # mieux en faisant
exo1_1qt <- exo1_sum["1st Qu."]
exo1_1qt <- exo1_sum["3rd Qu."]

# exercice 2
# Normalisez les données suivantes : 200, 300, 400, 600, 1000. Utiliser la normalisation min-max avec max=1 et min=0 ; et la normalisation z_score.
exo2_var <- c(200, 300, 400, 600, 1000)
# definition de la methode Min-Max de normalisation sachant que (X – min(X))/(max(X) – min(X)) -> Min-Max Normalization.
min_max_norm <- function(x, min, max) {
  (x - min) / (max - min)
}
# définition de la méthode z-score sachant que (X – μ) / σ -> Z-Score Standardization.
exo2_z_score <- function(x){
  (x - mean(x)) / sd(x)
}

# apply Min-Max normalization to our dataset
exo2_min_max_norm <- as.data.frame(lapply(exo2_var, min_max_norm))
# apply z-score normalization to our dataset
exo2_z_score_norm <- as.data.frame(lapply(exo2_var, exo2_z_score))

# exercice 3
