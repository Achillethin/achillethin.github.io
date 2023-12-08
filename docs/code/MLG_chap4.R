# ----------------------------------------
# Chap. n°4 : ANCOVA
# ----------------------------------------

# Chargement des packages utiles
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(scales) 
library(viridis)

# Chargement des donnees en specifiant le type des variables
donnees <- read.table("../data/wagesmicrodata.csv", 
                      sep = ";", dec = ",", header = TRUE, 
                      colClasses = c(rep("numeric", 2), 
                                     "factor", 
                                     rep("numeric" , 3), 
                                     rep("factor", 6)))
attach(donnees)

# Comment remplacer les 0 et 1 par H et F ?
# --- On ajoute les nouveaux niveaux souhaite
levels(donnees$SEX) <- c(levels(donnees$SEX), "H", "F")
# --- On remplace les 0 par H
donnees$SEX[donnees$SEX == "0"] <- "H"
# --- On remplace les 1 par F
donnees$SEX[donnees$SEX == "1"] <- "F"


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------


# Modele singulier
# ----------------------------------------
# --- Variable reponse = log(WAGE)
# --- Variable explicative = SEX et EDUCATION
reg <- lm(log(WAGE) ~ SEX * EDUCATION)

# Estimation des coefficient de regression
reg$coefficients # ou coef(reg)

# Valeur ajustees
reg$fitted.values # ou fitted(reg)

# Residus observes
reg$residuals # ou resid(reg)

# Estimation de la variance du bruit
summary(reg)$sigma^2


# ----------------------------------------
# ------ Validation des hypotheses -------
# ----------------------------------------

# Graphiques (version basique)
# --- Creation d'une fenetre graphique pour 4 graphes (2 lignes, 2 colonnes)
par(mfrow = c(2, 2))
plot(reg, col = SEX, pch = 16)
# --- Retour vers une fenetre graphique standard (1 ligne, 1 colonne)
par(mfrow = c(1, 1))

# -------------------------- Pour plus tard ----------------------------------------

# Graphiques (version ggplot)
# --- la commande theme_bw() permet d'avoir un flond bland avec une grille (optionnel)
autoplot(reg, colour = "SEX", shape = "SEX") + theme_bw()

# Par défaut ggplot ne trace pas la bande associee a la distance de Cook de 1 ou 0.5
# --- Fonction donnant la borne superieure pour une distance de Cook de d et un levier h
cd_cont_pos <- function(h , d, model) {
  sqrt(d * (length(coef(model) + 1)) * (1 - h)^2 / h)
}
# --- Fonction donnant la borne inferieure pour une distance de Cook de d et un levier h
cd_cont_neg <- function(leverage, d, model) {
  -cd_cont_pos(leverage, d, model)
}
# --- Largeur de fenetre pour x et y
x_lim = c(.001, .03)

# --- Graphique Residuals vs Leverage
pl <- autoplot(reg, colour = "SEX", shape = "SEX", which = 5, ncol = 1, label.size = 1) + theme_bw()
# --- Ajout de la borne superieure pour d = .5
pl <- pl + stat_function(fun = cd_cont_pos, args = list(d = 0.5, model = reg), 
                xlim = x_lim, lty = 2, colour = "red")
# --- Ajout de la borne inferieure pour d = .5
pl <- pl + stat_function(fun = cd_cont_neg, args = list(d = 0.5, model = reg), 
                           xlim = x_lim, lty = 2, colour = "red") 
# --- Ajout de la borne superieure pour d = 1
pl <- pl + stat_function(fun = cd_cont_pos, args = list(d = 1, model = reg), 
                xlim = x_lim, lty = 2, colour = "red") 
# --- Ajout de la borne inferieure pour d = 1
pl <- pl + stat_function(fun = cd_cont_neg, args = list(d = 1, model = reg), 
                xlim = x_lim, lty = 2, colour = "red")
# --- Affichage du graphique
print(pl)

# ----------------------------------------------------------------------------------


# ----------------------------------------
# -------- Bilan de la regression --------
# ----------------------------------------

summary(reg)
# Residuals : quartiles associés aux résidus observés

# Coefficients : l'estimation des coefficients (depend de la contrainte chosie) 
# --- Estimate = estimation du parametre
# --- Std. Error = estimation de l'écart type (racine carré de la variance) de l'estimateur
# --- t value = valeur de la statistique de test (Test de Student sur la nullité du coefficient)
# --- Pr(>|t|) = p-valeur pour le test de Student sur la nullité du coefficient
# ------ Si la p-valeur est inferieure a 5%, on rejette H0 (le coefficient n'est pas nul)
# ------ L'interpretation de ces tests depend de la contrainte choisie

# Residual standard error = estimation de l'écart type du bruit

# Multiple R-squared = coefficient de determination
# Adjusted R-sqaured = coefficient de determination ajuste

# F-statistic = valeur de la statistique de test pour le test de Fisher global
# p-value = p-valeur pour le test de Fisher global
# ------ Si la p-valeur est inferieure a 5%, on rejette H0 (le modèle ANOVA est pertinent)


# ----------------------------------------
# Test de Fisher : modeles emboites
# ----------------------------------------

# Modele additif v.s. modele avec interaction
# ----------------------------------------
# --- Ajustement du modele additif 
reg_add <- lm(log(WAGE) ~ SEX + EDUCATION)
# --- Test de comparaison des modeles
anova(reg_add, reg)


# Test de type I
# ----------------------------------------
anova(reg)


# Test de type II
# ----------------------------------------
library(car)
Anova(reg)

# ----------------------------------------
# ---------- Capacite predictive ---------
# ----------------------------------------

# Acces au coefficient de determination
summary(reg)$r.squared
# Acces au coefficient de determination ajuste
summary(reg)$adj.r.squared

