# ----------------------------------------
# Chap. n°3 : ANOVA
# ----------------------------------------

# Chargement des packages utiles
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(scales) 
library(viridis)

# Chargement des donnees en specifiant le type des variables
donnees <- read.table("../data/ozone.txt", header = TRUE,
                      colClasses = c(rep("numeric", 11), rep("factor", 2)))
attach(donnees)


# ------------------------------------------------------------ #
# ------------------------- 2 FACTEURS ----------------------- #
# ------------------------------------------------------------ #

# ----------------------------------------
# ---------- Analyse descriptive ---------
# ----------------------------------------

# Niveaux/modalités d'une variable qualitative
levels(vent)
levels(temps)

# Effectifs par niveau
table(vent)
table(temps)

# Moyennes par niveau
df_means <- aggregate(list(maxO3 = maxO3), list(vent = vent, temps = temps), mean)


# -------------------------- Pour plus tard ----------------------------------------

# Boxplot par categorie (version ggplot)
# --- Structure de base du graphique
p <- ggplot(donnees)
# --- Changement du theme
p <- p + theme_bw() + scale_color_viridis_d(alpha = 1, option = "D")
# --- Ajout du nuage de points
p + geom_boxplot(aes(x = vent, y = maxO3, col = temps))


# Graphe des interactions (version ggplot)
# --- Structure de base du graphique
p <- ggplot(data = df_means, aes_string(y = "maxO3", x = "vent", col = "temps"))
# --- Changement du theme
p <- p + theme_bw() + scale_color_viridis_d(alpha = 1, option = "D")
# --- Ajout des points
p <- p + geom_point() 
# --- Ajout des lignes
p <- p + geom_line(aes_string(group = "temps", linetype = "temps"))
# --- Affichage du graphique
print(p)

# ----------------------------------------------------------------------------------


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

# Modele regulier
# ----------------------------------------
# --- Variable reponse = maxO3
# --- Variables explicatives = vent et temps
reg_regular <- lm(maxO3 ~ vent * temps - 1)


# Modele singulier
# ----------------------------------------
# --- Variable reponse = maxO3
# --- Variables explicatives = vent et temps
reg <- lm(maxO3 ~ vent * temps)

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
plot(reg)
# --- Retour vers une fenetre graphique standard (1 ligne, 1 colonne)
par(mfrow = c(1, 1))

# -------------------------- Pour plus tard ----------------------------------------

# Graphiques (version ggplot)
# --- la commande theme_bw() permet d'avoir un flond bland avec une grille (optionnel)
autoplot(reg) + theme_bw()

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
pl <- autoplot(reg, which = 5, ncol = 1, label.size = 1) + theme_bw()
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
reg_add <- lm(maxO3 ~ vent + temps)
# --- Test de comparaison des modeles
anova(reg_add, reg)


# Test de l'effet d'un facteur
# ----------------------------------------
# --- Modele ANOVA 1 Facteur avec le vent
reg_vent <- lm(maxO3 ~ vent, data = donnees)
# --- Test de l'effet du facteur temps
anova(reg_vent, reg_add)

# --- Modele ANOVA 1 Facteur avec le temps
reg_temps <- lm(maxO3 ~ temps, data = donnees)
# --- Test de l'effet du facteur vent
anova(reg_temps, reg_add)


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

