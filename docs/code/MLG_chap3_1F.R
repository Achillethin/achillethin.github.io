# ----------------------------------------
# Chap. n°3 : ANOVA
# ----------------------------------------

# Chargement des packages utiles
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(scales) 

# Chargement des donnees en specifiant le type des variables
donnees <- read.table("../data/ozone.txt", header = TRUE,
                      colClasses = c(rep("numeric", 11), rep("factor", 2)))
attach(donnees)


# ------------------------------------------------------------ #
# ------------------------- 1 FACTEUR ------------------------ #
# ------------------------------------------------------------ #

# ----------------------------------------
# ---------- Analyse descriptive ---------
# ----------------------------------------

# Niveaux/modalités d'une variable qualitative
levels(vent)

# Effectifs par niveau
table(vent)

# Representations graphiques (version basique)
# --- Diagramme en baton (frequences)
barplot(table(vent)/nrow(donnees),
        # Graphical parameters
        xlab = "vent", ylab = "Fréquence")


# -------------------------- Pour plus tard ----------------------------------------

# Representations graphiques (version ggplot)
# --- Structure de base du graphique
p <- ggplot(donnees)
# --- Changement du theme
p <- p + theme_bw()
# --- Diagramme en baton (frequences)
p + geom_bar(aes(x = vent, y = (..count..)/sum(..count..))) + ylab("Fréquence")

# ----------------------------------------------------------------------------------

# Moyennes par niveau
aggregate(list(maxO3 = maxO3), list(vent = vent), mean)

# Boxplot par categorie (version basique)
# --- Ex. entre maxO3 et vent
boxplot(maxO3 ~ vent,
        pch = 16, lty = 1, outwex = 0,
        pars = list(boxwex = 0.9, staplewex = 0.2),
        xlab = "vent", ylab = "maxO3")

# -------------------------- Pour plus tard ----------------------------------------

# Boxplot par categorie (version ggplot)
# --- Structure de base du graphique
p <- ggplot(donnees)
# --- Changement du theme
p <- p + theme_bw()
# --- Ajout du boxplot
p + geom_boxplot(aes(x = vent, y = maxO3))

# ----------------------------------------------------------------------------------


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

# Modele regulier
# ----------------------------------------
# --- Variable reponse = maxO3
# --- Variable explicative = vent
reg_regular <- lm(maxO3 ~ vent - 1)


# Modele singulier
# ----------------------------------------
# --- Variable reponse = maxO3
# --- Variable explicative = vent
reg <- lm(maxO3 ~ vent)

# Estimation des coefficient de regression
reg$coefficients # ou coef(reg)

# Valeur ajustees
reg$fitted.values # ou fitted(reg)

# Residus observes
reg$residuals # ou resid(reg)

# Estimation de la variance du bruit
summary(reg)$sigma^2

# Modification de la contrainte
# --- Changer la modelaite de reference
lm(maxO3 ~ C(vent, base = 2), data = donnees)
# --- Contrainte de type somme
lm(maxO3 ~ C(vent, sum), data = donnees)


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
# Test de Student : comparaison 2 à 2 des moyennes 
# ----------------------------------------

# Test multiple sans ajustement
pairwise.t.test(maxO3, vent, p.adjust.method = "none")

# Test multiple avec correction de Bonferroni
pairwise.t.test(maxO3, vent, p.adjust.method = "bonferroni")

# Test multiple avec correction de Benjamini \& Hochberg
pairwise.t.test(maxO3, vent, p.adjust.method = "BH")


# ----------------------------------------
# ---------- Capacite predictive ---------
# ----------------------------------------

# Acces au coefficient de determination
summary(reg)$r.squared
# Acces au coefficient de determination ajuste
summary(reg)$adj.r.squared

