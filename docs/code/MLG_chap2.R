# ----------------------------------------
# Chap. n°2 : Regression lineaire multiple
# ----------------------------------------

# Chargement des packages utiles
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(scales) 

# Chargement des donnees en specifiant le type des variables
donnees <- read.table("../data/ozone.txt", header = TRUE,
                      colClasses = c(rep("numeric", 11), rep("factor", 2)))


# ----------------------------------------
# ---------- Analyse descriptive ---------
# ----------------------------------------

# Coefficient de correlation linearire empirique
cor(donnees[, c("maxO3", "Ne12", "T12", "Vx12")])


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

# Regression lineaire multiple
# --- Variable reponse = maxO3
# --- Variables explicatives = Ne12, T12 et Vx12
reg <- lm(maxO3 ~ Ne12 + T12 + Vx12, data = donnees)

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
# ------- Intervalles de confiance -------
# ----------------------------------------

# Parametres de regression
confint(reg, level = .95)

# Variance du bruit
alpha <- .05
n <- nrow(donnees)
# --- Estimation de la variance
S2 <- summary(reg)$sigma^2
# --- Intervalle de confiance de niveau 1 - alpha
(n - 2) * S2 / (qchisq(c(1 - alpha/2, alpha/2), n - 2))


# ----------------------------------------
# -------- Bilan de la regression --------
# ----------------------------------------

summary(reg)
# Residuals : quartiles associés aux résidus observés

# Coefficients : l'estimation des coefficients : 
#                beta_0 (ordonnée à l'origine = Intercept)
#                beta_i (coefficients associés à chaque variable explicative), 
# --- Estimate = estimation du parametre
# --- Std. Error = estimation de l'écart type (racine carré de la variance) de l'estimateur
# --- t value = valeur de la statistique de test (Test de Student sur la nullité du coefficient)
# --- Pr(>|t|) = p-valeur pour le test de Student sur la nullité du coefficient
# ------ Si la p-valeur est inferieure a 5%, on rejette H0 (le coefficient n'est pas nul) 

# Residual standard error = estimation de l'écart type du bruit

# Multiple R-squared = coefficient de determination
# Adjusted R-sqaured = coefficient de determination ajuste

# F-statistic = valeur de la statistique de test pour le test de Fisher global
# p-value = p-valeur pour le test de Fisher global
# ------ Si la p-valeur est inferieure a 5%, on rejette H0 (le modèle de regression est pertinent)


# ----------------------------------------
# Test de Fisher pour des modeles emboites 
# ----------------------------------------

# Definition du modele reduit (modele le plus simple)
reg_0 <- lm(maxO3 ~ T12, data = donnees)

# Comparaison entre le modele reduit et le modele complet
anova(reg_0, reg)


# ----------------------------------------
# ---------- Capacite predictive ---------
# ----------------------------------------

# Acces au coefficient de determination
summary(reg)$r.squared
# Acces au coefficient de determination ajuste
summary(reg)$adj.r.squared


# ----------------------------------------
# -------------- Predictions -------------
# ----------------------------------------

# Nouvelles valeurs de la variable explicative
# --- la colonne du data.frame DOIT avoir le meme nom que dans les donnees initiales
x_new <- data.frame("Ne12" = 6, "T12" = 20, "Vx12" = -3)

# Valeurs predites
predict(reg, newdata = x_new)

# Valeurs predites et erreur de prediction
predict(reg, newdata = x_new, interval = "prediction", level = .95)


# ----------------------------------------
# ---------- Annexe : colinearite --------
# ----------------------------------------

library(faraway)
vif(reg)
# Si le VIF est superieur a 5, on considere qu'il y a colinearite 
# entre cette variable et les autres. On la retire.
