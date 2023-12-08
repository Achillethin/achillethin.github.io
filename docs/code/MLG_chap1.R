# ----------------------------------------
# Chap. n°1 : Regression lineaire simple
# ----------------------------------------

# Chargement des packages utiles
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(scales) 

# Chargement des donnees en specifiant le type des variables
donnees <- read.table("../data/wagesmicrodata.csv", sep = ";", dec = ",", header = TRUE, 
                      colClasses = c(rep("numeric", 2), "factor", 
                                     rep("numeric" , 3), rep("factor", 6)))


# ----------------------------------------
# ---------- Resumes numeriques ----------
# ----------------------------------------

# Moyenne empirique pour la colonne EDUCATION
x_bar <- mean(donnees$EDUCATION)
# Moyenne empirique pour la colonne WAGE
y_bar <- mean(donnees$WAGE)
# Coefficient de correlation empirique
rho_xy <- cor(donnees$EDUCATION, donnees$WAGE)

# Test de significativite de la correlation empirique
cor.test(donnees$EDUCATION, donnees$WAGE)


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

# Regression lineaire simple
# --- Variable reponse = WAGE
# --- Variable explicative = EDUCATION
reg <- lm(WAGE ~ EDUCATION, data = donnees)

# Nom des elements contenus dans reg
names(reg)

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
# ------ Transformation des donnees ------
# ----------------------------------------

reg <- lm(log(WAGE) ~ EDUCATION, data = donnees)


# ----------------------------------------
# --------- Droite de regression ---------
# ----------------------------------------

# Graphiques (version basique)
# --- Nuage de points
plot(donnees$EDUCATION, log(donnees$WAGE),
     # Parametres graphique,
     xlab = "EDUCATION", ylab = "log(WAGE)", pch = 16)
# --- Ajout de la droite de regression
abline(a = reg$coefficients[1], b = reg$coefficients[2],
       # Parametres graphiques
       lwd = 1.5, col = "royalblue")

# -------------------------- Pour plus tard ----------------------------------------

# Graphique (version ggplot)
# --- Creation d'un tableau contenant les donnees initiales et les valeurs ajustees
df <- data.frame(donnees[, c("WAGE", "EDUCATION")], ADJ.VAL = reg$fitted.values)
# --- Creation du spport graphique
pl1 <- ggplot(data = df, aes_string(x = "EDUCATION", y = "log(WAGE)")) + theme_bw()
# --- Ajout du nuage de points
pl1 <- pl1 + geom_point() 
# --- Ajout de la droite de regression
pl1 <- pl1 + geom_line(data = df, aes(y = ADJ.VAL), col = "royalblue")
# --- Affichage du graphique
print(pl1)

# ----------------------------------------------------------------------------------


# ----------------------------------------
# ------- Intervalles de confiance -------
# ----------------------------------------

# Parametres de regression
confint(reg, level = .97)

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
#                a (pente = Variable explicative), 
#                b (ordonnée à l'origine = Intercept)
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
# ------ Dans le cas de la regression simple ce test est redondant avec le test de Student.


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
x_new <- data.frame("EDUCATION" = c(3, 5, 12))

# Valeurs predites
predict(reg, newdata = x_new)

# Valeurs predites et estimation de la variance du predicteur
predict(reg, newdata = x_new, se.fit = TRUE)

# Valeurs predites et intervalles de confiance associes
predict(reg, newdata = x_new, interval = "confidence")

# Valeurs predites et erreur de prediction
predict(reg, newdata = x_new, interval = "prediction", level = .95)



# Graphiques (version basique)
# --- Grille de valeurs pour la prediction
x_pred <- data.frame("EDUCATION" = 2:18)
# --- Predictions et intervalles de confiance associes
conf_int <- predict(reg, newdata = x_pred, interval = "confidence")
# --- Predictions et intervalles de prediction
pred_int <- predict(reg, newdata = x_pred, interval = "prediction")
# --- Nuage de points
plot(donnees$EDUCATION, log(donnees$WAGE),
     # Parametres graphique,
     xlab = "EDUCATION", ylab = "log(WAGE)", pch = 16)
# --- Ajout de la droite de regression
abline(a = reg$coefficients[1], b = reg$coefficients[2],
       # Parametres graphiques
       lwd = 1.5, col = "royalblue")
# --- Ajout de la bande de confiance
polygon(x = c(x_pred$EDUCATION, rev(x_pred$EDUCATION)), 
        y = c(conf_int[, "upr"], rev(conf_int[, "lwr"])), 
        lwd = .1, col = alpha('firebrick', .5))
# --- Ajout de l'intervalle de prediction
polygon(x = c(x_pred$EDUCATION, rev(x_pred$EDUCATION)), 
        y = c(pred_int[, "upr"], rev(pred_int[, "lwr"])), 
        lwd = .1, col = alpha('grey', .3))


# -------------------------- Pour plus tard ----------------------------------------

# Graphiques (version ggplot) 
# --- ajout de la bande de confiance sur le graphe pl1 (c.f. plus haut)
pl1 + geom_smooth(data = df, method = "lm", col = "royalblue", fill = "firebrick", alpha = 0.5)

# --- ajout de l'erreur de prediction sur le graphe pl1 (c.f. plus haut)
# ------ Le plus simple est d'ajouter l'erreur de prediction pour toutes
# ------ les valeurs de la variable explicative presentes dans les donnees
# Remarque = il est normal d'obtenir un warning en procedant ainsi
df <- data.frame(df, predict(reg, interval = "prediction"))
pl1 + geom_smooth(data = df, aes(ymin = lwr, ymax = upr), stat = "identity", 
                  linetype = 0, alpha = 0.5)

# --- Version alternative
df_pred <- data.frame(EDUCATION = x_pred, pred_int)
# --- On cree une colonne WAGE artificielle utile pour la fonction geom_smooth
df_pred$WAGE <- exp(df_pred$fit)
pl1 + geom_smooth(data = df_pred, aes(ymin = lwr, ymax = upr), stat = "identity", 
                  alpha = 0.5)

# ----------------------------------------------------------------------------------

