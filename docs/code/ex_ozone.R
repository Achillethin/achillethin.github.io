# Espace de travail
setwd("../data")

# Chargement des donnees en specifiant le type des variables
donnees <- read.table("ozone.txt", header = TRUE, dec = ".",
                      colClasses = c(rep("numeric", 11), rep("factor", 2)))
attach(donnees)

# ------------------------------------------------------------
# ---------------- Regression lineaire simple ----------------
# ------------------------------------------------------------

# Variable reponse : maxO3
# Variable explicative T12

# ----------------------------------------
# ---------- Analyse descriptive ---------
# ----------------------------------------

# Moyenne variable explicative
x_bar <- mean(T12)
# Moyenne variable reponse
y_bar <- mean(maxO3)
# Coefficient de correlation empirique
rho_xy <- cor(T12, maxO3)
# Test de nullite du coefficient de correlation empirique
cor.test(T12, maxO3)


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

reg <- lm(maxO3 ~ T12)
summary(reg)

# Droite d'ajustée
plot(T12, maxO3, pch = 16)
abline(a = coef(reg)[1], b = coef(reg)[2], col = "royalblue", lwd = 2)


# ----------------------------------------
# ------ Validation des hypotheses -------
# ----------------------------------------

par(mfrow = c(2, 2))
plot(reg, pch = 16)

# ----------------------------------------
# ------- Intervalles de confiance -------
# ----------------------------------------

confint(reg)

# ----------------------------------------
# -------- Bilan de la regression --------
# ----------------------------------------

summary(reg)


# ----------------------------------------
# -------------- Predictions -------------
# ----------------------------------------

x_new <- data.frame(T12 = c(5, 35))

# --- Pour le modele maxO3 ~ T12
predict(reg, newdata = x_new, interval = "confidence", level = .95)
predict(reg, newdata = x_new, interval = "prediction", level = .95)


# ------------------------------------------------------------
# --------------- Regression lineaire multiple ---------------
# ------------------------------------------------------------

# Variable reponse : maxO3
# Variables explicatives : T12, Vx12, Ne12

# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

reg_mult <- lm(maxO3 ~ T12 + Vx12 + Ne12)


# ----------------------------------------
# ------ Validation des hypotheses -------
# ----------------------------------------

plot(reg_mult, pch = 16)


# ----------------------------------------
# -------- Bilan de la regression --------
# ----------------------------------------

summary(reg_mult)


# ----------------------------------------
# --- Test de Fisher : modeles emboites --
# ----------------------------------------

anova(reg, reg_mult)


# ----------------------------------------
# -------- Probleme de colinearite -------
# ----------------------------------------

cor(donnees[, c("T12", "Ne12", "Vx12", "T9", "Ne9", "Vx9")])

# Matrice de design
X <- as.matrix(cbind(rep(1, nrow(donnees)), 
                     donnees[, c("T12", "Ne12", "Vx12", "T9", "Ne9", "Vx9")]))
# --- Spectre de t(X)X
eigen(t(X) %*% X)$values


# --- Detection des problemes de colinearite
# ------ Definition du modele complet
reg_comp <- lm(maxO3 ~ T12 + Ne12 + Vx12 + T9 + Ne9 + Vx9)
# ------ Calcul du VIF
library(car) # ou library(faraway) 
vif(reg_comp)

# --- Suppression de T12
reg_comp <- lm(maxO3 ~ Ne12 + Vx12 + T9 + Ne9 + Vx9)
# ------ Calcul du VIF
vif(reg_comp)
# ------ Bilan de la regression
summary(reg_comp)
# ------ Comparaion des capacites predictives
summary(reg)$adj.r.squared
summary(reg_mult)$adj.r.squared
summary(reg_comp)$adj.r.squared

# ------------------------------------------------------------
# ---------------------- ANOVA 1 Facteur ---------------------
# ------------------------------------------------------------

# Variable reponse : maxO3
# Variable explicative : vent


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


# Moyennes par niveau
aggregate(list(maxO3 = maxO3), list(vent = vent), mean)

# Boxplot par categorie (version basique)
# --- Ex. entre maxO3 et vent
boxplot(maxO3 ~ vent,
        pch = 16, lty = 1, outwex = 0,
        pars = list(boxwex = 0.9, staplewex = 0.2),
        xlab = "vent", ylab = "maxO3")


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

reg_anova_1f <- lm(maxO3 ~ vent)


# ----------------------------------------
# ------ Validation des hypotheses -------
# ----------------------------------------

# Graphiques (version basique)
# --- Creation d'une fenetre graphique pour 4 graphes (2 lignes, 2 colonnes)
par(mfrow = c(2, 2))
plot(reg_anova_1f, pch = 16)
# --- Retour vers une fenetre graphique standard (1 ligne, 1 colonne)
par(mfrow = c(1, 1))


# ----------------------------------------
# -------- Bilan de la regression --------
# ----------------------------------------

summary(reg_anova_1f)


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

summary(reg_anova_1f)$adj.r.squared


# ------------------------------------------------------------
# --------------------- ANOVA 2 Facteurs ---------------------
# ------------------------------------------------------------

# Variable reponse = maxO3
# Variables explicatives = vent et temps


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


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

reg_anova_2f <- lm(maxO3 ~ vent * temps)


# ----------------------------------------
# ------ Validation des hypotheses -------
# ----------------------------------------

# Graphiques (version basique)
# --- Creation d'une fenetre graphique pour 4 graphes (2 lignes, 2 colonnes)
par(mfrow = c(2, 2))
plot(reg_anova_2f, pch = 16)
# --- Retour vers une fenetre graphique standard (1 ligne, 1 colonne)
par(mfrow = c(1, 1))


# ----------------------------------------
# -------- Bilan de la regression --------
# ----------------------------------------

summary(reg_anova_2f)


# ----------------------------------------
# Test de Fisher : modeles emboites
# ----------------------------------------

# Modele additif v.s. modele avec interaction
# ----------------------------------------
# --- Ajustement du modele additif 
reg_add <- lm(maxO3 ~ vent + temps)
# --- Test de comparaison des modeles
anova(reg_add, reg_anova_2f)


# Test de l'effet d'un facteur
# ----------------------------------------
# --- Modele ANOVA 1 Facteur avec le vent
reg_vent <- lm(maxO3 ~ vent, data = donnees)
# --- Test de l'effet du facteur temps
anova(reg_vent, reg_anova_2f)

# --- Modele ANOVA 1 Facteur avec le temps
reg_temps <- lm(maxO3 ~ temps, data = donnees)
# --- Test de l'effet du facteur vent
anova(reg_temps, reg_anova_2f)


# Test de type I
# ----------------------------------------
anova(reg_anova_2f)


# Test de type II
# ----------------------------------------
library(car)
Anova(reg_anova_2f)

# ----------------------------------------
# ---------- Capacite predictive ---------
# ----------------------------------------

summary(reg_anova_2f)$adj.r.squared
