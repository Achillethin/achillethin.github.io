# Espace de travail
setwd("../data")

# Chargement des donnees en specifiant le type des variables
donnees <- read.table("eucalyptus.txt", header = TRUE, dec = ".",
                      colClasses = c(rep("numeric", 2), rep("factor", 1)))
attach(donnees)

# ------------------------------------------------------------
# ---------------- Regression lineaire simple ----------------
# ------------------------------------------------------------

# Variable reponse : ht
# Variable explicative circ

# ----------------------------------------
# ---------- Analyse descriptive ---------
# ----------------------------------------

# Moyenne variable explicative
x_bar <- mean(circ)
# Moyenne variable reponse
y_bar <- mean(ht)
# Coefficient de correlation empirique
rho_xy <- cor(circ, ht)
# Test de nullite du coefficient de correlation empirique
cor.test(circ, ht)


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

reg <- lm(ht ~ circ)
summary(reg)

# Droite d'ajustée
plot(circ, ht, pch = 16)
abline(a = coef(reg)[1], b = coef(reg)[2], col = "royalblue", lwd = 2)

# -------------------------- Pour plus tard ----------------------------------------

# Graphique (version ggplot)
# --- Creation d'un tableau contenant les donnees initiales et les valeurs ajustees
df <- data.frame(donnees[, c("ht", "circ")], ADJ.VAL = reg$fitted.values)
# --- Creation du spport graphique
pl1 <- ggplot(data = df, aes_string(x = "circ", y = "ht")) + theme_bw()
# --- Ajout du nuage de points
pl1 <- pl1 + geom_point() 
# --- Ajout de la droite de regression
pl1 <- pl1 + geom_line(data = df, aes(y = ADJ.VAL), col = "royalblue")
# --- Affichage du graphique
print(pl1)

# ----------------------------------------------------------------------------------


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
# ----- Transformation du regresseur -----
# ----------------------------------------

# Variable reponse : ht
# Variable explicative : log(circ) ou sqrt(circ)


reg_log <- lm(ht ~ log(circ))
plot(reg_log, pch = 16)


reg_sqrt <- lm(ht ~ sqrt(circ))
plot(reg_sqrt, pch = 16)


# --- Comparaison des capacites predictives
summary(reg)$r.squared
summary(reg_log)$r.squared
summary(reg_sqrt)$r.squared


# ----------------------------------------
# -------------- Predictions -------------
# ----------------------------------------

x_new <- data.frame(circ = c(23, 45))

# --- Pour le modele ht ~ circ
predict(reg, newdata = x_new, interval = "confidence", level = .95)
predict(reg, newdata = x_new, interval = "prediction", level = .95)

# --- Pour le modele ht ~ log(circ)
predict(reg_log, newdata = x_new, interval = "confidence", level = .95)
predict(reg_log, newdata = x_new, interval = "prediction", level = .95)

# --- Pour le modele ht ~ sqrt(circ)
predict(reg_sqrt, newdata = x_new, interval = "confidence", level = .95)
predict(reg_sqrt, newdata = x_new, interval = "prediction", level = .95)


# ------------------------------------------------------------
# --------------- Regression lineaire multiple ---------------
# ------------------------------------------------------------

# Variable reponse : ht
# Variables explicatives : circ et sqrt(circ)

# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

reg_add <- lm(ht ~ circ + sqrt(circ))


# ----------------------------------------
# ------ Validation des hypotheses -------
# ----------------------------------------

plot(reg_add, pch = 16)


# ----------------------------------------
# -------- Bilan de la regression --------
# ----------------------------------------

summary(reg_add)


# ----------------------------------------
# --- Test de Fisher : modeles emboites --
# ----------------------------------------

anova(reg, reg_add)


# ------------------------------------------------------------
# ---------------------- ANOVA 1 Facteur ---------------------
# ------------------------------------------------------------

# Variable reponse : ht
# Variable explicative : bloc


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

reg_anova <- lm(ht ~ bloc)


# ----------------------------------------
# ------ Validation des hypotheses -------
# ----------------------------------------

plot(reg_anova, pch = 16)


# ----------------------------------------
# -------- Bilan de la regression --------
# ----------------------------------------

summary(reg_anova)


# ----------------------------------------
# ----- Test de Student : comp. 2 a 2 ----
# ----------------------------------------

pairwise.t.test(ht, bloc, "none")
pairwise.t.test(ht, bloc, "BH")


# ----------------------------------------
# --- Test de Fisher : modeles emboites --
# ----------------------------------------

anova(reg, reg_add)


# ------------------------------------------------------------
# --------------------------- ANCOVA -------------------------
# ------------------------------------------------------------


# ----------------------------------------
# --------- Ajustement du modele ---------
# ----------------------------------------

reg_ancova <- lm(ht ~ bloc * log(circ))


# ----------------------------------------
# ------ Validation des hypotheses -------
# ----------------------------------------

plot(reg_ancova, col = bloc, pch = 16)


# ----------------------------------------
# -------- Bilan de la regression --------
# ----------------------------------------

summary(reg_ancova)

# Test de type I
anova(reg_ancova)

# Test de type II
library(car)
Anova(reg_ancova)

