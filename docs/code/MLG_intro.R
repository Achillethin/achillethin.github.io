# ----------------------------------------
# Introduction
# ----------------------------------------

library(ggplot2)
library(ggfortify)
library(gridExtra)
library(scales) 

# ----------------------------------------
# ---- Chargement d'un jeu de donnees ----
# ----------------------------------------

# Fixer le chemin d'acces au fichier dans lequel je travaille
getwd()
setwd('Users/achillethin/Desktop/cours Dauphine/data/')
#setwd("../data/")

# Chargement a partir d'un fichier excel
library(readxl)
donnees <- read_excel("wagesmicrodata.xls")

# Chargement a partir d'un fichier csv
donnees <- read.csv("../data/wagesmicrodata.csv", sep = ";", dec = ",")

# Chargement a l'aide de la fonction read.table
donnees <- read.table("../data/wagesmicrodata.csv", sep = ";", dec = ",", header = TRUE)


# ----------------------------------------
# ------ Visiualisation des donnees ------
# ----------------------------------------

# Voir les premieres lignes du jeu de donnees
head(donnees)


# ----------------------------------------
# -------- Variables quantitatives -------
# ----------------------------------------

# Resumes numeriques (ex. sur la colonne WAGE)
# --- Moyenne
mean(donnees$WAGE)
# --- Quartiles
quantile(donnees$WAGE, c(.25, .5, .75))
# --- Resumes
summary(donnees$WAGE)


# Representations graphiques (version basique)
# --- Histogramme
hist(donnees$WAGE, freq = FALSE,
     # Graphical parameters
     xlab = "WAGE")
# --- Boxplot
boxplot(donnees$WAGE,
        # Graphical parameters
        pch = 16, lty = 1, outwex = 0,
        pars = list(boxwex = 0.9, staplewex = 0.2),
        main = "Boxplot of WAGE",
        horizontal = TRUE, xlab = "WAGE")


# -------------------------- Pour plus tard ----------------------------------------

# Representations graphiques (version ggplot)
# --- Structure de base du graphique
p <- ggplot(donnees)
# --- Changement du theme
p <- p + theme_bw()
# --- Histogramme
p + geom_histogram(aes(x = WAGE))
# --- Boxplot
p + geom_boxplot(aes(x = WAGE))

# ----------------------------------------------------------------------------------


# ----------------------------------------
# -------- Variables qualitatives --------
# ----------------------------------------

# Specifier les colonnes a considerer comme des variables qualitatives
i_factor <- c("OCCUPATION", "SEX", "MARR", "RACE", "SECTOR", "SOUTH", "UNION")
# --- Pour les colonnes dont le nom est dans i_factor, on applique la fonction
# --- as.factor. Puis on replace le contenu des colonnes, par le meme contenu
# --- mais de type factor
for (i in i_factor) {
  donnees[, i] <- as.factor(donnees[, i])
}

# Remarque : avec read.table on peut eviter cette etape en precisant le type des colonnes
# au moment du chargement des donnees.
donnees <- read.table("wagesmicrodata.csv", sep = ";", dec = ",", header = TRUE, 
                      colClasses = c(rep("numeric", 2), "factor", 
                                     rep("numeric" , 3), rep("factor", 6)))


# Resumes numeriques (ex. sur la colonne OCCUPATION)
# --- Effectifs de chacune des catégories (modalités)
table(donnees$OCCUPATION)
# --- On peut aussi y acceder avec la commande summary
summary(donnees$OCCUPATION)

# Remarque : pour obtenir les frequences de chaque categorie, il suffit de diviser 
# l'effectif de la categorie par le nombre total d'observations
table(donnees$OCCUPATION)/nrow(donnees)


# Representations graphiques (version basique)
# --- Diagramme en baton (effectifs)
barplot(table(donnees$OCCUPATION),
        # Graphical parameters
        xlab = "OCCUPATION", ylab = "Effectif")
# --- Diagramme en baton (frequences)
barplot(table(donnees$OCCUPATION)/nrow(donnees),
        # Graphical parameters
        xlab = "OCCUPATION", ylab = "Fréquence")


# -------------------------- Pour plus tard ----------------------------------------

# Representations graphiques (version ggplot)
# --- Structure de base du graphique
p <- ggplot(donnees)
# --- Changement du theme
p <- p + theme_bw()
# --- Diagramme en baton (effectifs)
p + geom_bar(aes(x = OCCUPATION)) + ylab("Effectif")
# --- Diagramme en baton (frequences)
p + geom_bar(aes(x = OCCUPATION, y = (..count..)/sum(..count..))) + ylab("Fréquence")

# ----------------------------------------------------------------------------------


# ----------------------------------------
# - Relation quantitative - quantitative -
# ----------------------------------------

# Nuage de points (version basique)
# --- Ex. entre EDUCATION et WAGE
plot(donnees$EDUCATION, donnees$WAGE,
     # Graphical parameters,
     xlab = "EDUCATION", ylab = "WAGE", pch = 16)


# -------------------------- Pour plus tard ----------------------------------------

# Nuage de points (version ggplot)
# --- Structure de base du graphique
p <- ggplot(donnees)
# --- Changement du theme
p <- p + theme_bw()
# --- Ajout du nuage de points
p + geom_point(aes(x = EDUCATION, y = WAGE))

# ----------------------------------------------------------------------------------


# ----------------------------------------
# -- Relation qualitative - quantitative -
# ----------------------------------------

# Boxplot par categorie (version basique)
# --- Ex. entre WAGE et SEX
boxplot(donnees$WAGE ~donnees$SEX,
        pch = 16, lty = 1, outwex = 0,
        pars = list(boxwex = 0.9, staplewex = 0.2),
        xlab = "SEX", ylab = "WAGE")

# -------------------------- Pour plus tard ----------------------------------------

# Boxplot par categorie (version ggplot)
# --- Structure de base du graphique
p <- ggplot(donnees)
# --- Changement du theme
p <- p + theme_bw()
# --- Ajout du boxplot
p + geom_boxplot(aes(x = SEX, y = WAGE))

# ----------------------------------------------------------------------------------

