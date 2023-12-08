rgen_x <- function(n, v) {
  x <- matrix(0, n, 4)
  x[, 1] <- rexp(n, .5)
  x[, 2] <- runif(n, 0, 3)
  x[, 4] <- rexp(n, 1)
  x[, 3] <- x[, c(1, 2, 4)] %*% v - runif(n, 0, 1)
  return(x)
}

v <- c(1, 2, .3)
# --- Group n°1
n_1 <- 142
a_1 <- c(1, .5, 0.1, -.5)
b_1 <- 1.7

x_1 <- rgen_x(n_1, v)
y_1 <- x_1 %*% a_1 + b_1

# --- Group n°2
n_2 <- 124
a_2 <- c(1, 2.5, 0.001, -4)
b_2 <- 1.3

x_2 <- rgen_x(n_2, v)
y_2 <- x_2 %*% a_2 + b_2

z <- rnorm(n_1 + n_2, sd = .3)
u <- runif(n_1 + n_2, 0, 2)

y <- c(y_1, y_2)
y <- y + z + u


donnees <- data.frame(y = y, x = rbind(x_1, x_2) , 
                      f_1 = c(rep(1, n_1), rep(2, n_2)), 
                      f_2 = sample(1:4, n_1 + n_2, TRUE))
donnees <- donnees[sample(1:nrow(donnees)),]
colnames(donnees) <- c("y", "x_1", "x_2", "x_3", "x_4", "f_1", "f_2")

write.table(donnees, file = "toy_example.csv", row.names = FALSE, sep = " ")

rm(list = ls())

setwd("../data/")
donnees <- read.table("toy_example.csv", sep = " ", header = TRUE, 
                      colClasses = c(rep("numeric", 5), rep("factor", 2)))
attach(donnees)
reg <- lm(y ~ x_1 * f_1)
summary(reg)
anova(reg)
