# Task 1A:
### Done on paper



# Task 1B:
x <- c(1, 2, 3, 4)
t <- c(1, 3, 2, 4)

a1 <- c(1, 1, 1, 1)
model1 <- lm(t ~ x, weights = a1)
plot(x, t, xlim = c(0,5), ylim = c(0,5), asp = 1)
abline(model1)
print(model1$coefficients)

a2 <- c(0.1, 5, 5, 0.1)
model2 <- lm(t ~ x, weights = a2)
plot(x, t, xlim = c(0, 5), ylim = c(0, 5), asp = 1)
abline(model2)
print(model2$coefficients)

a3 <- c(5, 0.1, 0.1, 5)
model3 <- lm(t ~ x, weights = a3)
plot(x, t, xlim = c(0, 5), ylim = c(0, 5), asp = 1)
abline(model3)
print(model3$coefficients)



# Part 2:
library(carData)
data(Chirot)
chirot_mat <- as.matrix(Chirot)



# Part 3:
library(DAAG) # install.packages("DAAG") if necessary
library(lattice) # Loading required package: lattice
x <- seq(10,40, .1) # a sequence used to plot lines
L1 <- lm(chemical ~ magnetic, data = ironslag)
plot(ironslag$magnetic, ironslag$chemical, main = "Linear fit", pch = 16)
yhat1 <- L1$coef[1] + L1$coef[2] * x
lines(x, yhat1, lwd = 2, col = "blue")



# Part 4:
x <- 1:100
set.seed(1)
e <- rnorm(100, 0, 40)
t <- 30 + 3.4 * x + e # the true slope is 3.4, the true intercept is 30
coefs <- coef(lm(t ~ x)) # the coefficients from using lm
print(coefs) #if you use set.seed(1), this should be 35.266629, 3.381958

plot(x, t)
abline(coef = coefs, col = "red")










