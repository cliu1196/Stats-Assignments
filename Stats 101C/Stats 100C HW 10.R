# Use R! Please find a data set with at least 3 predictors and
# compute the following:

library(readr)
hw10d <- read_csv("R Code/leverage.csv")
View(leverage)

X <- cbind(c(hw10d$X1), c(hw10d$X2), c(hw10d$X3))
Y <- c(hw10d$Y)

# (1) (X'X), (X'X)^-1, X'Y
t(X)%*%X
solve(t(X)%*%X)
t(X)%*%Y

# (2) ^Beta = (X'X)^-1*X'Y
solve(t(X)%*%X)%*%t(X)%*%Y

# (3) H X*(X'X)^-1*X'
X%*%solve(t(X)%*%X)%*%t(X)

# (4) ^Y = X*^B = X*(X'X)^-1*X'Y
X%*%solve(t(X)%*%X)%*%t(X)%*%Y

# (5) e = (Y - ^Y) = (Y - HY) = (I - H)*Y
C <- X%*%solve(t(X)%*%X)%*%t(X)%*%Y
Y - C