# For a (3x3) Matrix with x11, ... , xnk having some arbitrary Independent values.
# The only one that doesn't change is c(1, 1, 1).
X <- cbind(c(1, 1, 1), c(3, 5, 20), c(69, 13, 11))
H <- X%*%solve(t(X)%*%X)%*%t(X)
B <- c(1, 1, 1)
C <- B%*%H
print(C)
