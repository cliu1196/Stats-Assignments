### Define Variables
n <- nrow(mtcars)
y <- mtcars$mpg
ones <- rep(1, n)

X1 <- as.matrix(cbind(ones, mtcars[, 2:4]))
head(X1)

X2 <- as.matrix(mtcars[, 5:8])

### Create H Matrix and X Matrix
H_maker <- function(X) {
  Hat_mat <- X %*% solve.default(t(X) %*% X) %*% t(X)
  return(Hat_mat)
}

X_full <- as.matrix(cbind(X1, X2))

H12 <- H_maker(X_full)
n
dim(H12)
head(H12)

H1<- H_maker(X1)

I <- diag(n)

### Find Estimate for Beta1.2
beta2.1hat <- solve(t(X2) %*% (I - H1) %*% X2) %*% t(X2) %*% (I - H1) %*% y
beta2.1hat

### Prove result (a)
LHS <- t(y) %*% (I - H12) %*% y
RHS <- t(y - X2 %*% beta2.1hat) %*% (I - H1) %*% (y - X2%*%beta2.1hat)
LHS
RHS


### Prove part (b)
LHS <- t(y) %*% (I - H1) %*% y - t(y) %*% (I - H12) %*% y
MID <- t(beta2.1hat) %*% t(X2) %*% (I - H1) %*% y
RHS <- t(beta2.1hat) %*% (t(X2) %*% (I - H1) %*% X2) %*% beta2.1hat
LHS
MID
RHS


### Prove part (c)
# Check to see if idempotent
library("matrixcalc")
is.idempotent.matrix((I - H1), tol = 1e-08)

# Create beta1hat matrix
beta1hat <- solve(t(X1) %*% X1) %*% t(X1) %*% y

# You should get: cov(beta1hat, beta2.1hat) and
# solve(t(X1) %*% X1) %*% t(X1) %*% (I - H1) %*% t(X2) %*% solve(t(X2) %*% (I - H1) %*% X2)
# Realize that t(X1) %*% (I - H1) is... (using as.vector for simpler format)
as.vector(t(X1) - t(X1))
