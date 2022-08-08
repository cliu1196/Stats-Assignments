y <- mtcars$mpg
View(mtcars)
X <- mtcars[,3:7]
X <- as.matrix(X)

n <- nrow(X)
X <- cbind(rep(1,n), X)

X1 <- X[,1:3]
X2 <- X[,4:6]

### 1. Regress y on X1
H1 <- X1 %*% solve (t(X1) %*% X1) %*% t(X1)
e1star <- (diag(n) - H1) %*%y

### 2. Regress X2 on X1
e2star <- (diag(n) - H1) %*%X2
e2star

### 3. Regress e1star on e2star
beta2hat <- solve (t(e2star) %*%e2star)%*%t(e2star)%*%e1star
beta2hat

mod1 <- lm(y ~ X)
summary(mod1)$coefficients
beta2hat


######################################################################

ones <- rep(1,n)
mean_sweeper <- diag(n) -(1/n)*ones%*%t(ones)

#1. Deviations of y - same as residuals of y on column 1 (all ones)
e1star <- mean_sweeper %*%y

#2. deviations of X_(0)
e2star <- mean_sweeper %*% X[,2:ncol(X)]
e2star

#3. Regress e1star on e2star
beta_coeff <- solve(t(e2star)%*%e2star) %*% t(e2star) %*% e1star


######################################################################

#
Xnew <- X[,1:3]
Xnewnew <- X[,4] #corresponds to beta_3 hat

Hnew <- Xnew %*% solve(t(Xnew)%*%Xnew)%*% t(Xnew)

# Regress y on Xnew
e1star <- (diag(n) - Hnew) %*%y

# Regress Xnewnew on Xnew
e2star <- (diag(n) - Hnew)%*%Xnewnew

# Regress e1star on e2star
beta_hat3 <- solve(t(e2star)%*%e2star)%*%t(e2star)%*%e1star
beta_hat3

summary (lm(y ~ X[,1:4]))$coefficients 
