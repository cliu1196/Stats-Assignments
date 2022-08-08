# Stats 100C HW 11 Question (B)


# Question B.1
# Step 0: Set paramemters up and create matrix
y <- mtcars$cyl
View(mtcars)
X <- mtcars[,3:7]
X <- as.matrix(X)

n <- nrow(X)
X <- cbind(rep(1,n), X)

X1 <- X[,1:3]
X2 <- X[,4:6]

# Step 1: Regress y on X1
H1 <- X1 %*% solve (t(X1) %*% X1) %*% t(X1)
e1star <- (diag(n) - H1) %*%y
e1star

# Step 2: Regress X2 on X1
e2star <- (diag(n) - H1) %*%X2
e2star

# Step 3: Regress e1star on e2star
beta2hat <- solve (t(e2star) %*%e2star)%*%t(e2star)%*%e1star
beta2hat

# Step 4: Solve and compare
mod1 <- lm(y ~ X)
summary(mod1)$coefficients
beta2hat



# Question B.2
# Step 0: Create the "mean sweeper"
ones <- rep(1,n)
sweeper <- diag(n) - (1/n)*ones%*%t(ones)

# Step 1: Multiply "mean sweeper" by our y to create e1star and find deviations
e1 <- sweeper %*%y

# Step 2: Find deviations of X_(0)
e2 <- sweeper %*% X[,2:ncol(X)]
e2

# Step 3: Regress e1star on e2star
beta_coeff <- solve(t(e2)%*%e2)%*%t(e2)%*%e1
beta_coeff


# Question B.3
# Step 0: Create our matrices and use such that Xnewnew corresponds to beta_3 hat
Xnew <- X[,1:3]
Xnewnew <- X[,4] 

Hnew <- Xnew %*% solve(t(Xnew)%*%Xnew)%*% t(Xnew)

# Step 1: Regress y on Xnew
e1starnew <- (diag(n) - Hnew) %*%y

# Step 2: Regress Xnewnew on Xnew
e2starnew <- (diag(n) - Hnew)%*%Xnewnew

# Step 3: Regress e1star on e2star
beta_hat3 <- solve(t(e2starnew)%*%e2starnew)%*%t(e2starnew)%*%e1starnew
beta_hat3

summary (lm(y ~ X[,1:4]))$coefficients 

