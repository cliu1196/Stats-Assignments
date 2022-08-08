#Homework 17 - solutions:

a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/jura.txt",header=TRUE)

y <- a$Pb
x1 <- a$Cd
x2 <- a$Co
x3 <- a$Cr
x4 <- a$Cu
x5 <- a$Ni
x6 <- a$Zn
#================================================
#Question (a):
ones <- rep(1, nrow(a))

X <- as.matrix(cbind(ones, x1,x2,x3,x4,x5,x6))
#================================================
#Question (b):
#Compute the beta_hat vector:
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

e <- y -  X %*% beta_hat

t(e) %*% e / ((nrow(a)-6-1))

#Compute se^2:
se2 <- (t(y) %*% y - t(beta_hat) %*% t(X) %*% y)/(nrow(a)-6-1)

#Verify using the lm function:
q <- lm(y ~ x1+x2+x3+x4+x5+x6)
summary(q)

#Compute the hat matrix:
H <- X %*% solve(t(X) %*% X) %*% t(X)
#================================================
#Question (c):
#Test the overall significance of the model.
C <- as.matrix(cbind(rep(0,6), diag(6)))
g <- c(0,0,0,0,0,0)

F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_hat-g)) / (6*se2) 

#================================================
#Question (d):
#Test beta1=beta3=0
C <- matrix(c(0,1,0,0,0,0,0, 0,0,0,1,0,0,0),2,7,byrow=TRUE)
g <- c(0,0)

F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_hat-g)) / (2*se2) 

#================================================
#Question (e):
#Repeat (d) using the extra sum of squares principle.

#Full model:
qf <- lm(y ~ x1+x2+x3+x4+x5+x6)

#Reduced model:
qr <- lm(y ~ x2+x4+x5+x6)

#Compute the SSE for each model:
ssef <- summary(qf)$sigma^2*(nrow(a)-6-1)
sser <- summary(qr)$sigma^2*(nrow(a)-4-1)

#Compute the F ratio:
f <- ((sser-ssef)/2)/(ssef/(nrow(a)-6-1))

#================================================
#Question (f):
#Test beta4=0.

v <- solve(t(X) %*% X)
t <- (beta_hat[5]-0) / (se2^.5*v[5,5]^.5)

#Note: Verify using summary(q) from question (e) using summary(qf).

#================================================
#Question (g):
#Under H0 we have beta1+beta2-3beta5=2 and beta3+beta5+beta6=3.

C <- matrix(c(0,1,1,0,0,-3,0, 0,0,0,1,0,1,1), 2,7,byrow=TRUE)
g <- c(2,3)

beta_c <- beta_hat - solve(t(X) %*% X) %*% t(C) %*% solve(C %*% solve(t(X) %*% X) %*%  t(C)) %*% (C %*% beta_hat-g)

#Constrained residuals:
e_c <- y - X %*% beta_c

se22 <- t(e_c) %*% e_c / (nrow(a)-6-1+2)

#================================================
#Question (h):
#We will partition C into C1 and C2 as follows:

#===========
C1 <- C[,3:4]
C2 <- C[,-(3:4)]

X1 <- as.matrix(cbind(x2,x3))

X2 <- as.matrix(cbind(ones,x1,x4,x5,x6))



yr <- y - X1 %*% solve(C1) %*% g
X2r <- X2 - X1 %*% solve(C1) %*% C2


beta2_c <- solve(t(X2r) %*% X2r) %*% t(X2r) %*% yr

ec <- yr - X2r %*% beta2_c
t(ec) %*% ec


#============
C1 <- matrix(c(0,1,1,0,0, 0,0,0,1,0), 2,5, byrow=TRUE)

C2 <- matrix(c(-3,0,1,1), 2,2, byrow=TRUE)

#Now partition X into X1 and X2 accordingly:
X1 <- as.matrix(cbind(ones,x1,x2,x3,x4))
X2 <- as.matrix(cbind(x5,x6))

yr <- y - X2 %*% solve(C2) %*% g
X1r <- X1 - X2 %*% solve(C2) %*% C1

beta1_c <- solve(t(X1r) %*% X1r) %*% t(X1r) %*% yr

beta2_c <- solve(C2) %*% (g - C1 %*% beta1_c)

g <- c(2,3)

beta_c <- beta_hat - solve(t(X) %*% X) %*% t(C) %*% solve(C %*% solve(t(X) %*% X) %*%  t(C)) %*% (C %*% beta_hat - g)

#F test using extra sum of squares principle:
#Full model:
qf <- lm(y ~ x1+x2+x3+x4+x5+x6)

#Reduced model:
qr <- lm(yr ~ X1r[,2]+X1r[,3]+X1r[,4]+X1r[,5])

#Compute the SSE for each model:
ssef <- summary(qf)$sigma^2*(nrow(a)-6-1)
sser <- summary(qr)$sigma^2*(nrow(a)-4-1)

#Compute the F ratio:
f <- ((sser-ssef)/2)/(ssef/(nrow(a)-6-1))

#================================================
#Question (i):
C <- matrix(c(0,1,1,0,0,-3,0, 0,0,0,1,0,1,1), 2,7,byrow=TRUE)
g <- c(2,3)

F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_hat-g)) / (2*se2) 

#================================================
#Question (j):
#Use var(beta_c)=A %*% var_beta_hat %*% t(A) from class notes:

var_beta_hat <- as.numeric(se2) * solve(t(X) %*% X)

A <- diag(7) - solve(t(X) %*% X) %*% t(C) %*% 
  solve(C%*%solve(t(X)%*%X)%*%t(C)) %*% C

var_beta_c <- A %*% var_beta_hat %*% t(A)
#================================================
