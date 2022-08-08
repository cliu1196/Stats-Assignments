a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/body_fat.txt", header=TRUE)
#Response variable:
y <- a$y
#Predictor variables:
x1 <- a$x11
x2 <- a$x12
x3 <- a$x13
x4 <- a$x14
x5 <- a$x15

# HW 18 Q(a):
ones <- rep(1, nrow(a)) 
X <- as.matrix(cbind(ones, x1, x2, x3, x4, x5))
X
txx <-- t(X)%*%X
txx
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y 
beta_hat
hat <- X %*% solve(t(X) %*% X) %*% t(X)
hat
y_hat <- X%*%beta_hat
y_hat
e <- y - X %*% beta_hat
e
se2 <- (t(y) %*% y - t(beta_hat) %*% t(X) %*% y) / (nrow(a)-5-1)
se2

# HW 18 Q(b):
# Testing:
# H0: beta2 = beta4 = beta5 = 0
# Ha:at least one of these betas is not equal to 0
q <- c(0,0,1,0,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1)
C <- matrix(q,2,6,byrow=TRUE)
g <- c(0,0)
F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_hat-g)) / (3*se2)
F


# HW 18 Q(c):
# Full model:
qf <- lm(a$y ~ x1+x2+x3+x4+x5)

# Reduced model:
q <- c(0,0,1,0,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1)
q1 <- c(0,0,0,1)
C1 <- matrix(q1,2,2, byrow=TRUE)
q2 <- c(0,0,1,0,0, 0,0,0,0,1, 0,0,0,0,0)
C2 <- matrix(q2,3,5, byrow=TRUE)
X1 <- as.matrix(cbind(x1,x2))
X2 <- as.matrix(cbind(ones,x3,x4,x5))
X2r <- X2 - X1 %*% solve(C1) %*% C2
yr <- y - X1 %*% solve(C1) %*% g
beta2_r <- solve(t(X2r) %*% X2r) %*% t(X2r) %*% yr
beta1_r <- solve(C1) %*% (g - C2 %*% beta2_r)
yr <- y - X1 %*% solve(C1) %*% g
qr <- lm(yr ~ X2r[,2]+X2r[,3]+X2r[,4])

# Compute the SSE for each model:
ssef <- summary(qf)$sigma^2*(nrow(a)-5-1)
sser <- summary(qr)$sigma^2*(nrow(a)-3-1)

# Compute the F ratio:
f <- ((sser-ssef)/2)/(ssef/(nrow(a)-5-1))
f



# HW 18 Q(d):
n <- nrow(a)
sigma2 <- 50
beta1 <- c(-42.0, 2.4, -0.5, 1.9, 0.1, -1.6)
alpha <- 0.05
qnorm(alpha, lower.tail=FALSE)
F <- (t(C%*%beta1-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta1-g)) / (3*sigma2)
F
nrow(a)
m1 <- lm(y ~ x2 + x4 + x5)
summary(m1)
# Error is 0.00000693
# Power is 0.9999931



# HW 18 Q(e):
q1 <- c(2,1,1,1,2,3, 0,2,5,1,1,1)
C <- matrix(q1,2,6,byrow=TRUE)
g1 <- c(15, 25)
gam <- matrix(g1,2,1)

beta_hat_c <- beta_hat - solve(t(X) %*% X) %*% t(C) %*% solve(C %*% solve(t(X) %*% X) %*%  t(C)) %*% (C %*% beta_hat-g)
beta_hat_c


