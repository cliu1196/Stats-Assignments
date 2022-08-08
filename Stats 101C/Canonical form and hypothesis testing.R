#Canonical form and hypothesis testing.

#Prepare the data:
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/body_fat.txt", header=TRUE) 

#Dependent variable: 
y <- a$y

#Independent variables: 
x1 <- a$x6 
x2 <- a$x7 
x3 <- a$x8 
x4 <- a$x9 
x5 <- a$x10 

ones <- rep(1, nrow(a)) 
X <- as.matrix(cbind(ones,x1,x2,x3,x4,x5)) 

#Estimate the beta vector: 
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% a$y 

#Compute se^2: 
se2 <- (t(a$y) %*% a$y - t(beta_hat) %*% t(X) %*% a$y) / (nrow(a)-5-1)

#Constrained least squares:
#Suppose we use the constraints:
#beta1+beta2+beta3=1 and 
#beta1-beta2+2beta3=2

#Define matrix C and vector gamma: 
q <- c(0,1,1,1,0,0,0,1,-1,2,0,0) 
C <- matrix(q, 2,6, byrow=TRUE) 
g <- c(1,2)

beta_hat_c <- beta_hat - solve(t(X) %*% X) %*% t(C) %*% solve(C %*% solve(t(X) %*% X) %*%  t(C)) %*% (C %*% beta_hat - g)


#================================
#================================
#Repeat using canonical form.
#First we solve out the restrictions imposed by the linear hypothesis:
q1 <- c(1,1,1,-1)
C1 <- matrix(q1,2,2, byrow=TRUE)

q2 <- c(0,1,0,0,0,2,0,0)
C2 <- matrix(q2,2,4, byrow=TRUE)

X1 <- as.matrix(cbind(x1,x2))

X2 <- as.matrix(cbind(ones,x3,x4,x5))

X2r <- X2 - X1 %*% solve(C1) %*% C2

yr <- y - X1 %*% solve(C1) %*% g

beta2_r <- solve(t(X2r) %*% X2r) %*% t(X2r) %*% yr

beta1_r <- solve(C1) %*% (g - C2 %*% beta2_r)


#Aside comment: 
#Use the lm function.
#Reduced model:
qr <- lm(yr ~ X2r[,2]+X2r[,3]+X2r[,4])










#================================
#================================
#To test the hypothesis we can use the extra sum of squares principle:

#Full model:
qf <- lm(a$y ~ x1+x2+x3+x4+x5)

#Reduced model:
qr <- lm(yr ~ X2r[,2]+X2r[,3]+X2r[,4])

#Compute the SSE for each model:
ssef <- summary(qf)$sigma^2*(nrow(a)-5-1)
sser <- summary(qr)$sigma^2*(nrow(a)-3-1)

#Compute the F ratio:
f <- ((sser-ssef)/2)/(ssef/(nrow(a)-5-1))

#Compare with F tst for the general linear hypothesis:
#Define matrix C and vector gamma: 
q <- c(0,1,1,1,0,0,0,1,-1,2,0,0) 
C <- matrix(q, 2,6, byrow=TRUE) 
g <- c(1,2)

F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_hat-g)) / (2*se2) 

