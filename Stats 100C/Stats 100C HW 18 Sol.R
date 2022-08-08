#Homework 18 - solutions:

a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/body_fat.txt", header=TRUE) 

#Dependent variable: 
y <- a$y

#Independent variables: 
x1 <- a$x11
x2 <- a$x12
x3 <- a$x13
x4 <- a$x14
x5 <- a$x15 

#Question (a):
ones <- rep(1, nrow(a)) 
X <- as.matrix(cbind(ones,x1,x2,x3,x4,x5)) 

#Estimate the beta vector: 
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y 

e <- y - X %*% beta_hat

#Compute se^2: 
se2 <- (t(y) %*% y - t(beta_hat) %*% t(X) %*% a$y) / (nrow(a)-5-1)

#===========================================
#Question b:
#Test:
#H0: beta2=beta4=beta5=0
#Ha: At least one of these beta !=0.
#Using the general F test:
#Define matrix C and vector gamma: 
q <- c(0,0,1,0,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1) 
C <- matrix(q, 3,6, byrow=TRUE) 
g <- c(0,0,0) 
F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_hat-g)) / (3*se2)


#Question c:
#Test:
#H0: beta2=beta4=beta5=0
#Ha: At least one of these beta !=0.
#Extra sum of squares principle:
#Run the two models:

#Full model:
qf <- lm(a$y ~ x1+x2+x3+x4+x5)

#Reduced model:
qr <- lm(a$y ~ x1+x3)

#Compute the SSE for each model:
ssef <- summary(qf)$sigma^2*(nrow(a)-5-1)
sser <- summary(qr)$sigma^2*(nrow(a)-2-1)

#Compute the F ratio:
f <- ((sser-ssef)/3)/(ssef/(nrow(a)-5-1))



#=======================================
#Question (d):
#Suppose 
sigma12 <- 50
beta <- as.matrix(c(-42, 2.4, -0.5, 1.9, 0.1,-1.6))
ncp <- (t(C%*%beta-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta-g))/sigma12

type_2_error <- pf(qf(.95,3,245), 3,245,ncp)

#=======================================
#Question (e):
qq <- c(2,1,1,1,2,3,0,2,5,1,1,1)

C <- matrix(qq, 2,6, byrow=TRUE)

g <- c(15,25)

beta_c <- beta_hat - solve(t(X) %*% X) %*% t(C) %*% solve(C %*% solve(t(X) %*% X) %*%  t(C)) %*% (C %*% beta_hat-g)

#Estimate sigma^2:
ec <- y - X %*% beta_c
se2c <- t(ec) %*% ec / (nrow(a)-5-1+2)
