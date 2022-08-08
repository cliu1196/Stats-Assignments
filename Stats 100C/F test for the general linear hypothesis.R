#F test for the general linear hypothesis - examples

a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/body_fat.txt", header=TRUE) 

#Dependent variable: 
y <- a$y

#Independent variables: 
x1 <- a$x6 
x2 <- a$x7 
x3 <- a$x8 
x4 <- a$x9 
x5 <- a$x10 


#Estimation:
#Construct the design matrix X: 

ones <- rep(1, nrow(a)) 
X <- as.matrix(cbind(ones,x1,x2,x3,x4,x5)) 

#Estimate the beta vector: 
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y 

#Compute se^2: 
se2 <- (t(y) %*% a$y - t(beta_hat) %*% t(X) %*% y) / (nrow(a)-5-1)



#Testing:
#H0: beta2=0
#Ha: beta2 not equal 0

#Using t test:
q <- c(0,0,1,0,0,0)
t <- (beta_hat[3]-0)/(se2* t(q) %*%solve(t(X) %*% X) %*% q)^.5

#Using F test:
q <- c(0,0,1,0,0,0) 
C <- matrix(q, 1,6, byrow=TRUE) 
g <- c(0) 
F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*% beta_hat-g)) / (1*se2)
#==========================================================#=========




#==========================================================#=========
#Testing:
#H0: beta1-beta2=0
#Ha: beta1-beta2 not equal 0

q1 <- c(0,1,-1,0,0,0)
C1 <- matrix(q1, 1,6, byrow=TRUE)

#Using the F test:
F1 <- (t(C1%*%beta_hat-g)%*%solve(C1%*%solve(t(X)%*%X)%*%t(C1))%*%(C1%*% beta_hat-g)) / (1*se2)

#Using the t test:
t1 <- (beta_hat[2]-beta_hat[3]-0)/(se2* t(q1) %*%solve(t(X) %*% X) %*% q1)^.5

=================================================================
  =================================================================
  =================================================================
  
  
  #Test:
  #H0: beta2=beta5=0
  #Ha: At least one of these beta !=0.
  #Extra sum of squares principle:
  #Run the two models:
  
  #Full model:
  qf <- lm(a$y ~ x1+x2+x3+x4+x5)

#Reduced model:
qr <- lm(a$y ~ x1+x3+x4)

#Compute the SSE for each model:
ssef <- summary(qf)$sigma^2*(nrow(a)-5-1)
sser <- summary(qr)$sigma^2*(nrow(a)-3-1)

#Compute the F ratio:
f <- ((sser-ssef)/2)/(ssef/(nrow(a)-5-1))


#Using the general F test:
#Define matrix C and vector gamma: 
q <- c(0,0,1,0,0,0,0,0,0,0,0,1) 
C <- matrix(q, 2,6, byrow=TRUE) 
g <- c(0,0) 
F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_hat-g)) / (2*se2) 

#============================================










#F test for individual beta_i using the extra sum of squares:
#Test:
#H0: beta3=0
#Ha: beta3 != 0.
#Extra sum of squares principle:
#Run the two models:

#Full model:
qf <- lm(a$y ~ x1+x2+x3+x4+x5)

#Reduced model:
qr <- lm(a$y ~ x1+x2+x4+x5)

#Compute the SSE for each model:
ssef <- summary(qf)$sigma^2*(nrow(a)-5-1)
sser <- summary(qr)$sigma^2*(nrow(a)-4-1)

#Compute the F ratio:
f <- ((sser-ssef)/1)/(ssef/(nrow(a)-5-1))

#Compare with t statistic in the full model.
#t^2 for testing H0: beta3=0 is equal to f statistic.
summary(qf)
