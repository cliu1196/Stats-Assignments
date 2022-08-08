a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/jura.txt",
                header=TRUE)

# Set up variables
y <- a$Pb
x1 <- a$Cd
x2 <- a$Co
x3 <- a$Cr
x4 <- a$Cu
x5 <- a$Ni
x6 <- a$Zn


# HW 17 Q(a):
#Estimation:
#Construct the design matrix X: 
ones <- rep(1, nrow(a)) 
X <- as.matrix(cbind(ones, x1, x2, x3, x4, x5, x6)) 
X


# HW 17 Q(b):
#Estimate the beta vector: 
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y 
beta_hat

#Compute se^2: 
se2 <- (t(y) %*% y - t(beta_hat) %*% t(X) %*% y) / (nrow(a)-6-1)
se2

#Compute the hat matrix:
hat <- X %*% solve(t(X) %*% X) %*% t(X)
hat


# HW 17 Q(c):
#Using F test General:
q <- c(0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0 ,0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1)
C <- matrix(q,6,7,byrow=TRUE) 
g <- rep(0,6)
F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C %*%beta_hat-g)) / (nrow(C)*se2)
F



# HW 17 Q(d):
#Testing:
#H0: (beta1, beta3)' = 0
#Ha: (beta1, beta3)' =/= 0
#Test beta1=beta3=0
Cd <- matrix(c(0,1,0,0,0,0,0, 0,0,0,1,0,0,0),2,7,byrow=TRUE)
gd <- c(0,0)
Fd <- (t(Cd%*%beta_hat-gd)%*%solve(Cd%*%solve(t(X)%*%X)%*%t(Cd))%*%(Cd%*%beta_hat-gd)) / (2*se2)
Fd


# HW 17 Q(e):
#Testing:
#H0: (beta1, beta3)' = 0
#Ha: (beta1, beta3)' =/= 0
#Extra sum of squares principle:
#Run the two models:
#Full model:
qe1 <- lm(a$y ~ x1+x2+x3+x4+x5+x6)
summary(qe1)
qe2 <- lm(a$y ~ x2+x4+x5+x6)
summary(qe2)

ssef <- summary(qe1)$sigma^2*(nrow(a)-6-1)
sser <- summary(qe2)$sigma^2*(nrow(a)-4-1)
f <- ((sser-ssef)/1)/(ssef/(nrow(a)-6-1))
f
summary(qf)


# HW 17 Q(f):
#Testing:
#H0: beta4 = 0
#Ha: beta4 =/= 0
qb <- c(0,0,0,0,1,0,0)
t <- (beta_hat[5]-0)/(se2* t(qb) %*%solve(t(X) %*% X) %*% qb)^.5
t

# HW 17 Q(g):
q <- c(0,1,1,0,0,-3,0, 0,0,0,1,0,1,1)
C <- matrix(q,2,7, byrow = TRUE)
g <- c(2,3)
beta_hat_c <- beta_hat - solve(t(X) %*% X) %*% t(C) %*% solve(C %*% solve(t(X) %*% X) %*%  t(C)) %*% (C %*% beta_hat-g)
ec <- y - X %*% beta_hat_c
ssec <- t(ec) %*% ec

beta_hat_c
ssec/(nrow(a)-6-1+2)


# HW 17 Q(h):
# H0: beta1+beta2+beta3=1 = beta1-beta2+2beta3=2
# Ha: Not true
q <- c(0,1,1,0,0,-3,0, 0,0,0,1,0,1,1)
q1 <- c(-3,0,1,1)
C1 <- matrix(q1,2,2, byrow=TRUE)
q2 <- c(0,1,1,0,0, 0,0,0,1,0)
C2 <- matrix(q2,2,5, byrow=TRUE)

X1 <- as.matrix(cbind(x1,x2))
X2 <- as.matrix(cbind(ones,x3,x4,x5,x6))
X2r <- X2 - X1 %*% solve(C1) %*% C2
yr <- y - X1 %*% solve(C1) %*% g
beta2_r <- solve(t(X2r) %*% X2r) %*% t(X2r) %*% yr
beta1_r <- solve(C1) %*% (g - C2 %*% beta2_r)

#Residuals from canonical form of the model:
ec1 <- yr - X2r %*% beta2_r
ssec1 <- t(ec1) %*% ec1
ssec1/(nrow(a)-6-1+2)

# F-test:
q <- c(0,1,1,0,0,-3,0, 0,0,0,1,0,1,1)
C <- matrix(q,2,7, byrow = TRUE)
g <- c(2,3)
beta_hat_c <- beta_hat - solve(t(X) %*% X) %*% t(C) %*% solve(C %*% solve(t(X) %*% X) %*%  t(C)) %*% (C %*% beta_hat-g)
ec <- y - X %*% beta_hat_c
ssec <- t(ec) %*% ec

beta_hat_c
ssec/(nrow(a)-6-1+2)





# HW 17 Q(i):
F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C %*%beta_hat-g)) / (nrow(C)*se2)
F

q <- c(0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0 ,0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1)
q1i <- c(0,0, 0,0, 0,0, 0,0, 0,0, 1,0, 0,1)
C1i <- matrix(q1i,2,2, byrow=TRUE)
q2i <- c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1, 0,0,0,0,0, 0,0,0,0,0)
C2i <- matrix(q2i,2,5, byrow=TRUE)
gi <- rep(0,6)
X1 <- as.matrix(cbind(x1,x2))
X2 <- as.matrix(cbind(ones,x3,x4,x5,x6))
X2r <- X2 - X1 %*% solve(C1i) %*% C2
yr <- y - X1 %*% solve(C1i) %*% gi
beta2_ri <- solve(t(X2r) %*% X2r) %*% t(X2r) %*% yr
beta1_ri <- solve(C1) %*% (gi - C2i %*% beta2_ri)

#Residuals from canonical form of the model:
ec1i <- yr - X2r %*% beta2_ri
ssec1i <- t(ec1i) %*% ec1i
ssec1i/(nrow(a)-6-1+2)




# HW 17 Q(j):
ones <- rep(1, nrow(a)) 
X <- as.matrix(cbind(ones, x1, x2, x3, x4, x5, x6)) 
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y 
se2 <- (t(y) %*% y - t(beta_hat) %*% t(X) %*% y) / (nrow(a)-6-1)
hat <- X %*% solve(t(X) %*% X) %*% t(X)
varcov <- as.numeric(se2)* solve(t(X) %*% X)
varcov
