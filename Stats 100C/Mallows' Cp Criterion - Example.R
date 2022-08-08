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
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

#Residuals:
e <- y - X %*% beta_hat
SSE <- t(e) %*% e

#Compute se^2: 
se2 <- SSE/(nrow(a)-5-1)


#===========================
#===========================
#Drop x2 and x5:
p <- 4
ones <- rep(1, nrow(a)) 
X1 <- as.matrix(cbind(ones,x1,x3,x4)) 

#Estimate the beta vector: 
beta_hat_p <- solve(t(X1) %*% X1) %*% t(X1) %*% y 

#Compute SSEp:
ep <- y-X1 %*% beta_hat_p
SSEp <- t(ep) %*% ep

#Compute Cp:
Cp1 <- p + SSEp/se2 - (nrow(a)-p)


#===========================
#===========================
#Drop only x2:
p <- 5
ones <- rep(1, nrow(a)) 
X1 <- as.matrix(cbind(ones,x1,x3,x4,x5)) 

#Estimate the beta vector: 
beta_hat_p <- solve(t(X1) %*% X1) %*% t(X1) %*% y 

#Compute SSEp:
ep <- y-X1 %*% beta_hat_p
SSEp <- t(ep) %*% ep

#Compute Cp:
Cp2 <- p + SSEp/se2 - (nrow(a)-p)


#===========================
#===========================
#What if we drop x1 and x3:
p <- 4
ones <- rep(1, nrow(a)) 
X1 <- as.matrix(cbind(ones,x2,x4,x5)) 

#Estimate the beta vector: 
beta_hat_p <- solve(t(X1) %*% X1) %*% t(X1) %*% y 

#Compute SSEp:
ep <- y-X1 %*% beta_hat_p
SSEp <- t(ep) %*% ep

#Compute Cp:
Cp3 <- p + SSEp/se2 - (nrow(a)-p)

#===========================
#===========================
#And what if we drop x1, x2:
p <- 4
ones <- rep(1, nrow(a)) 
X1 <- as.matrix(cbind(ones,x3,x4,x5)) 

#Estimate the beta vector: 
beta_hat_p <- solve(t(X1) %*% X1) %*% t(X1) %*% y 

#Compute SSEp:
ep <- y-X1 %*% beta_hat_p
SSEp <- t(ep) %*% ep

#Compute Cp:
Cp4 <- p + SSEp/se2 - (nrow(a)-p)


Cp <- c(Cp1, Cp2, Cp3, Cp4)
p <- c(4, 5, 4, 4)

plot(p, Cp, xlim=c(0,6))

abline(0,1)