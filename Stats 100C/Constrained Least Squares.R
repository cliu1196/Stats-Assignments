#Constrained least squares:

a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/soil_complete.txt", header=TRUE)

ones <- rep(1, nrow(a))

X <- as.matrix(cbind(ones, a$zinc, a$copper))

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% a$lead

qq <- c(2,1,-1,1,2,3)

C <- matrix(qq, 2,3, byrow=TRUE)

g <- c(10,20)

beta_c <- beta_hat - solve(t(X) %*% X) %*% t(C) %*% solve(C %*% solve(t(X) %*% X) %*%  t(C)) %*% (C %*% beta_hat-g)

#Residuals from constrained least squares:
ec <- a$lead - X %*% beta_c
