a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100c/soil_complete.txt", header=TRUE)

#Create the X matrix:
ones <- rep(1, nrow(a))
X <- as.matrix(cbind(ones,a$cadmium, a$copper, a$zinc))

#Compute the beta_hat vector:
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% a$lead

#==================================
#Add an extra row in X:  (1,2.75,29.3,380.3).
#New matrix X:
x0 <- c(1, 2.75, 29.3, 380.3)
X1 <- rbind(X,x0)

#Add an extra y value:  128.
#New y vector:
y0 <- 128
y1 <-c(a$lead, y0)

#Find the new OLS estimator:
beta_hat_new <- solve(t(X1) %*% X1) %*% t(X1) %*% y1

#==================================


#==================================
#Use the add/delete method:
e0 <- y0-t(x0) %*% beta_hat
h00 <- t(x0) %*% solve(t(X) %*% X) %*% x0

qq <- e0/(1+h00)

bbb <- beta_hat + solve(t(X) %*% X) %*% x0  *as.numeric(qq)