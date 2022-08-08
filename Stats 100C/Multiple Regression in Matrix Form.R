a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100c/soil_complete.txt", header=TRUE)

#See the first six rows of the data:
head(a)

#Create the X matrix:
ones <- rep(1, nrow(a))
X <- as.matrix(cbind(ones,a$cadmium, a$copper, a$zinc))

#Compute the beta_hat vector:
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% a$lead

#Compute se^2:
se2 <- (t(a$lead) %*% a$lead - t(beta_hat) %*% t(X) %*% a$lead)/(nrow(a)-3-1)

#Verify using the lm function:
q <- lm(a$lead ~ a$cadmium+a$copper+a$zinc)
summary(q)

#Compute the hat matrix:
hat <- X %*% solve(t(X) %*% X) %*% t(X)

#Part of the hat matrix:
hat[1:5, 1:5]

#Diagonal elements of hat matrix (leverage values) sum up to 2:
sum(diag(hat))

#Sum of each row or column equal 1:
sum(hat[1,])
sum(hat[,130])

#Compute the variance covariance matrix of the beta_hat matrix:
varcov <- as.numeric(se2)* solve(t(X) %*% X)

#Same answer can be obtained using:
vcov(q)
