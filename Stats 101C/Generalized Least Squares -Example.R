#Generalized least squares
#Create the V matrix:
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/kriging_1.txt", header=TRUE)

b <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/kriging_11.txt", header=TRUE) 

x <- as.matrix(cbind(a$x, a$y))

x1 <- rep(rep(0,7),7)             #Initialize
dist <- matrix(x1,nrow=7,ncol=7)  #the distance matrix 

for (i in 1:7){
  for (j in 1:7){
    dist[i,j]=((x[i,1]-x[j,1])^2+(x[i,2]-x[j,2])^2)^.5		
  }
}

c0 <- 0
c1 <- 10
alpha <- 3.33

x1 <- rep(rep(0,7),7)              #Initialize
V <- matrix(x1,nrow=7,ncol=7)     #the C matrix

for(i in 1:7){
  for (j in 1:7){
    V[i,j]=c1*exp(-dist[i,j]/alpha)
    if(i==j){V[i,j]=c0+c1}
  }
}
#Find inverse of V:
C1 <- solve(V)

#Read data for regression:
a1 <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/soil_complete.txt", header=TRUE) 
a <- a1[1:7,]

#Create the X matrix:
ones <- rep(1, nrow(a))
X <- as.matrix(cbind(ones,a$cadmium, a$copper, a$zinc))

#Compute the beta_hat vector:
beta_hat <- solve(t(X) %*% C1 %*% X) %*% t(X) %*% C1 %*% a$lead

#Compute the inverse square root matrix:
q <- eigen(V)
C2 <- diag(q$values^-.5,7,7)

vec <- q$vectors
V1 <-  vec %*% C2 %*% t(vec)

#Compute the hat matrix:
hat <- V1 %*% X %*% solve(t(X) %*% C1 %*% X) %*% t(X) %*% V1

#Diagonal elements of hat matrix (leverage values) sum up to k+1:
sum(diag(hat))

#Sum of each row or column equal 1:
sum(hat[1,])
sum(hat[,1])
sum(hat[,5])

yy <- V1 %*% a$lead
sst <- (nrow(a)-1)*var(yy)

e <- yy - V1 %*% X %*% beta_hat
sse <- t(e) %*% e / 3

yhat <- V1 %*% X %*% beta_hat
dif <- yhat - mean(yy)
ssr <- sum(dif^2)

t(yy) %*% yy
t(e) %*% e
t(yhat) %*% yhat