#Read the data:
#The data were obtained from https://www.iaaf.org/

#Men's world records in various distances:
a1 <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/men_rec.txt", header=TRUE) 

a2 <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/women_rec.txt", header=TRUE) 


#Use logarithms for both time and distance:
mtime <- log(a1$time)
mdist <- log(a1$dist)

wtime <- log(a2$time)
wdist <- log(a2$dist)


#We want to test beta_10=beta_20 and beta_11=beta_21
m_ones <- rep(1, nrow(a1))
w_ones <- rep(1, nrow(a2))

m_zeros <- rep(0, nrow(a1))
w_zeros <- rep(0, nrow(a2))

col1 <- c(m_ones,w_zeros)
col2 <- c(mdist <- log(a1$dist), w_zeros)
col3 <- c(m_zeros, w_ones)
col4 <- c(m_zeros, wdist <- log(a2$dist))

X <- as.matrix(cbind(col1,col2,col3,col4))

#Response variable:
y <- c(mtime, wtime)

#Compute beta_hat vector:
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

#Compute the residuals:
e <- y - X %*% beta_hat

#Compute se^2:
se2 <- t(e) %*% e / (27)

#Test hypothesis:
#H0: beta_10=beta_20 and beta_11=beta_21

#Method A:
#Use F test for the general linear hypothesis:
#Define matrix C and vector gamma: 
q <- c(1,0,-1,0,  0,1,0,-1) 
C <- matrix(q, 2,4, byrow=TRUE) 
g <- c(0,0)

F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_hat-g)) / (2*se2) 

#===========================================
#===========================================
#Method B:  Use full and reduced model.  The full model is the same as above.  For the reduced model use beta_10=beta_20 and beta_11=beta_21.

#X matrix under H0 (call it X1):
ones <- c(m_ones, w_ones)
x <- c(mdist, wdist)

X1 <- as.matrix(cbind(ones, x))

#Compute beta_hat vector:
beta_hat1 <- solve(t(X1) %*% X1) %*% t(X1) %*% y

#Compute the residuals:
e1 <- y - X1 %*% beta_hat1

sser <- t(e1) %*% e1

ssef <- t(e) %*% e

#Compute F statistic:
f <- (sser-ssef) / (2*se2)

