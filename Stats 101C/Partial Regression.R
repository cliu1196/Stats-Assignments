#Partial regression.
#Access the data:
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/jura.txt", header=TRUE)

#Rename the variables:
y <- a$Pb

x1 <- a$Cd
x2 <- a$Co
x3 <- a$Cr
x4 <- a$Cu
x5 <- a$Ni
x6 <- a$Zn

#Regression of y on x1,x2,x3,x4,x5,x6:
q <- lm(y ~ x1+x2+x3+x4+x5+x6)

#Regression of y on x1,x2,x3,x4:
q1 <- lm(y ~ x1+x2+x3+x4)

#Residuals from the regression of y on x1,x2,x3,x4:
e1 <- q1$res

#Regression of x5 on x1,x2,x3,x4:
q5 <- lm(x5 ~ x1+x2+x3+x4)
e5 <- q5$res

#Regression of x6 on x1,x2,x3,x4:
q6 <- lm(x6 ~ x1+x2+x3+x4)
e6 <- q6$res

#Run regression of e1 on e5 and e6:
lm(e1 ~ e5+e6)

#Verify that the coefficients are the same:
summary(q)

==========================================================
  #Using matrix/vector approach:
  #Full regression:
  ones <- rep(1, nrow(a))
X <- as.matrix(cbind(ones,x1, x2, x3, x4, x5, x6))
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

#Partial regression of y on x1,x2,x3,x4:
ones <- rep(1, nrow(a))
X1 <- as.matrix(cbind(ones,x1, x2, x3, x4))

beta_hat1 <- solve(t(X1) %*% X1) %*% t(X1) %*% y

H1 <- X1 %*% solve(t(X1) %*% X1) %*% t(X1)

y11 <- (diag(nrow(a)) - H1) %*% y

#Create the X2 matrix:
X2 <- as.matrix(cbind(x5, x6))

X22 <- (diag(nrow(a)) - H1) %*% X2

beta2_hat <- solve(t(X22) %*% X22) %*% t(X22) %*% y11
