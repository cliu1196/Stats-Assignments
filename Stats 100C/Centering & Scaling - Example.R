#Centering and scaling - example:

#Access the data:
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/soil_complete.txt", header=TRUE)

#Response variable:
y <- a$lead

#Predictor variables:
x1 <- a$cadmium
x2 <- a$copper
x3 <- a$zinc

#Center the predictor variables:
z1 <- x1-mean(x1)
z2 <- x2-mean(x2)
z3 <- x3-mean(x3)

#Compute the diagonal entries of matrix D:
s11 <- sum(z1^2)
s22 <- sum(z2^2)
s33 <- sum(z3^2)

#Construct the matrix D:
D <- diag(c(s11^.5, s22^.5, s33^.5), 3,3)

#Construct the matrix Z:
Z <- as.matrix(cbind(z1,z2,z3))

#Construct the matrix Zs:
Zs <- Z %*% solve(D)

#Compute the delta vector of the centered and scaled model:
delta_hat <- solve(t(Zs) %*% Zs) %*% t(Zs) %*% y

#Transform back to the original units:
beta_hat <- solve(D) %*% delta_hat

#Verify the correlation matrix of the predictors:
xx <- as.data.frame(cbind(x1,x2,x3))
cor(xx)

#This correlation matrix can also be obtained by Zs'Zs:
t(Zs) %*% Zs
=======================================================================
  #R^2, MSE, F, and t statistics remain the same,
  #original predictors, or centered/scaled predictors:
  
  #a.Original predictors:
  q <- lm(y ~x1+x2+x3)
summary(q)

#b.Centered/scaled predictors:
qq <- lm(y ~ Zs[,1]+Zs[,2]+Zs[,3])
summary(qq)

#Find variance covariance matrix of beta_hat:
vcov(q)

#Find variance covariance matrix of delta_hat (includes also g0):
vcov(qq)

#How do we find var-covar matrix of beta_hat
#from var-covar matrix of delta_hat?
#From class notes: cov(delta_hat)=sigma^2*R^(-1).
#But, delta_hat = D*beta_hat => beta_hat = D^(-1) * delta_hat.
#Therefore, cov(beta_hat)=sigma^2 * D^(-1)* R^(-1) * D^(-1)
#For our data:
summary(q)$sigma^2*solve(D) %*% solve(t(Zs) %*% Zs) %*% solve(D)

#Find the variance of delta1_hat:
#From class notes: var(delta1_hat)=sigma^2/(1-r'R22^(-1)r).
R <- t(Zs) %*% Zs
r <- R[2:3,1]
var_delta1_hat <- summary(q)$sigma^2/(1-t(r) %*% solve(R[2:3,2:3]) %*% r)

#We can get the same result if we compute:
#var_delta1_hat=sigma^2/(1-R1^2), where R1^2 is the R^2 from the regression
#of Zs1 on Zs2 and Zs3:
qs <- lm(Zs[,1] ~ Zs[,2] + Zs[,3])
summary(qs)

var_delta1_hat <- summary(q)$sigma^2/(1-0.8891563)

#We can also find the variance of beta1_hat from the 
#variance of delta1_hat using:
var_beta1_hat <- summary(q)$sigma^2/((1-0.8891563)*(nrow(a)-1)*var(x1))
=======================================================================