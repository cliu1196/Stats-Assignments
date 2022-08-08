## R Command for MSE 3 different estimator for p
p <- seq(0,1,.01)
n <- 100

mse1 <- p*(1-p)/n

mse2 <- p*(1-p)/n + .0009

mse3 <- n*p*(1-p)/106^2 + ((n*p-106*p+3)/106)^2

plot(p, mse2, type="l", col="blue", lwd=3, ylim=c(0,.0035), ylab="MSE")
points(p, mse1, type="l", col="green", lwd=3)
points(p, mse3, type="l", col="purple", lwd=3)


## Order Statistics a simulation example
#Generate 5000 pairs random numbers from exp(1/100)
#First 10000 and then collapse the vector into 5000:
x <- rexp(10000,1/100)

#The 5000x2 matrix:
y <- matrix(x, 5000,2)

#Consider the two components connected in series:
#Find the minimum of the two numbers:
yy <- apply(y, 1,min)

hist(yy)

#Find the empirical probability that the system fails within 80 hours:
sum(yy<=80)/5000

#Find the theoretical probability:
#We have seen that the minimum follows the exponential distribution with parameter lambda=2/100=1/50.  Therefore the probability that the system fails within 80 hours is:

1-exp(-(1/50)*80)


#===========================
#Suppose now the two components are connected in parallel.
#Find the maximum of the two numbers:
yy <- apply(y, 1,max)

hist(yy)

#Find the empirical probability that the system fails within 80 hours:
sum(yy<=80)/5000

## Bootstrap Confidence Intervals
Bootstrap confidence intervals:
  
  #A.  If you know the distribution from where the sample was taken:  Suppose #X1, ..., Xn from exponential with parameter lambda.
  data <- c(11.96, 5.03, 67.40, 16.07, 31.50, 7.73, 11.10, 22.38, 17.99,   36.72, 12.73, 33.81, 10.99,  72.70, 89.55, 48.96, 3.06, 30.33, 80.43,   56.14)

n <- length(data)
mean(data)
#Estimate of lambda:
lambda_hat <- 1/mean(data)

#Now we draw many samples of size n from an exponential distribution with #parameter lambda_hat:

x <- rexp(n, lambda_hat)
lambda_hat <- 1/mean(x)
lambda_hat

#Here is another one:
x <- rexp(n, lambda_hat)
lambda_hat <- 1/mean(x)
lambda_hat

#Repeat the procedure B=500 times (in other words, get 500 samples each one #of size n=20 and compute the sample mean):
B=500
xbar <- rep(0,B)
for(i in 1:B){
  x <- rexp(n, lambda_hat)
  xbar[i] = mean(x)
}
xbar

#Therefore:
lambda_bootstrap <- 1/xbar

lambda_bootstrap

hist(lambda_bootstrap)

#We need a 95% CI for lambda:  find the 2.5 percentile and 97.5 percentile #of the distribution of lambda_hat:

quantile(lambda_bootstrap, c(0.025, 0.975))






#B. If you don't know the distribution from where the sample was taken.  #Then treat your data as the population and take many samples of size n with #replacement:

data <- c(11.96, 5.03, 67.40, 16.07, 31.50, 7.73, 11.10, 22.38, 17.99,   36.72, 12.73, 33.81, 10.99,  72.70, 89.55, 48.96, 3.06, 30.33, 80.43,   56.14)

x <- sample(data, replace=TRUE)
theta_hat <- mean(x) 
theta_hat

#Do this many times:
B=500
theta_boot <- rep(0,B)
for(i in 1:B){
  x <- sample(data, replace=TRUE)
  theta_boot[i] = mean(x)
}

theta_boot


hist(theta_boot)


quantile(theta_boot, c(0.025, 0.975))


## Likelihood Ratio Test Example
n <- 15
sigma2 <- 16

mu0 <- 100

xbar <- seq(90,110,0.1) 

LRT <- exp(-(15/(2*16))*(xbar-100)^2)

plot(xbar,LRT, ylab= expression(lambda), xlab=expression(bar(X)),type="l", 
     main=expression(paste("Likelihood ratio test: ", 
                           mu[0]==100 ,  " against ", mu[0]!=100)))

abline(h=0)

#Suppose k=0.2
k <- 0.2
abline(h=0.2, col="blue")

#Find RR:
xbar1 <- mu0 + sqrt(-2*16*log(k)/n)
xbar2 <- mu0 - sqrt(-2*16*log(k)/n)

#Place xbar1 and xbar2 on the graph:
segments(xbar1,0, xbar1, .2, col="blue")
segments(xbar2,0, xbar2, .2, col="blue")
points(xbar1,0, pch=19)
points(xbar2,0, pch=19)

#What is alpha?
#P(xbar > xbar1 | mu0=100) + #P(xbar < xbar2 | mu0=100):
z1 <- (xbar1-100)/(4/sqrt(15))
z2 <- (xbar2-100)/(4/sqrt(15))

alpha <- pnorm(z1, lower.tail=FALSE) + pnorm(z2)



#Suppose k=0.10
k <- 0.10
abline(h=0.10, col="green")

#Find RR:
xbar1 <- mu0 + sqrt(-2*16*log(k)/n)
xbar2 <- mu0 - sqrt(-2*16*log(k)/n)

#Place xbar1 and xbar2 on the graph:
segments(xbar1,0, xbar1, .10, col="green")
segments(xbar2,0, xbar2, .10, col="green")
points(xbar1,0, pch=19)
points(xbar2,0, pch=19)

#What is alpha?
#P(xbar > xbar1 | mu0=100) + #P(xbar < xbar2 | mu0=100):
z1 <- (xbar1-100)/(4/sqrt(15))
z2 <- (xbar2-100)/(4/sqrt(15))

alpha <- pnorm(z1, lower.tail=FALSE) + pnorm(z2)


## Non-central chi-squared, t, and F Distributions in R
#Central and non-central chi-squared distributions.
#Central chi-squared dist:
x <- seq(0, 20, .15)
y <- dchisq(x, df=5, ncp=0)

plot(x, y, xaxt="n", yaxt="n", type="l", lwd=2,
     ylab=substitute(f(x)))


#Non-central chi-squared dist:
x <- seq(0, 20, .15)
y <- dchisq(x, df=5, ncp=3)

points(x, y, xaxt="n", yaxt="n", type="l", lwd=2, col="blue",
       ylab=substitute(f(x)))



#=================================================
#=================================================
#Central and non-central t distributions.
#Central t dist:
x <- seq(-10, 10, .05)
y <- dt(x,4,, ncp=0)

plot(x, y, xaxt="n", yaxt="n", type="l", lwd=2,
     ylab=substitute(f(x)), xlim=c(-20,20))


#Non-central t dist:
x <- seq(-16, 20, .1)
y <- dt(x, 4, ncp=2)

points(x, y, xaxt="n", yaxt="n", type="l", lwd=2, col="blue",
       ylab=substitute(f(x)))








#=================================================
#=================================================
#Central and non-central F distributions.
#Central F dist:
x <- seq(0, 10, .05)
y <- df(x, df1=3,df2=5, ncp=0)

plot(x, y, xaxt="n", yaxt="n", type="l", lwd=2,
     ylab=substitute(f(x)))

#Non-central F dist:
x <- seq(0, 10, .1)
y <- df(x, df1=3,df2=5, ncp=3)

points(x, y, xaxt="n", yaxt="n", type="l", lwd=2, col="blue",
       ylab=substitute(f(x)))



