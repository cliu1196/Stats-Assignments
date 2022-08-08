#Power of the test in simple regression - example.
#Read the data:
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/soil.txt", header=TRUE)
#Use only n=7:
a1 <- a[3:9,]

#Run simple regression of lead on zinc:
q <- lm(a1$lead ~ a1$zinc)

#Summary of the regression:
summary(q)

#Test the hypothesis H0: beta1=0 using alpha=0.05 and df=5.
#This is the critical t value= 2.570582 obtained using:
qt(0.975, 5)

#Conclusion:  Reject H0 because our test statistic is 9.577 > 2.570582.

#============================================================
#Now suppose we want to compute the power of the test if beta1=0.3 and sigma^2=625. We will use either the non-central t or the non-central F distributions.

#Using the non-central t distribution.  We need to compute the noncentrality parameter (see class notes):
beta1 <- 0.3
sigma <- 25
ncpar1 <- beta1*(6*var(a1$zinc))^.5/sigma

#Power and Type II error.
#Plot the central t and show the critical values:
x <- seq(-10, 10, .05)
y <- dt(x,5,, ncp=0)

plot(x, y, xaxt="n", yaxt="n", type="l", lwd=2,
     ylab=substitute(f(x)), xlim=c(-20,20))

#Critical values:
abline(v=2.570582)
abline(v=-2.570582)

#Plot the non-central t dist:
x <- seq(-16, 20, .1)
y <- dt(x, 5, ncp=ncpar1)

points(x, y, xaxt="n", yaxt="n", type="l", lwd=2, col="blue",
       ylab=substitute(f(x)))

#Compute the power:
pt(2.570582, 5, ncp=ncpar1, lower.tail=FALSE) + 
  pt(-2.570582, 5, ncp=ncpar1)

#The Type II error is 1-power.
#=========================================================
#Using non-central F distribution.
#We need the non-centrality parameter for the chi-squared distribution (see class notes).
ncpar2 <- beta1^2*6*var(a1$zinc)/sigma^2

#Critical value using the F distribution is equal to 6.607891 obtained as follows:
qf(0.95, 1,5)

#Power and Type II error.
#Central and non-central F distributions.
#Plot the central F dist:
x <- seq(0, 30, .05)
y <- df(x, df1=1,df2=5, ncp=0)

plot(x, y, xaxt="n", yaxt="n", type="l", lwd=2,
     ylab=substitute(f(x)))

#Critical value:
abline(v= 6.607891)

#Plot the non-central F dist:
x <- seq(0, 30, .1)
y <- df(x, df1=1,df2=5, ncp=ncpar2)

points(x, y, xaxt="n", yaxt="n", type="l", lwd=2, col="blue",
       ylab=substitute(f(x)))

#Compute the power:
1-pf(6.607891, 1,5, ncp=ncpar2)