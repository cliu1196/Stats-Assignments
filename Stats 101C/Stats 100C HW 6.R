a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100c/soil.txt",
                header=TRUE)
a1 <- a[1:30,]
q <- lm(a1$lead ~ a1$zinc)
summary(q)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 43.83661    6.72998   6.514 4.65e-07 ***
#   a1$zinc      0.21593    0.01185  18.225  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 20.28 on 28 degrees of freedom
# Multiple R-squared:  0.9223,	Adjusted R-squared:  0.9195 
# F-statistic: 332.1 on 1 and 28 DF,  p-value: < 2.2e-16

# We can see that the p-value: < 2.2e-16




beta1 <- 0.05
sigma2 <- 600
ncpar <- beta1^2*29*var(a1$zinc)/sigma2
q<- qf(0.95,1,28)
1-pf(q,1,28,ncp = ncpar)

# We can see that the Power: 0.9208974