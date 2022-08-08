### Installing Necessary Packages:
install.packages("arm")
install.packages("rstan")
install.packages("rstanarm")
install.packages("sure")



### Load Necessary Packages: 
library(MASS)
library(rstanarm)
library(rstan)
library(arm)
library(foreign)
library(sure)



### Checking number of CPU Cores:
library(parallel)
detectCores(all.tests = FALSE, logical = TRUE)



### Loading necessary data:
nes <- read.dta("http://www.stat.ucla.edu/~handcock/216/datasets/nes/nes5200_processed_voters_realideo.dta",convert.factors=FALSE)
attach(nes)
yr <- 1992
ok <- year==yr & presvote<3
vote <- presvote[ok] - 1
income <- nes$income[ok]
invlogit <- function (x) {1/(1+exp(-x))}
fit.1 <- glm(vote ~ income, family=binomial(link="logit"))
curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), 1, 5, ylim=c(-.01,1.01),
       xlim=c(-2,8), xaxt="n", xaxs="i", mgp=c(2,.5,0),
       ylab="Pr (Republican vote)", xlab="Income", lwd=4)
curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), -2, 8, lwd=.5, add=T)
axis (1, 1:5, mgp=c(2,.5,0))
mtext ("(poor)", 1, 1.5, at=1, adj=.5)
mtext ("(rich)", 1, 1.5, at=5, adj=.5)
points (jitter(income, .3), jitter(vote, .04), pch=20, cex=.1)
# Note no conversion into factors
brdata <- read.dta( "http://www.stat.ucla.edu/~handcock/216/examples/nes/nes5200_processed_voters_realideo.dta", convert.factors=FALSE)
Nes.red <- brdata[brdata$presvote < 3,] # Remove third-party candidates
Nes.red$presvote <- ifelse(Nes.red$presvote==2, 1, 0) # recode as Republican or not
Nes.red
# I suggest using "presvote", "female", "black", "educ3",
# "income", "partyid7", "ideo_feel", "state2"
# to select among thier alternative versions.
# Comparing glm with stan_glm



### Q1a: (Need to change variables)
fita <- glm(presvote ~ female + black + income + real_ideo + income:real_ideo,
           family = binomial(link = "logit"), data = Nes.red, subset=(year==1992))
summary(fita)
fitb <- stan_glm(presvote ~ female + black + income + real_ideo + income:real_ideo,
            family = binomial(link = "logit"), data = Nes.red, subset=(year==1992))
summary(fitb)



### Q1b:
fit1 <- stan_glm(presvote ~ female + black + income + real_ideo + income:real_ideo,
                 family = binomial(link = "logit"), data = Nes.red, subset=(year==1992), 
                 prior = normal(location = 0,scale = 100,autoscale = FALSE))
summary(fit1)



### Q2:
fit <- glm(presvote ~ female + black + income,
           family = binomial(link = "logit"), data = Nes.red, subset=(year==1964))
summary(fit)
nrow(nes)
yr1964 <- nes[nes$year == 1964 & nes$presvote < 3 & nes$year,]
table(yr1964$black)

### EXPLANATION for Q2:
# We see that the sample size for blacks (1964) have a much lower amount of samples compared to
# the overall sample size. The sample error is the inverse of the sample size. Therefore, a smaller
# sample size indicated a larger error for blacks (1964).

table(yr1964$presvote, yr1964$black)
# We can also see that there are blacks (1964) who only voted for one candidate. Their responses are 
# not a good estimate for this GLM.

fitc <- stan_glm(presvote ~ female + black + income,
           family = binomial(link = "logit"), data = Nes.red, subset=(year==1964))
summary(fitc)
# Rather than using Maximum Likelihood Estimation (MLE) to check the standard deviation, we use
# the MCMC (Bayesian model) method to apply and have more accurate values.The Bayesian model adds
# priors (independent by default) on the coefficients of the GLM.



### Q3:
fitb <- stan_glm(presvote ~ female + black + income + real_ideo + income:real_ideo,
                 family = binomial(link = "logit"), data = Nes.red, subset=(year==1992))
summary(fitb)
fitd <- stan_glm(presvote ~ female + black + income + real_ideo + income:real_ideo,
                 family = binomial(link = "probit"), data = Nes.red, subset=(year==1992))
summary(fitd)

## EXPLANATION for Q3:
# # The results are essentially the same. The logit model uses something called the cumulative
# distribution function of the logistic distribution. The probit model uses something called the
# cumulative distribution function of the standard normal distribution to define f(...). Both 
# functions will take any number and rescale it to fall between 0 and 1.



### Q4a:
nesdf <- data.frame(partyid7, real_ideo, dem_therm)
fit2 <- polr(factor(partyid7) ~ real_ideo + dem_therm, Hess = TRUE, method = "logistic",
             data = nesdf)
summary(fit2)



### Q4b
## EXPLANATION:
# We can use the t-value to test them for the t-test. The thing is that there is no
# default significance to test it against. Next we see the usual regression output 
# coefficient table including the value of each coefficient, standard errors, and
# t value, which is the ratio of the coefficient to its standard error. Finally, we see 
# the residual deviance and the AIC. Both the deviance and AIC are useful for model 
# comparison.


### Q4c:
plot(profile(fit2))
autoplot.polr(fit2, what = "fitted")