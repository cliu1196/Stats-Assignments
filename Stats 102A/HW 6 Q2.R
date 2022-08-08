
# TwoSampleTestR.r
# Randomization test for difference between two means.
# I will begin using the mean difference as my stsatistic,
# and then move to using the t statistic for the same purpose.
# When we pool the variances they should give the same result.

NoWaiting <- c(36.30, 42.07, 39.97, 39.33, 33.76, 33.91, 39.65, 84.92, 40.70, 39.65,
               39.48, 35.38, 75.07, 36.46, 38.73, 33.88, 34.39, 60.52, 53.63 )

Waiting <- c(49.48, 43.30, 85.97, 46.92, 49.18, 79.30, 47.35, 46.52, 59.68, 42.89,
             49.29, 68.69, 41.61, 46.81, 43.75, 46.55, 42.33, 71.48, 78.95)
n1 <- length(NoWaiting)
n2 <- length(Waiting)
N <- n1 + n2
meanNW <- mean(NoWaiting) ; meanW <- mean(Waiting) 
diffObt <- (meanW - meanNW)
Combined <- c(NoWaiting, Waiting)     # Combining the samples
#Now pool the variances
s2p <- (var(NoWaiting)*(n1-1) + var(Waiting)*(n2-1))/(n1 + n2 - 2)
# Write out the results
cat("The obtained difference between the means is = ",diffObt, '\n')
cat("The pooled variance is = ", s2p, '\n')
cat("Now we will run a standard parametric t test \n")
ttest <- t.test(Waiting, NoWaiting)
t.test(Waiting, NoWaiting)
cat("The resulting t test on raw data is = ", ttest$statistic, '\n')
print(ttest)

tObt <-  ttest$statistic
nreps <- 5000             # I am using 5000 resamplings of the data
meanDiff <- numeric(nreps)   #Setting up arrays to hold the results
t <- numeric(nreps)
set.seed(1086)
for ( i in 1:nreps) {
  data <- sample(Combined, N,  replace = FALSE)
  grp1 <- data[1:n1]
  grp2 <- na.omit(data[n1+1: N])
  meanDiff[i] <- mean(grp1) - mean(grp2)
  test <- t.test(grp1, grp2)
  t[i] <- test$statistic
}

# Just to demonstrate the equivalence of mean differences and t
cat("The correlation between mean differences and t is = ",'\n')
print(cor(meanDiff, t))
cat('\n')

# Rather than incrementing a counter as I go along, I am counting at the end.
cat("The number of times when the absolute mean differences exceeded diffObt = ",'\n')
absMeanDiff <- abs(meanDiff)
absDiffObt = abs(diffObt)
print(length(absMeanDiff[absMeanDiff >= absDiffObt]))
cat("The number of times when the t values exceeded the obtained t = ",'\n')
abst <- abs(t)
abstObt <- abs(tObt)
print(length(abst[abst >= abstObt]))
cat("The proportion of resamplings when each the mean diff or t exceeded the 
obtained value = ",'\n')
print (length(abs(absMeanDiff[absMeanDiff >= absDiffObt]))/nreps)
cat('\n')

par(mfrow = c(2,2))           # Set up graphic surface
hist(meanDiff, breaks = 25, ylim = c(0,425))

hist(t, breaks = 50, xlim = c(-5,5), ylim = c(0,425))

#  The following plots the empirical distribution of t from resampling
#     against the actual Student's t distribution on 36 df.
#     Notice the similarity even though the data are badly skewed.
hist(t, freq = FALSE, xlim = c(-5,5), ylim = c(0,.4 ), col = "lightblue",
     ylab = "density", xlab = "t", main = "")
par(new = T)
den <- seq(-5, 4.998, .002)
tdens <- dt(den,36)
plot(den, tdens, type = "l", ylim = c(0,.4), xlim = c(-5,5),
     col = "red", ylab = "", xlab = "", main = "Student t and resample t")


# dch   12/3/2013