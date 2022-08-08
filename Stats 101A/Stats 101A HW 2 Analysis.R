# Stats 101A HW 2
## on page 116 for the Textbook

# Part 1
## 1a)
library(readr)
data1 <- read_csv("UCLA Works/UCLA Winter 2020/Stats 101A/Homeworks/HW 2/Airfares.csv")
attach(data1)


m1a <- lm(Fare ~ Distance)
summary(m1a)

# Here the intercept is 48.971. Interpretation is if the Distance travelled is 
# zero also then also the fare is 48.971 and the slope is 0.2196. interpretation is if 
# the distance is increased by one unit then the fare will be increased by 0.2196 unit.



## 1b)
residm1a <- residuals(m1a) # getting the residuals
residm1a
fitm1a <- fitted(m1a) # getting the fitted
fitm1a

plot(residm1a, Distance)
plot(fitm1a, residm1a)



## 1c)
hatm1a <- hatvalues(m1a) #getting the levarages
hatm1a
max(hatm1a) # getting the maximum levarage

# Here the 13th value has the largest levarage. If the levarage is greater than 
# (2*(p+1))/n then the point is an outlier, where p is the no of regressor and n is the 
# no of observation here p=1 and n=17 so the cut off point is 0.235 here the levarage 
# is greater so its a outlier point

library(MASS)

studresidm1a <- studres(m1a) # getting the studentised residual
studresidm1a

max(studresidm1a) # getting the maximum studentised residual

# Here the 13th data point has the largest studentised residual and its a outlier.


## 1d)
cookdistm1a <- cooks.distance(m1a) #getting the cooks distances
cookdistm1a

max(cookdistm1a) # getting the maximum cooks diatnace


# the 13th point has the largest cooks distance if the cooks ditance is greater than 
# 4/(n-p-1) then the distanbce is worthy here 4/(n-p-1)=0.26 and the cooks ditance for 
# the point is 1.3696 so the distance is really noteworthy and the data point is an 
# influential point also.
