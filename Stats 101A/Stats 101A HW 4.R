# Stats 101A HW 4
# page 157
library(readr)
library(car)

# Problem 1
## Exercise 5-1
Prob1 <- read_csv("UCLA Works/UCLA Winter 2020/Stats 101A/Homeworks/HW 4/overdueNew (1).csv")
names(Prob1)
attach(Prob1)

m1 <- lm(LATE ~ BILL)
summary(m1)



# Problem 2
## Exercise 5-3
Prob2 <- read.table("UCLA Works/UCLA Winter 2020/Stats 101A/Homeworks/HW 4/Latour (2).txt", header = TRUE)
attach(Prob2)


## 2a)
m2 <- lm(Quality ~ EndofHarvest * Rain)
summary(m2)
### p-value = 0.0120 < 0.05 --> we can say it is significant.


## 2b)
### Linear Equation Estimated:
### Quality = 5.16122 + (-0.03145)*(EndofHarvest) + (1.78670)*(Rain) + 
###           (-0.08314)*(EndofHarvest)*(Rain)

### Case (i):
### No unwanted rain at the harvest
### Quality = 5.16122 + (-0.03145)*(EndofHarvest) + (1.78670)*(0) + 
###           (-0.08314)*(EndofHarvest)*(0) for Rain = 0

### --> Quality = 5.16122 + (-0.03145)*(EndofHarvest) --> slope = -0.03145
### Thus to decrease by 1 unit of quality
-1/-0.03145
### 31.7965 days OR approximately 32 days


### Case (ii):
### Some unwanted rain at the harvest
### Quality = 5.16122 + (-0.03145)*(EndofHarvest) + (1.78670)*(1) + 
###           (-0.08314)*(EndofHarvest)*(1) for Rain = 1

### --> Quality = 6.94792 + (-0.11459)*(EndofHarvest) --> slope = -0.11459
### Thus to decrease by 1 unit of quality
-1/-0.11459
### 8.726765 days OR approximately 9 days



# Problem 3
## Exercise 6-2
## 3a)
### 2 Concerns are: (1) multicollinearity --> important
###                 (2) linearity assumptions --> normality



# Problem 4
## Exercise 6-3
Prob4 <- read_csv("UCLA Works/UCLA Winter 2020/Stats 101A/Homeworks/HW 4/cars (4).csv")
attach(Prob4)
## 4a)
m3 <- lm(SuggestedRetailPrice ~ EngineSize + Cylinders + Horsepower + HighwayMPG + 
           Weight + WheelBase)
summary(m3)
par(mfrow = c(2, 2))
plot(m3)

### Not a valid model because two predictors are not significant, the plot violates the
### normality assumption, violates outliers and leverages, and violates linearity. 


## 4b)
### The residuals plot has a curved pattern, so we should do some transformation. It 
### violated the linearity assumption.


## 4c)
leverage <- hatvalues(m3)

### h_ii > 2 * (p + 1)/n for p = "number of predictors"
nrow(Prob4) # n = 234 & there are 7 predictors (p = 7)
which(leverage >= 2 * (7 + 1)/234 & abs(rstandard(m3)) >= 2)
### 223 & 223 --> bad leverage

### h_ii > 2 * average(h_ii)
which(leverage >= 2 * mean(leverage) & abs(rstandard(m3)) >= 2)
### 223 & 223 --> bad leverage


## 4d)
m4 <- lm(log(SuggestedRetailPrice) ~ I(EngineSize^(0.25)) + I(log(Cylinders)) + 
                  I(log(Horsepower)) + I(1/HighwayMPG) + Weight + I(log(WheelBase)) + 
                  Hybrid)
summary(m4)
par(mfrow = c(2, 2))
plot(m4)

### It is an improvement of the old model (m3), but it is still an invalid model. The new
### model (m4) has 2 predictors that are not significant, the plot the violates outliers
### and leverages, and violates linearity assumption. 


## 4e) 
m5 <- lm(log(SuggestedRetailPrice) ~ I(EngineSize^(0.25)) + I(log(Cylinders)) + 
           I(log(Horsepower)) + Weight + Hybrid)
summary(m5)
anova(m4, m5)

### Since the p-value for the F-test is large, we can remove the 2 predictors.


## 4f)
### A new categorical variable with Manufacturer, then add it to the regression model.



# Problem 5
## Exercise 6-5
Prob5 <- read_csv("UCLA Works/UCLA Winter 2020/Stats 101A/Homeworks/HW 4/pgatour2006 (5).csv")
attach(Prob5)
names(Prob5)

## 5a)
summary(powerTransform(cbind(PrizeMoney, DrivingAccuracy, GIR, PuttingAverage,
                             BirdieConversion, SandSaves, Scrambling, PuttsPerRound) - 1))

### Yes, I agree with this PowerTransformation because the LRT test shows that all parameters
### equal to zero. 

m6a_1 <- lm(PrizeMoney ~ DrivingAccuracy + GIR + PuttingAverage +
           BirdieConversion + SandSaves + Scrambling + PuttsPerRound)
summary(m6a_1)
par(mfrow = c(2, 2))
plot(m6a_1)

m6a_2 <- lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage +
             BirdieConversion + SandSaves + Scrambling + PuttsPerRound)
summary(m6a_2)
par(mfrow = c(2, 2))
plot(m6a_2)

### I agree with the log Transformation because the R-squared is greater with the log(y)
### and the diagnostic plots are better. For instance, the Residuals vs. Fitted is more
### equally spread and is a straight horizontal line. The normality assumption is fulfilled,
### Scale-Location plot is satisfied, and lastly the outliers and leverages are okay.


## 5b)
m6b_1 <- lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage +
              BirdieConversion + SandSaves + Scrambling + PuttsPerRound)
summary(m6b_1)
par(mfrow = c(2, 2))
plot(m6b_1)

pairs(~ log(PrizeMoney) + DrivingAccuracy + GIR + PuttingAverage +
        BirdieConversion + SandSaves + Scrambling + PuttsPerRound, main = "Scatterplot Matrix")
par(mfrow = c(1, 1))
plot(rstandard(m6b_1))

### After looking at the diagnostic plots, we can see that the log(y) is a good choice
### as they satisfy the assumptions for linearity and normality.


## 5c)
leverage_5 <- hatvalues(m6b_1)
which(leverage_5 >= 2 * mean(leverage_5))
## Leverages are 16, 40, 70, 77, 168, 178

which(abs(rstandard(m6b_1)) >= 2)
## Outliers are 9, 47, 63, 122, 180, 185


## 5d)
vif(m6b_1)

### It has multicollinearity. For the variables with ViF > 5 are:
### GIR, PuttingAverage, and PuttsPerRound


## 5e)
### No, because removing one predictor may influence the whole model. At least, only
### p-value should not determine this.