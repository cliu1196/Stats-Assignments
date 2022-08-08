# Stats 101A Extra Credit

# Loading Necessary Packages:
library(readr)
library(ggplot2)
library(car)
library(GGally)
library(ggpubr)


# Loading the Data:
Bordeaux <- read_csv("UCLA Works/UCLA Winter 2020/Stats 101A/Extra Credit/Bordeaux.csv")
attach(Bordeaux)
names(Bordeaux)



# Question (1):
# A) First, using ifelse function, convert x3, x4, x5, x6 and x7 variables into 
# categorical with "Yes" instead of 1 and "No" instead of 0. Name the new 
# variables as P95andAboveNew, FirstGrowthNew, CultWineNew, PomerolNew and 
# VintageSuperstarNew respectively.

## x3
P95andAbove
P95andAboveNew <- ifelse(P95andAbove == 1, "Yes", "No")
P95andAboveNew

## x4
FirstGrowth
FirstGrowthNew <- ifelse(FirstGrowth == 1, "Yes", "No")
FirstGrowthNew

## x5
CultWine
CultWineNew <- ifelse(CultWine == 1, "Yes", "No")
CultWineNew

## x6
Pomerol
PomerolNew <- ifelse(Pomerol == 1, "Yes", "No")
PomerolNew

## x7
VintageSuperstar
VintageSuperstarNew <- ifelse(VintageSuperstar == 1, "Yes", "No")
VintageSuperstarNew


# B) Create ggpairs plot for the response and the other two numerical predictors.
# What did you notice?

df_bordeaux <- Bordeaux[, c(2, 3, 4)]
ggpairs(df_bordeaux)
cor.test(x = ParkerPoints, y = CoatesPoints)

## Using ggcorr() to get a better look at the correlation between these variables
ggcorr(df_bordeaux, palette = "RdBu", label = TRUE)

### What I noticed is that there is that the correlation between the variables are
### not related as much to each other. The highest correlation is between Price 
### ParkerPoints at 0.609. This is good because we don't want our predictors in
### a regression model to be highly correlated with each other. We want them to
### be more correlated with the response variable. I can see that the correlation
### between Price and CoatesPoints are not very correlated, which could result in
### it as a not very good predictor.


# C) Create a MLR (call it m0) using the two numerical predictors only. Study the
# summary, anova, vif and diagnostics of the model.
m0 <- lm(Price ~ ParkerPoints + CoatesPoints, data = Bordeaux)
summary(m0)
anova(m0)
vif(m0)
par(mfrow = c(2, 2))
plot(m0)

### From our summary, we can see that the intercept and ParkerPoints are statistically
### significant, but our R-squared is not very high (0.3722). This tells us that 
### maybe our model isn't the best fitted model. We can also see that the variable
### CoatesPoints is not statisticallty significant. Our anova test tells us that
### only ParkerPoints is significant. Since CoatesPoints is not statistically 
### significant, we REJECT the null hypothesis. The ViF is good since none of the 
### predictors have a value greater than 5. For the diagnostic plots, we can see
### that the Scale-Location and Residuals vs. Fitted plots have their linearity
### assumptions satisfied, even is they are not perfectly horizontal. As for the
### normality assumption, it is overall satisfied with the exception of a few small
### points. We can see the Leverage and Outliers are not satisfied. We would need
### to conduct some transformations to make the plots look better.


# D) Use leveragePlots and mmps on m0. What did you notice? Run powerTransform 
# and inversResponsePlot functions on m0. What do you need to do to make m0 
# a better model? Do it.
leveragePlots(m0)
mmps(m0)

### From our leveragePlots(), we can see that ParkerPoints variable has some
### increasing pattern, and this mean it is significant. As for CoatesPoints,
### the line is horizontal, mean this is not significant. From our mmps(), we
### observe that our model fits the data relatively well for ParkerPoints and 
### Fitted Values. Meanwhile, the CoatesPoints data is not very well aligned 
### with our model, telling us this is not a significant predictor to our model.

summary(powerTransform(cbind(Price, ParkerPoints, CoatesPoints) ~ 1))

par(mfrow = c(1, 1))
inverseResponsePlot(m0)

### Inverse response plot is suggesting power of 0 as the best transformation of
### the response variable. We would take the log() of x and y.

m0_log <- lm(log(Price) ~ log(ParkerPoints) + 
               log(CoatesPoints), data = Bordeaux)
summary(m0_log)
par(mfrow = c(2, 2))
plot(m0_log)

### The Normal Q-Q plot looks better and the R-squared has gone up to 0.7618. Not
### only that, but now both our predictors are significant.


# E) A statistician suggested to use log transformation on the response variable
# and the two numerical predictors. Do you agree or disagree with that 
# suggestion? Try it first then compare to m0.
m0_log <- lm(log(Price) ~ log(ParkerPoints) + 
               log(CoatesPoints), data = Bordeaux)
summary(m0_log)
par(mfrow = c(2, 2))
plot(m0_log)

### The Normal Q-Q plot looks better and the R-squared has gone up to 0.7618. Not
### only that, but now both our predictors are significant. I agree with the 
### statistian suggestion.



# Question (2):
# A) Create a side by side box plots of the variable Price for each of the 
# categorical variables created in Question 1 Part A. Which of these predictors 
# you think are good predictor for the variable "Price"? Why? 
ggplot() + aes(x = P95andAboveNew, y = Price, color = P95andAboveNew) + 
   geom_boxplot()

ggplot() + aes(x = FirstGrowthNew, y = Price, color = FirstGrowthNew) + 
  geom_boxplot()

ggplot() + aes(x = CultWineNew, y = Price, color = CultWineNew) + 
  geom_boxplot()

ggplot() + aes(x = PomerolNew, y = Price, color = PomerolNew) + 
  geom_boxplot()

ggplot() + aes(x = VintageSuperstarNew, y = Price, color = VintageSuperstarNew) + 
  geom_boxplot()

### I would choose the predictor, CultWineNew, because both of the responses 
### cover a wider range for the Price response. We can get a better prediction
### based of the variability of the responses. You can clearly see that the "Yes"
### boxplot covers a wider range of the Price.


# B) Create a MLR (call it mcat) using all categorical predictor to predict 
# "Price". Check summary, anova, vif and diagnostics of the model. Does the MLR 
# summary agrees with your answer in part A?
mcat <- lm(Price ~ P95andAboveNew + FirstGrowthNew + CultWineNew + 
           PomerolNew + VintageSuperstarNew)
summary(mcat)
par(mfrow = c(2, 2))
plot(mcat)
anova(mcat)
vif(mcat)

### Based off of the summary(), we can see that our predictor for CultWineNew is
### significant, which is a good sign. Our diagnostic plots are the following:
### (Residuals vs. Fitted) assumption satisfied, (Normal Q-Q) assumption of 
### normality not satisfied, (Scale-Location) not satisfied since there is an
### increasing line, (Residuals vs Leverage) not satisfied. Our anova() tells us
### that CultWineNew is definitely significant at a value for our F-statistics of
### Pr(>F) = 4.143e-16. Therefore, we REJECT our null hypothesis. The vif() is 
### satisfied for all as the predictors all have values less than 5. Overall,
### the summary agrees with my answer from Q2a.

# C) Interpret the y-intercept and all the partial slopes in your mcat MLR.
### Our intercept is the "No" responses from all our predictors.
### Y = B0 + B1*x1 + B2*x2 + B3*x3 + B4*x4 + B5*x5 -->

### Price = Intercept + P95andAboveNewYes*(x1) + FirstGrowthNewYes*(x2) +
###  CultWineNewYes*(x3) + PomerolNewYes*(x4) + VintageSuperstarNewYes*(x5) -->

### Price = 142.4  + 507.5*(x1) + 2170.6*(x2) + 4711.3*(x3) + 775.7*(x4) + 
###         1614.9*(x5)

### The slopes are as follows:
### P95andAboveNewYes --> 507.5
### FirstGrowthNewYes --> 2170.6
### CultWineNewYes --> 4711.3
### PomerolNewYes --> 775.7
### VintageSuperstarNewYes --> 1614.9


# D) Create 5C2 = 10 pairwise interaction plots. Which ones you think should be 
# added to your model mcat?

## P95andAboveNew & FirstGrowthNew
p1 <- ggplot() + aes(x = P95andAboveNew, y = Price, color = FirstGrowthNew,
               group = FirstGrowthNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p1 # Do NOT add

## P95andAboveNew & CultWineNew
p2 <- ggplot() + aes(x = P95andAboveNew, y = Price, color = CultWineNew,
               group = CultWineNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p2 # Do add

## P95andAboveNew & PomerolNew
p3 <- ggplot() + aes(x = P95andAboveNew, y = Price, color = PomerolNew,
               group = PomerolNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p3 # Do add

## P95andAboveNew & VintageSuperstarNew
p4 <- ggplot() + aes(x = P95andAboveNew, y = Price, color = VintageSuperstarNew,
               group = VintageSuperstarNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p4 # Do NOT add

## FirstGrowthNew & CultWineNew
p5 <- ggplot() + aes(x = FirstGrowthNew, y = Price, color = CultWineNew,
               group = CultWineNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p5 # Do add

## FirstGrowthNew & PomerolNew
p6 <- ggplot() + aes(x = FirstGrowthNew, y = Price, color = PomerolNew,
               group = PomerolNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p6 # Do NOT add

## FirstGrowthNew & VintageSuperstarNew
p7 <- ggplot() + aes(x = FirstGrowthNew, y = Price, color = VintageSuperstarNew,
               group = VintageSuperstarNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p7 # Do add

## CultWineNew & PomerolNew
p8 <- ggplot() + aes(x = CultWineNew, y = Price, color = PomerolNew,
               group = PomerolNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p8 # Do add

## CultWineNew & VintageSuperstarNew
p9 <- ggplot() + aes(x = CultWineNew, y = Price, color = VintageSuperstarNew,
                     group = VintageSuperstarNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p9 # Do NOT add

## PomerolNew & VintageSuperstarNew
p10 <- ggplot() + aes(x = PomerolNew, y = Price, color = VintageSuperstarNew,
               group = VintageSuperstarNew) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
p10 # Do NOT add

### I would add p5 and p8 to my model. They are FirstGrowthNew & CultWineNew (p5)
### and CultWineNew & PomerolNew (p8).

# E) Create a new MLR using the categorical predictors and the significant 
# pairwise interactions (call it mcat2). Check summary, anova, and diagnostics 
# of the model.
mcat2 <- lm(Price ~ P95andAboveNew + FirstGrowthNew + CultWineNew + 
              PomerolNew + VintageSuperstarNew +
              FirstGrowthNew:CultWineNew + 
              CultWineNew:PomerolNew)
summary(mcat2)
par(mfrow = c(2, 2))
plot(mcat2)
anova(mcat2)

### We can see the interaction effects are significant in our model, along with
### all our other predictors still. It also has a much higher R-squared of 0.9106.
### From the diagnostic plot, The Residuals vs. Fitted and Residuals vs. 
### Leverage have both their assumptions satisfied for assumption of linearity. 
### The Normal Q-Q and Scale-Location plots do NOT have their assumptions 
### satisfied though. From our anova F-test, we can see they all significant and
### REJECT our Null Hypothesis. 


# F) Conduct a Partial F-test between mcat and mcat2. What do you conclude?
anova(mcat, mcat2)

### We can conclude that our mcat2 model is better than our mcat model. It is
### significant when conducting the partial F-test between mcat and mcat2. 
### We would REJECT the Null Hypothesis. Therefore, I would choose the mcat2 
### model.



# Question (3):
# A) Create a MLR (call it m1) using the suggested transformation on the 
# numerical variables in the data along with the categorical predictors listed 
# in your MLR mcat (No interaction terms). A total of 7 predictors. Check 
# summary, anova, vif and diagnostics of the model.
m1 <- lm(log(Price) ~ log(ParkerPoints) + 
               log(CoatesPoints) + P95andAboveNew + FirstGrowthNew +
               CultWineNew + PomerolNew + VintageSuperstarNew, data = Bordeaux)
summary(m1)
par(mfrow = c(2, 2))
plot(m1)
anova(m1)

### From our newest model (m1), we can see from the summary that all the predictors
### are significant, except for P95andAboveNew. Our model has a R-squared of
### 0.9278 that is really good for fitting our model. From our diagnostic plots,
### all of our assumptions (linearity, normality, equal variance, leverages) are
### satisfied. From our anova test, we can see that all the F-tests on our predictors
### are significant, except for P95andAboveNew. Therefore, we would REJECT the 
### Null Hypothesis, except for P95andAboveNew.


# B) Create another MLR (call it mfull) using the suggested transformation on 
# the numerical variables in the data along with the categorical predictors 
# listed in your MLR mcat with the significant interaction terms). A total of 
# 7 predictors. Check summary, anova, and diagnostics of the model.
mfull <- lm(log(Price) ~ log(ParkerPoints) + 
              log(CoatesPoints) + P95andAboveNew + FirstGrowthNew +
              CultWineNew + PomerolNew + VintageSuperstarNew + 
              FirstGrowthNew:CultWineNew + 
              CultWineNew:PomerolNew,
            data = Bordeaux)
summary(mfull)
par(mfrow = c(2, 2))
plot(mfull)
anova(mfull)

### From our newest model (mfull), we can see from the summary that all the predictors
### are significant, except for P95andAboveNew and our interaction effects. 
### Our model has a R-squared of 0.9288 that is really good for fitting our 
### model. From our diagnostic plots, the Residual vs. Fitted plot and Residuals
### vs. Leverage plot satisfied the assumptions. Although, the Normal Q-Q is less
### normal, and the Scale-Location plot is less horizontal. From our anova test,
### we can see that all the F-tests on our predictors are significant, except 
### for P95andAboveNew and our interaction effects. Therefore, we would REJECT 
### the Null Hypothesis, except for P95andAboveNew and our interaction effects.


# C) Interpret the y-intercept and all the partial slopes in your mfull MLR.
summary(m1)
summary(mfull)
anova(m1, mfull)

## m1 model:
### Our intercept is the "No" responses from all our predictors.

### log(Price) = Intercept + log(ParkerPoints)*(x1) + log(CoatesPoints)*(x2) + 
###         P95andAboveNewYes*(x3) + FirstGrowthNewYes*(x4) +
###  CultWineNewYes*(x5) + PomerolNewYes*(x6) + VintageSuperstarNewYes*(x7) -->

### log(Price) = -51.14156  + 11.58862*(x1) + 1.62053 *(x2) + 0.10055*(x3) + 
###         0.86970*(x4) + 1.35317*(x5) + 0.53644*(x6) + 0.61590*(x7)

### The slopes are as follows:
### log(ParkerPoints) --> 11.58862
### log(CoatesPoints) --> 1.62053
### P95andAboveNewYes --> 0.10055
### FirstGrowthNewYes --> 0.86970
### CultWineNewYes --> 1.35317
### PomerolNewYes --> 0.53644
### VintageSuperstarNewYes --> 0.61590


## mfull model:
### Our intercept is the "No" responses from all our predictors.

### log(Price) = Intercept + log(ParkerPoints)*(x1) + log(CoatesPoints)*(x2) + 
###         P95andAboveNewYes*(x3) + FirstGrowthNewYes*(x4) +
###  CultWineNewYes*(x5) + PomerolNewYes*(x6) + VintageSuperstarNewYes*(x7)
###  FirstGrowthNewYes:CultWineNewYes*(x8) + CultWineNewYes:PomerolNewYes*(x9) 
###                          -->

### log(Price) = -50.94429  + 11.53989*(x1) + 1.63100 *(x2) + 0.09584*(x3) + 
###         0.87269*(x4) + 1.23195*(x5) + 0.50935*(x6) + 0.61583*(x7) +
###         0.10088*(x8) + 0.27747*(x9)

### The slopes are as follows:
### log(ParkerPoints) --> 11.53989
### log(CoatesPoints) --> 1.63100
### P95andAboveNewYes --> 0.09584
### FirstGrowthNewYes --> 0.87269
### CultWineNewYes --> 1.23195
### PomerolNewYes --> 0.50935
### VintageSuperstarNewYes --> 0.61583
### FirstGrowthNewYes:CultWineNewYes --> 0.10088   
### CultWineNewYes:PomerolNewYes --> 0.27747


# D) Which of the predictors need to be dropped from mfull?
### We should drop P95andAboveNewYes, FirstGrowthNewYes:CultWineNewYes, 
### and CultWineNewYes:PomerolNewYes variables from our model (mfull).


# E) Create a MLR (mred). Conduct partial F-test.
mred <- lm(log(Price) ~ log(ParkerPoints) + 
             log(CoatesPoints) + FirstGrowthNew +
             CultWineNew + PomerolNew + VintageSuperstarNew,
           data = Bordeaux)
anova(mred, mfull)

### We cannot conclude that our mred model is better than our mfull model. It is
### NOT significant when conducting the partial F-test between mred and mfull. 
### We would NOT REJECT the Null Hypothesis. Therefore, I cannot say that the full
### model is better than the reduced model.



# Question (4)
# A) State your final MLR based on your answers to the previous three questions.
summary(mred)
par(mfrow = c(2, 2))
plot(mred)
anova(mred)

### I would choose the reduce model (mred) as my final MLR. Not only does (mred)
### have diagnostic plots that satisfy all the assumptions, but all the predictors
### being used have significance. We have the highest R-squared of 92.72% of our
### model explained. It is also shown not to be overfitting nor is it underfitting
### for our model. Our F-test has a smaller p-value than 0.05, so we would REJECt
### our NUll Hypothesis. Therefore, our reduced model (mred) is my choice of MLR 
### to use.


# B) Interpret the y-intercept and all the partial slopes in your final MLR.
### The intercept tells us the "No" responses from all our predictors.
summary(mred)
### log(Price) = Intercept + log(ParkerPoints)*(x1) + log(CoatesPoints)*(x2) + 
###          FirstGrowthNewYes*(x3) + CultWineNewYes*(x4) + PomerolNewYes*(x5) +
###          VintageSuperstarNewYes*(x6) -->

### log(Price) = -56.47547  + 12.78432*(x1) + 1.60447 *(x2) + 0.86149*(x3) + 
###         1.33601*(x4) + 0.53619*(x5) + 0.59470*(x6)

### The slopes are as follows:
### log(ParkerPoints) --> 12.78432
### log(CoatesPoints) --> 1.60447
### FirstGrowthNewYes --> 0.86149
### CultWineNewYes --> 1.33601
### PomerolNewYes --> 0.53619
### VintageSuperstarNewYes --> 0.59470


# C) Identify the Unusually highly priced wines and the Unusually lowly priced 
# wines based on your final model.
leverages <- hatvalues(mred)

## Detects leverages:
which(leverages >= 2 * mean(leverages))
### Leverages are: 7, 8, 41, 53, 55, 59, 67, 68 

## Detects Outliers:
which(abs(rstandard(mred)) >= 2)
### Outliers are: 58, 61, 67


# D) Identify the wines that can be considered good leverage points in your 
# final MLR.

## Detect Bad leverages:
nrow(Bordeaux) # n = 72, p = 6
which(leverages >= 2 * mean(leverages) & abs(rstandard(mred)) >= 2)
which(leverages >= 2 * (6 + 1)/72 & abs(rstandard(mred)) >= 2)
### Bad Leverages is: 67

### Good Leverages are: 7, 8, 41, 53, 55, 59, 68