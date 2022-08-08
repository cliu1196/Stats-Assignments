# Stats 101A HW 1
library(readr)
library(ggplot2)
NCBirth <- read_csv("UCLA Works/UCLA Winter 2020/Stats 101A/Homeworks/HW 1/NCBirthNew.csv")
attach(NCBirth)


# Problem 1
## 1a)
Q1a <- ggplot(NCBirth, aes(`Birth Weight (g)`)) + geom_histogram(na.rm = FALSE, binwidth = 500, 
                                                          color = 'black', fill = 'green')
Q1a

t.test(`Birth Weight (g)`, mu = 4300)

## 1b)
Gender_new <- ifelse(`Gender of child` == 1, 'Male', 'Female')
NCBirth <- cbind(NCBirth, Gender_new)

Q1b <- ggplot(NCBirth, aes(Gender_new)) + geom_bar(width = 0.5) + xlab('Genders')
Q1b

## 1c)
binom.test(table(Gender_new), p = 0.5, conf.level = 0.95)
### We REJECT the NULL hypothesis.

## 1d)
binom.test(table(Gender_new), p = 0.5, conf.level = 0.90)
### We REJECT the NULL hypothesis.


# Problem 2
Mom_Tran <- ifelse(MomTran == 1, 'Tran', 'Non-Tran')
NCBirth <- cbind(NCBirth, Mom_Tran)

## 2a)
ggplot(NCBirth, aes(x = Mom_Tran, y = `Birth Weight (g)`)) + geom_boxplot(na.rm = FALSE) + 
  xlab('Mom Transferred')

## 2b)
t.test(`Birth Weight (g)` ~ Mom_Tran, var.equal = TRUE)
### p-value = < 2.2e-16

## 2c)
Q2c_SLR <- lm(`Birth Weight (g)` ~ `Gest Age (BC)`, data = NCBirth)
Q2c_SLR

## 2d)
Q2d <- ggplot(NCBirth, aes(x = `Gest Age (BC)`, y = `Birth Weight (g)`)) + geom_point() + 
  geom_smooth(na.rm = FALSE, method = 'lm') + xlab('Gestational Age (Birth Certificate)')

Q2d

## 2e)
summary(Q2c_SLR)
### For every increase in Gest Age (BC), we see, ON AVERAGE, a 182.880 units increase in
### the Birth Weight (g).

### For the y-intercept, we can see that Age is about 17 years old and -3770.069 for 
### Weight. This means that the initial Gest. Age (BC) is that -2284.0 g is the 
### minimum weight tested.

## 2f)
confint(Q2c_SLR)

## 2g)
anova(Q2c_SLR)
### SSE = 1971863947, SSR = 1961149716, SST = 3933013663


# Problem 3
## 3a)
Gest_Age_df_Q3a <- data.frame(`Gest Age (BC)`= c(20))
predict(Q2c_SLR, data = NCBirth, Gest_Age_df_Q3a, interval = 'confidence')

## 3b)
predict(Q2c_SLR, data = NCBirth, Gest_Age_df_Q3a, interval = 'prediction')

## 3c)
### The prediction interval is always wider than confidence interval. The difference is
### that the prediction interval predicts something yet to be observed, which makes it
### have a wider interval. Confidence interval is based on something known, therefore
### the interval is smaller and more accurate.


# Problem 4
## 4a)
Q4a_SLR <- lm(`Birth Weight (g)` ~ Mom_Tran, data = NCBirth)
Q4a_SLR

## 4b)
Q4b <- ggplot(NCBirth, aes(x = Mom_Tran, y = `Birth Weight (g)`)) + geom_point() + 
  geom_smooth(na.rm = FALSE, method = 'lm') + xlab('Mom Transferred')

Q4b

## 4c)
summary(Q4a_SLR)
### We can't really predict anything from this Linear Model, since we are comparing two
### Variables that are different. We are coming categorical vs. numerical values in our
### linear model. Numerically for the summary, we can say something, but the plot does
### NOT support the claims made in the summary. Therefore, we are unable to draw any
### definite conclusions. We cannot interpret the slope, since there is none, and the
### y-intercept, since it is categorical vs. numerical.

## 4d)
summary(Q4a_SLR)
t.test(`Birth Weight (g)` ~ Mom_Tran, var.equal = TRUE)
### Both have a p-value = < 2.2e-16. We can conclude that we REJECT the NULL hypothesis.


# Problem 5
index <- which(is.na(`Birth Weight (g)`) | is.na(`Gest Age (BC)`))
NC_Birth_Clean <- NCBirth[-index,]

## 5a)
x <- NC_Birth_Clean$`Gest Age (BC)`
y <- NC_Birth_Clean$`Birth Weight (g)`
n <- length(NC_Birth_Clean$`Birth Weight (g)`)

Sxx <- sum(x^2) - sum(x)^2 / n
Syy <- sum(y^2) - sum(y)^2 / n
Sxy <- sum(x * y) - (sum(x) * sum(y)) / n

Sxx
Syy
Sxy
  
## 5b)
cov(`Birth Weight (g)`, `Gest Age (BC)` , use = 'complete.obs')

## 5c)
cor(`Birth Weight (g)`, `Gest Age (BC)` , use = 'complete.obs')

## 5d)
Q5d_SLR <- lm(`Birth Weight (g)` ~ `Gest Age (BC)`, data = NCBirth)
summary(Q5d_SLR)
### For the y-intercept, it is -3770.069 units. The slope is 182.880 units.

## 5e)
Gest_Age_df_Q5e <- data.frame(`Gest Age (BC)`= c(40))
predict(Q5d_SLR, Gest_Age_df_Q5e, interval = 'confidence')

