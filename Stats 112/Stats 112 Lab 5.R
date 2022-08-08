---
  title: "MLR/OR Tutorial"
author: "Donatello Telesca"
date: "December 5, 2018"
output: html_document
---
  
  ## Problem 1
  Suppose you have a data-set with five predictors, $X_1$ = GPA, $X_2$ = IQ, $X_3$ = Gender (1 Female, 0 Male), $X_4$ = Interaction between GPA and IQ, and $X_5$ = Interaction between GPA and Gender. The response of interest is starting salary after graduation (in thousands of dollars). Suppose we use the least square to fit the model and obtain

|Predictor | Coefficient |
  |---:|:---:|
  | Intercept | $\hat{\beta}_0$ = 50 |
  | GPA |  $\hat{\beta}_1$ = 20 |
  | IQ |  $\hat{\beta}_2$ = 0.07|
  | Gender |  $\hat{\beta}_3$ = 35|
  | GPA $\times$ IQ|  $\hat{\beta}_4$ = 0.01|
  | GPA $\times$ Gender|  $\hat{\beta}_5$ = -10|
  
  
  
  #####(a)
  Which answer is correct, and why?
  
  i) For a fixed value of IQ and GPA, males earn more on average than females.[FALSE]

ii) For a fixed value of IQ and GPA, females earn more on average than males.[FALSE]

iii) For a fixed value of IQ and GPA, males earn more on average than females provideed that the GPA is high enough. [TRUE]

iv)  For a fixed value of IQ and GPA, females earn more on average than males provided that the GPA is high enough.[FALSE]


\newpage

#####(b) 
Predict the salary of a female with IQ of 110 and GPA of 4.0.

$$50 + 35 + 4.0\times(20-10) + 110\times 0.07 + 4.0\times110\times0.01 $$
  
  #####(c) 
  True or false: Since the coefficient associated with the interaction term IQ $\times$ GPA is very small, there is little
evidence of an interaction effect. Justify your answer.

*The mangnitude of coefficients can only be assessed in relation to the magnitude of the predictor. A better summary to assess the need for interactions would be the T-value or the p-value.*
  
  
  
  ## Problem 2
  For 97 countries in the world, data are given for birth rates, death
rates, infant death rates, life expectancies for males and females, and
Gross National Product. We will focus on **infant death rates**, as the main outcome (dependent variable), as
it is often considered a good  indicator of economic and social welfare. 

You may load data into R following the following code
```{r}
fileurl <- "https://www.dropbox.com/s/r1f39apm780obei/poverty.dat.txt?dl=1"
dat <- read.table(fileurl, header=T, na.strings = '*')
```


$\;$
  
  VARIABLE DESCRIPTIONS:
  
  | Name           | Description   |
  |---------------:|:--------------|
  |LiveBirthRate   |   Live birth rate per 1,000 of population|
  |DeathRate       |   Death rate per 1,000 of population|
  |InfantDeathRate |   Infant deaths per 1,000 of population under 1 year old|
  |LifeExpM |   Life expectancy at birth for males|
  |LifeEmpF|   Life expectancy at birth for females|
  | GDP |  Gross National Product per capita in U.S. dollars |
  |CountryGroup   |Country Group|
  ||          1 = Eastern Europe|
  ||          2 = South America and Mexico|
  ||          3 = Western Europe, North America, Japan, Australia, New Zealand|
  ||          4 = Middle East|
  ||          5 = Asia|
  |       |  6 = Africa|
  |County |   Country|
  
  
  For simplicity, remove all data related to missing GDP values.

```{r}
dat <- subset(dat, !is.na(dat$GDP))
```

#####(a)
For each predictor, fit a simple linear regression model to predict the response. Describe your results and create some plots to back up your assertions.  


\newpage
#####(b)
Consider assessing how Infant Death relates to country groupings. Fit a linear regression on CountryGroup and interpret the coefficients.

Note you will need to tell R that you are working with a factorial predictor.
```{r}
library(knitr)
dat$CountryGroup <- factor(dat$CountryGroup)
#
model1 <- lm(InfantDeathRate ~ CountryGroup, data=dat)
kable(summary(model1)$coef, digits=3)
# 
# Original definition of baseline ------------------------
subset(dat, CountryGroup==1)[,6:8] 
#
# Change baseline group to OECD group (group 3) ----------
#
dat$CountryGroup <- relevel(dat$CountryGroup, ref=3)
model2 <- lm(InfantDeathRate ~ CountryGroup, data=dat)
kable(summary(model2)$coef, digits=3)
#
# See the function contrasts for more ------------------
```






\newpage
#####(c)
For the model in (b) assess the assumptions needed for valid regression analysis, by producing simple plots. Are there any obvious violations?
  Motivate your answer by referring to a specific graphical summary and propose a possible solution.

```{r fig1, fig.width=10, fig.height=4, message=FALSE,include=TRUE, echo=F, fig.cap='Diagnostics on the original scale (no./1,000)'}
par(mfrow=c(1,2))
plot(model1$fitted.values, abs(model1$residuals))
lines(lowess(model1$fitted.values, abs(model1$residuals)))
qqnorm(model1$residuals)
qqline(model1$residuals)
```


```{r fig3, fig.width=10, fig.height=4, message=FALSE,include=TRUE, echo=F,fig.cap='Residual Diagnostics on the Log Scale'}
par(mfrow=c(1,2))
dat$y  <- log(dat$InfantDeathRate)
model3 <- lm(dat$y ~ dat$CountryGroup)
plot(model3$fitted.values, abs(model3$residuals))
lines(lowess(model3$fitted.values, abs(model3$residuals)))
qqnorm(model3$residuals)
qqline(model3$residuals)
```


*Figure 1, shows that the data may exhibit some heteroschedasticity. A log or square root transformation may provide the necessary correction.*
  
  \newpage
#####(d)
Consider a model that predicts infant death rates using both Male and Female life expectancy. Fit a linear model and summarize your results.
How are these finding reconciled with your analysis in (a)? How would you suggest solving this problem?
  
  ```{r}
# Mother's life expectancy
fit1 <- lm(dat$InfantDeathRate ~ dat$LifeExpM)
kable(summary(fit1)$coef, digit=3)

# Father's life expectancy
fit2 <- lm(dat$InfantDeathRate ~ dat$LifeEmpF)
kable(summary(fit2)$coef, digit=3)

# Mother and Father's life expectancy
fit3 <- lm(dat$InfantDeathRate ~ dat$LifeExpM + dat$LifeEmpF)
kable(summary(fit3)$coef, digit=3)
```

```{r fig2, fig.width=4, fig.height=4, message=FALSE,include=TRUE}
plot(dat$LifeExpM, dat$LifeEmpF)
```

\newpage
#####(e)
Consider a model that predicts infant deaths rates using CountryGroup and GDP as main effects. Interpret the coefficients associated with this multiple regression.
Is this model tenable from a diagnostic perspective? What linear regression assumptions is clearly violated?
  
  ```{r}
model4 <- lm(dat$InfantDeathRate ~ dat$CountryGroup + dat$GDP)
kable(summary(model4)$coef, digits=3)
```

```{r fig4, fig.width=10, fig.height=5, message=FALSE, include=TRUE, fig.cap='GDP and Infant Death Rates Diagnostics'}
par(mfrow=c(1,2))
plot(dat$GDP, model4$residuals)
lines(lowess(dat$GDP, model4$residuals))
abline(h=0)
plot(dat$GDP, dat$InfantDeathRate)
lines(lowess(dat$GDP, dat$InfantDeathRate))
```


**
  
  
  \newpage
#####(f)
In the model of (e), verify if a polynomial regression can fix things in this case using the appropriate graphical summary. 
Can you think of a transformation of GDP that would make the assumption of linearity more tenable? Verify your proposal with the appropriate graphical summary.


```{r fig5, fig.width=10, fig.height=5, message=FALSE, include=TRUE, fig.cap='GDP and Infant Death Rates Diagnostics', echo=F}
par(mfrow=c(1,2))
plot(dat$GDP, model4$residuals)
lines(lowess(dat$GDP, model4$residuals))
abline(h=0)
plot(dat$GDP, dat$InfantDeathRate)
lines(lowess(dat$GDP, dat$InfantDeathRate))
```

```{r fig6, fig.width=10, fig.height=5, message=FALSE, include=TRUE, fig.cap='GDP and Infant Death Rates Diagnostics - Using Log GDP', echo=F}
par(mfrow=c(1,2))
dat$lGDP <- log(dat$GDP)
model5 <- lm(dat$InfantDeathRate ~ dat$lGDP + dat$CountryGroup)
plot(dat$lGDP, model5$residuals)
lines(lowess(dat$lGDP, model5$residuals))
abline(h=0)
plot(dat$lGDP, dat$InfantDeathRate)
lines(lowess(dat$lGDP, dat$InfantDeathRate))
```


\newpage
```{r}
kable(summary(model4)$coef, digits=3)
kable(summary(model5)$coef, digits=3)
```




\newpage
## Problem 3
In this excercise, you will use simulated data to understant the performance of different model selection procedures.
Generate a predictor $X$ of length $n=100$ from a N(0,1), an error vector $\epsilon$, and define $Y$ as:
  
  $$Y = \beta_0 +\beta_1 X + \beta_2 X^2 + \beta_3 X^3 + \epsilon$$
  
  
  
  
  You may follow the code below to generate your artificial data frame **dat**:
  ```{r}
# Set random seed ... to be avoided in real simulations, 
# but ok here in order to get replicable results
set.seed(1234)
#
# Generate model truth
#
x       <- rnorm(100,0,1)                                # random draw from normal 
epsilon <- rnorm(100,0,1)                                # random draw from normal
B       <- c(2,0.9,2,-1.5)                               # fixed arbitrary coefficients
y       <- B[1] + B[2]*x + B[3]*x^2 + B[4]*x^3 + epsilon # Simulated Data
#
# Generate a data-set with potentially spurious covariates
#
dat <- data.frame(y=y, x1=x, x2=x^2, x3=x^3, x4=x^4, x5=x^5, x6=x^6, x7=x^7, x8=x^8)
```


####(a)
Install the R library **leaps** and perform best subset selection, using the function **regsubsets()**, in order to choose the best model containing the predictors
$X, X^2, X^3, \ldots, X^8$. What is the best model obtained according to $C_p$, AIC, and adjusted $R^2$? Show some plots to provide evidence of your answer, and report the coefficients of the best model obtained.


```{r}
require(leaps)
y <- y
x <- as.matrix(dat[,2:9])
sub1 <- leaps(x , y, method='Cp')
sub2 <- regsubsets(y~., data=dat)
#
par(mfrow=c(1,2))
plot(sub2, scale='Cp')
plot(sub1$size, log(sub1$Cp), pch=19, cex=1.5, xlab='Model size', ylab='log(Cp)')
```



####(b) 
Repeat (a), using forward stepwise selection and also using backward stepwise selection. How does your answer compare to the results in (a)?
  
  ```{r}
sub3 <- regsubsets(y~., data=dat, method='forward')
sub4 <- regsubsets(y~., data=dat, method='backward')
par(mfrow=c(1,2))
plot(sub3, scale='Cp')
plot(sub4, scale='Cp')
```

## Problem 4
A study aims to relate pelvinc inflammatory disease to smoking. We observe:
  
  
  | | Disease | No Disease|
  |--:|:--:|:--:|
  |Smoker     | 77 | 123 |
  |Non Smoker | 54 | 171 |
  
  
  #####(a)
  i) Calculate the probabilities of disease in the Smoker and Non-Smoker groups.

P(Disease|Smoker) = $\frac{77}{200}$ = `r 77/(200)`

P(Disease|Nonsmoker) = $\frac{54}{171}$ = `r 54/225` 

ii) Calculate a Risk Ration of disease of Smokers Vs. Non-Smokers.

RR = $\frac{P(Disease|Smoker)}{P(Disease|Nonsmoker)}$ = `r (77*225)/(54*200)`

iii) Calculate the Odds of Disease in the Smoker and Non-Smoker groups.

oddsSmoker = $\frac{P(Disease|Smoker)}{P(No Disease|Smoker)}$ = `r (77/200)/(1-(77/200))`

OddsNonSmoker = $\frac{P(Disease|NonSmoker)}{P(No Disease|NonSmoker)}$ = `r (54/225)/(1-(54/225))`


iv) Calculate the Odds Ratio of Disease of Smokers Vs. Non-Smokers.

OR = $\frac{OddsSmoker}{OddsNonSmoker}$ = `r ((77/200)/(1-(77/200)))/((54/225)/(1-(54/225)))`