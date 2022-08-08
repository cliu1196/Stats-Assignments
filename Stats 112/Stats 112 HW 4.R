library(readr)
hw4 <- read_csv("UCLA Works/UCLA Fall 2019/Stats 112/Homeworks/HW 4/sex discrimination.csv")
View(sex_discrimination)
attach(hw4)



# QUESTION ONE
# 1)
hist(discrimination)
# 2)
summary(discrimination)
# 3)
discut<-cut(discrimination,br=c(0,35.29,58.82,100),lables=c("low","average","high"), right=FALSE)
# 4)
table(discut)
# 5)
table(discut, edur)
# 6)
m1 <- lm(leadership ~ discut + publicoffice)
# 7)
library(car)
residualPlots(m1)
summary(m1)
# 8,9,10 are explanation
library(car)
influencePlot(m1)
nrow(hw4)
4/2250



# QUESTION TWO
# 1,2 are explanation
m2 <- lm(leadership ~ discut * edur)
summary(m2)
# 3
table(discut, edur)
residualPlots(m2)
table(discut)
table(edur)
install.packages("effects")
# 4)
edur <- factor(edur)
discut <- factor(discut)
library(effects)
plot(allEffects(m2),ask=FALSE)
# 5 explanation
# 6)
1-((1-0.0384)*(808-1))/(808-5-1)



# QUESTION THREE
library(readr)
satactgpa <- read_csv("UCLA Works/UCLA Fall 2019/Stats 112/Homeworks/HW 4/satactgpa.csv")
View(satactgpa)
# 1)
attach(satactgpa)
m3 <- lm(ACT ~ SATV + SATM + GPA)
library(car)
vif(m3)
m4 <- lm(SATV ~ ACT + SATM + GPA)
vif(m4)



# QUESTION FOUR
# 1)
