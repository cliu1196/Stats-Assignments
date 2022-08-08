library(readr)
campusclimate <- read_csv("UCLA Works/UCLA Fall 2019/Stats 112/Fall 19 Checking for understanding folder, what plot to use, normal model, campus climate data, UN d/campusclimate.csv")
View(campusclimate)

library(readr)
un <- read_csv("UCLA Works/UCLA Fall 2019/Stats 112/Fall 19 Checking for understanding folder, what plot to use, normal model, campus climate data, UN d/un.csv")
View(un)

pnorm(+1)
# Gives probability of standard normal distribution and gives the z-score of +1
# Like saying on the table/graph Mu + Var()
pnorm(+2)
# Like saying Mu + 2Var()

qnorm(0.95)
# What score do I need to get for my (X) under normal standard score?
# this gives the actual z-score; 1.645 = (X - M)/SD()
attach(campusclimate)
hist(friendlyenvp)
hist(campusclimate$friendlyenvp)
qqnorm(friendlyenvp)
# first step is to check how your data looks like; if it was right, then it'd be straight line

# Tier 1 People:
# Q2 with the UN Data
attach(un)
summary(infantMortality)
summary(un$infantMortality)

mean(infantMortality,na.rm=T)
sd(infantMortality,na.rm=T)
pnorm(0.6843421)
# No, I wouldn't hire this person because of the M = 44 mistakes
1 - pnorm(0.684321)
# It's just because they have a 25% of having infants surviving.
