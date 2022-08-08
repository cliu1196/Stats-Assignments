library(readr)
campusclimate <- read_csv("Stats 112/R Codes/Raw Data/campusclimate.csv")
hw2d <- read_csv("Stats 112/R Codes/Raw Data/campusclimate.csv")
a <- na.omit(hw2d$friendlyenvp)
b <- na.omit(hw2d$academicsp)
a <- (hw2d$friendlyenvp)
b <- (hw2d$academicsp)

mean(a)
sd(a)

mean(b)
sd(b)

zadata = (a - mean(a))/sd(a)
zbdata = (b - mean(b))/sd(b)
plot(zadata, zbdata)
plot(scale(a), scale(b))

cov(a, b)
cor(a, b, use = "complete.obs")
cor(b, a)

c = cov(a,b)/(sd(a)*sd(b))
c




cor(x=scale(friendlyenvp,center = T,scale = T),
    y=scale(academicenvp,center = T,scale = T),use = "complete.obs")
cor.test(x=scale(friendlyenvp,center = T,scale = T),
    y=scale(academicenvp,center = T,scale = T),use = "complete.obs")

cor.test(x=friendlyenvp,y=academicenvp)



mydata <- hw2d[, c("academicenvp","friendlyenvp","exclusionaryp","diversepersepectivesp")]

cor(mydata ,use="complete.obs")
plot(mydata)
