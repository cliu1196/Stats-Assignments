# Stats 101A HW 2
library(readr)
test <- read.csv("~/UCLA Works/UCLA Winter 2020/Stats 101A/Homeworks/Kaggle 1st Try/FifaNoY.csv")
attach(test)
colnames(test)

m1 <- lm(`Skill Moves` ~ Age + `International Reputation` + `Work Rate` + Positioning, 
         data = test)
summary(m1)


Nationalitynew <- as.integer(Nationality)
WageNew <- predict(m1, test)
ifelse(is.na(WageNew), mean(WageNew), WageNew)
which(is.na(WageNew))


Ob <- c(1:5462)
sol <- cbind(Ob,WageNew) 
write.csv(sol,'sol.csv',row.names = FALSE)
