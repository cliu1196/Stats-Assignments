library(readr)
hw1d <- read_csv("UCLA Works/UCLA Fall 2019/Stats 112/R Codes/Raw Data/lgbt-2.csv")
View(lgbt_2)


hw1d$ethnicity
is.numeric(hw1d$ethnicity)
nrow(hw1d)
ncol(hw1d)
hw1d[2, ]
library(ggplot2)
pie(hw1d$ethnicity)
na.omit(hw1d$ethnicity)
is.integer(hw1d$ethnicity)

str(hw1d)
View(hw1d$ethnicity)

a <- na.omit(hw1d$ethnicity)
barplot(table(a))

barplot(table(x = na.omit(hw1d$ethnicity)))


b <- na.omit(hw1d$lgbtacceptp)
mean(b)
median(b)
sd(b)
var(b)

hist(b)
plot(x = table(b), y = table(a))



library(ggplot2)
g <- ggplot(hw1d)
g + geom_bar(aes(x = lgbtacceptp, fill = (educationr)), position = "fill") + 
  scale_y_continuous(labels = scales::percent_format())

# EX:
prop.table(table(hw1d$gender, hw1d$difficultmom),1)

qqnorm(hw1d$lgbtacceptp)
qqline(hw1d$lgbtacceptp)


qnorm(0.8)
