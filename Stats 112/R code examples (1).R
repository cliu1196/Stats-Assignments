#examples of code to use to plot
#my Data is stored as lgbt_2 just change these to whatever you want to name the data
#I will be using the word data frame to refer how the data is store. 
#Summary gives you a quick look into your data and what type of variable it is
summary(lgbt_2)

h<-c(2,3,4)
mean(c(2,34,5))
#attach allows you to access the columns of your data frame(where the data is stored) 
#without having to do something like lgbt_$obama
attach(lgbt_2)
lgbt_2$obama
obama
#You can check to see if they are the same
#class() lets you see if your variable is numerical, categorical, or other. 
class(obama)
#ggplot2 make sure to install the package if you are tyring to use ggplot and call the library 
library(ggplot2)
#the below line makes a ggplot object where data= tells it which data frame you will be looking at

#looking at the data there are missing values. To remove these missing values when we do calculations
#for example mean do mean(na.omit)

mean(na.omit(lgbtsupport6))
#standard deviation is sd(), variance is var(), median is median()

median(na.omit(lgbtsupport6))

#Contingency tables
# the ,1 gets you the row percentages or only looking at females and males independently
prop.table(table(gender,difficultmom),1)

#graphing using ggplot 
#this line creates a ggplot object and data = lgbt_2 says that the data will be coming from the 
#lgbt_2 data frame


#geom_brar creates a barplot and aes(lets you add features or specify what you are graphing,(what
#is your explanatory and response variable) and lets you add collors.
#g+geom_bar(aes(x=education),fill=lgbt_2$lgbtsocialacceptance)+scale_y_continuous(labels = scales::percent_format()
                                                                    #for 
#model<-lm(~)

#setwd()
#lgbt_2$
  
#qqnorm(lgbt_2$lgbtacceptp)
#qqline(lgbt_2$lgbtacceptp)


qnorm(.8)


#segmented bar chart
g<- ggplot(data=lgbt_2)

g+geom_bar(aes(x=educationr,fill=(lgbtsocialacceptance)),position="fill" )+ 
  scale_y_continuous(labels=scales::percent_format())
