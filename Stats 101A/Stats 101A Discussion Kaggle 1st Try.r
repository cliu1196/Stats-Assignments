#Put into an Rmd
#load data (train data)
data <- read.csv(FifaTrainNew.csv)
test <- read_csv(FifaNoY.csv)

#don't need to regress on all the columns 
lm(WageNew ~..., data = data) #choose any meaningful columns of your choice 

#choose variables to establish your regression 
model1 <- lm(WageNew ~ Age + International.Reputation + Work.Rate + Position, data = data)
summary(model1)

#recode categorical variable as integers (Dummy variables) using factor()
#if categorical has n levels, you need (n-1) Dummy variables 
#predict function 
wageNew <- predict(model1, test)
ifelse(is.na(wageNew), mean(train$wageNew), wageNew)

which(is.na(wageNew))

#get an output, two columns 
Ob <- c(1:5462)
sol <- cbind(Ob,wageNew) #write.csv accepts data.frames  

write.csv(sol,'sol.csv',row.names = FALSE)

#if prediction has missing data (NAs), you need to handle NAs 
#it is popular to replace missing data by average (can use if else function)

#learn to be comfortable with kaggle 
#learn to train a linear model by yourself 
#learn how to output a file 

#it's suggested by professor to use write.csv()
#also suggested to use write.table() ... more popular and generates output 