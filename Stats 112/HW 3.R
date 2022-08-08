library(readr)
A <- read_csv("UCLA Works/UCLA Fall 2019/Stats 112/R Codes/Raw Data/campusclimate.csv")

# To View the Dataset
View(campusclimate)

# Replacing our Variables and renaming them
# B is our predictor(X-axis) and C is our outcome(Y-axis)
B <- A$friendlyenvp
C <- A$academicenvp

# Create our linear model with scatterplot
m1 <- lm(C ~ B)
plot(m1)
summary(m1)


# Creating our subset of A
a <- subset(A, academicenvp > 10 & friendlyenvp > 40,
                          select = c(academicenvp, friendlyenvp))

# Replacing our Variables and renaming them
# b is our predictor(X-axis) and b is our outcome(Y-axis)
b <- a$friendlyenvp
c <- a$academicenvp

# Create our linear model with scatterplot
m2 <- lm(c ~ b)
plot(m2)
summary(m2)

# Comparing our m1 and m2 (just the coefficients)
summary(m1)$coefficients
summary(m2)$coefficients

# Comparing our m1 and m2 (Full)
summary(m1)
summary(m2)

# Plotting both m1 and m2
plot(m1)
plot(m2)


# Interpretting our Subset to see if our X-axis Min is zero
summary(m2)$coefficients
summary(b)

