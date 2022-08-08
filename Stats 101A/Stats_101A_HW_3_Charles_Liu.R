# Stats 101A HW 3

library(readr)
NCD <- read_csv("UCLA Works/UCLA Winter 2020/Stats 101A/Homeworks/HW 3/NCBirthNew.csv")
attach(NCD)

# Problem 1
## 1a)
cor(NCD[, c("Birth Weight (g)", "AveCigs")], use = "pairwise.complete.obs", 4)

cormat <- round(cor(NCD[, c("Birth Weight (g)", "AveCigs", "Age of father", 
                            "Age of mother", "Visits", 'Wt Gain', "Gest Age")], 
                    use = "pairwise.complete.obs"),4)
cormat

## 1b)
library(car)
scatterplotMatrix( ~ `Birth Weight (g)` + AveCigs + `Age of father` + 
                     `Age of mother` + Visits + `Wt Gain` + `Gest Age`, 
                   data = NCD)
# Largest circle age of father and mother are highly correlated


## 1c) 

# visit, wt gain, gest




# Problem 2
## 2a)



# New predictors are significant since F is big and significant p value so we reject NULL
# this is for problem 4
# model 2 will always have the better r-sq and be the better model since more explanation
# significance so better to have 6 predictors
# improvement of r-sq is very small
# all new predictors are significant 6 of them. More significant than 3 significance
# More significant predictors lead to better fitted model