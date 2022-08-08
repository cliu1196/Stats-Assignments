n <- 5
a <- 1
b <- 9
x <- 0

# Q3a

qbeta(c(0.025, 0.975), a + x, b + n - x)

# Q3b

1 - pbeta(1/3, a + x, b + n - x)

