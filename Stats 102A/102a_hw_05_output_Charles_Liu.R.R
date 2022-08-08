# Stats 102A HW 5
library(ggplot2)

# Problem 1
## 1a)
# Formula is [(2^(n-1)) - 1]; for this case n = 3
# We are going to use 7 as the bias. The largest positive exponent is 7. 
# The most negative exponent is -6.

## 1b)
# 0 1001 01100


## 1c)
# 1.0


## 1d)
# 1.03125 or 1 + 1/32


## 1e)
# 0 0001 00000
# 1/64 or 0.015625


## 1f)
# 0 0000 00001
# 1/2048 or 0.00048828


## 1g)
# 0 0000 11111
# 31/2048 or 0.01513672


## 1h)
# 0 1110 11111
# 252.0


## 1i)
# 1/32 is our machince epsilon.
# The 10-bits only counts 1/2, 1/4, 1/8, 1/16, and 1/32 on the decimals because we are
# using 5-bits for the mantissa.


## 1j)
# 1/16
# 0.0625


## 1k)
# 1/8
# 0.125



# Problem 2
fixedpoint_show <- function(ftn, x0, iter = 5){
  # applies fixed-point method to find x such that ftn(x) = x
  # ftn is a user-defined function
  # df_points_1 and df_points_2 are used to track each update
  # it will be used to plot the line segments showing each update
  # each line segment connects the points (x1, y1) to (x2, y2)
  df_points_1 <- data.frame(
    x1 = numeric(0),
    y1 = numeric(0),
    x2 = numeric(0),
    y2 = numeric(0))
  df_points_2 <- df_points_1
  xnew <- x0
  cat("Starting value is:", xnew, "\n")
  # iterate the fixed point algorithm
  for (i in 1:iter) {
    xold <- xnew
    xnew <- ftn(xold)
    cat("Next value of x is:", xnew, "\n")
    # vertical line segments, where x1 = x2
    df_points_1[i, ] <- c(x1 = xold, y1 = xold, x2 = xold, y2 = xnew)
    # horizontal line segments, where y1 = y2
    df_points_2[i, ] <- c(x1 = xold, y1 = xnew, x2 = xnew, y2 = xnew)
  }
  # use ggplot to plot the function and the segments for each iteration
  # determine the limits to use for the plot
  # start is the min of these values. we subtract .1 to provide a small margin
  plot_start <- min(df_points_1$x1, df_points_1$x2, x0) - 0.1
  # end is the max of these values
  plot_end <- max(df_points_1$x1, df_points_1$x2, x0) + 0.1
  # calculate the value of the funtion fx for all x
  x <- seq(plot_start, plot_end, length.out = 200)
  fx <- rep(NA, length(x))
  for (i in seq_along(x)) {
    fx[i] <- ftn(x[i])
  }
  function_data <- data.frame(x, fx) # data frame containing the function values
  p <- ggplot(function_data, aes(x = x, y = fx)) +
    geom_line(color = "royalblue", size = 1) + # plot the function
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_1, color = "black", lty = 1) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_2, color = "red", lty = 2) +
    geom_abline(intercept = 0, slope = 1) + # plot the line y = x
    coord_equal() + theme_bw()
  print(p) # produce the plot
  xnew # value that gets returned
}

## 2a)
### x0 = 1
a2_1 <- function(x) cos(x)
fixedpoint_show(a2_1, 1, iter = 10)

### x0 = 3
a2_2 <- function(x) cos(x)
fixedpoint_show(a2_2, 3, iter = 10)

### x0 = 6
a2_3 <- function(x) cos(x)
fixedpoint_show(a2_3, 6, iter = 10)


## 2b)
### x0 = 2
b2_1 <- function(x) exp(exp(-x))
fixedpoint_show(b2_1, 2, iter = 10)


## 2c)
### x0 = 2
c2_1 <- function(x) x - log(x) + exp(-x)
fixedpoint_show(c2_1, 2, iter = 10)


## 2d)
### x0 = 2
d2_1 <- function(x) x + log(x) - exp(-x)
fixedpoint_show(d2_1, 2, iter = 6)



# Problem 3
newtonraphson_show <- function(ftn, x0, iter = 5) {
  # applies Newton-Raphson to find x such that ftn(x)[1] == 0
  # ftn is a function of x. it returns two values, f(x) and f'(x)
  # x0 is the starting point
  # df_points_1 and df_points_2 are used to track each update
  df_points_1 <- data.frame(
    x1 = numeric(0),
    y1 = numeric(0),
    x2 = numeric(0),
    y2 = numeric(0))
  df_points_2 <- df_points_1
  xnew <- x0
  cat("Starting value is:", xnew, "\n")
  # the algorithm
  for(i in 1:iter){
    xold <- xnew
    f_xold <- ftn(xold)
    xnew <- xold - f_xold[1]/f_xold[2]
    cat("Next x value:", xnew, "\n")
    # the line segments. You will need to replace the NAs with the appropriate values
    df_points_1[i, ] <- c(xold, ftn(xold)[1], xold, 0) # vertical segment
    df_points_2[i, ] <- c(xold, ftn(xold)[1], xnew, 0) # tangent segment
  }
  plot_start <- min(df_points_1$x1, df_points_1$x2) - 0.1
  plot_end <- max(df_points_2$x1, df_points_2$x2) + 0.1
  x <- seq(plot_start, plot_end, length.out = 200)
  fx <- rep(NA, length(x))
  for (i in seq_along(x)) {
    fx[i] <- ftn(x[i])[1]
  }
  function_data <- data.frame(x, fx)
  p <- ggplot(function_data, aes(x = x, y = fx)) +
    geom_line(color = "royalblue", size = 1) + # plot the function
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_1, color = "black", lty = 1) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_2, color = "red", lty = 2) +
    geom_abline(intercept = 0, slope = 1) # plot the line y = x
  print(p)
  xnew # value that gets returned
}

## 3extra)
f3 <- function(x) {
  value <- x^2 - 4 # f(x)
  derivative <- 2*x # f'(x)
  return(c(value, derivative))
}

### x0 = 10
newtonraphson_show(f3, 10, iter = 8)

## 3a)
# example of how your functions could be written
a3_1 <- function(x) {
  value <- cos(x) - x # f(x)
  derivative <- -sin(x) - 1 # f'(x)
  return(c(value, derivative))
}

### x0 = 1
newtonraphson_show(a3_1, 1, iter = 8)

### x0 = 3
newtonraphson_show(a3_1, 3, iter = 8)

### x0 = 6
newtonraphson_show(a3_1, 6, iter = 8)


## 3b)
b3_1 <- function(x) {
  value <- log(x) - exp(-x)
  derivative <- 1/x + exp(-x)
  return(c(value, derivative))
}

### x0 = 2
newtonraphson_show(b3_1, 2, iter = 8)


## 3c)
c3_1 <- function(x) {
  value <- x^3 - x - 3
  derivative <- 3*x^2 - 1
  return(c(value, derivative))
}

### x0 = 0
newtonraphson_show(c3_1, 0, iter = 8)


## 3d)
d3_1 <- function(x) {
  value <- x^3 - 7*x^2 + 14*x - 8
  derivative <- 3*x^2 - 14*x + 14
  return(c(value, derivative))
}

### x0 = 1.1
newtonraphson_show(d3_1, 1.1, iter = 8)

### x0 = 1.3
newtonraphson_show(d3_1, 1.3, iter = 8)

### x0 = 1.4
newtonraphson_show(d3_1, 1.4, iter = 8)

### x0 = 1.5
newtonraphson_show(d3_1, 1.5, iter = 8)

### x0 = 1.6
newtonraphson_show(d3_1, 1.6, iter = 8)

### x0 = 1.7
newtonraphson_show(d3_1, 1.7, iter = 8)



# Problem 4
secant_show <- function(ftn, x0, x1, iter = 5) {
  df_points_1 <- data.frame(
    x1 = numeric(0),
    y1 = numeric(0),
    x2 = numeric(0),
    y2 = numeric(0))
  df_points_2 <- df_points_1
  cat("Starting values are:", x0, "and", x1, "\n")
  # the algorithm
  for(i in 1:iter){
    x0old <- x0
    x1old <- x1
    f_xold_x0 <- ftn(x0)
    f_xold_x1 <- ftn(x1)
    xnew <- x1old - f_xold_x1 * ((x0old - x1old) / (f_xold_x0 - f_xold_x1))
    # update the point
    x1 <- xnew
    x0 <- x1old
    cat("Next x value:", xnew, "\n")
    # the line segments
    df_points_1[i, ] <- c(x0old, ftn(x0old), x1old, ftn(x1old))
    df_points_2[i, ] <- c(x1old, ftn(x1old), xnew, 0)
  }
  plot_start <- min(df_points_1$x0, df_points_1$x1, x0) - 0.1
  plot_end <- max(df_points_2$x0, df_points_2$x1, x0) + 0.1
  x <- seq(plot_start, plot_end, length.out = 200)
  fx <- rep(NA, length(x))
  for (i in seq_along(x)) {
    fx[i] <- ftn(x[i])
  }
  function_data <- data.frame(x, fx) # dataframe containing the function
  p <- ggplot(function_data, aes(x = x, y = fx)) +
    geom_line(color = "royalblue", size = 1) + # plot the function
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_1, color = "black", lty = 1) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_2, color = "red", lty = 2) +
    geom_abline(intercept = 0, slope = 1) # plot the line y = x
  print(p)
  x1
}


a4_1 <- function(x) cos(x) - x
### x0 = 1, x1 = 2
secant_show(a4_1, 1, 2, iter = 5)


a4_2 <- function(x) log(x) - exp(-x)
### x0 = 1, x1 = 2
secant_show(a4_2, 1, 2, iter = 5)


a4_3 <- function(x) x^2 - 0.5
### x0 = 4, x1 = 3.5
secant_show(a4_3, 4, 3.5, iter = 5)



# Problem 5
##### A modifcation of code provided by Eric Cai
golden = function(f, lower, upper, tolerance = 1e-5) {
  golden.ratio = 2/(sqrt(5) + 1)
  ## Use the golden ratio to find the initial test points
  x1 <- lower + golden.ratio * (upper - lower)
  x2 <- upper - golden.ratio * (upper - lower)
  ## the arrangement of points is:
  ## lower ----- x2 --- x1 ----- upper
  ### Evaluate the function at the test points
  f1 <- f(x1)
  f2 <- f(x2)
  while (abs(upper - lower) > tolerance) {
    if (f2 > f1) {
      # the minimum is to the right of x2
      lower <- x2 # x2 becomes the new lower bound
      x2 <- x1 # x1 becomes the new x2
      f2 <- f1 # f(x1) now becomes f(x2)
      x1 <- lower + golden.ratio * (upper - lower)
      f1 <- f(x1) # calculate new x1 and f(x1)
    } else {
      # then the minimum is to the left of x1
      upper <- x1 # x1 becomes the new upper bound
      x1 <- x2 # x2 becomes the new x1
      f1 <- f2
      x2 <- upper - golden.ratio * (upper - lower)
      f2 <- f(x2) # calculate new x2 and f(x2)
    }
  }
  (lower + upper)/2 # the returned value is the midpoint of the bounds
}


f <- function(x) { (x - 3)^2 }
### lower = 0, upper = 10, tolerance = 1e-5
golden(f, 0, 10)


### I created a function for the 15 iterations for various starting points
contour_mapping <- function(a, b) {
  g <- function(x, y) {
    5 * x ^ 2 - 6 * x * y + 5 * y ^ 2
  }
  x <- seq(-1.5, 1, len = 100)
  y <- seq(-1.5, 1, len = 100)
  x_i <- a
  y_i <- b
  x0 <- 0
  y0 <- 0
  cat("Starting values are:", x_i, "and", y_i, "\n")
  for(i in 1:15) {
    x0 <- x_i
    y0 <- y_i
    g_x <- function(x) {
      # x = x, y = y0
      g_x_y0 <- g(x, y0)
      return(g_x_y0)
    }
    x0 <- golden(g_x, lower = -1.5, upper = 1.5, tolerance = 1e-5)
    g_y <- function(y) {
      # x = x0, y = y
      g_x0_y <- g(x0, y)
      return(g_x0_y)
    }
    y0 <- golden(g_y, lower = -1.5, upper = 1.5, tolerance = 1e-5)
    x_i <- x0
    y_i <- y0
    cat("Next values of x & y are:", x0, "&", y0, "\n")
    # the line segment
  }
  contour_df <- data.frame(
    x = rep(x, each = 100),
    y = rep(y, 100),
    z = outer(x, y, g)[1:100^2]
  )
  ggplot(contour_df, aes(x = x, y = y, z = z)) +
    geom_contour(binwidth = 0.9) +
    geom_segment(aes(x = x0, y = y0, xend = x_i, yend = y_i),
                 data = contour_df, color = "black", lty = 1, inherit.aes = FALSE) +
    theme_bw()
}


### a = -1.5, b = -1.5
contour_mapping(-1.5, -1.5)


### a = -1.5, b = 1
contour_mapping(-1.5, 1)


### WIP on updating the line segments with z (Studying for Midterm on 2/28/2020)
df_points_1 <- data.frame(
  x_a = numeric(0),
  y_a = numeric(0),
  z_a = numeric(0))
df_points_2 <- df_points_1


df_points_1[i, ] <- c(x_i, x0, x0)
df_points_2[i, ] <- c(y_i, y_i, y0)



contour_df <- data.frame(
  x = rep(x, each = 100),
  y = rep(y, 100),
  z = outer(x, y, g)[1:100^2]
)

p <- ggplot(contour_df, aes(x = x, y = y, z = z)) +
  geom_contour(binwidth = 0.9) + 
  geom_segment(aes(x = x0, y = y0, xend = x_i, yend = y_i),
               data = df_points_2, color = "black", lty = 1, inherit.aes = FALSE) +
  geom_segment(aes(x = x0, y = y0, xend = x_i, yend = y_i),
               data = df_points_2, color = "black", lty = 1, inherit.aes = FALSE) +
  theme_bw()
print(p)


# EXTRA CREDIT
### I am unable to do it since I have a midterm on Friday (2/28/2020). Am I able to have
### an extension on this extra credit? I feel as if I can complete this.