#Central and non-central chi-squared distributions.
#Central chi-squared dist:
x <- seq(0, 20, .15)
y <- dchisq(x, df=5, ncp=0)

plot(x, y, xaxt="n", yaxt="n", type="l", lwd=2,
     ylab=substitute(f(x)))


#Non-central chi-squared dist:
x <- seq(0, 20, .15)
y <- dchisq(x, df=5, ncp=3)

points(x, y, xaxt="n", yaxt="n", type="l", lwd=2, col="blue",
       ylab=substitute(f(x)))



#=================================================
#=================================================
#Central and non-central t distributions.
#Central t dist:
x <- seq(-10, 10, .05)
y <- dt(x,4,, ncp=0)

plot(x, y, xaxt="n", yaxt="n", type="l", lwd=2,
     ylab=substitute(f(x)), xlim=c(-20,20))


#Non-central t dist:
x <- seq(-16, 20, .1)
y <- dt(x, 4, ncp=2)

points(x, y, xaxt="n", yaxt="n", type="l", lwd=2, col="blue",
       ylab=substitute(f(x)))








#=================================================
#=================================================
#Central and non-central F distributions.
#Central F dist:
x <- seq(0, 10, .05)
y <- df(x, df1=3,df2=5, ncp=0)

plot(x, y, xaxt="n", yaxt="n", type="l", lwd=2,
     ylab=substitute(f(x)))

#Non-central F dist:
x <- seq(0, 10, .1)
y <- df(x, df1=3,df2=5, ncp=3)

points(x, y, xaxt="n", yaxt="n", type="l", lwd=2, col="blue",
       ylab=substitute(f(x)))

