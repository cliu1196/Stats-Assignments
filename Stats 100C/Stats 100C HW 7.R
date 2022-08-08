a1 <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100c/soil.txt",
                 header=TRUE)

a <- a1[1:155,]

vz <- a$zinc
vl <- a$lead
ml <- lm(vl ~ vz)
summary(ml)

# t-value: 39.681

beta0 <- 17.367688
beta1 <- 0.289523

qt(0.975, 153) 



top <- ((beta0 - 3*beta1) - (15) - (0))
bot <- sqrt((1/155) + ((mean(vz) + 3)^2)/sum((vz - mean(vz)^2))

top/bot
tratio <- top/bot

tratio


