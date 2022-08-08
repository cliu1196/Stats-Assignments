#Simulate geostatistical data
#Generate geostatistical data.

library(maps)

#Plot the US map:
q <- map("state")

#Generate 300 points:
x <- runif(300,q$range[1], q$range[2])
y <- runif(300,q$range[3], q$range[4])

#Find the state in which each point belongs
in.what.country <- map.where(database="state", x, y) 

#Remove NA values:
in.this.country <- which(is.na(in.what.country)==0)
locations <- as.data.frame(cbind(x,y))
loc <- locations[in.this.country,]

#Compute the distance matrix:
coord <- loc

x1 <- rep(rep(0,nrow(loc)),nrow(loc))             #Initialize
dist <- matrix(x1,nrow=nrow(loc),ncol=nrow(loc))  #the distance matrix 

for (i in 1:nrow(loc)){
  for (j in 1:nrow(loc)){
    dist[i,j]=((coord[i,1]-coord[j,1])^2+(coord[i,2]-coord[j,2])^2)^0.5
  }
}
#Create the covariance matrix.  The exponential covariance function is used:
c0 <- 0
c1 <- 0.0005
alpha <- 3.0

x1 <- rep(rep(0,nrow(loc)),nrow(loc))              #Initialize
C1 <- matrix(x1,nrow=nrow(loc),ncol=nrow(loc))     #the C1 matrix

for(i in 1:nrow(loc)){
  for (j in 1:nrow(loc)){
    C1[i,j]=c1*exp(-dist[i,j]/alpha)
    if(i==j){C1[i,j]=c0+c1}
  }
}

#Cholesky decomposition:
L <- chol(C1)

#Create the error vector:
e <- rnorm(nrow(loc))

#The mean value.  Assume constant mean:
mu <- 0.06

#Generate the data:
data <- mu + t(L) %*% e

#Create a bubble plot:
map("state", add=TRUE)

points(loc$x, loc$y, xlim=c(-125,-66),ylim=c(25,50), xlab="Longitude",
       ylab="Latitude", main="Simulated data", "n")

title("Simulated data")

#Add the bubble circles:
points(loc$x, loc$y, cex=data/mean(data), pch=19, col="blue")

legend("bottomleft",pt.cex=c(quantile(data, c(0.25, .50, 0.75))/mean(data)),pch=c(19,19,19), legend=c(round(quantile(data, c(0.25, .50, 0.75)),2)), col=c("blue", "blue", "blue"))

box()