#Comparing regression equations:
#x1 Density determined from underwater weighing
#y Percent body fat from Siri????Ts (1956) equation
#x3 ? Age (years)?????#x4 ? Weight (lbs)?????#x5 ? Height (inches)?????#x6 ? Neck circumference (cm)?????#x7 ? Chest circumference (cm)?????#x8 ? Abdomen circumference (cm)?????#x9 ? Hip circumference (cm)?????#x10 ? Thigh circumference (cm)?????#x11 ? Knee circumference (cm)?????#x12 ? Ankle circumference (cm)?????#x13 ? Biceps (extended) circumference (cm)?????#x14 ? Forearm circumference (cm)?????#x15 ? Wrist circumference (cm)?????

a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/body_fat.txt", header=TRUE) 

#Split the data into 2 sets:
a1 <- a[which(a$x3<46),] #Age less than 46
a2 <- a[which(a$x3>=46),] #Ages 46 or older


ones1 <- rep(1, nrow(a1))
ones2 <- rep(1, nrow(a2))


#Use notation in the handout:
X11 <- as.matrix(cbind(ones1, a1$x6, a1$x10, a1$x11))
X12 <- as.matrix(cbind(a1$x4, a1$x8))
zeros1 <- matrix(rep(0,2*nrow(a1)),nrow(a1),2)

X21 <- as.matrix(cbind(ones2, a2$x6, a2$x10, a2$x11))
X22 <- as.matrix(cbind(a2$x4, a2$x8))
zeros2 <- matrix(rep(0,2*nrow(a2)),nrow(a2),2)

#Construct entire matrix X:
Xa <- as.matrix(cbind(X11, X12, zeros1))
Xb <- as.matrix(cbind(X21, zeros2, X22))

X <- as.matrix(rbind(Xa,Xb))

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% a$y

#Compute se^2:
se2 <- (t(a$y) %*% a$y - t(beta_hat) %*% t(X) %*% a$y)/(nrow(a)-7-1)


#Test hypothesis:
#H0: beta14=b24, beta15=beta25

#Method A:
#Use F test for the general linear hypothesis:
#Define matrix C and vector gamma: 
q <- c(0,0,0,0,1,0,-1,0,  0,0,0,0,0,1,0,-1) 
C <- matrix(q, 2,8, byrow=TRUE) 
g <- c(0,0) 
F <- (t(C%*%beta_hat-g)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_hat-g)) / (2*se2) 








#=========
#Use full and reduced model.
#The full model (unrestricted) same as above.
#Here is the reduced model:
#Under H0: beta14=beta24=beta4 and 
#beta15=beta25=beta5

ones <- rep(1, nrow(a))
Xr <- as.matrix(cbind(ones, a$x6, a$x10, a$x11, a$x4, a$x8))

beta_c <- solve(t(Xr) %*% Xr) %*% t(Xr) %*% a$y

#Compute F statistic:
#SSEr <- ec'*ec
ec <- a$y - Xr %*% beta_c
SSEr <- t(ec) %*% ec

#SSEf <- ef'*ef
ef <- a$y - X %*% beta_hat
SSEf <- t(ef) %*% ef

F <- ((SSEr - SSEf)/2) / se2
