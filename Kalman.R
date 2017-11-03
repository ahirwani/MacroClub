library(mnormt)
xhat <- c(0.2,-0.2)
Sigma <- matrix(c(0.4,0.3,0.3,0.45),ncol=2)
x1 <- seq(-2,4,length=151)
x2 <- seq(-4,2, length=151)
f <- function(x1,x2,mean=xhat, varcov=Sigma)
    dmnorm(cbind(x1,x2),mean,varcov)  #dmnrom returns a vector of density values (possibly log-transformed)

z <- outer(x1,x2,f)

mycols <- topo.colors(100,0.5)
#Plot the density
image(x1,x2,z,col=mycols,main="Prior density",xlab=expression('x'[1]), ylab=expression('x'[2]))
contour(x1,x2,z,add=T)
points(0.2,-0.2,add=T)
text(0.1,-0.2,labels=expression(hat(x)),adj=1)

###Get Sensor Info

R <- 0.5*Sigma
z2 <- outer(x1,x2,f,mean=c(2.3,-1.9),varcov=R)
image(x1,x2,z2,col=mycols, main="sensor density")#backgrond colours
contour(x1,x2,z2,add=TRUE)## Sensonr Contour
points(2.3,-1.9,pch=19)
text(2.2,-1.9, labels="y",adj=1)
contour(x1,x2,z,add=TRUE) ## Original Actual Contour
points(0.2,-0.2,pch=19)
text(0.1,-0.2,labels=expression(hat(x)),adj=1)

#Using Bayesian Statistics - know robot location given Y. p(x|y) = p(y|x)p(x)/p(y)
#y|x ~ N(Gx,R); x~N(x,E)
#Now because x,y are normal and G is the identity matrix
#xf = (E-1 + R-1)-1 (E-1 x + R-1 y)
#Ef = (E-1 + R-1)-1

#Using Matrix Inversion Identity
#(A-1 + B-1)-1 = A - A(A+B)-1A = A(A+B)-1 B
#xf = (E - E(E+R)-1E)(E-1x + R-1y)
#x-E(E+R)-1x + ER-1y - E(E+R)-1*ER-1y
#x+E(E+R)-1(y-x)
#... Ef = E-E(E+R)-1 E

G= diag(2) #identity matrix
y <- c(2.4,-1.9)
xhatf <- xhat + Sigma %*% t(G) %*% solve(G %*% Sigma %*% t(G)+R) %*% (y- G %*% xhat)
Sigmaf <- Sigma - Sigma %*% t(G) %*% solve(G %*% Sigma %*% t(G)+R) %*% G %*% Sigma

z3 <- outer(x1,x2,f,mean=c(xhatf),varcov=Sigmaf)
#Now Plot
image(x1, x2, z3, col=mycols,      xlab=expression('x'[1]), ylab=expression('x'[2]),
      main="Filtered density")
contour(x1, x2, z3, add=TRUE)
points(xhatf[1], xhatf[2], pch=19)
text(xhatf[1]-0.1, xhatf[2],
     labels = expression(hat(x)[f]), adj = 1)
lb <- adjustcolor("black", alpha=0.5)
contour(x1, x2, z, add=TRUE, col=lb)
points(0.2, -0.2, pch=19, col=lb)
text(0.1, -0.2, labels = expression(hat(x)), adj = 1, col=lb)
contour(x1, x2, z2, add=TRUE, col=lb)
points(2.3, -1.9, pch=19, col=lb)
text(2.2, -1.9,labels = "y", adj = 1, col=lb)

###Now assume robot is moving - have a lienar model that explains how state evolves over time - based on wheel spin.
#xt+1 = Axt + wt+1 where Wt is noise A is (1.2,0,0,-0.2) Q = 0.3Sigma
#E(Axf + w)= AE(xf)+E(w) = Axf = Ax + AEG' (GEG'+R)-1 (y-Gx)

A <- matrix(c(1.2,0,0,-0.2),ncol=2)
Q <- 0.3 * Sigma
K <- A %*% Sigma %*% t(G) %*% solve(G %*% Sigma %*% t(G)+R)
xhatnew <- A %*% xhat + K %*% (y-G %*% xhat)
Sigmanew <- A %*% Sigma %*% t(A) - K %*% G %*% Sigma %*% t(A) + Q

z4 <- outer(x1,x2, f, mean=c(xhatnew), varcov=Sigmanew)
image(x1, x2, z4, col=mycols,
      xlab=expression('x'[1]), ylab=expression('x'[2]),
      main="Predictive density")
contour(x1, x2, z4, add=TRUE)
points(xhatnew[1], xhatnew[2], pch=19)
text(xhatnew[1]-0.1, xhatnew[2],
     labels = expression(hat(x)[new]), adj = 1)
contour(x1, x2, z3, add=TRUE, col=lb)
points(xhatf[1], xhatf[2], pch=19, col=lb)
text(xhatf[1]-0.1, xhatf[2], col=lb, 
     labels = expression(hat(x)[f]), adj = 1)
contour(x1, x2, z, add=TRUE, col=lb)
points(0.2, -0.2, pch=19, col=lb)
text(0.1, -0.2, labels = expression(hat(x)), adj = 1, col=lb)
contour(x1, x2, z2, add=TRUE, col=lb)
points(2.3, -1.9, pch=19, col=lb)
text(2.2, -1.9,labels = "y", adj = 1, col=lb)

### Image Can also be done with lattice

library(lattice)
grid <- expand.grid(x=x1,y=x2)
grid$Prior <- as.vector(z)
grid$Likelihood <- as.vector(z2)
grid$Posterior <- as.vector(z3)
grid$Predictive <- as.vector(z4)
contourplot(Prior + Likelihood + Posterior + Predictive ~ x*y, 
            data=grid, col.regions=mycols, region=TRUE,
            as.table=TRUE, 
            xlab=expression(x[1]),
            ylab=expression(x[2]),
            main="Kalman Filter",
            panel=function(x,y,...){
              panel.grid(h=-1, v=-1)
              panel.contourplot(x,y,...)
            })


####################### Kalman filter for Random Walk
#Kalman filter is an example of an adaptive model, dynamic linear model. unlike Simple moving average, 
#or FIR that has a fixed set of windowing parameters, the kalman filter constantly updates information to produce adaptive filtering on fly
#xt = A*xt-1 + w  #xt is time series, A is state transition matrix, wt is white noise
#zt = H*xt+ v   #zt is estimate of actual signal covariance with respect to x, xt is estimated center of time series, v is noise
#zt is value of time series we are trying to capture and model with xt
#With Hidden Markov models - hiddena nd observed state variables are here

###Logistic Growth with noise
logistG <- function(r,p,k,t){
  k*p*exp(r*t)/(k+p*exp(r*t)-1)
}

k <- 100
p0 <- 0.1*k
r <-0.2
deltaT <- 0.1

set.seed(12345)

obsVariance <- 25
nObs <- 250
nu <- rnorm(nObs, mean=0,sd=sqrt(obsVariance))

pop <- c(p0, logistG(r,p0,k,(1:(nObs-1))*deltaT))+ nu
plot(pop); lines(c(p0, logistG(r,p0,k,(1:(nObs-1))*deltaT)),col="blue") #with what it looks like without noise
tail(pop); head(pop);
Estimate <- data.frame(Rate=rep(NA,nObs), Population = rep(NA,nObs))

library(numDeriv)
a <- function(x, k, deltaT){c(r=x[1],logistG(r=x[1],p=x[2],k,deltaT))}
G <- t(c(0,1))

Q <- diag(c(0,0))
R <- obsVariance

x <- c(r,p0)  ##original vales x0,p0
Sigma <- diag(c(144,25)) #original variance

for(i in 1:nObs){
  xobs <- c(0,pop[i]) #x observation of population item i
  y <- G %*% xobs
  #Filter
  SigTermInv <- solve(G %*% Sigma %*% t(G) + R)
  xf <- x + Sigma %*% t(G) %*% SigTermInv %*% (y - G %*% x)
  Sigma <- Sigma - Sigma %*% t(G) %*% SigTermInv %*% G %*% Sigma
  A <- jacobian(a, x=x, k=k, deltaT = deltaT)
  K <- A %*% Sigma %*% t(G) %*% solve(G %*% Sigma %*% t(G) + R)
  Estimate[i,] <- x
  
  #Predict
  x <- a(x = xf, k=k, deltaT = deltaT) + K %*% (y- G %*% xf)
  Sigma <- A %*% Sigma %*% t(A) - K %*% G %*% Sigma %*% t(A) + Q
}

#Plot output
op <- par(mfrow= c(2,1))
time <- c(1:nObs)*deltaT
plot(y=pop, x= time, t= 'l',main = "Population growth", xlab="Time",ylab="Population")
curve(logistG(r,p0,k,x),from=0,to=max(time), col=2,add=TRUE, lwd = 1)
lines(y=Estimate$Population, x= time, col="orange",lwd=2)
legend("bottomright",legend=c("Data","Actual","Estimate"),bty='n',col=c("black","red","orange"),lty=1,lwd=2)
plot(y= Estimate$Rate,x=time,t='l',main="Estimated Growth Rate",xlab="Time",ylab="Rate",col="orange",lwd=2)
abline(h=r, col=adjustcolor("red",alpha=0.5),lwd=2)
legend("topright",legend=c("Actual","Estimate"),bty="n",col=c("red","orange"),lty=1,lwd=2)
par(op)

#####Example of Kalman filter for estimating a fixed value with measurement error from Welch and Bishop

count = 50
true_value = -0.377727 #actual value
z = rnorm(count, mean=true_value,sd=0.1)

Q=1e-5 #process variance

#Allocate space
xhat = rep(0,count)#a posteri estimate at each step
P = rep(0,count)#a posteri error estimate
xhatminus = rep(0,count) #a prior estimate
Pminus = rep(0,count) #a priori error estimate
K=rep(0,count) #gain

#estimate of measu. variance
R = 1**2

#initialise guesses: assume true_value=0, error is 1.0
xhat[1] <- 0
P[1] <- 1

for(k in 2:count){
  #time update
  xhatminus[k] <- xhat[k-1]
  Pminus[k] <- P[k-1] + Q
  
  #measurement update
  K[k] = Pminus[k]/(Pminus[k]+R)
  xhat[k] = xhatminus[k] + K[k] * (z[k] - xhatminus[k])
  P[k] = (1-K[k])*Pminus[k]
  
}
par(mfrow=c(2,1))

plot(xhat, 
     type="l", 
     col="blue", 
     xlab="Iteration", 
     ylab="Volts",
     ylim=1.05 * c(min(z,true_value,xhat),max(z,true_value,xhat))
)
points(z,  pch=2)

abline(true_value,0)

plot(Pminus[2:count],type="l",xlab="Iteration", ylab=substitute(Variance~(Volts^2 )))