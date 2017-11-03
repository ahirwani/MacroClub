

alpha = 0; sigma = 1; T=1; n=2^12; X0 = 0.1;
dt = T/n

t = seq(0,T,by = dt)
x=c(X0, alpha*dt + sigma*sqrt(dt)*rnorm(n,mean=0,sd=1))
Xt = cumsum(x)
plot(t,Xt,type='l',xlab = "time")

head(x)

install.packages("sde")
library(sde)

mu = 0.16; sigma = 0.9; P0 = 40 ; T = 1/12 #1 month
nt=50; n=2^8;

dt = T/n; t = seq(0,T,by=dt); plot(t)
X=matrix(rep(0,length(t)*nt),nrow=nt)
plot(X)

str(X)
for(i in 1:nt) {X[i,]=GBM(x=P0, r = mu, sigma=sigma, T=T, N=n)}

ymax = max(X); ymin=min(X)

plot(t,X[2,],t='l',ylim=c(ymin,ymax),col=1,ylab="PriceP(t)",xlab="time t")
