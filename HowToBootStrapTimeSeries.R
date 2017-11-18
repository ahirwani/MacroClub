##Boot Strapping Time Series

options(digits = 4) ; library(tseries)
y = ts(read.csv("mindex1.csv",header = T)[,4] ,frequency = 12,  start = c(1948,1) ) 
par(mfrow=c(1,1))
plot(y, main = "Misery Index over Time", ylab = "Index", lwd = 2,ty = "b",cex.main = 2)
# Stationary?
adf.test(y) 
kpss.test(y)
#Looks like it.
######## Bootstrap based on IID innovations ##########
numlags = 12 # choose the model..
n = length(y)
ar1 = ar.ols(y, demean = F,intercept=T, order.max = numlags,aic = F) # standard OLS
plot(ar1$res)
scaled.res = scale(ar1$res) # that is important for the theory to hold.
summ(scaled.res) # see Code category to get this "summ" function
b1 = y[1:numlags] # Naturally the first few residual does not exist so we replace them with 0. So the bootstrap vale is the same as the original
R = 100 # how many bootstrap samples do you want?
res.star = matrix(nrow = (n-numlags),ncol = R) 
obs.star = matrix(nrow = n,ncol = R) # will hold the bootstrapped series

for (i in 1:R){
  res.star[,i] = sample(na.omit(scaled.res),(n-numlags), replace = T)
  obs.star[1:numlags,i] = b1 # for the first obs we plug the original data
  for (j in (numlags+1):n){
    obs.star[j,i] = ar1$x.intercept + ar1$ar%*%obs.star[(j-1):(j-numlags),i] + res.star[(j-numlags),i]
  }}
# Sanity check:
plot(obs.star[,"Pick random realization"], ty = "l") 
plot(y, ty = "l", main = "Original") 


arima.sim(list(order = c(numlags,0,0), 
               ar =ar.ols(y, demean = T,intercept=F, order.max = numlags,aic = F)$ar ),
          n = n, sd = sd(y)/2 )