library(vars)

#returns information criteria and final prediction error for sequential increasing lag order up to VAR p process
#seasona = inclusion of seasonal dummy variables
#type = deterministic regressors to include - constant, trend, both, none
data(Canada)
library(fpp)
data(usconsumption)
VARselect(Canada, lag.max = 5, type = "const")
VARselect(usconsumption, lag.max=8, type= "both")$selection 

var.us <- VAR(usconsumption, p=3, type = "both")
var.can <- VAR(Canada, p=2, type = "const")

##Portmanteau test is a test that the residuals are uncorrelated.
#If p-value < 0.05 then you have serial correlation of errors
serial.test(var.us,lags.pt=10,type = "PT.asymptotic")
serial.test(var.can, lags.pt=10, type = "PT.asymptotic")

summary(var.us)

fcast.us <- forecast(var.us)
plot(fcast.us, xlab="Year")