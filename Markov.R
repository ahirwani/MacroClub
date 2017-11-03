##### MARKOV SWITCHING MODEL


source("C:/R/MAXIM/Tools/FunctionList.R")
library(MSwM)

MSData <- read.csv("C:/R/MAXIM/Data/MSData.csv")
attach(MSData)

str(MSData)
#LRY - log of nominal GDP of MY
#LRV - Volatility of Stock Market
#LRC - log of Bank Credit
#INT - Interest Rate
#LVS - Log of Value of Stocks
#LGS - log of Growth of Stock

#Define dependant variable (1 variable for each equation)

#Value of Stock
yLVS = cbind(LVS)
#Log of Growth Stock
yLGS = cbind(LGS)

#x - Nominal GDP, Bank Credit, Interest Rate, Volatiltiy of Stock
x =cbind(LRY,LRC,INT,LRV)

olsLVS <- lm(yLVS ~ x)

olsLGS <- lm(yLGS ~ x) 

summary(olsLVS)
summary(olsLGS)

#k is number of regimes, 6 is for means of 5 variables + 1 for volatility
msLVS = msmFit(olsLVS, k=2, sw= rep(TRUE,6))

msLGS = msmFit(olsLGS, k=2, sw= rep(TRUE,6))

summary(msLVS)

#Now Plot the Regimes
#Plot Probabilities
plotProb(msLVS, which=1)
#Plot the chart with the indicator of timeline
plotProb(msLVS, which=2)

#Now Get Diagnostic Sets
plotDiag(msLVS, regime = 1, which = 1)
plotDiag(msLVS, regime = 1, which = 2)