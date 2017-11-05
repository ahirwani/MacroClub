rm(list = ls())

setwd("/Users//adhya/Documents/R_Macro/")
library(lpSolve)
library(openxlsx)
library(lubridate)
library(zoo)
library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

data.macro <- read.xlsx("MacroIndices.xlsx")
data.macro[,1] <- convertToDate(data.macro[,1],origin="1899-12-30")
n.rows <- nrow(data.macro)
n.cols <- ncol(data.macro) -1

# When reading a csv/excel/etc file, data is imported as a dataframe of factors. 
# Factors are not a particularly useful kind of way to store the financial series, so the next steps
# convert the return data from factors to numeric vectors. We could have also used the xts package for 
# handling time series.
x <- data.macro[,-1]
m <- sapply(data.macro[,-1],function(x) as.numeric(as.character(x)))
data.macro[,-1] <- m

mean_var_optimizer_unconstrained <- function (mean_returns, cov_matrix, rf)
{
  iota <- rep(1,ncol(cov_matrix))
  
  #frontier portfolios
  min_var_weights <- ( solve(cov_matrix) %*% iota ) / (t(iota) %*% solve(cov_matrix) %*% iota)[1]
  max_ret_weights <- ( solve(cov_matrix) %*% mean_returns ) / (t(iota) %*% solve(cov_matrix) %*% mean_returns)[1]

  min_var_return <- t(min_var_weights) %*% mean_returns
  max_ret_return <- t(max_ret_weights) %*% mean_returns
  min_var_var <- (t(min_var_weights) %*% cov_matrix %*% min_var_weights)
  max_ret_var <- (t(max_ret_weights) %*% cov_matrix %*% max_ret_weights)
  sharpe_min_var <- (min_var_return[1] -rf) / sqrt(min_var_var[1])
  sharpe_max_ret <- (max_ret_return[1] -rf) / sqrt(max_ret_var[1])

  #full frontier is linear combination of the above, find maximum sharpe
  return_seq <- min_var_return[1] + seq(1,50,1) * (max_ret_return[1]-min_var_return[1]) /50
  max_sharpe <- 0
  max_sharpe_weights <- min_var_weights

  for(i in 1:50)
  {
    return_seq[i] 
    lambda <- (return_seq[i] - max_ret_return[1]) / (min_var_return[1] - max_ret_return[1])
    p_weight <- lambda*min_var_weights + (1-lambda) * max_ret_weights
    p_variance <- t(p_weight) %*% cov_matrix %*% p_weight
    sharpe <- (return_seq[i] -rf )/ sqrt(p_variance)
    if(sharpe>max_sharpe)
    {
      max_sharpe <- sharpe
      max_sharpe_weights <- p_weight
    }
  }
  
  return(list(min_var_weights, max_ret_weights, max_sharpe_weights))
}

#####################################################################################################################

mean_var_optimizer_long_only <- function (returns.data, rf)
{
  returns.data <- weekly.ts
  mean_returns <- colMeans(returns.data)
  cov_mat <- cov(returns.data)
  port <- portfolio.spec(assets= c("Dollar Index", "S&P500", "MSCI EM", "MSCI DM", "Commodities", 
                                   "EM FX", "REITs", "Hedge Funds", "US BOnds"))
  #Box constraints
  port <- add.constraint(port, type="box", min=0.01, max= 0.9)
  #Weight sum / Leverage
  port <- add.constraint(portfolio = port, type = "full_investment")
  #Generate random portfolios
  rportfolios <- random_portfolios(port, permutations = 100, rp_method = "sample")
  # Get minimum variance portfolio
  minvar.port <- add.objective(port, type = "risk", name = "var")
  # Optimize
  minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", 
                                   rp = rportfolios)
  # Generate maximum return portfolio
  maxret.port <- add.objective(port, type = "return", name = "mean")
  # Optimize
  maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", 
                                   rp = rportfolios)
  
  # Generate vector of returns
  minret <- minvar.opt$weights %*% mean_returns
  maxret <- maxret.opt$weights %*% mean_returns
  
  vec <- minret + seq(1,50,1) * (maxret-minret) /50
  eff.frontier <- data.frame(Risk = rep(NA, length(vec)),
                             Return = rep(NA, length(vec)), 
                             SharpeRatio = rep(NA, length(vec)))
  
  frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(returns.data))
  colnames(frontier.weights) <- colnames(returns.data)
  max_sharpe_index <-1
  
  i<-1
  for(i in 1:length(vec)){
    eff.port <- add.constraint(port, type = "return", name = "mean", return_target = vec[i])
    eff.port <- add.objective(eff.port, type = "risk", name = "var")
    # eff.port <- add.objective(eff.port, type = "weight_concentration", name = "HHI",
    #                            conc_aversion = 0.001)
    
    eff.port <- optimize.portfolio(returns.data, eff.port, optimize_method = "ROI")
    eff.frontier$Risk[i] <- sqrt(t(eff.port$weights) %*% cov_mat %*% eff.port$weights)
    eff.frontier$Return[i] <- eff.port$weights %*% mean_returns
    eff.frontier$Sharperatio[i] <- (eff.port$Return[i] -rf) / eff.port$Risk[i]
    
    frontier.weights[i,] = eff.port$weights
    if(eff.frontier$Sharperatio[i] > eff.frontier$Sharperatio[max_sharpe_index])
      max_sharpe_index <- i
    #print(paste(round(i/length(vec) * 100, 0), "% done..."))
  }
  
  return(list(minvar.opt$weights,maxret.opt$weights,frontier.weights[max_sharpe_index,]))
}


################################################################################


strategy_meanvar <- function(data.assets, lookback, rebal, policy_weight, data.rf)             #Lookback & rebal in days
{
  no_cols <- ncol(data.assets) -1
  day_seq <- seq(1,nrow(data.assets),rebal)                              #note how this has fixing bias
  data.assets_rebal <- data.assets[day_seq,]
  no_rows <- nrow(data.assets_rebal)
  no_periods <- lookback/rebal
  ret_assets <- data.assets_rebal[-1,]
  ret_assets[,2:(no_cols+1)] <- data.assets_rebal[-1,-1]/data.assets_rebal[-no_rows,-1] -1
  ret_rf <- data.rf[-1,-1]/data.rf[-no_rows,-1] -1
  
  wealth_minvar <- data.assets_rebal[lookback:no_rows,1:2]
  colnames(wealth_minvar) <- c("Date","Wealth")
  wealth_minvar[,2] <- 1
  wealth_maxret <- wealth_minvar
  wealth_maxsharpe <- wealth_minvar
  wealth_policy <- wealth_minvar
  
  weight_minvar <- data.assets_rebal[lookback:no_rows,]
  weight_maxret <- data.assets_rebal[lookback:no_rows,]
  weight_maxsharpe <- data.assets_rebal[lookback:no_rows,]
  
  i<-1
  for(i in 1:(no_rows-lookback-1))
  {
    mean_ret <- colMeans(ret_assets[i:i+lookback,-1])
    mean_rf <- mean(ret_rf[i:i+lookback])
    cov_mat <- cov(ret_assets[i:(i+lookback),-1])
    #cor_mat <- cor(ret_assets[i:(i+lookback),-1])
    weights <- mean_var_optimizer(mean_ret, cov_mat, mean_rf)            #Risk Free: T-bills
    
    #mean_returns <- mean_ret
    #cov_matrix <- cov_mat
    #rf <- mean_rf
    
    weight_minvar[i,-1] <- weights[[1]]
    weight_maxret[i,-1] <- weights[[2]]
    weight_maxsharpe[i,-1] <- weights[[3]]
    
    wealth_minvar[i+1,2] <- wealth_minvar[i,2]*(1+sum(weight_minvar[i,-1]*ret_assets[i+lookback+1,-1]))
    wealth_maxret[i+1,2] <- wealth_maxret[i,2]*(1+sum(weight_maxret[i,-1]*ret_assets[i+lookback+1,-1]))
    wealth_maxsharpe[i+1,2] <- wealth_maxsharpe[i,2]*(1+sum(weight_maxsharpe[i,-1]*ret_assets[i+lookback+1,-1]))
    wealth_policy[i+1,2] <- wealth_policy[i,2]*(1+sum(policy_weight*ret_assets[i+lookback+1,-1]))
  }
  
  plot(wealth_minvar$Date,wealth_minvar$Wealth, xlab="Date", ylab = "Wealth", main = "Comparison of Strategies", 
       col="black", ylim=c(-5,2), type ='l')
  #lines(wealth_maxret$Date, wealth_maxret$Wealth,  col="Blue")
  lines(wealth_maxsharpe$Date, wealth_maxsharpe$Wealth, col="Red")
  lines(wealth_policy$Date, wealth_policy$Wealth, col="Green")
  #legend("topright", legend=c("Min Variance", "Max Return","Max Sharpe", "Policy"),col=c("black","blue","red","green"),lty=1)
  #return(list(wealth_minvar,wealth_maxret,wealth_maxsharpe,wealth_policy))
}

statistics <- function(X)                 #function to compute statistics for any given array
{
  sample_mean=mean(X)
  sample_sd=sd(X)
  sample_skew=skewness(X)
  sample_kurtosis=kurtosis(X)
  sample_sharpe= (sample_mean - rf)/sample_sd
  return(round(c(sample_mean,sample_sd,sample_skew,sample_kurtosis,sample_sharpe),3))
}

i<-1
policy_weight <- c(0.05,0.3,0.05,0.15,0.10,0.05,0.05,0.05,0.20)
strategy_meanvar(data.macro,155,5,policy_weight)
data.assets <-data.macro[,1:10]
data.ts <- xts(data.assets[,-1],order.by = as.Date(data.assets[,1], "%m/%d/%Y"))
lookback <- 155
rebal <- 5
data.rf <- data.macro[,1:2]
data.rf[,2] <- data.macro[,12]
mean_var_optimizer_long_only(data.assets[,2:10],0.0016)


########################################################################################################

#Data matrices if needed 
#Daily
daily_returns <- data.macro[-1,]
daily_returns[,2:(n.cols+1)] <- data.macro[-1,-1]/data.macro[-n.rows,-1] -1
mean_returns_daily <- colMeans(daily_returns[,-1])
cov_matrix_daily <- cov(daily_returns[,-1])

#5 business days (holding period)
five_day<- seq(1,n.rows,5)
five_day.macro <- data.macro[five_day,]
n.weeks <- nrow(five_day.macro)

weekly_returns <- five_day.macro[-1,]
weekly_returns[,2:(n.cols+1)] <- five_day.macro[-1,-1]/five_day.macro[-n.weeks,-1] -1
weekly_returns <- weekly_returns[,1:10]
weekly.ts <- xts(weekly_returns[,-1],order.by = as.Date(weekly_returns[,1], "%m/%d/%Y"))
mean_returns_weekly <- colMeans(weekly_returns[,-1])
cov_matrix_weekly <- cov(weekly_returns[,-1])
cor_matrix_weekly <- cor(weekly_returns[,-1])
rf <- unname(mean_returns_weekly[n.cols])

#monthly holding period
monthly <- seq(1,n.rows,20)
monthly.macro <- data.macro[monthly,]
n.months <- nrow(monthly.macro)

monthly_returns <- monthly.macro[-1,]
monthly_returns[,2:(n.cols+1)] <- monthly.macro[-1,-1]/monthly.macro[-n.months,-1] -1
mean_returns_monthly <- colMeans(monthly_returns[,-1])
cov_matrix_monthly <- cov(monthly_returns[,-1])
cor_matrix_monthly <- cor(monthly_returns[,-1])
