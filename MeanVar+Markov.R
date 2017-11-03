rm(list = ls())

#setwd("/Users//adhya/Documents/R_Macro/")
#setwd("C://R//MAXIM//MacroClub")
library(openxlsx)
library(lubridate)
library(zoo)
library(xts)

data.macro <- read.xlsx("MacroIndices.xlsx")
data.macro[,1] <- convertToDate(data.macro[,1],origin="1899-12-30")
data_ret_frame <-data.macro[-1,]
data_ret_frame[,2:ncol(data.macro)] <- log(data.macro[-1,-1]/data.macro[-nrow(data.macro),-1])
data_ret <- xts(data_ret_frame[,-1],order.by = as.Date(data_ret_frame[,1], "%m/%d/%Y"))

monthly_day<- seq(21,nrow(data_ret),21)
cum_return_21day <- rollapply(data_ret,width=21,sum)
monthly_returns <-cum_return_21day[monthly_day][-1,1:9]            #Dont use t bills/ notes since they are safe
data_month <-data.frame(coredata(monthly_returns))                 #convert to data frame
data_month_all <- data.frame(coredata(cum_return_21day[monthly_day][-1,]))           #in case you need all

rf <- 0.003              #Monthly risk free
mean_returns <- colMeans(data_month)
cov_returns <- cov(data_month)

mean_var_optimizer_unconstrained(mean_returns, cov_returns, rf)

mean_var_optimizer_unconstrained <- function (mean_returns, cov_matrix, rf)
{
  iota <- rep(1,ncol(cov_matrix))
  
  #frontier portfolios
  min_var_weights <- ( solve(cov_matrix) %*% iota ) / (t(iota) %*% solve(cov_matrix) %*% iota)[1]
  max_ret_weights <- ( solve(cov_matrix) %*% mean_returns ) / (t(iota) %*% solve(cov_matrix) %*% mean_returns)[1]
  
  #The second frontier is NOT a max return portfolio actually, but I keep names same to avoid changes
  
  min_var_return <- t(min_var_weights) %*% mean_returns
  max_ret_return <- t(max_ret_weights) %*% mean_returns
  min_var_var <- (t(min_var_weights) %*% cov_matrix %*% min_var_weights)
  max_ret_var <- (t(max_ret_weights) %*% cov_matrix %*% max_ret_weights)
  sharpe_min_var <- (min_var_return[1] -rf) / sqrt(min_var_var[1])
  sharpe_max_ret <- (max_ret_return[1] -rf) / sqrt(max_ret_var[1])
  
  #full frontier is linear combination of the above, find maximum sharpe
  return_seq <- min_var_return[1] + seq(1,250,1) * (max_ret_return[1]-min_var_return[1]) /50
  max_sharpe <- 0
  max_sharpe_weights <- min_var_weights
  
  #Need to run till sharpe decreases from before, currently fixed number
  
  for(i in 1:250)
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
  
  max_sharpe_return <- t(max_sharpe_weights) %*% mean_returns
  return(list(min_var_weights, max_ret_weights, max_sharpe_weights))
}

strategy_meanvar <- function(data_assets_ret, lookback, rebal, policy_weight, rf)             #Lookback is no of periods (months for monthly data)
{
  no_cols <- ncol(data_assets_ret)
  no_rows <- nrow(data_assets_ret)
  
  wealth_minvar <- data_assets_ret[lookback:no_rows,1]
  colnames(wealth_minvar) <- c("Wealth")
  wealth_minvar[,1] <- 1
  wealth_maxret <- wealth_minvar
  wealth_maxsharpe <- wealth_minvar
  wealth_policy <- wealth_minvar
  
  weight_minvar <- data_assets_ret[(lookback-1):no_rows,]
  weight_maxret <- data_assets_ret[(lookback-1):no_rows,]
  weight_maxsharpe <- data_assets_ret[(lookback-1):no_rows,]
  
  for(i in 1:(no_rows-lookback))
  {
    mean_ret <- colMeans(data_assets_ret[i:i+lookback,])
    cov_mat <- cov(data_assets_ret[i:(i+lookback),])
    #cor_mat <- cor(data_assets_ret[i:(i+lookback),-1])
    weights <- mean_var_optimizer_unconstrained(mean_ret, cov_mat, rf)            #Risk Free: Given as input
    
    #mean_returns <- mean_ret
    #cov_matrix <- cov_mat
    #rf <- mean_rf
    weights[[1]]
    weight_minvar[i,] <- weights[[1]]
    weight_maxret[i,] <- weights[[2]]
    weight_maxsharpe[i,] <- weights[[3]]
    
    wealth_minvar[i+1,1] <- wealth_minvar[i,1]*(1+sum(coredata(weight_minvar[i,])*coredata(data_assets_ret[i+lookback-1,])))
    wealth_maxret[i+1,1] <- wealth_maxret[i,1]*(1+sum(coredata(weight_maxret[i,])*coredata(data_assets_ret[i+lookback-1,])))
    wealth_maxsharpe[i+1,1] <- wealth_maxsharpe[i,1]*(1+sum(coredata(weight_maxsharpe[i,])*coredata(data_assets_ret[i+lookback-1,])))
    wealth_policy[i+1,1] <- wealth_policy[i,1]*(1+sum(policy_weight*coredata(data_assets_ret[i+lookback-1,])))
  }
  
  plot(index(wealth_minvar),wealth_minvar[,1], xlab="Date", ylab = "Wealth", main = "Comparison of Strategies", 
       col="black", type ='l')
  #lines(wealth_maxret$Date, wealth_maxret$Wealth,  col="Blue")
  plot(index(wealth_maxsharpe), wealth_maxsharpe[,1], col="Red",type ='l')
  plot(index(wealth_policy), wealth_policy[,1], col="Green", type='l')
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
strategy_meanvar(data_month,36,1,policy_weight,rf)
lookback <- 36
rebal <- 1
data_assets_ret <- monthly_returns

############################################################Markov############################################


markov_regime <- function(data_returns)
{
  mean_ret <- colMeans(data_returns)
  cov_ret <- cov(data_returns)
  cov_inv <- solve(cov_ret)
  turbulence <- apply(data_returns,1, function(x) sqrt(0.5* (t(x-mean_ret)%*% cov_inv %*%(x-mean_ret))))
  hmm <- depmix(turbulence ~ 1, family = gaussian(), nstates = 3, data=data.frame(turbulence=turbulence))
  hmmfit <- fit(hmm, verbose = FALSE)
  post_probs <- posterior(hmmfit)
  statedef <- state_descript(hmmfit)
  transition <- trmat(hmmfit)
  next_prob <- data.matrix(post_probs[length(turbulence),2:4]) %*% transition
  
  return(list(statedef,next_prob))
}

####################################################################################
##Need to edit this#################

#strategy 1 : w_stock = max_turb * 0 + med_turb *0.6 + low_turb * 1
#strategy 2 : w_stock = max prob state
weight_params=c(0.9,0.6,0.3)

wealth_blend <- monthly_returns[120:nrow(monthly_returns),1]
colnames(wealth_blend) <- c("Blended Markov")
wealth_blend[,1] <- 1
benchmark <- wealth_blend
colnames(benchmark) <- c("Benchmark")
weight_blend <- wealth_blend[1:(nrow(wealth_blend)-1)]
colnames(weight_blend) <- c("Equity Weight")
wealth_choose <- wealth_blend
colnames(wealth_choose) <- c("Max Prob Markov")
weight_choose <- weight_blend
states_list <- list()
nextprob_list <- list()

for(i in 2:nrow(wealth_blend))
{
  data <- data_month[1:(118+i),]
  markov <- markov_regime(data)
  states_list[[i-1]] <- markov[[1]]
  nextprob_list[[i-1]] <- markov[[2]]
  states <- states_list[[i-1]]
  nextprob <- nextprob_list[[i-1]]
  weight_blend[i-1] <- weight_params[3]*nextprob[which.max(states)] + weight_params[1]*nextprob[which.min(states)] + 
    weight_params[2] * nextprob[which(states==sort(states,partial=2)[2])]
  wealth_blend[i,1] <- wealth_blend[i-1,1]* (weight_blend[[i-1,1]]*exp(data_month_all[[119+i,1]]) +
                                               (1-weight_blend[[i-1,1]])*exp(data_month_all[[119+i,3]]))
  weight_choose[i-1] <- weight_params[rank(states)[which.max(nextprob)]]
  wealth_choose[i,1] <- wealth_choose[i-1,1]* (weight_choose[[i-1,1]]*exp(data_month_all[[119+i,1]]) +
                                                 (1-weight_choose[[i-1,1]])*exp(data_month_all[[119+i,3]]))
  benchmark[i,1] <- benchmark[[i-1,1]]*(0.6*exp(data_month[[119+i,1]]) + 0.4*exp(data_month_all[[119+i,3]]))
}