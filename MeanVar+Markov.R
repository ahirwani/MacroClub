rm(list = ls())

setwd("/Users//adhya/Documents/R_Macro/MacroClub/")
#setwd("C://R//MAXIM//MacroClub")

##########################################################################
#Setup

library(openxlsx)
library(xlsx)
library(lubridate)
library(zoo)
library(xts)
library(moments)
library(PerformanceAnalytics)
<<<<<<< HEAD
library('depmixS4')
library('quantmod')
=======
library(depmixS4)
>>>>>>> 59a4061066978b473b8e47a3d3c672307c5ed044

data.macro <- read.xlsx("MacroIndices.xlsx")
data.macro[,1] <- convertToDate(data.macro[,1],origin="1899-12-30")
data_ret_frame <-data.macro[-1,]
data_ret_frame[,2:ncol(data.macro)] <- log(data.macro[-1,-1]/data.macro[-nrow(data.macro),-1])
data_ret <- xts(data_ret_frame[,-1],order.by = as.Date(data_ret_frame[,1], "%m/%d/%Y"))

monthly_day<- seq(21,nrow(data_ret),21)
cum_return_21day <- rollapply(data_ret,width=21,sum)
monthly_returns <-cum_return_21day[monthly_day][,1:9]            #Dont use t bills/ notes since they are safe
data_month <-data.frame(coredata(monthly_returns))                 #convert to data frame
data_month_all <- data.frame(coredata(cum_return_21day[monthly_day][-1,]))           #in case you need all

rf <- 0.001              #Monthly risk free from T-Bills 
policy_weight <- c(0.05,0.3,0.05,0.15,0.10,0.05,0.05,0.05,0.20)

#########################################################################
#Static In Sample

mean_returns <- colMeans(monthly_returns)
cov_matrix <- cov(monthly_returns)

w <- mean_var_optimizer_unconstrained(mean_returns, cov_matrix, rf)
data_assets_ret <- monthly_returns

static_portfolios(w[[1]][,1],w[[3]][,1],policy_weight)
w_minvar <- w[[1]][,1]
w_sharpe <- w[[3]][,1]

static_portfolios <- function(w_minvar,w_sharpe,policy)
{
  wealth_minvar <- rbind(data_assets_ret[1,1],data_assets_ret[,1])
  colnames(wealth_minvar) <- c("Wealth")
  wealth_minvar[,1] <- 1
  wealth_w2 <- wealth_minvar
  wealth_maxsharpe <- wealth_minvar
  wealth_policy <- wealth_minvar 
  
  for(i in 1:(nrow(wealth_minvar)-1))
  {
    wealth_policy[i+1,1] <- wealth_policy[i,1]*(1+sum(policy_weight*coredata(data_assets_ret[i,])))
    wealth_minvar[i+1,1] <- wealth_minvar[i,1]*(1+sum(w_minvar*coredata(data_assets_ret[i,])))
    wealth_maxsharpe[i+1,1] <- wealth_maxsharpe[i,1]*(1+sum(w_sharpe*coredata(data_assets_ret[i,])))
  }
  
  plot(index(wealth_minvar),wealth_minvar[,1], xlab="Date", ylab = "Wealth", main = "Comparison of Strategies", 
       col="black", cex.main=0.8, ylim=c(0.6,2), type ='l')
  lines(index(wealth_policy), wealth_policy[,1], col="Blue", type='l')
  lines(index(wealth_maxsharpe), wealth_maxsharpe[,1], col="Red",type ='l')
  legend("topleft", legend=c("Min Variance", "Max Sharpe", "Policy"),col=c("black","red", "blue"),lty=1)
  statistics(wealth_minvar)
  statistics(wealth_policy)
  statistics(wealth_maxsharpe)
}

#####################################################################################

mean_var_optimizer_unconstrained <- function (mean_returns, cov_matrix, rf)
{
  iota <- rep(1,ncol(cov_matrix))
  
  #frontier portfolios
  min_var_weights <- ( solve(cov_matrix, tol=1e-21) %*% iota ) / (t(iota) %*% solve(cov_matrix, tol=1e-21) %*% iota)[1]
  w2_weights <- ( solve(cov_matrix, tol = 1e-21) %*% mean_returns ) / (t(iota) %*% solve(cov_matrix, tol=1e-21) %*% mean_returns)[1]
  
  #The second frontier is NOT a max return portfolio
  
  min_var_return <- t(min_var_weights) %*% mean_returns
  w2_return <- t(w2_weights) %*% mean_returns
  min_var_var <- (t(min_var_weights) %*% cov_matrix %*% min_var_weights)
  #hack for negative variance
  if(min_var_var < 0){min_var_var <- 0.001}
  w2_var <- (t(w2_weights) %*% cov_matrix %*% w2_weights)
  #hack for negative variance
  if(w2_var < 0){w2_var <- 0.001}
  sharpe_min_var <- (min_var_return[1] -rf) / sqrt(min_var_var[1])
  sharpe_w2 <- (w2_return[1] -rf) / sqrt(w2_var[1])
  
  #full frontier is linear combination of the above, find maximum sharpe
  #create sequence of 250 with interval as 1/25th of diff b/w returns
  
  return_seq <- min_var_return[1] + seq(1,250,1) * (w2_return[1]-min_var_return[1]) /25
  max_sharpe <- sharpe_min_var
  max_sharpe_weights <- min_var_weights
  
  #Need to run till sharpe decreases from before to get tangent portfolio
  for(i in 1:250)
  {
    lambda <- (return_seq[i] - w2_return[1]) / (min_var_return[1] - w2_return[1])
    p_weight <- lambda*min_var_weights + (1-lambda) * w2_weights
    p_variance <- t(p_weight) %*% cov_matrix %*% p_weight
    #hack for negative variance
    if(p_variance < 0){p_variance <- 0.001}
    sharpe <- (return_seq[i] -rf )/ sqrt(p_variance)
    if(max(p_weight)>1) {break}                         #constraint on leverage is 1
    if(sharpe>max_sharpe)
    {
      max_sharpe <- sharpe[1,1]
      max_sharpe_weights <- p_weight
    }
    else
      {break}                               #if sharpe is lower than max sharpe, we have reached tangency
  }
  
  max_sharpe_return <- t(max_sharpe_weights) %*% mean_returns
  return(list(min_var_weights, w2_weights, max_sharpe_weights))
}

#####################################################################################
#Dynamic Strategy

strategy_meanvar <- function(data_assets_ret, lookback, rebal, policy_weight, rf)             #Lookback is no of periods (months for monthly data)
{
  no_cols <- ncol(data_assets_ret)
  no_rows <- nrow(data_assets_ret)
  
  wealth_minvar <- data.frame(Wealth=data_assets_ret[lookback:no_rows,1])
  #colnames(wealth_minvar) <- c("Wealth")
  wealth_minvar[,1] <- 1
  wealth_policy <- wealth_maxsharpe <- wealth_minvar
  
  weight_minvar <- data_assets_ret[(lookback-1):no_rows,]
  weight_maxsharpe <- data_assets_ret[(lookback-1):no_rows,]
  
  #i<-12                       #One of the cases where the results are weird, but look right...
  
  for(i in 1:(no_rows-lookback))
  {
    mean_ret <- colMeans(data_assets_ret[i:i+lookback,])
    cov_mat <- cov(data_assets_ret[i:(i+lookback),])
    #cor_mat <- cor(data_assets_ret[i:(i+lookback),-1])
    weights <- mean_var_optimizer_unconstrained(mean_ret, cov_mat, rf)            #Risk Free: Given as input
    
    #mean_returns <- mean_ret
    #cov_matrix <- cov_mat
    #rf <- mean_rf
    weight_minvar[i,] <- weights[[1]]
    weight_maxsharpe[i,] <- weights[[3]]
    
    wealth_minvar[i+1,1] <- wealth_minvar[i,1]*(1+sum(coredata(weight_minvar[i,])*coredata(data_assets_ret[i+lookback-1,])))
    wealth_maxsharpe[i+1,1] <- wealth_maxsharpe[i,1]*(1+sum(coredata(weight_maxsharpe[i,])*coredata(data_assets_ret[i+lookback-1,])))
    wealth_policy[i+1,1] <- wealth_policy[i,1]*(1+sum(policy_weight*coredata(data_assets_ret[i+lookback-1,])))
  }
  
  plot(index(wealth_minvar),wealth_minvar[,1], xlab="Date", ylab = "Wealth", main = "Comparison of Strategies", 
       col="black", cex.main=0.8, ylim=c(0.6,1.5), type ='l')
  lines(index(wealth_policy), wealth_policy[,1], col="Blue", type='l')
  lines(index(wealth_maxsharpe), wealth_maxsharpe[,1], col="Red",type ='l')
  legend("topleft", legend=c("Min Variance", "Max Sharpe", "Policy"),col=c("black","red", "blue"),lty=1)
  #plot(index(wealth_maxsharpe), wealth_maxsharpe[,1], col="Red",type ='l')
  #return(list(wealth_minvar,wealth_w2,wealth_maxsharpe,wealth_policy))
  statistics(wealth_minvar)
  statistics(wealth_policy)
  statistics(wealth_maxsharpe)
}

strategy_meanvar(data_month,36,1,policy_weight,rf)
lookback <- 36
rebal <- 1
data_assets_ret <- monthly_returns

###################################################################
#Results

maxdrawdown<-function(returns){
  minSum <-9999
  thisSum <- 0
  end_index<-1
  for( j in 1:nrow(returns)) {
    thisSum= thisSum+returns[[j,1]]
    if (thisSum > 0) {
      thisSum = 0
    } else if(thisSum < minSum) {
      minSum = thisSum
      end_index=j
    }
  }
  return(c(exp(minSum)-1,end_index))
}

statistics<-function(port)
{
  
  returns <- data.frame(Returns= port[-nrow(port),])
  for(i in 1:(nrow(port)-1))
  {
    returns[i,1] <- log(port[[i+1,1]]/port[[i,1]])
  }
  
  ##First measures
  m<-mean(returns[,1])
  cumul_ret<-port[[nrow(port),1]]/port[[1,1]] -1
  sigma<-sd(returns[,1])
  sharpe<-(m-rf)/sigma
  skew<-unname(skewness(returns))
  kurt<-unname(kurtosis(returns))
  worstday<-returns[order(returns)[1:10],1] ##10 worst days
  quantiles<-quantile(returns[,1],seq(0.01,0.05,length=6))
  cvar<-ES(returns[,1],p=0.95,method="gaussian") ##cvar
  
  #Drawdown
  a<-maxdrawdown(returns)
  maxDrawdow<-a[1] ##max drawdown in simple returns
  Data<-data.frame(date = index(returns), returns, row.names=NULL)
  end_index<-a[2] ##index for the end date of the max drawdown
  end_date<-Data[end_index,1] ##end date of max drawdown
  start_index<-which.max(port[1:end_index,1]) ##index for the start date of the maw drawdown
  start_date<-Data[start_index,1] ##start date of the max drawdown
  d<-list(start_date,end_date,maxDrawdow)
  names(d)<-c("Start date","End date","Return")
  
  ##Output list
  l<-list(m,cumul_ret,sigma,sharpe,skew,kurt,worstday,d,quantiles,cvar)
  names(l)=c("Mean","Cumulative Return","Volatility","Sharpe Ratio","Skewness",
             "Kurtosis","10 worst daily drawdowns","Max Drawdown","Quantiles","CVAR")
  print(l)
}

############################################################Markov############################################

#Information functions
trmat <- function ( d ) {
  M <- attributes ( d ) $ nstates
  Mat <- matrix ( 0, M, M )
  for ( i in 1 : M ) {
    for ( j in 1 : M ) {
      Mat [ i, j ] <- ( attributes ( d ) $ transition [[ i ]] )@ parameters $ coefficients [ j ]
    } 
  } 
  return(Mat)
}

state_descript <- function ( d ) {
  M <- attributes ( d ) $ nstates
  Mat <- matrix ( 0, M)
  for ( i in 1 : M ) {
    Mat [ i ] <- unname((attributes(d) $response[[i]])[[1]] @parameters $coefficients)
  } 
  return(Mat)
}

#Computing regimes
markov_regime <- function(data_returns)
{
  mean_ret <- colMeans(data_returns)
  cov_ret <- cov(data_returns)
  cov_inv <- solve(cov_ret)
  turbulence <- apply(data_returns,1, function(x) sqrt(0.5* (t(x-mean_ret)%*% cov_inv %*%(x-mean_ret))))    #decision variable turbulence
  hmm <- depmix(turbulence ~ 1, family = gaussian(), nstates = 2, data=data.frame(turbulence=turbulence))   #fit 2 states
  hmmfit <- fit(hmm, verbose = FALSE)
  post_probs <- posterior(hmmfit)
  statedef <- state_descript(hmmfit)
  transition <- trmat(hmmfit)
  next_prob <- data.matrix(post_probs[length(turbulence),2:3]) %*% transition        #make sure dimensions are conformable with no states
  
  return(list(statedef,next_prob,post_probs))
}

####################################################################################
##Markov Mean Variance Strategy#################

#Start with 3 years of calibration, and keep increasing
wealth_minvar <- monthly_returns[36:nrow(monthly_returns),1]
colnames(wealth_minvar) <- c("Markov 2 States")
wealth_minvar[,1] <- 1
wealth_maxsharpe <- wealth_minvar
benchmark <- wealth_minvar
colnames(benchmark) <- c("Benchmark")
weight_minvar <- monthly_returns[36:nrow(monthly_returns),]
weight_minvar[,] <- 0 
weight_maxsharpe <- weight_minvar
states_list <- list()
nextprob_list <- list()

#i<-11 computationally singular case
for(i in 2:nrow(wealth_minvar))
{
  print(paste0("Working on State: ",i," of ",nrow(wealth_minvar)))
  data <- data_month[1:(34+i),]
  markov <- markov_regime(data)
  states_list[[i-1]] <- markov[[1]]
  nextprob_list[[i-1]] <- markov[[2]]
  states <- states_list[[i-1]]
  nextprob <- nextprob_list[[i-1]]
  post_prob <- markov[[3]]
  data_state1 <- data[which(post_prob[,1]==1),] # 
  data_state2 <- data[which(post_prob[,1]==2),]
  weights_state1 <- mean_var_optimizer_unconstrained(colMeans(data_state1),cov(data_state1),rf)
  weights_state2 <- mean_var_optimizer_unconstrained(colMeans(data_state2),cov(data_state2),rf)
  
  weight_minvar[i-1,] <- weights_state1[[1]]*nextprob[1] + weights_state2[[1]]*nextprob[2] 
  weight_maxsharpe[i-1,] <- weights_state1[[3]]*nextprob[1] + weights_state2[[3]]*nextprob[2] 
  
  wealth_minvar[i,1] <- wealth_minvar[i-1,1]* sum(coredata(weight_minvar[i-1,])*exp(data_month[35+i,]))
  wealth_maxsharpe[i,1] <- wealth_maxsharpe[i-1,1]* sum(coredata(weight_maxsharpe[i-1,])*exp(data_month[35+i,]))
  
  benchmark[i,1] <- benchmark[[i-1,1]]*sum(policy_weight*exp(data_month[35+i,]))
}
