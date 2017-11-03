GMMGaussian <- function(x){

  T <- length(x)
  
  mu_hat <- (1/T)*sum(x)
  sigma_hat <- sqrt((1/T)* sum(x^2)-mu_hat^2)
  
  mom <- c(x - mu_hat, x^2 - mu_hat^2 - sigma_hat^2)
  
  f <- matrix(mom, nrow = 2) 
  d <- matrix(c(-1,  0,  -2*mu_hat,  -2*sigma_hat), nrow =2, ncol =2)
  S <- (1/T)*(f%*%t(f))
  
  V <- solve(t(d) %*% solve(S) %*% d)
  
  result <- list(mu_hat = mu_hat, sigma_hat = sigma_hat,SE_mu = sqrt((1/T)*V[1,1]),SE_sigma = sqrt((1/T)*V[2,2]))
  
  return(result)
}