source("GMMGaussian.R")

N <- 10000
mu <- 0
sigma <- 1
T=500

coverage <- matrix(data = 0, nrow = 2, ncol = N)

for (n in 1:N) {
  x <- mu + sigma *rnorm(T,0,1)
  Results <- GMMGaussian(x)
  
  coverage[1,n] <- (abs(Results$mu_hat - mu) < 1.96 * Results$SE_mu)
  coverage[2,n] <- (abs(Results$sigma_hat - sigma) < 1.96 * Results$SE_sigma)
}

y <- rowMeans(coverage)