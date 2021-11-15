# Michael Huang
# CFRM 420
# Homework 5

# Question 1

library(IntroCompFinR)
library(PerformanceAnalytics)
sample <- "1998-01::2012-05"
sp500.daily.price <- sp500DailyPrices[sample]
sp500.daily.ret <- na.omit(Return.calculate(sp500.daily.price, method="log"))

sp500_mean <- mean(sp500.daily.ret)
sp500_var <- var(sp500.daily.ret)

alpha <- 0.05
n <- 252

sp500_sd <- sqrt(sp500_var)
r <- rnorm(n=n, mean=sp500_mean, sd=sp500_sd)

ci <- sp500_mean+c(-1,1)*qnorm(1-alpha/2)*sd(r)/sqrt(n)

# Question 2

mu <- c(2, 3)
sigma <- cbind(c(1, 0.8), c(0.8, 2))

# 2(a)

# set seed for consistent data
set.seed(123)

for (n in c(20, 100, 500)) {
  sample <- mvtnorm::rmvnorm(n=n, mean=mu, sigma=sigma)
  rho <- cor(sample)
  kde <- density(rho)
  
  # fix cov here, should just be list of index 1/1
  plot(kde, main=paste0("kde with ", as.character(n), " samples"))
  
  bias <- (mean(rho) * n) - sum(rho)
  print(bias)
  
  se <- var(rho)
  print(se)
}

  
  

