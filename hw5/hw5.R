# Michael Huang
# CFRM 420
# Homework 5

# Question 1

library(IntroCompFinR)
library(PerformanceAnalytics)
sample <- "1998-01::2012-05"
sp500.daily.price <- sp500DailyPrices[sample]
sp500.daily.ret <- na.omit(Return.calculate(sp500.daily.price, method="log"))

sp500.yearly.ret <- c()
for (year in 1998:2012) {
  sp500.yearly.ret <- append(sp500.yearly.red, sum(sp500.daily.ret[as.character(year)]))
}

sp500_ret_mean <- mean(sp500.yearly.ret)
sp500_ret_var <- var(sp500.yearly.ret)
sp500_ret_sd <- sd(sp500.yearly.ret)

alpha <- 0.05
n <- 252

# set seed for consistent data
set.seed(123)

r <- rnorm(n=n, mean=sp500_ret_mean, sd=sp500_ret_sd)
ci <- sp500_ret_mean + c(-1,1)*qnorm(1-alpha/2)*sp500_ret_sd/sqrt(length(sp500.yearly.ret))

# Question 2

mu <- c(2, 3)
sigma <- cbind(c(1, 0.8), c(0.8, 2))

# 2(a)

# set seed for consistent data
set.seed(123)

for (n in c(20, 100, 500)) {
  
  rho_list <- c()
  
  for (i in 1:10000) {
    sample <- mvtnorm::rmvnorm(n=n, mean=mu, sigma=sigma)
    rho <- cor(sample)
    cor <- rho[1,2]
    
    rho_list <- append(rho_list, cor)
  }
  
  kde <- density(rho_list)
  
  plot(kde, main=paste0("kde with ", as.character(n), " samples"))
  
  bias <- (mean(rho_list) * 10000) - sum(rho_list) 
  print(paste("bias", bias))
  
  se <- sqrt(var(rho_list))
  print(paste("se", se))
}
