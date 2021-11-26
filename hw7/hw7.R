# Michael Huang
# CFRM 420
# Homework 7

# Question 1

# 1(a)

r1 <- c(9.5,17.6,25.1,-5.0,29.0,13.6,11.7,8.5,19.5,6.7,23.5,22.5) / 100
r2 <- c(-3.8,14.0,16.3,-0.7,32.9,4.9,3.8,6.6,4.7,-0.4,8.3,24.8) / 100

alpha = 0.05

t.test(r1, r2, paired = TRUE, alternative = "greater")

# 1(b)

mu0 = 0.16

t.test(x=r2, mu=mu0)

# Question 2

# 2(b)

sims <- 10000
n <- 100
mu <- 0
sigma_sq <- 1

p_vals <- numeric(10000)

for (i in 1:sims) {
  x <- rnorm(n, mean=mu, sd=sigma_sq)
  
  test <- t.test(x)
  p_vals[i] <- test$p.value
}

hist(p_vals, plot=TRUE)

# 2(c)

mu <- 0.2

p_vals <- numeric(10000)

for (i in 1:sims) {
  x <- rnorm(n, mean=mu, sd=sigma_sq)
  
  test <- t.test(x)
  p_vals[i] <- test$p.value
}

hist(p_vals, plot=TRUE)

# 2(d)

mu <- 0.05

p_vals <- numeric(10000)

for (i in 1:sims) {
  x <- rnorm(n, mean=mu, sd=sigma_sq)
  
  test <- t.test(x)
  p_vals[i] <- test$p.value
}

hist(p_vals, plot=TRUE)

# 2(e)

mu <- 0.05
n <- 200

p_vals <- numeric(10000)

for (i in 1:sims) {
  x <- rnorm(n, mean=mu, sd=sigma_sq)
  
  test <- t.test(x)
  p_vals[i] <- test$p.value
}

hist(p_vals, plot=TRUE)

# 2(f)

library(IntroCompFinR)
library(PerformanceAnalytics)
sample <- "1998-01::2012-05"
msft.daily.price <- msftDailyPrices[sample]
msft.daily.ret <- na.omit(Return.calculate(msft.daily.price, method="log"))

ret <- coredata(msft.daily.ret)
ret.mean <- mean(ret)
ret.sd <- sd(ret)

ret <- ret - ret.mean
ret <- ret / ret.sd

n <- 100

p_vals <- numeric(10000)

for (i in 1:sims) {
  x <- sample(ret, n, replace=TRUE)
  x <- coredata(x)
  
  test <- t.test(x)
  p_vals[i] <- test$p.value
}

hist(p_vals, plot=TRUE)

# 2(g)

ret <- ret + 0.05

p_vals <- numeric(10000)

for (i in 1:sims) {
  x <- sample(ret, n, replace=TRUE)
  
  test <- t.test(x)
  p_vals[i] <- test$p.value
}

hist(p_vals, plot=TRUE)



