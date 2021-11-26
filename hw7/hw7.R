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

library(IntroCompFinR)
library(PerformanceAnalytics)
sample <- "1998-01::2012-05"
msft.daily.price <- msftDailyPrices[sample]
msft.daily.ret <- na.omit(Return.calculate(msft.daily.price, method="log"))


