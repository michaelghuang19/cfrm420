# Michael Huang
# CFRM 420
# Homework 6

# Question 1

set.seed(633)

library(IntroCompFinR)
library(PerformanceAnalytics)
sample <- "1998-01::2012-05"
msft.daily.price <- msftDailyPrices[sample]
msft.daily.ret <- na.omit(Return.calculate(msft.daily.price, method="log"))
sp500.daily.price <- sp500DailyPrices[sample]
sp500.daily.ret <- na.omit(Return.calculate(sp500.daily.price, method="log"))
data <- merge(msft.daily.ret,sp500.daily.ret)

# 1(a)

cor.stat <- function(x,i){
  cor(x[i,1],x[i,2])
}

library(boot)
bootcor <- boot(data, statistic=cor.stat, R=1)
print(bootcor)

# 1(b)

B <- 10000

bootcor <- boot(data, statistic=cor.stat, R=B)
print(bootcor)

# 1(c)

plot(bootcor)

# 1(d)
ci.q <- quantile(bootcor, prob=c(0.025,0.975))
print(ci.q)
ci.n <- cor.stat(data) + c(-1,1)*qnorm(0.975)*sd(bootcor)
print(ci.n)

# Question 2

set.seed(425)
scale <- 100; rate <- 1/scale
n.samp <- 30
(x <- rexp(n.samp, rate=rate))

# 2(a)

# boot.ci
type=c("norm","perc","bca")
