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

ci <- boot.ci(bootcor, conf=0.95, type=c("norm","perc"))
print(ci)

# 1(e)
rho_hat <- cor(data)
n <- length(data)

# Question 2

set.seed(425)
scale <- 100; rate <- 1/scale
n.samp <- 30
x <- rexp(n.samp, rate=rate)

# 2(a)

B = 1000
mean.boot <- function(x, idx){
  mean(x[idx])
}

bootmean <- boot(x, statistic=mean.boot, R=B)
plot(bootmean)

# 2(b)

ci <- boot.ci(bootmean, conf=0.90, type=c("norm","perc","bca"))

# 2(c)

n.mc = 1000

ci.normal <- numeric(2)
ci.percent <- numeric(2)
ci.bca <- numeric(2)

for (trial in 1:n.mc) {
  x <- rexp(n.samp, rate=rate)
  bootmean <- boot(x, statistic=mean.boot, R=B)
  ci <- boot.ci(bootmean, conf=0.90, type=c("norm","perc","bca"))
  
  normal_lo <- ci$normal[2]
  normal_hi <- ci$normal[3]
  
  ci.normal[1] += (1/n.mc) * ci$normal[2]
  
  percent_lo <- ci$percent[4]
  percent_hi <- ci$percent[5]
  
  bca_lo <- ci$bca[4]
  bca_hi <- ci$bca[5]
}


