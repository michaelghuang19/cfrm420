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
bootcor <- boot(data, statistic=cor.stat, R=1000)
print(bootcor$t[1])

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
n <- length(data$MSFT)

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
beta = 100

pr.norm <- 0
pr.percent <- 0
pr.bca <- 0

for (trial in 1:n.mc) {
  x <- rexp(n.samp, rate=rate)
  bootmean <- boot(x, statistic=mean.boot, R=B)
  ci <- boot.ci(bootmean, conf=0.90, type=c("norm","perc","bca"))
  
  pr.norm <- pr.norm + (1/n.mc) * (ci$normal[2] <= beta && beta <= ci$normal[3])
  pr.percent <- pr.percent + (1/n.mc) * (ci$percent[4] <= beta && beta <= ci$percent[5])
  pr.bca <- pr.bca + (1/n.mc) * (ci$bca[4] <= beta && beta <= ci$bca[5])
}

print(pr.norm)
print(pr.percent)
print(pr.bca)
