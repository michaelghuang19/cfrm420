# Michael Huang
# CFRM 420
# Homework 3

# Question 1 Setup
library(quantmod)
library(PerformanceAnalytics)
bond.data <- getSymbols("BSV", auto.assign=FALSE, from="2009-01-01",
                        to="2021-09-01")$BSV.Adjusted
fund.data <- getSymbols("FMAGX", auto.assign=FALSE, from="2009-01-01",
                        to="2021-09-01")$FMAGX.Adjusted
sp500.data <- getSymbols("^GSPC", auto.assign=FALSE, from="2009-01-01",
                         to="2021-09-01")$GSPC.Adjusted
bond.daily.ret <- na.omit(Return.calculate(bond.data, method="log"))
fund.daily.ret <- na.omit(Return.calculate(fund.data, method="log"))
sp500.daily.ret <- na.omit(Return.calculate(sp500.data, method="log"))
bond.monthly.ret <- na.omit(Return.calculate(to.monthly(bond.data, OHLC=FALSE),
                                             method="log"))
fund.monthly.ret <- na.omit(Return.calculate(to.monthly(fund.data, OHLC=FALSE),
                                             method="log"))
sp500.monthly.ret <- na.omit(Return.calculate(to.monthly(sp500.data, OHLC=FALSE),
                                              method="log"))

# 1(a)

plot(fund.daily.ret, main="Fund Log Returns", xlab="dates", ylab="log returns")

# 1(b)

mean(fund.monthly.ret)
var(fund.monthly.ret)
skewness(fund.monthly.ret)
kurtosis(fund.monthly.ret) + 3

# 1(c)

bond_sd <- sd(bond.monthly.ret)
fund_sd <- sd(fund.monthly.ret)
sp500_sd <- sd(sp500.monthly.ret)
sd_array <- c(bond_sd, fund_sd, sp500_sd)

bond_mean <- mean(bond.monthly.ret)
fund_mean <- mean(fund.monthly.ret)
sp500_mean <- mean(sp500.monthly.ret)
mean_array <- c(bond_mean, fund_mean, sp500_mean)

cor(sd_array, mean_array)

# 1(d)

kde <- density(fund.daily.ret)
plot(kde, main = "Fund KDE")
fund_mean <- mean(fund.daily.ret)
fun_var <- var(fund.daily.ret)
x <- seq(-1,1,len=1000)
norm_fit <- dnorm(x, mean = fund_mean, sd = sqrt(fun_var))
plot(kde, main = "Fund KDE with Norm Fit")
lines(x, norm_fit, col="blue",lwd=2)

# 1(e)

quantile(fund.daily.ret, 0.01)

# 2(a)

qqnorm(bond.monthly.ret, main = "Bond Monthly Returns QQ Plot")
qqline(bond.monthly.ret)

# 2(b)

x = c(0.25, 0.75)
qnorm(x, mean=0, sd=1)
q_25 <- quantile(bond.monthly.ret, 0.25)
q_75 <- quantile(bond.monthly.ret, 0.75)

