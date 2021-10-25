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

plot(fund.data, fund.daily.ret, main="Fund Log Returns", xlab="dates", ylab="log returns")

# 1(b)

mean(fund.monthly.ret)
var(fund.monthly.ret)
skewness(fund.monthly.ret)
kurtosis(fund.monthly.ret) + 3

# 1(c)

sd(bond.monthly.ret)
sd(fund.monthly.ret)
sd(sp500.monthly.ret)

mean(bond.monthly.ret)
mean(fund.monthly.ret)
mean(sp500.monthly.ret)

# 1(d)

kde <- density(fund.daily.ret)
plot(kde, main = "Fund KDE")
fund_mean <- mean(fund.daily.ret)
fun_var <- var(fund.daily.ret)


# 1(e)



# 2(a)



# 2(b)



# 2(c)


