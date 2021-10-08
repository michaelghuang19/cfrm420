# Michael Huang
# CFRM 420
# Homework 1

# 1(a)

library(quantmod)
msft.data <- getSymbols("MSFT", auto.assign=FALSE, from="2021-01-01", to="2021-09-01")
msft.prices <- msft.data$MSFT.Adjusted

daily_len <- length(msft.prices)
# get prev-day starting price data as list
msft.prev_daily <- coredata(msft.prices)[1:daily_len - 1]
msft.daily_returns <- (msft.prices[2:daily_len] - msft.prev_daily) / msft.prev_daily

plot(msft.daily_returns)

# 1(b)

msft.monthly_prices <- to.monthly(msft.prices, OHLC=FALSE)
msft.monthly_prices
monthly_len <- length(msft.monthly_prices)
msft.monthly_returns <- log(msft.monthly_prices[2:monthly_len] / coredata(msft.monthly_prices)[1:monthly_len - 1])
msft.monthly_returns
