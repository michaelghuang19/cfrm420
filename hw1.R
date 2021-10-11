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

# 2
msft.data <- getSymbols("MSFT", auto.assign=FALSE, from="2021-01-01", to="2021-09-01")
msft.prices <- msft.data$MSFT.Adjusted
start_msft <- coredata(msft.prices['2021-01-04'])
mid_msft <- coredata(msft.prices['2021-05-03'])
end_msft <- coredata(msft.prices['2021-08-31'])
print(c(start_msft, mid_msft, end_msft))

aapl.data <- getSymbols("AAPL", auto.assign=FALSE, from="2021-01-01", to="2021-09-01")
aapl.prices <- aapl.data$AAPL.Adjusted
start_aapl <- coredata(aapl.prices['2021-01-04'])
mid_aapl <- coredata(aapl.prices['2021-05-03'])
end_aapl <- coredata(aapl.prices['2021-08-31'])
print(c(start_aapl, mid_aapl, end_aapl))

start_value <- 1000
start_msft_shares <- 0.6 * start_value / start_msft 
start_aapl_shares <- 0.4 * start_value / start_aapl
print(c(start_msft_shares, start_aapl_shares))

mid_value <- start_msft_shares * mid_msft + start_aapl_shares * mid_aapl
mid_value

mid_msft_shares <- 0.4 * mid_value / mid_msft
mid_aapl_shares <- 0.6 * mid_value / mid_aapl
print(c(mid_msft_shares, mid_aapl_shares))

end_value <- mid_msft_shares * end_msft + mid_aapl_shares * end_aapl
end_value

arithmetic <- (end_value - start_value) / start_value
logarithmic <- log(end_value / start_value)
print(c(arithmetic, logarithmic))

# 3
