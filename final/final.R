library(boot)
library(Ecdat)
library(forecast)
library(IntroCompFinR)
library(PerformanceAnalytics)

### Question 1

library(forecast)
library(Ecdat)
inflat <- ts(Mishkin[,1], freq=1)

# 1(a)

auto.arima(inflat)
# Series: inflat 
# ARIMA(1,1,1) 
# 
# Coefficients:
#   ar1      ma1
# 0.2383  -0.8772
# s.e.  0.0550   0.0269
# 
# sigma^2 estimated as 8.587:  log likelihood=-1221.62
# AIC=2449.25   AICc=2449.29   BIC=2461.83

# 1(b)

Arima(inflat, order=c(1,1,0))
# Series: inflat 
# ARIMA(1,1,0) 
# 
# Coefficients:
#   ar1
# -0.3849
# s.e.   0.0420
# 
# sigma^2 estimated as 10.39:  log likelihood=-1268.34
# AIC=2540.68   AICc=2540.7   BIC=2549.07

Arima(inflat, order=c(0,1,1))
# Series: inflat 
# ARIMA(0,1,1) 
# 
# Coefficients:
#   ma1
# -0.7932
# s.e.   0.0355
# 
# sigma^2 estimated as 8.9:  log likelihood=-1230.85
# AIC=2465.7   AICc=2465.73   BIC=2474.09

Arima(inflat, order=c(2,1,2))
# Series: inflat 
# ARIMA(2,1,2) 
# 
# Coefficients:
#   ar1     ar2      ma1      ma2
# -0.3936  0.2095  -0.2531  -0.5612
# s.e.   0.3052  0.0756   0.3057   0.2636
# 
# sigma^2 estimated as 8.587:  log likelihood=-1220.61
# AIC=2451.21   AICc=2451.34   BIC=2472.18

model <- Arima(inflat, order=c(1,1,1))
# Series: inflat 
# ARIMA(1,1,1) 
# 
# Coefficients:
#   ar1      ma1
# 0.2383  -0.8772
# s.e.  0.0550   0.0269
# 
# sigma^2 estimated as 8.587:  log likelihood=-1221.62
# AIC=2449.25   AICc=2449.29   BIC=2461.83

# 1(d)

model_residuals <- residuals(model)
Box.test(model_residuals, lag=24, type="Ljung-Box", fitdf=1) 
# Box-Ljung test
# 
# data:  model_residuals
# X-squared = 30.251, df = 23, p-value = 0.1423

# 1(e)

forecast(model, h=10)
# Point Forecast       Lo 80    Hi 80     Lo 95     Hi 95
# 492       3.706101 -0.04934654 7.461548 -2.037360  9.449561
# 493       4.589298  0.59646732 8.582129 -1.517210 10.695807
# 494       4.799773  0.73061175 8.868933 -1.423472 11.023017
# 495       4.849930  0.72946543 8.970396 -1.451777 11.151638
# 496       4.861884  0.69566778 9.028099 -1.509794 11.233561
# 497       4.864732  0.65438693 9.075077 -1.574435 11.303899
# 498       4.865411  0.61165730 9.119165 -1.640144 11.370966
# 499       4.865573  0.56891110 9.162234 -1.705604 11.436750
# 500       4.865611  0.52648058 9.204742 -1.770516 11.501739
# 501       4.865620  0.48443580 9.246805 -1.834823 11.566064

### Question 2

set.seed(602)
library(boot)
library(IntroCompFinR)
library(PerformanceAnalytics)
time <- "1998-01::2012-05"
msft.daily.price <- msftDailyPrices[time]
msft.daily.ret <- na.omit(Return.calculate(msft.daily.price, method="log"))
sp500.daily.price <- sp500DailyPrices[time]
sp500.daily.ret <- na.omit(Return.calculate(sp500.daily.price, method="log"))
ret <- merge(msft.daily.ret,sp500.daily.ret)

# 2(a)(i)

sigma_diff <- function(x, idx){
  sigma1 <- sd(x[idx, 1])
  sigma2 <- sd(x[idx, 2])
  result <- sigma1 - sigma2
    
  return(result)
}

boot_result <- boot(ret, statistic=sigma_diff, R=5000) 
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
# boot(data = ret, statistic = sigma_diff, R = 5000)
# 
# 
# Bootstrap Statistics :
#   original        bias    std. error
# t1* 0.008198634 -3.923461e-06 0.000491408

# 2(a)(ii)

boot.ci(boot_result, conf=0.90, type=c("norm", "perc")) 
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 5000 bootstrap replicates
# 
# CALL : 
#   boot.ci(boot.out = boot_result, conf = 0.9, type = c("norm", 
#                                                        "perc"))
# 
# Intervals : 
#   Level      Normal             Percentile     
# 90%   ( 0.0074,  0.0090 )   ( 0.0074,  0.0090 )  
# Calculations and Intervals on Original Scale

# 2(a)(iii)

plot(boot_result)
# see attached.

# 2(b)

set.seed(816)
n.sim <- 10000
m.ret <- y.ret <- numeric(n.sim)
for(i in 1:n.sim){
  m.ret[i] <- sum(sample(sp500.daily.ret, size=21, replace=TRUE))
  y.ret[i] <- sum(sample(sp500.daily.ret, size=252, replace=TRUE))
}

library(tseries)
jarque.bera.test(m.ret) 
# Jarque Bera Test
# 
# data:  m.ret
# X-squared = 49.864, df = 2, p-value = 1.486e-11
jarque.bera.test(y.ret)
# Jarque Bera Test
# 
# data:  y.ret
# X-squared = 2.6632, df = 2, p-value = 0.2641

### Question 3

# 3(b)(i)

set.seed(438)
r <- 0.0132+0.0455*rt(n=60, df=10)

t.test(x=r,mu=0)
# One Sample t-test
# 
# data:  r
# t = 1.5595, df = 59, p-value = 0.1242
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.003282982  0.026478995
# sample estimates:
#   mean of x 
# 0.01159801 

# p-value is greater than 0.05, so not rejected at 5%

# 3(b)(ii)

rnorm_fixed <- function(n, mu, sd) {
  mu + sd*scale(rnorm(n))
}
r <- rnorm_fixed(98, 0.0116, 0.0576)
print(mean(r))
print(sd(r))

t.test(x=r,mu=0)
# One Sample t-test
# 
# data:  r
# t = 1.9936, df = 97, p-value = 0.049
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.0000519305 0.0231480695
# sample estimates:
#   mean of x 
# 0.0116 

# gives us p-value of 0.049 < 0.05

### Question 4
