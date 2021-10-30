# Michael Huang
# CFRM 420
# Homework 4

# 1(c)

z <- rnorm(1001)
y <- z[1:1000] * z[2:1001]
y2 <- y^2
acf(y2, lag.max = 1000)

# 2(c)

ar1.acf <- ARMAacf(ar=0.4, lag.max=250)
ar1.model <- list(ar=c(0.4))
y <- arima.sim(model=ar1.model, n=250)
plot(y, type="l", main="2(c) AR(1) Simulation")

# 2(e)
# as adapted from lecture

n <- 250
phi <- 1.02
sigma.e <- 1
e <- c(NA,rnorm(n, sd=sigma.e)) # Makes epsilon_1 = e[2] so the index is consistent with y y <- rep(NA, n)
y[1] <- 0
for (i in 2:n) {
  y[i] <- phi*y[i-1]+e[i]
}
y <- y[-1]
plot(y, type="l", main="2(e) AR(1) Simulation")

