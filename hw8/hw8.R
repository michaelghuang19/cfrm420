# Michael Huang
# CFRM 420
# Homework 8

# Question 1

# 1(a)

sol <- polyroot(c(1, -0.5))
Mod(sol)

# 1(b)

sol <- polyroot(c(1, 1.5, -0.5))
Mod(sol)

sol <- polyroot(c(1, 5, -8, 0, 0, 2))
Mod(sol)

# Question 2

# 2(c)

A.data <- c(1, 0.5, 0, 0.5, 1, 0, 0, 0.5, 1)
A <- matrix(A.data, nrow=3, ncol=3, byrow=TRUE)
A_inv <- solve(A)

c.data <- c(2.75, 0.5, -1)
c <- matrix(c.data, nrow=3, ncol=1, byrow=TRUE)

result <- A_inv %*% c

# 2(d)

rho_0 <- result[1] / result[1]
rho_1 <- result[2] / result[1]
rho_2 <- result[3] / result[1]
rho_3 <- -0.5*result[3] / result[1]
rho_4 <- -0.5*rho_3 / result[1]
rho_5 <- -0.5*rho_4 / result[1]

arma_result <- ARMAacf(-0.5, c(1, -1), lag.max=5)

# Question 3

library(Ecdat)
data <- diff(Tbrate[,1])

# 3(a)

acf <- acf(ts(data, frequency=1))
ci <- 1.96 * (1 / sqrt(length(data)))
x <- seq(-1, 24)
y <- rep(ci, length(x))
lines(x, y, col='blue')
lines(x, -y, col='blue')

# 3(b)




