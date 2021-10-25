# Michael Huang
# CFRM 420
# Homework 2

# 1(a)
a_vector <- dpois(c(3,4,5), 2)
sum(a_vector)

# 1(b)
pnorm(-4)

# 1(c)
pt(-4*1.25, 10)

# 1(d)
ratio <- 0.0002686668 / 3.167124e-05
ratio

# 2(a)
qnorm(0.01, 0.0006, 0.02)

# 2(b)
exp(1)^(-0.04592696) * 10

# 3(a)
sigma <- cbind(c(1, -1.2), c(-1.2, 2))
eigen(sigma)$values

# 3(b)
mu <- cbind(1, 2)
mean <- mu[1] + mu[2]
mean

variance <- sigma[1,1] + sigma[2,2] + 2 * sigma[1,2]
variance