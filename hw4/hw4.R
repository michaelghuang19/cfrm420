# Michael Huang
# CFRM 420
# Homework 4

# 1(c)

z <- rnorm(1001)
y <- z[1:1000] * z[2:1001]
y2 <- y^2
acf(y2, lag.max = 1000)


