# Michael Huang
# CFRM 420
# Midterm

# Question 2
set.seed(493)

# 2(c)
n.sim = 10000
p0 <- cbind(10, 10)
mu <- cbind(0.05, 0.1)
covm <- cbind(c(0.002, 0.001), c(0.001, 0.003))
tt <- 12
below.margin <- numeric(n.sim) 
for(i in 1:n.sim) {
  r <- mvtnorm::rmvnorm(tt, mean=mu, sigma=covm)
  r0 <- rbind(log(p0), r)
  asset.prices <- exp(apply(r0, 2, cumsum))
  total.prices <- apply(asset.prices, 1, sum)
  below.margin[i] <- sum(total.prices < 60) > 0
}
sum(below.margin)
(pr <- sum(below.margin)/n.sim)

# Question 3
getwd()
setwd("/Users/mhuang19/Documents/uw_21.22/cfrm420/cfrm420/midterm")
data <- read.csv('data.csv')
