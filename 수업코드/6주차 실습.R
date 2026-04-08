#1
m <- 6
sigma <- 2
n <- 36
target <- 0.5
pnorm(6.5, mean = 6, sd = 2/sqrt(36))

#2
m <- 1200 
sigma <- 180
se <- sigma / sqrt(n)
pnorm(1230, mean = m, sd = se) - pnorm(1170, mean = m, sd = se)

#3 
m <- 72
sigam <- 8
n <- 100
target <- 1.5
se <- sigma / sqrt(n)
pnorm(m + 1.5, mean = m, sd = se) - pnorm(m - 1.5, mean = m, sd = se)

#4 
m <- 7 
sigma <- 1.5
n <- 49 
target <- 0.3
pnorm(m - 0.3, mean = m, sd = se)
